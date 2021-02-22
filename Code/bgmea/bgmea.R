# check if packages are installed and install/load them
pkglist <- c('dplyr', 'readxl', 'stringr', 'reshape2', 'assertthat')
for(packagename in pkglist) {
  if(!require(packagename, character.only = TRUE)) install.packages(packagename)
  require(packagename, character.only = TRUE)
}
# set working directory
wd <- '~/Insync/sei2112@columbia.edu/Google Drive - Shared with me/Sarah Summer 2020/COVID-19/'
setwd(wd)
inpath <- "Data/raw/bgmea/BGMEA Members' List_2020_07_18_merge_online_list.xlsx"
outpath <- 'Data/intermediate/bgmea/bgmea.csv'

dat <- read_excel(inpath)
dat <- dat %>%
  rename_all(str_to_lower) %>% # make all variable names lowercase
  rename_all(~str_replace_all(., '[:punct:]', '')) %>% # strip punctuation
  rename_all(~str_replace_all(., ' ', '_')) %>% # replace whitespace with underscore
  rename_at(11:ncol(dat), ~paste0('public_', .)) %>% # append 'public' to vars from public dataset
  rename(public_email1 = public_email_adddress, # rename public vars to match private
         public_unit_name = public_membercompany_name,
         public_bcode = public_bgmea_reg_no
         ) %>%
  mutate(mailing_address = paste0(officeadd1, ', ', officeadd2, ', ', officezone), # create concatenated address variable to compare between public and private
         public_year_of_establishment = as.numeric(str_extract(public_date_of_establishment, '[0-9]{4}$')), # extract year of establishment
         public_year_of_establishment = ifelse(public_year_of_establishment <=2020, public_year_of_establishment, NA),
         bcode_coalesce = coalesce(bcode, public_bcode))

initNrow <- nrow(dat)
initNBcode <- length(unique(dat$bcode_coalesce))

# clean factory types
cleanCodes <- function(var) {
  types <- str_split(dat[[var]], '[,1]') %>%
    lapply(str_to_lower) %>%
    lapply(str_trim)
  names(types) <- paste0(dat$bcode_coalesce, '_')
  types <- unlist(types) 
  data.frame(types = types, bcode_coalesce = str_replace(names(types), '_.*', '')) %>%
    filter(types != '') %>%
    return()
}
dat <- cleanCodes('public_factory_type') %>%
    mutate(factoryTypes = ifelse(types == 'knit_composite', 
                                 'public_factory_type_knit', 
                                 paste0('public_factory_type_', types))) %>%
  select(-types) %>%
  dcast(bcode_coalesce ~factoryTypes, fun.aggregate = length) %>%
  merge(dat, by = 'bcode_coalesce', all.y = TRUE)

# assertions to make sure that nothing has gone wrong with the merge
assert_that(nrow(dat) == initNrow)
assert_that(length(unique(dat$bcode_coalesce)) == initNBcode)
assert_that(all(is.na(dat$public_factory_type[is.na(dat$public_factory_type_knit)])))
assert_that(all(is.na(dat$public_factory_type[is.na(dat$public_factory_type_sweater)])))
assert_that(all(is.na(dat$public_factory_type[is.na(dat$public_factory_type_woven)])))

# clean principal product
dat <- cleanCodes('public_principal_exportable_product') %>% 
  group_by(bcode_coalesce) %>%
  mutate(public_product_type = paste0('public_product_type', 1:n())) %>%
  dcast(bcode_coalesce ~ public_product_type, value.var = 'types') %>%
  merge(dat, by = 'bcode_coalesce', all.y = TRUE)
# assertions to make sure that nothing has gone wrong with the merge
assert_that(nrow(dat) == initNrow)
assert_that(length(unique(dat$bcode_coalesce)) == initNBcode)
assert_that(all(is.na(dat$public_principal_exportable_product[is.na(dat$public_product_type1)])))

write.csv(dat, outpath)
