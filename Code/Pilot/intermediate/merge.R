
outpath <- 'Data/intermediate/mergedSurveys2.csv'

survey <- read_excel('Data/raw/2017_survey_data/Bangladesh Garments Labour Survey-2017.xlsx')

# read in Stata file. There is some issue with the factor labels here,
# which takes a little bit of fixing
df <- read.dta('Data/raw/2017_survey_data/data_NK_2017.dta', convert.factors = FALSE)

labTab <- attr(df, 'label.table')
for(var in names(labTab)) {
  lab <- labTab[[var]]
  if(length(unique(names(lab))) < length(lab)) {
    t <- table(names(lab))
    problems <- names(t[t > 1])
    for(problem in problems) {
      names(lab)[names(lab) == problem] <- paste0(problem, 1:t[problem])
    }
    labTab[[var]] <- lab
  }
}


for(var in colnames(df)) {
   if(var %in% names(labTab) & length(labTab[[var]]) > 1 & var != 'q3_6') {
    lab <- labTab[[var]]
    df[,var] <- factor(df[,var], levels = fixEncoding(lab), labels = fixEncoding(names(lab)))
   } else if(is.numeric(df[,var]) & var != 'qid') {  # deal with the fact that many numeric vars have 99 coded as dk
    replace <- ifelse(var == 'q5_8', 88, 99)
    #df[,paste0(var, '_oth')] <- (df[,var] == replace & !is.na(df[,var]))
    df[df[,var] == replace& !is.na(df[,var]),var] <- NA
  }
}

# make sure that there is a difference of 150  in the number of IDs
assert_that(sum(!(df$qid %in% survey$Idno)) == 150)

df <- df %>% merge(survey, by.x = 'qid', by.y = 'Idno')
assert_that(nrow(df) == nrow(survey))
assert_that(length(unique(df$qid)) == length(unique(survey$Idno)))

write.csv(df, outpath)
