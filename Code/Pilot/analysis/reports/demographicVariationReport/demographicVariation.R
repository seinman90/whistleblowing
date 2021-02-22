library(readxl)
library(dplyr)
library(readstata13)
library(assertthat)
wd <- '~/Insync/sei2112@columbia.edu/Google Drive - Shared with me/Sarah Summer 2020/COVID-19/'
setwd(wd)

# these are the IDs for the factories we are interested in
factories <- c(90, 63, 13)

survey2017 <- read.dta13('Data/raw/2017_survey_data/data_NK_2017 (2).dta') %>%
  filter(factorycode %in% factories)

source('Code/Pilot/analysis/summaryStats.R')
# manually fix too-short labels
labs <-  varlabel(survey2017)
labs['q15_8'] <- "Do you know whether any incident of sexual harassment in this factory has been raised with the WPC or the management?"
labs['q15_9'] <- "Do you know whether any incident of sexual harassment in this factory has been raised with the government’s department of labour inspector in the past 12 months?"
labs[paste0('q14_1_', 1:7)] <- paste0('When workers have complaints, they raise them at ', labs[paste0('q14_1_', 1:7)])

newMulti <- function(d, var) {
  #obs <- sum(!is.na(d[[paste0(var, '1')]]))
  l <- ifelse(paste0(var, '1') %in% names(labs), labs[paste0(var, '1')], labs[paste0(var, '_1')])
  obs <- nrow(d)
  d %>%
    analyseMulti(var) %>%
    mutate(v1 = l)%>%
    return()
}

analyseQ3 <- function(d = survey2017) {

  analyseNumeric(d, 'q3_1') %>% mutate(v1 = 'Respondent age') %>%
    rbind(
      analyseNumeric(d %>% mutate(q3_2 = as.numeric(q3_2) - 1), 
                     'q3_2') %>% mutate(v1 = 'Female')
    ) %>%
    rbind(
      analyseNumeric(d, 'q3_3') %>% mutate(v1 = 'Years of schooling')
    )%>%
    rbind(
      d %>% 
        mutate(v = as.character(q3_4) == 'married') %>%
        analyseNumeric('v') %>%
        mutate(v1 = 'Married')
    ) %>%     rbind(
      d %>% 
        mutate(v = as.character(q3_4) == 'separated/divorced/widow') %>%
        analyseNumeric('v') %>%
        mutate(v1 = 'Separated, divorced or widow')
    ) %>%
    select(-Median) %>%
    return()
}

analyseQ7 <- function(d = survey2017) {

  d %>% group_by(Response = q7_11s) %>% summarise(n = n()) %>% ungroup() %>% mutate(p = n/sum(n), v1 = 'Section') %>%
    rbind(
      d %>% group_by(Response = q7_12d) %>% summarise(n = n())  %>% ungroup() %>% mutate(p = n/sum(n), v1 = 'Designation')
    ) %>%
    return()
}



getEmp <- function(d, var) {
  d %>% 
    analyseCat(var) %>%
    mutate(v1 = labs[var]) %>%
    return()
}

analyseQ9.1 <- function(d = survey2017) {
  
  res <- getEmp(d, 'q9_1') 
}

analyseQ9.2 <- function( d = survey2017) {
  res <- getEmp(d, 'q9_2_1')
  for(var in paste0('q9_2_', 2:6)) {
    res <- res %>% rbind(getEmp(d, var) %>% arrange(Response))
  }
  return(res)
}

analyseQ10.1 <- function(d = survey2017) {
  res <- getEmp(d, 'q10_1_1')
  for(var in paste0('q10_1_', 2:5)) {
    res <- res %>% rbind(getEmp(d, var) %>% arrange(Response))
  }
  return(res)
}

analyseQ10 <- function(d = survey2017) {
  
  res <- data.frame()
  for(var in paste0('q10_', 2:4)) {
    d[is.na(d[,var]),var] <- levels(d[,var])[length(levels(d[,var]))] 
    res <- res %>% rbind(getEmp(d, var) %>% arrange(Response))
  }
  res <- res %>%
    rbind(
      d %>%
      newMulti('q10_5')
    ) %>%
    rbind(getEmp(d, 'q10_6')) %>%
    rbind(
      d %>%
        newMulti('q10_7')
    ) %>%
    rbind(getEmp(d, 'q10_8'))
  
  # # deal with NA values here
  # for(var in c('q10_91', 'q10_92')) {
  #   levels(d[,var]) <- c(levels(d[,var]), 'none')
  #   d[is.na(d[[var]]), var] <- 'none'
  # }
  res <- res %>%
    rbind(
      d %>%
        newMulti('q10_9')
    ) 
  
  levels(d$q10_10) <- c(levels(d$q10_10), 'never')
  d$q10_10[is.na(d$q10_10)] <- 'never'
  res <- res %>%
    rbind(getEmp(d,  'q10_10'))
  
  return(res)
}

analyseQ10.12 <- function( d = survey2017) {

  res <- getEmp(d, 'q10_12') %>%
    rbind(
      getEmp(d, 'q10_16')
    ) 
  
  return(res)
}

analyseYesNo <- function(d, var) {
  d[!(d[,var] %in% c('yes', 'no')),var] <- NA
  res <- data.frame(v1 = var, n = sum(d[,var] == 'yes', na.rm = TRUE), p = mean(d[,var] == 'yes', na.rm = TRUE))%>%
    mutate(v1 = labs[var], Response = '% saying yes') 
  return(res)
}

analyseQ14.1 <- function(d = survey2017) {
  res <- getEmp(d, 'q14_1_1')
  for(var in paste0('q14_1_', 2:7)) {
    res <- res %>%
      rbind(getEmp(d, var))
  }
  return(res)
}

analyseQ14 <- function(d = survey2017) {

  res <-analyseYesNo(d, 'q14_2') %>%
    rbind(
      getEmp(d, 'q14_5')
    )
  for(var in c(paste0('q14_', 6:8), 'q14_10')) {
    res <- res %>%
      rbind(analyseYesNo(d, var))
  }
  for(var in paste0('q14_11_', 1:5)) {
    res <- res %>%
      rbind(getEmp(d, var))
  }
  res <- res %>%
    rbind(
      analyseYesNo(d, 'q14_12')
    ) %>%
    rbind(getEmp(d, 'q14_13')) %>%
    rbind(analyseYesNo(d, 'q14_14'))
  for(var in paste0('q14_', 15:17)) {
    res <- res %>% rbind(getEmp(d, var))
  }
  res <- res %>% rbind(analyseYesNo(d, 'q14_18'))
  
  return(res)
}

analyseQ15 <- function( d = survey2017) {

  analyseYesNo(d, 'q15_7') %>%
    rbind(
      analyseYesNo(d, 'q15_8')
    ) %>%
    rbind(
      analyseYesNo(d, 'q15_9')
    ) %>%
    return()
}

analyseQ17 <- function( d = survey2017) {

  res <- getEmp(d, 'q17_1_1')
  for(var in c(paste0('q17_1_', 2:13), 'q17_2')) {
    res <- res %>%
      rbind(getEmp(d, var))
  }
  return(res)
}

analyseQ18 <- function(d = survey2017) {
 
  res <- getEmp(d, 'q18_1_1')%>% mutate(v1 = paste0('How often do you feel ', v1, '?'))
  for(var in paste0('q18_1_', 2:6)) {
    res <- res %>%
      rbind(
        getEmp(d, var) %>% mutate(v1 = paste0('How often do you feel ', v1, '?'))
      )
  }
  # it seems that the labels haven't come through here correctly, manually fix
  for(i in 1:3) {
    var <- paste0('q18_2', i)
    d[d[[var]] > 9 & !is.na(d[[var]]),var]  <- 9
    d[,paste0('q18_2', i)] <- factor(d[,var], 
                      levels = 1:9, 
                      labels = c('Good behaviour of management',
                                 'Management look out for workers benefit and problems',
                                 'Good annual pay raise',
                                 'Fair salary',
                                 'Festival leave',
                                 'Paid leave',
                                 'Auto machine',
                                 'Safe building',
                                 'Other'))
  }
  res <- newMulti(d, 'q18_2') %>% 
    rbind(res)
                                           
  return(res)
}

makeFullTable <- function(fun, df = survey2017, cap = '', long = FALSE, colWidths = NA, genderSplit = FALSE, sewing = FALSE) {
  f <- get(fun)

  for(i in factories) {
    dfi <- df %>% 
      filter(factorycode == i)
    resi <- dfi %>% f()
    mergeVars <- 'v1'
    levels <- lapply(resi, levels)
    levels[['v1']] <- unique(resi$v1)
    if('Response' %in% names(resi)) mergeVars <- c('v1', 'Response')
    if(genderSplit) {
      resM <- dfi %>%
        filter(q3_2 == 'male') %>%
        f()
      resF <- dfi %>%
        filter(q3_2 != 'male') %>%
        f()
      resi <- resM %>% merge(resF, by = mergeVars, suffixes = c('zMale', 'zFemale'))
      header2 <- c(' ' = length(mergeVars),
                   rep(
                     c('Male' = ncol(resM) - length(mergeVars),
                       'Female' = ncol(resF) - length(mergeVars)), length(factories))
      )
    } else if(sewing) {
      resS <- dfi %>%
        filter(q7_11s == 'sawing/making') %>%
        f()
      header2 <- c(' ' = length(mergeVars),
                   rep(
                     c('All' = ncol(resi) - length(mergeVars),
                       'Sewing' = ncol(resS) - length(mergeVars)), length(factories))
      )
      resi <- resi %>% merge(resS, by = mergeVars, suffixes = c('zAll', 'zSewing'))
    }
    assign(paste0('res', i), resi)
    assign(paste0('n', i), nrow(dfi))
  }
  footer <- paste0('Factory 90: ', n90, ' obs, factory 63: ', n63, ' obs, factory 13: ', n13, ' obs')

  res <- res90 %>% 
      merge(res63, by = mergeVars) %>%
      merge(res13, by = mergeVars)
  header1 <- c(' ' = length(mergeVars),
                'Factory 90' = ncol(res90) - length(mergeVars),
                'Factory 63' = ncol(res63) - length(mergeVars),
                'Factory 13' = ncol(res13) - length(mergeVars))

  # correct factor levels
  for(var in mergeVars) {
    if(!is.null(levels[[var]])) {
      res[[var]] <- factor(res[[var]], levels = levels[[var]])
    } else res[[var]] <- factor(res[[var]], levels = unique(res[[var]]))
  }
  # make sure answers appear in correct order
  if('Response' %in% mergeVars) {
    res <- res %>% arrange(v1, Response)
  } else res <- res %>% arrange(v1)
  k <- kable(res,
        booktabs = TRUE,
        digits = 2,
        longtable = long,
        caption = cap,
        col.names = getColNames(resi, mergeVars, factory)) %>%
    kable_styling(latex_options =c("striped", "hold_position", 'repeat_header'))
  # deal with column spacing
  for(i in 1:length(mergeVars)) {
    w <-  ifelse(all(is.na(colWidths)), '7em', colWidths[i])
    k <- k %>% column_spec(i, width = w)
  }
  k <- k %>%
    collapse_rows(columns = 1)
  # add headers and footer
  if(genderSplit | sewing) k <- k %>% add_header_above(header2)
  k <- k %>%add_header_above(header1) %>%
    footnote(footer)
  # manual fix to prevent boxes overrunning
  if(fun == 'analyseQ10') {
    k <- k %>% 
      row_spec(c(20, 25),
              extra_latex_after = '[1cm]', 
              hline_after = FALSE)
  }

  print(k)
}

getColNames <- function(df, mergeVars, genderSplit) {
  responseNames <- setdiff(names(df), mergeVars) %>%
    str_replace('z.*', '')
  responseNamesAll <- rep(responseNames, 3)
  fullHeader <- c(rep('', length(mergeVars)), responseNamesAll)
  return(fullHeader)
}
