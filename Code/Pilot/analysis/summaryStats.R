
if(!('foreign' %in% rownames(installed.packages()))) install.packages(foreign); library(foreign)
if(!('dplyr' %in% rownames(installed.packages()))) install.packages(dplyr); library(dplyr)
if(!('assertthat' %in% rownames(installed.packages()))) install.packages(assertthat); library(assertthat)
if(!('reshape2' %in% rownames(installed.packages()))) install.packages(reshape2); library(reshape2)
if(!('lubridate' %in% rownames(installed.packages()))) install.packages(lubridate); library(lubridate)
if(!('ggplot2' %in% rownames(installed.packages()))) install.packages(ggplot2); library(ggplot2)
if(!('stringr' %in% rownames(installed.packages()))) install.packages(stringr); library(stringr)
if(!('knitr' %in% rownames(installed.packages()))) install.packages(knitr); library(knitr)
if(!('kableExtra' %in% rownames(installed.packages()))) install.packages(kableExtra); library(kableExtra)
if(!('readxl' %in% rownames(installed.packages()))) install.packages(readxl); library(readxl)


# takes in a dataframe and figures out 
# allows the possibility to manually override - 'exceptions' should be a named list
# of overrides of the form (questionName = Type)
getVarList <- function(df, exceptions = NULL) {

  classes <- getQuestionClasses(df)

  multiVarList <- getMultiVarList(classes)

  catVarList <- getCatVarList(classes, multiVarList)

  numericVarList <- getNumericVarList(classes)

  varLists <- handleExceptions(multiVarList, catVarList, numericVarList, exceptions)
  
  return(varLists)
}

getQuestionClasses <- function(df) {
  classes <- df %>% 
    summarise_all(class) %>% 
    melt(id.vars = NULL) %>%
    filter(str_detect(as.character(variable), '^q'))
  return(classes)
}

# this function allows us to manually recode certain variables where the algorithm
# which automatically detects how it should be analysed makes mistakes
handleExceptions <- function(multiVarList, catVarList, numericVarList, exceptions) {
  ret <- list()
  allVars <- c(multiVarList, catVarList, numericVarList)
  
  for(exceptionName in names(exceptions)) {
    exception <- exceptions[[exceptionName]]
    if(exceptionName == 'multi') {
      multiVarList <- c(multiVarList, exception)
      exception <- lapply(exception, function(x) return(allVars[str_detect(allVars, paste0(x, '_{0,1}[0-9]+$'))])) %>% unlist()
    } 
    catVarList <- setdiff(catVarList, exception)
    numericVarList <- setdiff(numericVarList, exception)
    if(exceptionName == 'numeric') {
      numericVarList <- c(numericVarList, exception)
    } else if(exceptionName == 'categorical') {
      catVarList <- c(catVarList, exception)
    } else{
      ret[[exceptionName]] <- exception
    }
  }
  ret[['multi']] <- multiVarList
  ret[['cat']] <- catVarList
  ret[['num']] <- numericVarList
  return(ret)
}

# this function detects which variables are multiple choice. Does it work? Seems to. Will it continue to work?
# That is what we wait with baited breath to discover.
getMultiVarList <- function(classes) {
  multi <-  classes %>%
    mutate(q = str_extract(variable, '_[0-9]+'),
           q = str_replace(q, '_', ''),
           q = as.numeric(q),
           top = str_extract(variable, 'q[0-9]+'),
           chr_end = str_detect(variable, '[a-z]$')) %>%
    group_by(top) %>% 
    mutate(lQ = lag(q, default = 0),
           lChr = lag(chr_end, default = FALSE),
           diff = (q != (lQ + 1)) & !chr_end & lQ < q,
           group = cumsum(diff)) %>%
    group_by(group) %>%
    mutate(possMulti = max(diff)) %>%
    group_by(top, group) %>%
    mutate(nGroup = n()) %>%
    filter(nGroup > 1, !chr_end, possMulti == TRUE, diff == TRUE)
  assert_that(all(str_detect(multi$variable, '1$')), msg = 'getMultiVarList Error: variable not ending in _1')
  multiVarList <- multi$variable %>% str_replace('1$', '')  
  return(multiVarList)
}

# this function gets a list of categorial variables to analyse
getCatVarList <- function(classes, multiVarList) {
  catVar <- classes$variable[classes$value %in% c('factor', 'logical', 'character')] %>% as.character()
  multi <- sapply(multiVarList, str_detect, string = catVar) %>%
    apply(1, max)
  catVar <- catVar[!multi]
  return(catVar)
}

# this function gets a list of numeric variables to analyse
getNumericVarList <- function(classes) {
  numVarList <- classes$variable[classes$value %in% c('integer', 'numeric')]
}

# this function runs the analysis
analysis <- function(dat, vars, sepVar = NA, questionLabs, idVar, 
                     nonMissingVarList, catVarList, numericVarList, multiVarList, 
                     rankVarList = NULL, specialVarList = NULL,
                     debug = FALSE) {
  
  # this will collect the variables that skipped being analysed
  unanalysed <- c()
  
  # loop through variables, performing correct analysis for each one
  iter <- 1
  for(var in vars) {
    if(debug) {
      print(var)
    }
    if(iter %% 4 == 0) {
      if(debug) {
        print(iter)
      }
      cat("\n\n\\pagebreak\n")
      cat("\n\n\\clearpage\n")
    }
    
    # analyse categorical variables
    if(var %in% catVarList) {
      res <- analyseCat(dat, var)
      seps <- c()
      if(!is.na(sepVar) & var != sepVar) { #sepVar handles if we want to look at different bits of the data separately, e.g comparing men to women
        seps <- levels(dat[[sepVar]])
        for(sep in seps) {
          if(!all(is.na(dat[dat[,sepVar] == sep,var]))) {
            res2 <- analyseCat(dat[(dat[[sepVar]]== sep),], 
                               var)
            res <- res %>% 
              merge(
                res2, suffixes = c('', sep),
                by = 'Response', all.x = TRUE
              )
          } else seps <- setdiff(seps, sep)
        }
      }
      res %>%
        arrange(Response) %>%
        customKable(var,  col.names = c('Response', rep(c('n', 'p'), length(seps) + 1)),
                  questionLabs, addHeader =  !is.na(sepVar), 
                  headerCols = c (1, rep(2, length(seps) + 1)),
                  headerColNames = c(' ', 'All', seps))
      iter <- iter + 1
    } else if(var %in% numericVarList) {
      res <- analyseNumeric(dat, var)
      if(!is.na(sepVar)) {
        seps <- levels(dat[,sepVar])
        res <- res %>% mutate(Sector = 'All')
        for(sep in seps) {
          if(!all(is.na(dat[dat[,sepVar] == sep,var]))) {
            res <- res %>%
              rbind(
                analyseNumeric(dat[dat[,sepVar] == sep,], var) %>% mutate(Sector = sep)
              )
          } else seps <- setdiff(seps, sep)
        }
      }
      customKable(res, col.names = names(res), var, questionLabs)
    } else if(any(sapply(multiVarList, function(x) str_detect(var, x)))) {
      v <- multiVarList[sapply(multiVarList, function(x) str_detect(var, x))]
      if(var == paste0(v, '1') | var == paste0(v, '_1')) { # only analyse the first of a list of multi-choice variables
        res <- analyseMulti(dat, v, idVar) 
        seps <- c()
        if(!is.na(sepVar) & var != sepVar) {
            seps <- levels(dat[[sepVar]])
            for(sep in seps) {
              if(!all(is.na(dat[dat[[sepVar]] == sep,var]))) {
                res <- res %>% 
                  merge(
                    analyseMulti(dat[dat[[sepVar]] == sep,], v, idVar), 
                    by = 'Response', all = TRUE, suffixes = c('', sep)
                  )
            } else seps <- setdiff(seps, sep)
          }
        }
        res %>%
          arrange(Response) %>%
          customKable(v, col.names = c('Response', rep(c("n", 'p'), length(seps) + 1)),
                        questionLabs, addHeader = !is.na(sepVar), 
                    headerCols = c(1, rep((ncol(res) - 1)/(length(seps) + 1), length(seps) + 1)),
                    headerColNames = c(' ', 'All', as.character(seps)))
      }
    } else if(any(sapply(rankVarList, function(x) str_detect(var, x)))) {
      if(str_detect(var, '1$')) {
        res <- analyseRank(dat, rankVarList[sapply(rankVarList, function(x) str_detect(var, x))]) 
        res %>%
          customKable( var = var, col.names = c("Response: All", paste0('Rank ', 1:(ncol(res) - 1))),
                       questionLabs = questionLabs)
        if(!is.na(sepVar)) {
          seps <- unique(dat[[sepVar]])
          for(sep in seps) {
            if(!all(is.na(dat[dat[,sepVar] == sep,var]))) {
              res <- analyseRank(dat[dat[,sepVar] == sep,], 
                                 rankVarList[sapply(rankVarList, function(x) str_detect(var, x))]) 
              res %>%
                customKable( var = var, 
                             col.names = c(paste0("Response: ", sep), 
                                           paste0('Rank ', 1:(ncol(res) - 1))),
                             questionLabs = questionLabs)
            }
          }
        }
      }
    }else if(any(sapply(names(specialVarList), function(x) var == x))) {
        var <- names(specialVarList)[sapply(names(specialVarList), function(x) str_detect(var, x))]
        func <- specialVarList[[var]]
        res <- func(dat, var)
    } else {
      unanalysed <- c(unanalysed, var)
    }
    iter <- iter + 1
  }
  if(debug) {
    return(unanalysed)
  }  else {
    return(NULL)
  }
}

# this function styles the Latex output using R's kable package
customKable <- function(df, var, col.names, questionLabs, addHeader = FALSE, headerCols = c (1, 1, 1, 1), 
                        #headerColNames = c('', 'All', 'Local', 'Compliant', "Don't know"), 
                        headerColNames = NA,
                        col1Width = NA) {
  kab <- df %>%
    kable(caption = getQuestionLabels(var, questionLabs), digits = 3,
          col.names = col.names,
          longtable = nrow(df) > 30,
          booktabs = TRUE) %>%
    column_spec(1, width = ifelse(is.na(col1Width), '10em', col1Width))%>%
    kable_styling(latex_options =c("striped", "hold_position", "repeat_header"))
  
  if(addHeader) {
    names(headerCols) <- as.character(headerColNames)
    kab <- kab %>% add_header_above(headerCols) 
  }
  print(kab)
}

# this variable checks that specified variables do not contain missing values
checkNonMissing <- function(dat, nonMissingVarList) {
  
  for(var in varlist) {
    assert_that(all(!is.na(dat[,var])))
  }
}


analyseInterviewDuration <- function(dat) {
  #get duration of interview
  dat <- dat %>%
    mutate(starttime = mdy_hm(start_time), 
           endtime = mdy_hm(end_time),
           duration = endtime - starttime)
  pl <- dat %>% analyseNumeric('duration') %>% mutate(Sector = 'All') %>%
    rbind(dat[dat$q66 == 'Local',] %>% analyseNumeric('duration')%>% mutate(Sector = 'Local')) %>%
    rbind(dat[dat$q66 == 'Compliant',] %>% analyseNumeric('duration')%>% mutate(Sector = 'Compliant'))
  return(pl)
}

# this function takes in a categorical variable and 
# outputs a table
analyseCat <- function(dat, var) {
  dat$Response <- dat[,var]
  res <- dat[!is.na(dat$Response),] %>% group_by(Response)  %>% summarise(n = n()) 
  
  # tack on factor levels with 0 obs
  unused <- levels(dat$Response)
  unused <- unused[!(as.character(unused) %in% as.character(res$Response))]
  if(length(unused) > 0) {
    res <- res %>%
      rbind(data.frame(Response = unused, n = 0))
  }

  res <- res %>%
    arrange(Response) %>%
    ungroup() %>% mutate(p = round(n/sum(n), 3))
  
  return(res)
}

# this variable returns the question label text in a format that Latex can parse
getQuestionLabels <- function(var, questionLabs) {

  if(var %in% names(questionLabs)) {
    lab <- questionLabs[[var]]
  } else {
    lab <- ''
  }
  lab <- paste0(str_replace_all(var, '_', '.'), ': ', lab)
  return(lab)
}

# this function analyses numeric variables
analyseNumeric <- function(dat, var) {
  dat %>% summarise_at(c(var), list(N = ~sum(!is.na(.)),
                                    Mean = ~round(mean(., na.rm = TRUE), 2), 
                                    Sd = ~round(sd(., na.rm = TRUE), 2),
                                    Min = ~round(min(., na.rm = TRUE), 2),
                                    Max = ~round(max(., na.rm = TRUE), 2),
                                    Median = ~round(quantile(., 0.5, na.rm = TRUE)))) %>%
    return()
}

# this function analyses multiple-choice variables
analyseMulti <- function(dat, var, idVar = 'respondent_id') {
  vars <- names(dat)[str_detect(names(dat), paste0('^', var, '_{0,1}[0-9]+$'))] 

  dat[,'respondent_id'] <- dat[[idVar]]
  
  
  dat2 <- dat[,vars] %>% 
    melt(id.vars = NULL, value.name = 'Response') %>% 
    filter(!is.na(Response)) %>%
    group_by(Response) %>% 
    summarise(n = n(), p = n/nrow(dat))

  if(is.factor(dat[,vars[1]])) {
    l <- c()
    for(v in vars) {
      lVar <- levels(dat[,v])
      l <- c(l, lVar[!(lVar %in% l)])
    }
    dat2$Response <- factor(dat2$Response, levels = l)
  }
  dat2 <- dat2 %>% arrange(Response)
  #colnames(dat2)[2] <- '% say yes'
  return(dat2)
}

# this function cuts off tables that have too many rows
fixNRows <- function(dat,cut = 20) {
  
  if(nrow(dat) > cut) {
    cutoff <- sort(dat[[2]], decreasing = TRUE)[cut + 1]
    levels(dat[,1]) <- c(levels(dat[,1]), "Other")
    dat[as.vector(dat[,2] < cutoff),1] <- 'Other'
    dat <- dat %>% group_by_(names(dat)[1]) %>% summarise_all(sum) 
  }
 dat %>% return()
}

# this analyses variables where different options are ranked
analyseRank <- function(dat, var, id = 'respondent_id') {
  vars <- names(dat)[str_detect(names(dat), paste0(var, '_.*[0-9]+$'))]
  dat %>%
    melt(id.vars = id, measure.vars = vars) %>% 
    mutate(rank = paste0('rank', str_extract(as.character(variable), '[0-9]+$'))) %>%
    dcast(as.formula(paste0(id,  '+ value ~ rank')), fun.aggregate = length) %>% 
    group_by(value) %>% summarise_at(vars(matches('rank[1-5]')), ~round(mean(.), 2)) %>%
    filter(!is.na(value)) %>%
    return()
}


# this function fixes an issue with the encoding
fixEncoding <- function(txt) return(str_replace(txt, "\x92", "'"))
