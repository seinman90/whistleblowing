library(readxl)
library(dplyr)
library(readstata13)
library(assertthat)
library(stringr)
library(caret)
library(reshape2)
library(multiwayvcov)
library(stargazer)

outcomes <- c(
  paste0('`q10_1_', 1:5, "less than once  in a monthonce/ twice  in a monthonce/ twice  in a weekeveryday`"),
  'q10_1Index',
   'q10_12yes',
   'q10_16yes',
   paste0('q17_1_', 1:13, 'satisfied'),
  '`q17_2.like friends`', '`q17_2.like family`','`q17_2.have conflicts`', '`q17_2.support one another`',
  paste0('q18_1_', 1:6, 'sometimesalways'),
  paste0('`q18_2.', c('ManagementGoodBehaviour`', 'SupervisorCareForWorkers`', 'GoodPayRise`', 'FairSalary`', 'FestivalLeave`', 'PaidLeave`', 'AutoMachine`', 'SafeBuilding`')),
                 paste0('q19_2_', 1:4, 'agree')
)

outcomeVarsList <- list(q10_1_part1 = paste0('`q10_1_', 1:5, "less than once  in a monthonce/ twice  in a monthonce/ twice  in a weekeveryday`"),
                        #q10_1_part2 = paste0('`q10_1_', 4:5, "less than once  in a monthonce/ twice  in a monthonce/ twice  in a weekeveryday`"),
                        q10_12 = 'q10_12yes',
                        q10_16 = 'q10_16yes',
                        # q17_1_part1 = paste0('q17_1_', 1:3, 'satisfied'),
                        # q17_1_part2 = paste0('q17_1_', 4:6, 'satisfied'),
                        # q17_1_part3 = paste0('q17_1_', 7:9, 'satisfied'),
                        # q17_1_part4 = paste0('q17_1_', 10:12, 'satisfied'),
                        # q17_1_part5 = paste0('q17_1_', 13, 'satisfied'),
                        q17_1_part1 = paste0('q17_1_', 1:5, 'satisfied'),
                        q17_1_part2 = paste0('q17_1_', 6:10, 'satisfied'),
                        q17_1_part3 = paste0('q17_1_', 11:13, 'satisfied'),
                        q17_2 = outcomes[str_detect(outcomes, 'q17_2')],
                        q18_1_part1 = paste0('q18_1_', 1:3),
                        q18_1_part2 = paste0('q18_1_', 4:6),
                        # q18_2_part1 = paste0('`q18_2_', c('ManagementGoodBehaviour`', 'SupervisorCareForWorkers`', 'GoodPayRise`')),
                        # q18_2_part2 = paste0('`q18_2_', c('FairSalary`', 'FestivalLeave`', 'PaidLeave`')),
                        # q18_2_part3 = paste0('`q18_2_', c('AutoMachine`', 'SafeBuilding`')),
                        q18_2_part1 = paste0('`q18_2_', c('ManagementGoodBehaviour`', 'SupervisorCareForWorkers`', 'GoodPayRise`', 'FairSalary`')),
                        q18_2_part2 = paste0('`q18_2_', c('FestivalLeave`', 'PaidLeave`', 'AutoMachine`', 'SafeBuilding`')),
                        q19_2_part1 = paste0('q19_2_', 1:4, 'agree')
                        #q19_2_part2 = paste0('q19_2_', 3:4, 'agree')
)

#outcomeVarsList <- outcomeVarsList[1]
modList <- list(
  mod1 = c(q9_1 = 'dummy', q9_2 = 'none'),
  mod2 = c(q9_1 = 'none', q9_2 = 'numeric'),
  mod3 = c(q9_1 = 'none', q9_2 = 'dummy'),
  mod4 = c(q9_1 = 'none', q9_2 = 'index'),
  mod5 = c(q9_1 = 'dummy', q9_2 = 'index')
)

cleanDat <- function(dat) {
  dat <- filterDat(dat)
  dat <- cleanOutcomes(dat)
  dat <- cleanPredictors(dat)
  makeSummaryTables(dat)
  return(dat)
}

makeSummaryTables <- function(dat) {
  
  labs <- getAllIndVarLabs() 
  labs <- labs[!str_detect(names(labs), 'factorycode')]
  labNames <- names(labs) %>% str_replace('TRUE', '') %>% str_replace_all('`', '')
  t <- dat %>% 
    select_at(labNames)
  names(t) <- labs
  stargazer(t, omit.summary.stat = 'n', header = FALSE, float.env = 'sidewaystable')
}

filterDat <- function(dat) {
  cat(paste0('Initial observations: ', nrow(dat), '\n'))
  cat(paste0('Dropping ', sum(dat$q7_11s != 'sawing/making'), ' observations not in sewing section\n'))
  cat(paste0('Dropping ', 
             sum(dat$q7_12d %in% c('quality supervisor', 'supervisor') & dat$q7_11s == 'sawing/making'), 
             ' observations are supervisors\n'))
  
  dat <- dat %>%
    filter(q7_11s == 'sawing/making') %>%
    filter(!(q7_12d %in% c('quality supervisor', 'supervisor')))
  
  cat(paste0('Leftover sample size: ', nrow(dat)))
  return(dat)
}
cleanOutcomes <- function(dat) {
  # recode dummy variables
  for(v in paste0('q10_1_', 1:5)) {
    dat <- dat %>% recodeDummy(v, levels(dat$q10_1_1)[2:5])
  }
  for(v in c('q10_12', 'q10_16')) {
    dat <- dat %>% recodeDummy(v, 'yes')
  }
  for(v in paste0('q17_1_', 1:13)) {
    dat <- dat %>% recodeDummy(v, 'satisfied')
  }
  dat <- dat %>% recodeLevels('q17_2')
  for(v in paste0('q18_1_', 1:6)) {
    dat <- dat %>% recodeDummy(v, c('sometimes', 'always'))
  }
  dat <- dat %>% 
    melt(id.vars = 'qid', measure.vars = paste0('q18_2', 1:3)) %>% 
    mutate(q18_2 = ifelse(value < 9, value, 9),
           q18_2 = factor(q18_2, levels = 1:9, 
                          labels = c('ManagementGoodBehaviour', 'SupervisorCareForWorkers',
                                     'GoodPayRise', 'FairSalary', 'FestivalLeave', 'PaidLeave',
                                     'AutoMachine', 'SafeBuilding', 'Other'))) %>%
    recodeLevels('q18_2') %>%
    group_by(qid) %>%
    summarise_at(vars(matches('q18_2\\..*')), sum, na.rm = TRUE) %>%
    merge(dat, by = 'qid')
  for(v in paste0('q19_2_', 1:4)) {
    dat <- dat %>% recodeDummy(v, 'agree')
  }

  return(dat)
}
recodeDummy <- function(dat, var, response) {
  assert_that(all(response %in% dat[[var]]),
              msg = paste0('Error in dummy recoding of variable ', var, ': response not found'))
  index <- dat[[var]] %in% response
  dat[[paste0(var, paste(response, collapse = ''))]] <- index
  return(dat)
}
recodeLevels <- function(dat, var) {
  form <- paste0('qid ~', var) %>% as.formula()
  vars <- dummyVars(form, data = dat)  %>% predict(dat) 
  for(v in colnames(vars)) {
    dat[[v]] <- vars[,v]
  }
  return(dat)
}
cleanPredictors <- function(dat) {
  dat <- recodeLevels(dat, 'q9_1')
  for(var in paste0('q9_2_', 1:6)) {
    dat[[paste0(var, 'Numeric')]] <- as.numeric(dat[[var]])
    dat <- dat %>%
      recodeDummy(var, c('strongly disagree', 'disagree', 'neutral'))
  }
  dat <- dat %>% recodeDummy('q3_2', 'female')
  dat <- dat %>% recodeDummy('q3_4', c('married', 'separated/divorced/widow'))
  dat <- dat %>% recodeDummy('q7_12d', 'helper/lineman')
  dat <- dat %>% recodeDummy('q7_12d', 'operatior')
  dat$factoryTenure <- 2017 - dat$q6_1
  dat$sectorExperience <- 2017 - dat$q5_1
  dat$factorycode <- as.character(dat$factorycode)
  
  dat <- prepareIndices(dat)
  return(dat)
}
runModel <- function(dat,outcomeVar, q9_1, q9_2, byFactory) {
  form <- getForm(outcomeVar, q9_1 = q9_1, q9_2 = q9_2)

  if(all(dat[[outcomeVar]] %in% c(0, 1))) {
    mod <- glm(formula = form, 
               data = dat, 
               family = binomial(link = 'logit'))
  } else {
    mod <- glm(formula = form, 
               data = dat, 
               family = gaussian)
    
  }

  
  if(byFactory) {
    cov <- cluster.boot(mod, dat$factorycode, parallel = TRUE,
                        boot_type = 'wild', 
                        wild_type = 'rademacher')
  } else {
    sum <- summary(mod)
    cov <- cluster.vcov(mod, dat$factory)
  }
  ses <- sqrt(diag(cov))
  names(ses) <- names(coef(mod))
  
  return(list(mod = mod, ses = ses))
}
makeTable <- function(outputList, outputVar, modType) {
  mods <- lapply(outputList, function(x) return(x[['mod']]))
  ses <- lapply(outputList, function(x) return(x[['ses']]))
  labels <- getLabels(outputList[[1]]$mod$coefficients %>% names(), type = 'independent')
  depLabel <- getLabels(outputVar, type = 'dependent') #%>% lapply(rep, 2) %>% unlist()
  modLabel <- getLabels(modType, type = 'mod')
  omitVars <- names(ses[[1]])
  omitVars <- omitVars[str_detect(omitVars, 'factorycode.*')] %>% 
    setdiff(c('factorycode90', 'factorycode63', 'factorycode13'))
  
  stargazer(mods, se = ses, omit = omitVars, 
            covariate.labels = labels, 
            dep.var.labels = depLabel,
            column.labels = c('All factories', 'Only factories 90, 63 and 13'),
            title = modLabel,
            report = 'vcsp*',
            float.env = 'sidewaystable',
            notes = 'SEs clustered by factory',
            no.space =TRUE,
            single.row = TRUE,
            omit.stat = c('aic', 'll'),
            keep.stat = c('adj.rsq', 'n'),
            model.names = TRUE,
            header = FALSE) 
  cat("\\clearpage")
}
getLabels <- function(covLabs, type = 'independent') {
  covLabs <- covLabs[!str_detect(covLabs, 'factorycode.*') | (covLabs %in% paste0('factorycode', c(90, 63, 13)))] %>%
    setdiff('(Intercept)')
  if(type == 'independent') allLabs <- getAllIndVarLabs()
  else if(type == 'dependent') allLabs <- getAllDepVarLabs()
  else if(type == 'mod') allLabs <- getAllModLabs()

  labInLabs <- covLabs %in% names(allLabs)
  assert_that(all(labInLabs), msg = paste('Label', paste(covLabs[!labInLabs], collapse = ' & ', 'not in labs')))
  labs <- sapply(covLabs, function(x) return(allLabs[x]))
  return(labs)
}
getAllIndVarLabs <- function() {
  labs <- c('Gender: female',
    'Age',
    "Years of schooling",
    'Ever married',
    '7.1: position helper/lineman',
    '7.1: position operator',
    'Tenure at factory (yrs)',
    'Experience in sector (yrs)',
    '9.1: Factory has rules',
    '9.1: Management consults workers',
    '9.1: Must obey orders',
    '9.2: Supervisor respects me (numeric)',
    '9.2: Supervisor doesn\'t use bad lang (numeric)',
    '9.2: Supervisor will side with me (numeric)',
    '9.2: Respect supervisor (numeric)',
    '9.2: Supervisor speaks openly (numeric)',
    '9.2: I get fair salary (numeric)',
    '9.2: Supervisor respects me (disagree dummy)',
    '9.2: Supervisor doesn\'t use bad lang (disagree dummy)',
    '9.2: Supervisor will side with me (disagree dummy)',
    '9.2: Respect supervisor (disagree dummy)',
    '9.2: Supervisor speaks openly (disagree dummy)',
    '9.2: I get fair salary (disagree dummy)',
    '9.2: Good supervisor rship (index)',
    'Factory code 90',
    'Factory code 63',
    'Factory code 13'
    )
  names(labs) <- c('q3_2femaleTRUE', 'q3_1', 'q3_3', '`q3_4marriedseparated/divorced/widow`TRUE',
                   '`q7_12dhelper/lineman`TRUE', 'q7_12doperatiorTRUE',
                         'factoryTenure', 'sectorExperience',"`q9_1.factory has rules that all workers must be aware of and follow`",
                         "`q9_1.the management regularly consults workers or their representatives to find out a`",
                         "`q9_1.we are lucky to get the job therefore, we must obey orders`",
                         "q9_2_1Numeric", "q9_2_2Numeric", "q9_2_3Numeric", "q9_2_4Numeric", "q9_2_5Numeric", "q9_2_6Numeric",
                         "`q9_2_1strongly disagreedisagreeneutral`TRUE", "`q9_2_2strongly disagreedisagreeneutral`TRUE", "`q9_2_3strongly disagreedisagreeneutral`TRUE", "`q9_2_4strongly disagreedisagreeneutral`TRUE", "`q9_2_5strongly disagreedisagreeneutral`TRUE", "`q9_2_6strongly disagreedisagreeneutral`TRUE",
                         'q9_2Index', 'factorycode90', 'factorycode63', 'factorycode13')
  return(labs)
}
getAllModLabs <- function() {
  labs <- c('Specification 1: 9.1 raw data + covariates + factory FE',
            'Specification 2: 9.2 raw data + covariates + factory FE',
            'Specification 3: 9.2 dummies for don\'t agree + covariates + factory FE',
            'Specification 4: 9.2 index over raw data + covariates + factory FE',
            'Specification 5: 9.1 raw data + 9.2 index + covariates + factory FE')
  names(labs) <- names(modList)
  return(labs)
}
getForm <- function(outcomeVar,  q9_1 = c('none', 'dummy'), q9_2 = c('index', 'dummy', 'numeric', 'none')) {
  assert_that(q9_2 %in% c('index', 'dummy', 'numeric', 'none'), msg = "q92 not equal to either index, var or none")
  
  form <- paste0(outcomeVar, '~ factorycode + q3_2female + q3_1 + q3_3 + `q3_4marriedseparated/divorced/widow` + sectorExperience + factoryTenure + `q7_12dhelper/lineman` + q7_12doperatior')
  if(q9_1 == 'dummy') {
    newVars <- names(dat)[str_detect(names(dat), 'q9_1\\.')]
    newVars <- setdiff(newVars, c('q9_1Index', 'q9_1.workers are treated like family members')) %>%
      paste0(collapse = "` + `")
    form <- paste0(form, " + `", newVars, "`")
  }

  if(q9_2 == 'index') {
    form <- paste0(form, '+ q9_2Index')
  } else if(q9_2 == 'numeric') {
    newVars <- names(dat)[str_detect(names(dat), 'q9_2_[0-9]+Numeric')]
    form <- paste0(c(form, newVars), collapse = ' + ')
  } else if(q9_2 == 'dummy') {
    newVars <- paste0('`q9_2_', 1:6, 'strongly disagreedisagreeneutral`') 
    form <- paste0(c(form, newVars), collapse = ' + ')
  }
  return(form)
}
# runs the makeIndex funciton over the variables that need to be indexed
prepareIndices <- function(dat) {
  #dat$q9_1Index <- dat %>% select_at(vars(matches('q9_1\\..*'))) %>% makeIndex()
  #dat$q9_2DummyIndex <- dat %>% select_at(vars(matches('q9_2_[0-9]+strongly.*'))) %>% makeIndex()
  dat$q9_2Index <- dat %>% select_at(vars(matches('q9_2_[0-9]+Numeric$'))) %>% makeIndex()
  dat$q10_1Index <- dat %>% select_at(vars(matches('q10_1_[0-9]+$'))) %>% mutate_all(~(as.numeric(.) - 1)) %>% makeIndex()
  return(dat)
}
# makes a 
makeIndex <- function(dat) {
 dat %>% 
    as.matrix() %>%
    apply(2, function(x) return((x - mean(x))/sd(x))) %>%
    apply(1, sum) %>%
    return()
}
getAllDepVarLabs <- function() {
  labs <- c('10.1: Ever physically abused',
            '10.1: Ever verbally abused',
            '10.1: Ever sexually harassed',
            '10.1: Ever humiliated',
            '10.1: Ever threatened',
            '10.1: Experience of abuse and harassment, index',
            '10.12: Ever injured in factory',
            '10.16: Feel safe in factory',
            '17.1: Satisfied with building safety',
            '17.1: Satisfied with fire and electricity safety',
            '17.1: Satisfied with healthy work environment',
            '17.1: Satisfied with working hours and overtime',
            '17.1: Satisfied with production target',
            '17.1: Satisfied with behaviour of management',
            '17.1: Satisfied with opportunities to complain',
            '17.1: Satisfied with salary and bonus',
            '17.1: Satisfied with salary payment date',
            '17.1: Satisfied with job security',
            '17.1: Satisfied with skill development opportunities',
            '17.1: Satisfied with promotion opportunities',
            '17.1: Satisfied overall',
            '17.2: Relationship w/colleagues like friends',
            '17.2: Relationship w/colleagues like family',
            '17.2: Relationship w/colleagues conflicted',
            '17.2: Relationship w/colleagues supportive',
            '18.1 Ever feel worried',
            '18.1 Ever feel afraid',
            '18.1 Ever feel alert',
            '18.1 Ever feel enthusiastic',
            '18.1 Ever feel proud',
            '18.1 Ever feel contented',
            '18.2 Thinks good management behaviour is important',
            '18.2 Thinks management looking out for workers is important',
            '18.2 Thinks good annual pay raise is important',
            '18.2 Thinks fair salary is important',
            '18.2 Thinks festival leave is important',
            '18.2 Thinks paid leave is important',
            '18.2 Thinks auto machine is important',
            '18.2 Thinks safe building is important',
            '19.2 Happy because salary is good',
            '19.2 Happy because work is safe',
            '19.2 Unhappy because can be fired any time',
            '19.2 Unhappy because works besides men')
  
  names(labs) <- outcomes
  return(labs)
}

# runs over all the different iterations of regressions we want, runs the models and outputs the tables
runModels <- function(dat) {

 for(outcomeVars in outcomeVarsList) {
      print(outcomeVars)
      for(modtype in names(modList)) {
        q9_1 <- modList[[modtype]]['q9_1']
        q9_2 <- modList[[modtype]]['q9_2']
        outputList <-lapply(outcomeVars, function(outcomeVar) {
          lapply(list(list(code = unique(dat$factorycode), byFactory = FALSE),
                      list(code = c(90, 63, 13), byFactory = TRUE)
          ), function(x) {
            byFactory <- x[['byFactory']]
            code <- x[['code']]
            dat %>% 
              filter(factorycode %in% code) %>% 
              runModel(outcomeVar, q9_1 = q9_1, q9_2 = q9_2, byFactory = byFactory)
          }
          )
        })# %>% unlist(recursive = FALSE)
        
        makeTable(outputList, outcomeVars, modtype)
      }  
  }
}



