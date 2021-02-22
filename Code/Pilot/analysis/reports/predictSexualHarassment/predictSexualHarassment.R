library(readxl)
library(dplyr)
library(readstata13)
library(assertthat)
library(stringr)
library(caret)
library(reshape2)
library(multiwayvcov)
library(stargazer)
library(clusterSEs)

cleanVarNames <- function(names) {
  names %>%
    str_replace_all('\\W', '') %>%
    return()
}

outcomes <- c(
  paste0('`q10_1_', 1:5, "less than once  in a monthonce/ twice  in a monthonce/ twice  in a weekeveryday`"),
  'q10_1Index',
   'q10_12yes',
   'q10_16yes',
   paste0('q17_1_', 1:13, 'satisfied'),
  '`q17_2.like friends`', '`q17_2.like family`',#'`q17_2.have conflicts`', 
  '`q17_2.support one another`',
  paste0('q18_1_', 1:6, 'sometimesalways'),
  paste0('`q18_2.', c('ManagementGoodBehaviour`', 'SupervisorCareForWorkers`', 'GoodPayRise`', 'FairSalary`', 
                      'FestivalLeave`', 'PaidLeave`', 'AutoMachine`', 'SafeBuilding`')),
                 paste0('q19_2_', 1:4, 'agree')
) %>% cleanVarNames()

outcomeVarsList <- list(
  q10_1_part1 = paste0('`q10_1_', 1:3, "less than once  in a monthonce/ twice  in a monthonce/ twice  in a weekeveryday`"),
                        q10_1_part2 = c(paste0('`q10_1_', 4:5, "less than once  in a monthonce/ twice  in a monthonce/ twice  in a weekeveryday`"), 'q10_1Index'),
                        q10_12 = 'q10_12yes',
                        q10_16 = 'q10_16yes',
                        q17_1_part1 = paste0('q17_1_', 1:3, 'satisfied'),
                        q17_1_part2 = paste0('q17_1_', 4:6, 'satisfied'),
                        q17_1_part3 = paste0('q17_1_', 7:9, 'satisfied'),
                        q17_1_part4 = paste0('q17_1_', 10:12, 'satisfied'),
                        q17_1_part5 = paste0('q17_1_', 13, 'satisfied'),
                        q17_2 = outcomes[str_detect(outcomes, 'q17_2')],
                        q18_1_part1 = paste0('q18_1_', 1:3, 'sometimesalways'),
                        q18_1_part2 = paste0('q18_1_', 4:6, 'sometimesalways'),
                        q18_2_part1 = paste0('`q18_2.', c('ManagementGoodBehaviour`', 'SupervisorCareForWorkers`', 'GoodPayRise`', 'FairSalary`')),
                        q18_2_part2 = paste0('`q18_2.', c('FestivalLeave`', 'PaidLeave`', 'AutoMachine`', 'SafeBuilding`')),
                        q19_2_1part1 = paste0('q19_2_', 1:2, 'agree'),
                        q19_2_2part2 = paste0('q19_2_', 3:4, 'agree')
) %>% lapply(cleanVarNames)

# create the list of specifications
modList <- list(
  mod1 = c(q9_1 = 'dummy', q9_2 = 'none', ada = NA),
  mod2 = c(q9_1 = 'none', q9_2 = 'numeric', ada = NA),
  mod3 = c(q9_1 = 'none', q9_2 = 'dummy', ada = NA),
  mod4 = c(q9_1 = 'none', q9_2 = 'index', ada = NA),
  mod5 = c(q9_1 = 'dummy', q9_2 = 'index', ada = NA)
)

# do initial data cleaning
cleanDat <- function(dat) {
  dat <- filterDat(dat)
  dat <- cleanOutcomes(dat)
  dat <- cleanPredictors(dat)
  names(dat) <- cleanVarNames(names(dat))
  return(dat)
}


# make initial summary table of independent variables
makeSummaryTables <- function(dat) {
  
  labs <- getAllIndVarLabs() 
  labs <- labs[!str_detect(names(labs), 'factorycode')]
  labs <- labs[!str_detect(names(labs), ':')]
  labNames <- names(labs) %>% str_replace('TRUE', '') %>% str_replace_all('`', '')
  t <- dat %>% 
    select_at(labNames)
  names(t) <- labs
  stargazer(t, omit.summary.stat = 'n', header = FALSE, float.env = 'sidewaystable', no.space = FALSE,
            title = 'Summary statistics for independent variables', 
            notes = 'For 9.2 numeric variables, 5 = strongly agree, 1 = strongly disagree')
}

# filter out the observations we're not interested in
filterDat <- function(dat) {
  initObs <- nrow(dat)
  cat(paste0('Initial observations: ', initObs, '\n'))
  
  sewingObs <-  sum(dat$q7_11s != 'sawing/making')
  cat(paste0('Dropping ', sewingObs, ' observations not in sewing section\n'))
  
  supervisorObs <-  sum(dat$q7_12d %in% c('quality supervisor', 'supervisor') & dat$q7_11s == 'sawing/making')
  cat(paste0('Dropping ', 
             supervisorObs, 
             ' observations are supervisors\n'))
  
  dat <- dat %>%
    filter(q7_11s == 'sawing/making') %>%
    filter(!(q7_12d %in% c('quality supervisor', 'supervisor'))) %>%
    group_by(factorycode) %>%
    mutate(factoryObs = n())
  factoryObs <- sum(dat$factoryObs == 1)
  cat(paste0('Dropping ', factoryObs, ' observations due to only respondent in factory'))
  dat <- dat %>%
    filter(factoryObs > 1) %>%
    ungroup()
  
  finalObs <- nrow(dat)
  assert_that(finalObs == (initObs - factoryObs - supervisorObs - sewingObs), 
              msg = "Filtering error, final observations not equal to initial observations minus dropped observations")
  cat(paste0('Leftover sample size: ', finalObs))
  
  return(dat)
}

# clean all outcome variables
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

# recode variables that need several categories to be collapsed into one to form a single dummy var
recodeDummy <- function(dat, var, response) {
  assert_that(all(response %in% dat[[var]]),
              msg = paste0('Error in dummy recoding of variable ', var, ': response not found'))
  index <- dat[[var]] %in% response
  dat[[paste0(var, paste(response, collapse = ''))]] <- index
  return(dat)
}

# split a categorical variable into multiple dummies
recodeLevels <- function(dat, var) {
  form <- paste0('qid ~', var) %>% as.formula()
  vars <- dummyVars(form, data = dat)  %>% predict(dat) 
  for(v in colnames(vars)) {
    dat[[v]] <- vars[,v]
  }
  return(dat)
}

# clean all RHS vars
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
  
  dat <- dat %>% group_by(factorycode) %>%
    mutate(pFemale = mean(q3_2female)) %>% ungroup()
  
  dat <- prepareIndices(dat)
  return(dat)
}

# run a single model
runModel <- function(dat,outcomeVar, q9_1, q9_2, byFactory, fe, ada = NA) {
  form <- getForm(outcomeVar, q9_1 = q9_1, q9_2 = q9_2, fe = fe, ada = ada)
  mod <- lm(form, dat)
  
  if(byFactory) {
    mod2 <- glm(formula = form, 
               data = dat,
               family = gaussian)
    coefDiff <- coef(mod) - coef(mod2)
    assert_that(all(is.na(coef(mod)) == is.na(coef(mod2))))
    coefDiff <- coefDiff[is.na(coefDiff)]
    assert_that(all(abs(coefDiff) < 0.01), msg = 'mod and mod2 differ in their coefs')
    invisible(capture.output(wild <- cluster.wild.glm(mod2, dat, ~factorycode, prog.bar = FALSE, report = FALSE)))    

    p <- wild$p.values
  } else {
    sum <- summary(mod)
    p <- sum$coefficients[,4]
  }
  names(p) <- names(coef(mod)[!is.na(coef(mod))])
  
  return(list(mod = mod, p = p))
}

# takes a list of models and outputs a latex table
makeTable <- function(outputList, outputVar, modType, outputVarTop, byFactory) {
  
  mods <- lapply(outputList, function(x) return(x[['mod']]))
  ps <- lapply(outputList, function(x) return(x[['p']]))
  labels <- getLabels(outputList[[1]]$mod$coefficients %>% names(), type = 'independent')
  depLabel <- getLabels(outputVar, type = 'dependent') 
  modLabel <- getLabels(modType, type = 'mod')
  tableLabel <- getLabels(outputVarTop, type = 'question')
  omitVars <- names(ps[[1]])
  omitVars <- omitVars[str_detect(omitVars, 'factorycode.*')]
  if(length(omitVars) == 0) omitVars <- 'zzzz'
  note <- 'Clustered by factory. Omitted category for 7.1: position = other.'
  if(modType %in% c('mod1', 'mod5')) {
    note <- paste0(note, ' Omitted category for 9.1: "Workers treated like family".')
  }
  title <- paste0(tableLabel, ', ', modLabel)
  if(byFactory) {
    title <- paste0(title, ' Factories 13, 63 and 90 only.')
  }
  stargazer(mods, 
            se = lapply(1:length(mods), function(x) return(NA)), 
            p = ps, 
            font.size = 'footnotesize',
            omit = omitVars,
            covariate.labels = labels, 
            dep.var.labels = depLabel,
            column.labels = rep(c('No factory FEs', 'With factory FEs'), length(mods)/2),
            title = title,
            report = 'vcsp*',
            float.env = 'sidewaystable',
            notes = note,
            no.space = FALSE,
            single.row = TRUE,
            omit.stat = c('aic', 'll'),
            keep.stat = c('adj.rsq', 'n'),
            model.names = TRUE,
            header = FALSE) 
  cat("\\clearpage")
}

# takes in a vector of labels and returns prettified text for latex output
getLabels <- function(covLabs, type = 'independent') {
  covLabs <- covLabs[!str_detect(covLabs, 'factorycode.*')] %>%
    setdiff('(Intercept)')
  if(type == 'independent') allLabs <- getAllIndVarLabs()
  else if(type == 'dependent') allLabs <- getAllDepVarLabs()
  else if(type == 'mod') allLabs <- getAllModLabs()
  else if(type == 'question') allLabs <- getAllQuestionLabs()

  labInLabs <- covLabs %in% names(allLabs)
  assert_that(all(labInLabs), msg = paste('Label', paste(covLabs[!labInLabs], collapse = ' & ', 'not in labs')))
  labs <- sapply(covLabs, function(x) return(allLabs[x]))
  return(labs)
}

# assembles prettified text for independent variables
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
    '9.2: Supervisor respects me (disagree = 1)',
    '9.2: Supervisor doesn\'t use bad lang (disagree = 1)',
    '9.2: Supervisor will side with me (disagree = 1)',
    '9.2: Respect supervisor (disagree = 1)',
    '9.2: Supervisor speaks openly (disagree = 1)',
    '9.2: I get fair salary (disagree = 1)',
    '9.2: Good supervisor rship (index)',
    'Factory code 90',
    'Factory code 63',
    'Factory code 13',
    'Female*age',
    'Female*ever married',
    'Number of workers in section',
    "\\% of workers in factory female"
    )
  names(labs) <- c('q3_2femaleTRUE', 'q3_1', 'q3_3', 'q3_4marriedseparateddivorcedwidowTRUE',
                   'q7_12dhelperlinemanTRUE', 'q7_12doperatiorTRUE',
                         'factoryTenure', 'sectorExperience',"q9_1factoryhasrulesthatallworkersmustbeawareofandfollow",
                         "q9_1themanagementregularlyconsultsworkersortheirrepresentativestofindouta",
                         "q9_1weareluckytogetthejobthereforewemustobeyorders",
                         "q9_2_1Numeric", "q9_2_2Numeric", "q9_2_3Numeric", "q9_2_4Numeric", "q9_2_5Numeric", "q9_2_6Numeric",
                         "q9_2_1stronglydisagreedisagreeneutralTRUE", "q9_2_2stronglydisagreedisagreeneutralTRUE", 
                   "q9_2_3stronglydisagreedisagreeneutralTRUE", "q9_2_4stronglydisagreedisagreeneutralTRUE", 
                   "q9_2_5stronglydisagreedisagreeneutralTRUE", "q9_2_6stronglydisagreedisagreeneutralTRUE",
                         'q9_2Index', 'factorycode90', 'factorycode63', 'factorycode13', 'q3_2femaleTRUE:q3_1',
                   'q3_2femaleTRUE:q3_4marriedseparateddivorcedwidowTRUE', 'q2_3num', 'pFemale') 
  return(labs)
}
getAllModLabs <- function() {
  labs <- c('Specification 1: 9.1 raw data + covariates.',
            'Specification 2: 9.2 raw data + covariates.',
            'Specification 3: 9.2 dummies for don\'t agree + covariates.',
            'Specification 4: 9.2 index over raw data + covariates.',
            'Specification 5: 9.1 raw data + 9.2 index + covariates.',
            'Edits for Ada 1',
            'Edits for Ada 2',
            'Edits for Ada 4',
            'Edits for Ada 5') 
  names(labs) <- c(paste0('mod', 1:5), paste0('ada', c(1,2,4, 5)))
  return(labs)
}
getForm <- function(outcomeVar,  q9_1 = c('none', 'dummy'), q9_2 = c('index', 'dummy', 'numeric', 'none'), fe, ada) {
  assert_that(q9_2 %in% c('index', 'dummy', 'numeric', 'none'), msg = "q92 not equal to either index, var or none")
  assert_that(is.logical(fe), msg = 'fe should be boolean')
  
  if(is.na(ada)) form <-  'q3_2female + q3_1 + q3_3 + q3_4marriedseparateddivorcedwidow + sectorExperience + factoryTenure + q7_12dhelperlineman + q7_12doperatior'

  if(q9_1 == 'dummy') {
    newVars <- names(dat)[str_detect(names(dat), 'q9_1[a-z]+')]
    newVars <- setdiff(newVars, c('q9_1Index', 'q9_1workersaretreatedlikefamilymembers')) %>%
      paste0(collapse = " + ")
    form <- paste0(form, " + ", newVars)
  }

  if(q9_2 == 'index') {
    form <- paste0('q9_2Index + ', form)
  } else if(q9_2 == 'numeric') {
    newVars <- names(dat)[str_detect(names(dat), 'q9_2_[0-9]+Numeric')]
    form <- paste0(c(newVars, form), collapse = ' + ')
  } else if(q9_2 == 'dummy') {
    newVars <- paste0('q9_2_', 1:6, 'stronglydisagreedisagreeneutral') 
    form <- paste0(c(newVars, form), collapse = ' + ')
  }
  # extra code to tack on the regressions ada asked for
  if(!is.na(ada)) {
    form <- 'q3_2female + q3_1 + q3_3 + q3_4marriedseparateddivorcedwidow'
    if(ada == 'ada2') form <- paste0(form, ' + q3_2female*q3_1 + q3_2female*q3_4marriedseparateddivorcedwidow')
    if(ada == 'ada4'| ada == 'ada5') form <- paste0(form, ' + q2_3num')
    if(ada == 'ada5') form <- paste0(form, '+ pFemale')
  } 
  
  if(fe) {
    form <- paste0(form,'  + factorycode')
  }
  form <- paste0(outcomeVar, '~', form)
  
  return(form)
}
# runs the makeIndex funciton over the variables that need to be indexed
prepareIndices <- function(dat) {
  dat$q9_2Index <- dat %>% select_at(vars(matches('q9_2_[0-9]+Numeric$'))) %>% makeIndex()
  dat$q10_1Index <- dat %>% select_at(vars(matches('q10_1_[0-9]+$'))) %>% mutate_all(~(as.numeric(.) - 1)) %>% makeIndex()
  return(dat)
}
# makes a standardised index from a df of numeric variables
makeIndex <- function(dat) {
 dat %>% 
    as.matrix() %>%
    apply(2, function(x) return((x - mean(x))/sd(x))) %>%
    apply(1, mean) %>%
    return()
}

getAllQuestionLabs <- function() {
  labs <- c('10.1: Likelihood of reporting ever experiencing different types of abuse',
            '10.12: Likelihood of reporting ever having been injured at the factory',
            '10.16: Likelihood of reporting feeling safe in factory',
            '17.1: Likelihood of reporting satisfaction with different aspects of job',
            '17.2: Likelihood of describing relationship with colleagues as...',
            '18.1: Likelihood of reporting experiencing different emotions at work',
            '18.2: Likelihood of thinking different job aspects are important for happiness',
            '19.2: Feel happy because of certain aspects of job',
            '19.2: Feel unhappy because of certain aspects of job')
  names(labs) <- names(outcomeVarsList) %>% 
    str_replace('part[0-9]', '') %>% 
    unique()
  return(labs)
}

getAllDepVarLabs <- function() {
  labs <- c('Physical abuse',
            'Verbal abuse',
            'Sexual harassment',
            'Humiliation',
            'Threats',
            'Abuse and harassment, index',
            'Ever injured in factory',
            'Feel safe in factory',
            'Building safety',
            'Fire/electricity safety',
            'Healthy work environment',
            'Working hours/overtime',
            'Production target',
            'Behaviour of management',
            'Opportunities to complain',
            'Salary/bonus',
            'Salary payment date',
            'Job security',
            'Skill development opportunities',
            'Promotion opportunities',
            'Satisfied overall',
            'Like friends',
            'Like family',
            'Conflicted',
            'Supportive',
            'Worried',
            'Afraid',
            'Alert',
            'Enthusiastic',
            'Proud',
            'Contented',
            'Good management behaviour',
            'Management looking out for workers',
            'Good annual pay raise',
            'Fair salary',
            'Festival leave',
            'Paid leave',
            'Auto machine',
            'Safe building',
            'Salary is good',
            'Work is safe',
            'Can be fired any time',
            'Works besides men')
  
  names(labs) <- outcomes
  return(labs)
}

# runs over all the different iterations of regressions we want, runs the models and outputs the tables
runModels <- function(dat) {

 for(outcomeVarTop in names(outcomeVarsList)) {
   outcomeVars <- outcomeVarsList[[outcomeVarTop]]

      for(modtype in names(modList)) {

        q9_1 <- modList[[modtype]]['q9_1']
        q9_2 <- modList[[modtype]]['q9_2']
        ada <- modList[[modtype]]['ada']
        print(ada)
          lapply(list(list(code = unique(dat$factorycode), byFactory = FALSE),
                      list(code = c(90, 63, 13), byFactory = TRUE)
          ), function(x) {
            byFactory <- x[['byFactory']]
            code <- x[['code']]
            print(paste0('byFactory', byFactory))
            outputList <- lapply(outcomeVars, function(outcomeVar) {
            mod1 <- dat %>% 
              filter(factorycode %in% code) %>% 
              runModel(outcomeVar, 
                       q9_1 = q9_1, 
                       q9_2 = q9_2,
                       byFactory = byFactory,
                       fe = TRUE,
                       ada = ada)
            mod2 <- dat %>% 
              filter(factorycode %in% code) %>% 
              runModel(outcomeVar, 
                       q9_1 = q9_1, 
                       q9_2 = q9_2,
                       byFactory = byFactory,
                       fe = FALSE,
                       ada= ada)
            return(list(mod1 = mod1, mod2 = mod2))
          }
          ) %>% unlist(recursive = FALSE)

            makeTable(outputList, 
                      outcomeVars, 
                      modtype, 
                      str_replace(outcomeVarTop, 'part[0-9]', ''),
                      byFactory)
        })
        
      }  
  }
}

cluster.wild.glm<-function(mod, dat, cluster, ci.level = 0.95, impose.null = TRUE, boot.reps = 1000, 
                           report = TRUE, prog.bar = TRUE, output.replicates = FALSE,
                           seed=NULL){
  
  if(is.null(seed)==F){                                               # if user supplies a seed, set it
    
    tryCatch(set.seed(seed),
             error = function(e){return("seed must be a valid integer")}, 
             warning = function(w){return(NA)}) 
    
  }
  
  if(mod$family[1] != "gaussian" | mod$family[2] != "identity"){
    stop("Use only with gaussian family models with a linear link")
  }
  if(output.replicates == TRUE & impose.null == TRUE){
    stop("Recovering bootstrap replicates requires setting impose.null = FALSE")
  }
  
  form <- mod$formula                                            # what is the formula of this model?
  if(!is.Formula(form)) form <- as.formula(form)
  variables <- all.vars(form)                                    # what variables are in this model?
  clust.name <- all.vars(cluster)                                # what is the name of the clustering variable?
  used.idx <- which(rownames(dat) %in% rownames(mod$model))      # what were the actively used observations in the model?
  dat <- dat[used.idx,]                                          # keep only active observations
  clust <- as.vector(unlist(dat[[clust.name]]))                  # store cluster index in convenient vector
  G<-length(unique(clust))                                       # how many clusters are in this model?
  # ind.variables <- attr(mod$terms, "term.labels")              # what independent variables are in this model? (deprecated)
  "%w/o%" <- function(x, y) x[!x %in% y]                         # create a without function (see ?match)
  dv <- variables %w/o% all.vars(update(form, 1 ~ .))            # what is the dependent variable?
  ind.variables.data <- all.vars(update(form, 1 ~ .))            # RHS variables in this model (before variable transforms)
  ind.variables.names.full <- names(coefficients(mod))           # printed names of coefs (w/ intercept)
  ind.variables.names <- rownames(summary(mod)$coefficients)     # printed names of coefs (w/ intercept), neglecting drops
  ind.variables <- ind.variables.names %w/o% "(Intercept)"       # what independent variables are in this model, neglecting drops and intercept?
  
  # in case dv is wrapped in a function, need to set it to its functional value
  # so that residuals can be added w/o incident
  dat$dv.new <- mod$y                                            # add transformed DV into data set
  
  form.new <- update(form, dv.new ~ .)                           # substitute in new dV
  
  # check to see whether any IVs are factors
  fac <- max(0,min(length(mod$xlevels),1))
  
  # do not impose the null for factor variables
  if( fac == 1 & impose.null == TRUE){
    cat("\n","\n", "Note: null not imposed (factor variables are present).", "\n", "\n")
    impose.null<-FALSE
  }
  
  # check whether there are (automatic) interaction terms
  interaction <- max(attr(mod$terms,"order"))
  
  # do not impose the null for interaction terms
  if( interaction > 1 & impose.null == TRUE){
    cat("\n","\n", "Note: null not imposed (interactions are present).", "\n", "\n")
    impose.null<-FALSE
  }
  
  # check for polynomial terms
  poly.check <- max(unlist(lapply(mod$model, FUN=class))=="poly")
  
  # do not impose the null for polynomial terms
  if( poly.check == 1 & impose.null == TRUE){
    cat("\n","\n", "Note: null not imposed (polynomial terms are present).", "\n", "\n")
    impose.null<-FALSE
  }
  
  # load in a function to create clustered standard errors
  # by Mahmood Arai: http://thetarzan.wordpress.com/2011/06/11/clustered-standard-errors-in-r/
  cl   <- function(dat, fm, cluster){
    #require(sandwich, quietly = TRUE)
    #require(lmtest, quietly = TRUE)
    M <- length(unique(cluster))
    N <- length(cluster)
    K <- fm$rank
    dfc <- (M/(M-1))
    uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
    vcovCL <- dfc*sandwich(fm, meat.=crossprod(uj)/N)
    coeftest(fm, vcovCL) }
  
  
  se.clust <- cl(dat, mod, clust)[ind.variables.names,2]               # retrieve the clustered SEs
  beta.mod <- coefficients(mod)[ind.variables.names]                   # retrieve the estimated coefficients
  w <- beta.mod / se.clust                                             # calculate the wald test statistics
  
  
  # if the null is to be imposed, execute the following code
  if(impose.null==TRUE){
    
    p.store <- c()                                                            # store wild boostrapped p-values
    w.store <- matrix(data=NA, nrow=boot.reps, ncol=length(ind.variables))    # store bootstrapped test statistics
    
    if(attr(mod$terms, "intercept") == 1 ){offset <- 1}else{offset <- 0}
    
    # no factors, so remove any variables that were dropped from original model
    # (code from http://www.cookbook-r.com/Formulas/Creating_a_formula_from_a_string/)
    
    #form.new <- as.formula(paste("dv.new", paste(ind.variables, collapse = " + "), sep=" ~ "))
    form.new <- as.formula(paste("dv.new", paste(ind.variables.data, collapse = " + "), sep=" ~ "))
    
    if(prog.bar==TRUE){cat("\n")}
    for(j in 1:length(ind.variables)){
      
      if(prog.bar==TRUE){cat("Independent variable being bootstrapped: ", ind.variables[j], "\n")}
      
      # run model imposing the null hypothesis

      #form.null <- as.formula( paste ("dv.new", "~", paste( ind.variables[1:length(ind.variables) %w/o% j], collapse= " + " ) ) )
      form.null <- as.formula( paste ("dv.new", "~", paste( ind.variables.data[1:length(ind.variables.data) %w/o% j], collapse= " + " ) ) )
      mod.null <- glm(form.null, data = dat, family = mod$family)
      null.resid <- residuals(mod.null)
      
      boot.dat <- dat           # copy the data set into a bootstrap resampling dataset
      wald.store <- c()         # create a container for storing the test statistics    
      
      if(prog.bar==TRUE){pb <- txtProgressBar(min = 0, max = boot.reps, initial = 0, style = 3)}
      for(i in 1:boot.reps){
        
        if(prog.bar==TRUE){setTxtProgressBar(pb, value=i)}
        
        # assign wild bootstrap weights
        weight <- c(1, -1)[rbinom(G, size=1, prob=0.5)                   # assign wild bootstrap weights
                           + 1][match(clust, unique(clust))]    
        pseudo.resid <- null.resid*weight                                # create pseudo-residuals using weights
        pseudo.dv <- predict(mod.null)+ pseudo.resid                     # create pseudo-observations using pseudo-residuals
        boot.dat[,"dv.new"] <- pseudo.dv                                 # create a bootstrap replicate data set (note dv.new)
        
        boot.mod <- glm(form.new, data = boot.dat, family = mod$family)  # run a model on the bootstrap replicate data (note form.new)
        
        se.boot <- cl(boot.dat, boot.mod, clust)[offset+j,2]             # retrieve the bootstrap clustered SE
        beta.boot <- coefficients(boot.mod)[offset+j]                    # store the bootstrap beta coefficient
        wald.store[i] <- beta.boot / se.boot                             # store the bootstrap test statistic
        
      }
      if(prog.bar==TRUE){close(pb)}
      
      p.store[j] <- 1 - ( sum( abs(w[offset + j]) > abs(wald.store) ) / boot.reps )    # calculate the wild bootstrap p-value
      w.store[,j] <- wald.store
      
    }
    
    # calculate t-stat for intercept, if present, w/o imposing the null
    if(attr(mod$terms, "intercept") == 1 ){
      
      if(prog.bar==TRUE){cat("Independent variable being bootstrapped:  Intercept (null not imposed)", "\n")}
      
      # don't impose the null for the constant (but still call it null.resid)
      null.resid <- residuals(mod)
      
      boot.dat <- dat           # copy the data set into a bootstrap resampling dataset
      wald.store <- c()         # create a container for storing the test statistics    
      
      if(prog.bar==TRUE){pb <- txtProgressBar(min = 0, max = boot.reps, initial = 0, style = 3)}
      for(i in 1:boot.reps){
        
        if(prog.bar==TRUE){setTxtProgressBar(pb, value=i)}
        
        weight <- c(1, -1)[rbinom(G, size=1, prob=0.5)                   # assign wild bootstrap weights
                           + 1][match(clust, unique(clust))]    
        pseudo.resid <- null.resid*weight                                # create pseudo-residuals using weights
        pseudo.dv <- predict(mod)+ pseudo.resid                          # create pseudo-observations using pseudo-residuals
        boot.dat[,"dv.new"] <- pseudo.dv                                 # create a bootstrap replicate data set (note dv.new)
        
        boot.mod <- glm(form.new, data = boot.dat, family = mod$family)  # run a model on the bootstrap replicate data (note form.new)
        
        se.boot <- cl(boot.dat, boot.mod, clust)[1,2]                    # retrieve the bootstrap clustered SE
        beta.boot <- coefficients(boot.mod)[1]                           # store the bootstrap beta coefficient
        wald.store[i] <- (beta.boot - beta.mod[1]) / se.boot             # store the bootstrap test statistic
        
      }
      if(prog.bar==TRUE){close(pb)}
      
      p.store <- c( 1 - ( sum( abs(w[1]) > abs(wald.store) ) / boot.reps ), p.store)    # calculate the wild bootstrap p-value
      w.store <- cbind(wald.store, w.store)
      
      
    }
    
    ci.lo = NULL
    ci.hi = NULL
    print.ci = NULL
    out.ci = NULL
    
    # if the null is NOT to be imposed...
  }else{
    
    if(prog.bar==TRUE){cat("Wild Cluster bootstrapping w/o imposing null...", "\n")}
    
    boot.dat <- dat                                              # copy the data set into a bootstrap resampling dataset
    w.store <- matrix(data=NA, nrow=boot.reps, ncol=length(ind.variables.names))    # store bootstrapped test statistics
    
    # keep track of the beta bootstrap replicates for possible output
    rep.store <- matrix(data=NA, nrow=boot.reps, ncol=length(beta.mod))
    colnames(rep.store) <- ind.variables.names
    
    resid <- residuals(mod)                                                         # get the residuals for the model
    
    if(prog.bar==TRUE){pb <- txtProgressBar(min = 0, max = boot.reps, initial = 0, style = 3)}
    for(i in 1:boot.reps){
      
      if(prog.bar==TRUE){setTxtProgressBar(pb, value=i)}
      
      weight <- c(1, -1)[rbinom(G, size=1, prob=0.5)                   # assign wild bootstrap weights
                         + 1][match(clust, unique(clust))] 
      pseudo.resid <- resid*weight                                     # create pseudo-residuals using weights
      pseudo.dv <- predict(mod)+ pseudo.resid                          # create pseudo-observations using pseudo-residuals
      boot.dat[,"dv.new"] <- pseudo.dv                                 # create a bootstrap replicate data set (note dv.new)
      
      boot.mod <- glm(form.new, data = boot.dat, family = mod$family)  # run a model on the bootstrap replicate data (note form.new)
      
      se.boot <- cl(boot.dat, boot.mod, clust)[,2]                     # retrieve the bootstrap clustered SE
      beta.boot <- coefficients(boot.mod)[ind.variables.names]         # store the bootstrap beta coefficient
      w.store[i,] <- (beta.boot-beta.mod) / se.boot                    # store the bootstrap test statistic
      
      rep.store[i,] <- beta.boot                                       # store the bootstrap beta for output
      
      
    }
    if(prog.bar==TRUE){close(pb)}
    
    comp.fun<-function(vec2, vec1){as.numeric(vec1>vec2)}                            # a simple function comparing v1 to v2
    p.store.s <- t(apply(X = abs(w.store), FUN=comp.fun, MARGIN = 1, vec1 = abs(w))) # compare the BS test stats to orig. result
    p.store <- 1 - ( colSums(p.store.s) / dim(w.store)[1] )                          # calculate the cluster bootstrap p-value
    
    
    # compute critical t-statistics for CIs
    crit.t <- apply(X=abs(w.store), MARGIN=2, FUN=quantile, probs=ci.level )
    ci.lo <- beta.mod - crit.t*se.clust
    ci.hi <- beta.mod + crit.t*se.clust
    
    
    
    print.ci <- cbind(ind.variables.names, ci.lo, ci.hi)
    print.ci <- rbind(c("variable name", "CI lower", "CI higher"), print.ci)
    
    out.ci <- cbind(ci.lo, ci.hi)
    rownames(out.ci) <- ind.variables.names
    colnames(out.ci) <- c("CI lower", "CI higher")
    
  }
  
  out <- matrix(p.store, ncol=1)
  colnames(out) <- c("wild cluster BS p-value")
  rownames(out) <- ind.variables.names
  out.p <- cbind(ind.variables.names, round(out, 3))
  out.p <- rbind(c("variable name", "wild cluster BS p-value"), out.p)
  
  printmat <- function(m){
    write.table(format(m, justify="right"), row.names=F, col.names=F, quote=F, sep = "   ")
  }
  
  if(report==T){
    
    cat("\n", "\n", "Wild Cluster Bootstrapped p-values: ", "\n", "\n")
    printmat(out.p)
    if(is.null(print.ci) == FALSE){
      cat("\n", "Confidence Intervals (derived from bootstrapped t-statistics): ", "\n", "\n")
      printmat(print.ci)
    }
    
    if(length(ind.variables.names) < length(ind.variables.names.full)){
      cat("\n", "\n", "****", "Note: ", length(ind.variables.names.full) - length(ind.variables.names), " variables were unidentified in the model and are not reported.", "****", "\n", sep="")
      cat("Variables not reported:", "\n", sep="")
      cat(ind.variables.names.full[!ind.variables.names.full %in% ind.variables.names], sep=", ")
      cat("\n", "\n")
    }
    
  }
  
  out.list<-list()
  out.list[["p.values"]]<-out
  out.list[["ci"]] <- out.ci
  if(output.replicates == TRUE){out.list[["replicates"]] <- rep.store}
  return(invisible(out.list))
  
  
  
}

