cleanQ15 <- function(dat) {
  # find garment industry experience
  # make sure that q17 is only filled in if answer to q16 is 'no'
  with(dat, assert_that(all(q16 == 'No' | !is.na(q17_year))))
  # make sure that q17 is filled in properly
  with(dat, assert_that(all(is.na(q17_year) + is.na(q17_month) %in% c(0, 2))))
  # make sure that q15 is filled in properly
  with(dat, assert_that(all(!is.na(q15_year))))
  with(dat, assert_that(all(!is.na(q15_month))))
  
  # create total experience variable
  dat <- dat %>%
    mutate(experience = ifelse(q16 == 'Yes',
                               12 * q17_year + q17_month,
                               12 * q15_year + q15_month))
  #with(dat, assert_that(all(experience >= 12 * q15_year + q15_month)))
  
  # check that q15 is less than q17 - total experience is not less than current experience
  ## NOTE - this fails due to what looks like a mistype in line 16, fix this
  dat$experience[dat$respondent_id == 78] <- 12 * dat$q15_year[dat$respondent_id == 78] + dat$q15_month[dat$respondent_id == 78]
  with(dat, assert_that(all(experience >= 12 * q15_year + q15_month)))
  
  experience <- dat$experience/12
  return(experience)
}