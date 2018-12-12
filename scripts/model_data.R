# relevel factors
str(dat)
dat$flee <- relevel(dat$flee, ref = c('Not fleeing'))
dat$threat_level <- relevel(dat$threat_level, ref = c('other'))
dat$race <- relevel(dat$race, ref = c('W'))

# get cumulative sum of unarmed and armed deaths by race 
dat$unarmed_ind <- ifelse(is.na(dat$armed_fac), 0, 
                          ifelse(dat$armed_fac == 'Unarmed', 1, 0))

# recode NA in race to unknown 
dat$race <- ifelse(is.na(as.character(dat$race)), 'unknown', dat$race)

# get cumulative sum of unarmed ind
dat$cum_sum_unarmed <- NA
unique_race <- unique(dat$race)
for(i in 1:length(unique(dat$race))){
  this_race <- unique_race[i]
  dat$cum_sum_unarmed[dat$race == this_race] <- cumsum(dat$unarmed_ind[dat$race == this_race])
}


# group by race and 
# estimate lm
mod <- glm(armed_fac ~ race, data = dat, family = 'binomial')
exp(cbind(coef(mod), confint(mod)))  


# estimate lm
mod <- glm(armed_fac ~ manner_of_death + age + gender + race + signs_of_mental_illness + threat_level +
             flee + body_camera, data = dat, family = 'binomial')

exp(cbind(coef(mod), confint(mod)))  
