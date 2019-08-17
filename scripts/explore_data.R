# this script will read in and analyze shootings data with demographics
source('functions.R')

# save data objects
dat <- readRDS('../data/cleaned_shootings_data.rda')
dat_demo <- readRDS('../data/cleaned_shootings_data_with_demographics.rda')

# extract year from date variable
dat$year <- format(as.Date(dat$date, format="%d/%m/%Y"),"%Y")
dat_demo$year <- format(as.Date(dat_demo$date, format="%d/%m/%Y"),"%Y")

# extract name into two columns
dat <- get_names(dat)
dat_demo <- get_names(dat_demo)

# recode '.' in column names
names(dat_demo) <- gsub('.', '_', names(dat_demo), fixed = TRUE)

# recode true and false 
plot_time(dat_demo, 
          var1 = 'race', 
          var2 = 'signs_of_mental_illness', 
          remove_unknown = TRUE, 
          filter_data = TRUE,
          filter_levels_1 = 'B|W',
          filter_levels_2 = 'True|False')

# age plots
plot_age(dat, var1 = 'Weapon', var2 = 'race', remove_unknown = TRUE)
plot_age(dat, var1 = 'Weapon', var2 = NULL, remove_unknown = TRUE)


# plot categorical variables
plot_data_bar(temp_dat = dat, 
              var1 = 'threat_level', 
              var2 = 'race', 
              filter_data = TRUE,
              filter_levels_1 = 'attack',
              filter_levels_2 = 'B|W|H',
              get_percentage = FALSE, 
              remove_unknown = TRUE)
# plot demo
plot_demo(temp_dat = dat_demo,
          var1 = 'race',
          var2 = NULL,
          var3 = 'percent_completed_hs',
          remove_unknown = TRUE,
          filter_levels_1 = 'B|W',
          filter_levels_2 = 'other')

##########
# group by race and summarise data set
##########

# first convert to numeric
dat_demo$age <- as.numeric(dat_demo$age)
dat_demo$percent_completed_hs <- as.numeric(dat_demo$percent_completed_hs)
dat_demo$median_income <- as.numeric(dat_demo$median_income)
dat_demo$share_white <- as.numeric(dat_demo$share_white)
dat_demo$share_black <- as.numeric(dat_demo$share_black)
dat_demo$share_native_american <- as.numeric(dat_demo$share_native_american)
dat_demo$share_asian <- as.numeric(dat_demo$share_asian)
dat_demo$share_hispanic <- as.numeric(dat_demo$share_hispanic)
dat_demo$poverty_rate <- as.numeric(dat_demo$poverty_rate)

# get a vector of population
temp <- data.frame(race = c('W', 'B', 'H', 'A'),
                   total_pop = c(233657078,40241818,58846134,16614625),
                   per_pop = c(73.3, 12.6, 17,5.2))

all_dat <- dat_demo %>% 
  filter(!grepl('N|O|unknown', race)) %>% 
  group_by(race) %>%
  summarise(tot_shot = sum(manner_of_death == 'shot', na.rm = TRUE),
            tot_shot_taser = sum(manner_of_death == 'shot and Tasered', na.rm = TRUE),
            mean_age = mean(age, na.rm = TRUE),
            tot_male = sum(gender == 'M', na.rm = TRUE),
            tot_fem = sum(gender == 'F', na.rm = TRUE),
            tot_mental = sum(signs_of_mental_illness == 'True', na.rm = TRUE),
            tot_attacking = sum(threat_level == 'attack', na.rm = TRUE),
            tot_not_flee = sum(flee == 'Not fleeing', na.rm = TRUE),
            tot_car_flee = sum(flee == 'Car', na.rm = TRUE),
            tot_foot_flee = sum(flee == 'Foot', na.rm = TRUE),
            tot_other_flee = sum(flee == 'Other', na.rm = TRUE),
            tot_body_cam = sum(body_camera == 'True', na.rm = TRUE),
            tot_real_gun = sum(Weapon == 'real_gun', na.rm = TRUE),
            tot_unarmed = sum(Weapon == 'unarmed', na.rm = TRUE),
            tot_misc = sum(Weapon == 'misc', na.rm = TRUE),
            tot_sharp = sum(Weapon == 'shapr_object', na.rm = TRUE),
            tot_misc = sum(Weapon == 'misc', na.rm = TRUE),
            mean_per_hs = mean(percent_completed_hs, na.rm = TRUE),
            mean_med_income = mean(median_income, na.rm = TRUE),
            mean_share_white = mean(share_white, na.rm = TRUE),
            mean_share_black = mean(share_black, na.rm = TRUE),
            mean_share_native = mean(share_native_american, na.rm = TRUE),
            mean_share_asian = mean(share_asian, na.rm = TRUE),
            mean_share_hispanic = mean(share_hispanic, na.rm = TRUE),
            mean_poverty_rate = mean(poverty_rate, na.rm = TRUE),
            counts = n())

  
# join temp with all_dat
all_dat <- inner_join(all_dat, temp, by = 'race')
all_dat$per_shot <- round((all_dat$counts/all_dat$per_pop)*100, 2)
