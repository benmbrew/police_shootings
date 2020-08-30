# load libraries
library(tidyverse)
library(reshape2)
library(pracma)
library(RColorBrewer)
# load data 
dat <- readRDS('../data/cleaned_shootings_data.rda')
dat$cum_sum_unarmed <- NULL
dat$manner_of_death <- NULL
dat$Weapon <- NULL
dat$id   <- NULL
# recode armed to armed_gun, armed other, unarmed
armed_gun <-'^gun$|gun and car|gun and knife|gun and sword|gun and vehicle|gun and explosives|hatchet and gun|machete and gun|vehicle and gun'
dat$armed_gun <- ifelse(grepl(armed_gun, dat$armed), 'Gun', 
                        ifelse(dat$armed == 'unarmed','Unarmed', 'Weapon other'))

# recode flee to fleeing, not fleeing, unknown
dat$flee_fac <- ifelse(grepl('Car|Foot|Other', dat$flee), 'Yes', 
                       ifelse(dat$flee == 'unknown', 'Unknown', 'No'))

# recode race and gender 
dat$gender <- ifelse(dat$gender == 'M', 'Male', 
                     ifelse(dat$gender == 'F', 'Female', 'Unknown'))
dat$race <- ifelse(dat$race == 'A', 'Asian',
                   ifelse(dat$race == 'B', 'Black',
                          ifelse(dat$race == 'H', 'Hispanic',
                                 ifelse(dat$race == 'N', 'Native',
                                        ifelse(dat$race == 'O', 'Other',
                                               ifelse(dat$race == 'unknown', 'Unknown', 'White'))))))

# make upper case 
dat$threat_level <- Hmisc::capitalize(dat$threat_level)

# get time variales
dat$year <- format(as.Date(dat$date, format="%Y-%m-%d"),"%Y")
dat$month_year <- format(as.Date(dat$date, format="%Y-%m-%d"),"%m-%Y")

# filter for male
dat <- dat %>% filter(gender == 'Male')
dat <- dat %>% filter(!is.na(age))




# make function that takes race and other variables,and age range.
# plots percent rolling avg over time and aggregated 












# function for over time rolling avgs


temp = dat
mov_avg_window=90
group_var = 'year'
filter_race = 'White|Black'
filter_gender = NULL
filter_age = c(0,100)
filter_date = c(min(dat$date),max(dat$date))
filter_state = '[a-zA-Z]'
filter_mental_illness = 'True|False'
filter_threat_level = 'Attack|Other|Undetermined'
filter_flee_fac = 'Yes|No|Unknown'
filter_body_cam = 'True|False'
filter_armed_gun = 'Gun|Unamred|Weapon other'
start_number = 0
plot_title=NULL
# create function to plot overtime: plot_type: frequency bar, points, line, smooth
plot_time <- function(temp,
                      group_var = 'year',
                      mov_avg_window=31,
                      filter_race = 'Asian|White|Black|Hispanic|Unknown',
                      filter_gender = 'Male|Female',
                      filter_age = c(0,100),
                      filter_date = c(min(dat$date), max(dat$date)),
                      filter_state = '[a-zA-Z]',
                      filter_mental_illness = 'True|False',
                      filter_threat_level = 'Attack|Other|Undetermined',
                      filter_flee_fac = 'Yes|No|Unknown',
                      filter_body_cam = 'True|False',
                      filter_armed_gun = 'Gun|Unamred|Weapon other',
                      start_number = 0,
                      plot_title=NULL){
  # filter data by inputs
  temp <- temp %>% filter(grepl(filter_race, temp$race))
  temp <- temp %>% filter(grepl(filter_gender, temp$gender))
  temp <- temp %>% filter(grepl(filter_state, temp$state))
  temp <- temp %>% filter(grepl(filter_mental_illness, temp$signs_of_mental_illness))
  temp <- temp %>% filter(grepl(filter_threat_level, temp$threat_level))
  temp <- temp %>% filter(grepl(filter_flee_fac, temp$flee_fac))
  temp <- temp %>% filter(grepl(filter_body_cam, temp$body_camera))
  temp <- temp %>% filter(grepl(filter_armed_gun, temp$armed_gun))
  temp <- temp %>% filter(age>= filter_age[1], age<=filter_age[2])
  temp <- temp %>% filter(date>= filter_date[1], date<=filter_date[2])
  
  if(group_var =='year'){
    
   temp <-  temp %>% 
     group_by(year, 
              race,
              gender, 
              threat_level, 
              flee_fac, 
              body_camera,
              armed_gun,
              signs_of_mental_illness) %>% 
     summarise(counts = n())
  } else {
    
  }
 
  
  # get rolling avg
  temp <- temp %>% 
    group_by(date,race) %>% 
    summarise(counts =n()) %>%
    group_by(race) %>%
    mutate(mvg_avg = movavg(counts,n = mov_avg_window,type='s'))
  
  # col vector
  col_vec <- brewer.pal(n = length(unique(temp$race)), name = 'Set1')
  y_axis_text <- paste0(mov_avg_window, ' day moving average')
  
  # get x and y limits 
  min_date <- min(temp$date)
  max_date <- max(temp$date)
  min_avg <- min(temp$mvg_avg)
  max_avg <- max(temp$mvg_avg)
  
  # plot
  ggplot(temp, 
         aes(date, 
             mvg_avg, 
             color = race)) + 
    geom_line() +
    scale_y_continuous(limits = c(min_avg, max_avg), 
                       breaks=seq(from = 1,to = (max(ceiling(temp$mvg_avg)) +1), by = 0.1))+
    scale_fill_manual(name = 'Race',
                      values = col_vec) +
    labs(x = 'Date',
         y = y_axis_text,
         title = plot_title)
}

# by race
plot_time(temp = all_dat,
          mov_avg_window=100,
          filter_race = 'Black|White',
          filter_age= c(0,100),
          filter_gender='Male',
          filter_armed_gun = 'Unarmed')

# redo function to groupby discreet time period (week, month, year, etC)

# deaths broken down by race over time (highlight high profile cases)

# by state




# Likelihood of being or doing something while being killed 

# Police killed over time

# crime analysis - likelihood of commiting types of crimes by race

# crime analysis - are blacks over or under represented in police deaths based on crimes
