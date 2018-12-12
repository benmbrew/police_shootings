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

# function for time plotting 
temp_dat <- dat_demo
var1 <- 'Weapon'
var2 <- 'race'
remove_unknown <- TRUE

plot_time <- function(temp_dat, var1, var2, remove_unknown){
  # get columns
  
  if(is.null(var2)){
    sub_dat <- temp_dat[, c('date',var1)]
    x_lab <- 'Date'
    y_lab <- gsub('_', ' ', Hmisc::capitalize(var1))
    names(sub_dat) <- c('V1', 'V2')
    
    grouped_dat <- sub_dat %>% 
      filter(!is.na(V1)) %>% 
      filter(!is.na(V2)) %>% 
      group_by(V1, V2) %>% summarise(counts = n()) %>%
      group_by(V2) %>% mutate(V3 = cumsum(counts))
    grouped_dat$counts <- NULL
    
    if(remove_unknown){
      grouped_dat <- grouped_dat %>% filter(V2 != 'unknown')
    }
    
    ggplot(grouped_dat, aes(V1, V3, group = V2, color = V2)) + 
      geom_line(size = 2)
    
  } else {
    sub_dat <- temp_dat[, c('date',var1, var2)]
    x_lab <- 'Date'
    y_lab <- ''
    names(sub_dat) <- c('V1', 'V2', 'V3')
    
    grouped_dat <- sub_dat %>% 
      filter(!is.na(V1)) %>% 
      filter(!is.na(V2)) %>% 
      filter(!is.na(V3)) %>% 
      group_by(V3) %>% 
      mutate(var_counts = n()) %>%
      group_by(V1, V2, V3) %>% mutate(tot_var = mean(var_counts),
                                      cum_sum = cumsum(var_counts)) 
    grouped_dat$percentage <- round(grouped_dat$counts/grouped_dat$tot_var, 2)
    
    
    grouped_dat <- sub_dat %>% 
      filter(!is.na(V2)) %>% 
      group_by(V2) %>% 
      mutate(var_counts = n()) %>%
      group_by(V1, V2) %>% summarise(tot_var = mean(var_counts),
                                     counts = n()) 
    grouped_dat$percentage <- round(grouped_dat$counts/grouped_dat$tot_var, 2)
    
    if(get_percentage){
      names(grouped_dat)[5] <- 'V3'
    } else {
      names(grouped_dat)[4] <- 'V3'
      
  }
  
  }
  
}

# age plots
plot_age(dat, var1 = 'Weapon', var2 = 'gender', remove_unknown = TRUE)
plot_age(dat, var1 = 'Weapon', var2 = NULL, remove_unknown = TRUE)


# plot categorical variables
plot_data_bar(dat, 
              var1 = 'Weapon', 
              var2 = 'threat_level', 
              get_percentage = FALSE, 
              remove_unknown = TRUE,
              age = TRUE)
