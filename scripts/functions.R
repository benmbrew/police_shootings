# functions
# set libraries
library(tidyverse)
library(readr)
library(Hmisc)
library(stringdist)
library(RColorBrewer)
library(scales)
library(ggthemes)
library(directlabels)

# temp_data <- race
get_city <- function(temp_data){
  temp_data <- inner_join(fuzzy_dict, temp_data, by = 'city')
  
  # make old_2011 variable the new geography variable and remove geography
  temp_data$city <- NULL
  names(temp_data)[names(temp_data) == 'old_dat_city'] <- 'city'
  names(temp_data)[names(temp_data) == 'geographic.area'] <- 'state'
  
  return(temp_data)
}

# function to split name into two columns first name and last name
get_names <- function(temp_dat){
  last_name <- unlist(lapply(strsplit(as.character(temp_dat$name), split = ' ', fixed = TRUE), 
                             function(x) x[length(x)]))
  first_name <- unlist(lapply(strsplit(as.character(temp_dat$name), split = ' ', fixed = TRUE), 
                              function(x) x[1]))
  temp_dat$first_name <- first_name
  temp_dat$last_name <- last_name
  temp_dat$name <- NULL
  rm(first_name, last_name)
  return(temp_dat)
}
# 
# temp_dat = dat 
# var1 = 'threat_level' 
# var2 = 'race' 
# filter_data = TRUE
# filter_levels_1 = 'attack'
# filter_levels_2 = 'B|W|H'
# get_percentage = FALSE 
# remove_unknown = TRUE

# temp_dat = dat 
# var1 = 'threat_level' 
# var2 = 'race' 
# filter_data = TRUE
# filter_levels_1 = 'attack'
# filter_levels_2 = 'B|W|H'
# get_percentage = FALSE 
# remove_unknown = TRUE

# plotting 2 variables both categorical
plot_data_bar <- function(temp_dat, 
                          var1, 
                          var2, 
                          filter_data,
                          filter_levels_1,
                          filter_levels_2,
                          get_percentage, 
                          remove_unknown){
  # get columns
  sub_dat <- temp_dat[, c(var1, var2)]
  x_lab <- gsub('_', ' ', Hmisc::capitalize(var1))
  y_lab <- gsub('_', ' ', Hmisc::capitalize(var2))
  names(sub_dat) <- c('V1', 'V2')
  
  if(filter_data){
    sub_dat <- sub_dat %>% filter(grepl(filter_levels_1, V1))
    sub_dat <- sub_dat %>% filter(grepl(filter_levels_2, V2))
  }
  
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
  
  # make color vector
  V2_length <- length(unique(grouped_dat$V2)) + 2
  col_vec <- brewer.pal(n = V2_length, name = 'Paired')
  
  # recode V1 and V2 for plot
  grouped_dat$V1 <- gsub('_', ' ', grouped_dat$V1)
  grouped_dat$V1 <- Hmisc::capitalize(grouped_dat$V1)
  grouped_dat$V2 <- gsub('_', ' ', grouped_dat$V2)
  grouped_dat$V2 <- Hmisc::capitalize(grouped_dat$V2)
  
  if(remove_unknown){
    grouped_dat <- grouped_dat %>% filter(V1 != 'Unknown')
    grouped_dat <- grouped_dat %>% filter(V2 != 'unknown')
  }
  # plot data 
  base_plot <- ggplot(grouped_dat, aes(reorder(V1, -V3), V3, group = V2, fill = V2)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    labs(x = x_lab,
         y = y_lab) +
    scale_fill_manual(name = '',
                      values = col_vec)
  
  if(get_percentage){
    base_plot <- base_plot + scale_y_continuous(labels = scales::percent)
  }
  
  base_plot <- base_plot +  theme_update() + theme(axis.text.x = element_text(size = 7)) 
  
  return(base_plot)
  
}

# plot that includes mean age as well
plot_age <- function(temp_dat, 
                     var1, 
                     var2, 
                     remove_unknown){
  
  if(is.null(var2)){
    # get columns
    sub_dat <- temp_dat[, c(var1, 'age')]
    x_lab <- gsub('_', ' ', Hmisc::capitalize(var1))
    y_lab <- 'Age'
    
    names(sub_dat) <- c('V1', 'V2')
    
    grouped_dat <- sub_dat %>% 
      filter(!is.na(V1)) %>% 
      filter(!is.na(V2)) %>% 
      group_by(V1) %>% 
      summarise(V2 = mean(V2, na.rm = TRUE)) 
    
    # recode V1 and V2 for plot
    grouped_dat$V1 <- gsub('_', ' ', grouped_dat$V1)
    grouped_dat$V1 <- Hmisc::capitalize(grouped_dat$V1)
    
    if(remove_unknown){
      grouped_dat <- grouped_dat %>% filter(V1 != 'Unknown')
    }
    # plot data 
    base_plot <- ggplot(grouped_dat, aes(reorder(V1, -V2), V2)) +
      geom_bar(stat = 'identity') +
      labs(x = x_lab,
           y = y_lab) + theme_update() + 
      theme(axis.text.x = element_text(size = 7))  
    
    
  } else {
    # get columns
    sub_dat <- temp_dat[, c(var1, var2, 'age')]
    x_lab <- gsub('_', ' ', Hmisc::capitalize(var1))
    y_lab <- 'Age'
    
    names(sub_dat) <- c('V1', 'V2', 'V3')
    
    grouped_dat <- sub_dat %>% 
      filter(!is.na(V2)) %>% 
      group_by(V2) %>% 
      mutate(var_counts = n()) %>%
      group_by(V1, V2) %>% summarise(V3 = mean(V3, na.rm = TRUE)) 
    
    
    # make color vector
    V2_length <- length(unique(grouped_dat$V2)) + 1
    col_vec <- brewer.pal(n = V2_length, name = 'Paired')
    
    # recode V1 and V2 for plot
    grouped_dat$V1 <- gsub('_', ' ', grouped_dat$V1)
    grouped_dat$V1 <- Hmisc::capitalize(grouped_dat$V1)
    grouped_dat$V2 <- gsub('_', ' ', grouped_dat$V2)
    grouped_dat$V2 <- Hmisc::capitalize(grouped_dat$V2)
    
    if(remove_unknown){
      grouped_dat <- grouped_dat %>% filter(V1 != 'Unknown')
      grouped_dat <- grouped_dat %>% filter(V2 != 'Unknown')
    }
    # plot data 
    base_plot <- ggplot(grouped_dat, aes(reorder(V1, -V3), V3, group = V2, fill = V2)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      labs(x = x_lab,
           y = y_lab) +
      scale_fill_manual(name = '',
                        values = col_vec)
    
    
    base_plot <- base_plot +  theme_update() + theme(axis.text.x = element_text(size = 7)) 
    
  }
  
  return(base_plot)
  
}


# function for time plotting 
plot_time <- function(temp_dat, 
                      var1, 
                      var2, 
                      filter_data,
                      filter_levels_1, 
                      filter_levels_2, 
                      remove_unknown){
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
    
    g <- ggplot(grouped_dat, aes(V1, V3, group = V2, color = V2)) + 
      geom_line(size = 1) +
      xlim(c(min(grouped_dat$V1), (max(grouped_dat$V1) + 200))) +
      labs(x = x_lab,
           y = y_lab) +
      scale_fill_manual(name = '',
                        values = col_vec) +
      geom_dl(data = grouped_dat,
              aes(label = Hmisc::capitalize(gsub('_', ' ', V2))), 
              method = list(dl.combine("last.points"), cex = 0.7), 
              alpha = 1, color = 'black') +
      theme_update() + 
      theme(legend.position = 'none')
    
  } else {
    sub_dat <- temp_dat[, c('date',var1, var2)]
    x_lab <- 'Date'
    y_lab <- 'Counts'
    names(sub_dat) <- c('V1', 'V2', 'V3')
    
    if(filter_data){
      sub_dat <- sub_dat %>% filter(grepl(filter_levels_1, V2))
      sub_dat <- sub_dat %>% filter(grepl(filter_levels_2, V3))
    }
    
    
    grouped_dat <- sub_dat %>% 
      filter(!is.na(V1)) %>% 
      filter(!is.na(V2)) %>% 
      filter(!is.na(V3)) %>% 
      group_by(V1,V2, V3) %>% 
      summarise(var_counts = n()) %>% 
      group_by(V2,V3) %>% 
      mutate(V4 = cumsum(var_counts))
    grouped_dat$var_counts <- NULL
    
    if(remove_unknown){
      grouped_dat <- grouped_dat %>% filter(V2 != 'unknown')
      grouped_dat <- grouped_dat %>% filter(V3 != 'unknown')
      
    }
    
    # make color vector
    col_length <- length(unique(interaction(grouped_dat$V2, grouped_dat$V3))) 
    col_vec <- brewer.pal(n = col_length, name = 'Spectral')
    
    g <- ggplot(grouped_dat, aes(V1, V4, color = interaction(V2, V3))) +
      geom_line(size = 1) +
      xlim(c(min(grouped_dat$V1), (max(grouped_dat$V1) + 200))) +
      labs(x = x_lab,
           y = y_lab) +
      scale_fill_manual(name = '',
                        values = col_vec) +
      geom_dl(data = grouped_dat,
              aes(label = paste0(gsub('_', ' ', V2),'\n', gsub('_', ' ', V3))), 
              method = list(dl.combine("last.points"), cex = 0.7), 
              alpha = 1, color = 'black') +
      theme_update() + 
      theme(legend.position = 'none')
    
  }
  
  return(g)
  
}


plot_demo <- function(temp_dat,
                      var1,
                      var2, 
                      var3,
                      remove_unknown,
                      filter_levels_1,
                      filter_levels_2) {
  # get columns
  if(is.null(var2)){
    sub_dat <- temp_dat[, c(var1, var3)]
    x_lab <- Hmisc::capitalize(var1)
    y_lab <- Hmisc::capitalize(gsub('_', ' ', var3))
    names(sub_dat) <- c('V1', 'V2')
    
    sub_dat <- sub_dat %>% filter(grepl(filter_levels_1, V1))
    sub_dat$V2 <- as.numeric(sub_dat$V2)
    
    grouped_dat <- sub_dat %>% 
      filter(!is.na(V1)) %>% 
      group_by(V1) %>% 
      summarise(var_counts = n(),
                mean_var = mean(V2, na.rm = TRUE)) 
    
    
    if(remove_unknown){
      grouped_dat <- grouped_dat %>% filter(V1 != 'unknown')
      
    }
    
    # make color vector
    
    g <- ggplot(grouped_dat, aes(V1, mean_var)) +
      geom_bar(stat = 'identity') +
      labs(x = x_lab,
           y = y_lab) +
      theme_update() 
    
  } else {
    
    sub_dat <- temp_dat[, c(var1, var2,var3)]
    x_lab <- Hmisc::capitalize(var1)
    y_lab <- Hmisc::capitalize(gsub('_', ' ', var2))
    names(sub_dat) <- c('V1', 'V2', 'V3')
    sub_dat$V3 <- as.numeric(sub_dat$V3)
    
    sub_dat <- sub_dat %>% filter(grepl(filter_levels_1, V1))
    sub_dat <- sub_dat %>% filter(grepl(filter_levels_2, V2))
    
    grouped_dat <- sub_dat %>% 
      filter(!is.na(V1)) %>% 
      filter(!is.na(V2)) %>% 
      filter(!is.na(V3)) %>% 
      group_by(V1,V2) %>% 
      summarise(V3 = mean(V3, na.rm = TRUE)) 
    
    
    if(remove_unknown){
      grouped_dat <- grouped_dat %>% filter(V1 != 'unknown')
      grouped_dat <- grouped_dat %>% filter(V2 != 'unknown')
      
    }
    
    # make color vector
    col_length <- length(unique(grouped_dat$V2)) + 2
    col_vec <- brewer.pal(n = col_length, name = 'Spectral')
    
    g <- ggplot(grouped_dat, aes(V1, V3, fill = V2)) +
      geom_bar(stat = 'identity') +
      labs(x = x_lab,
           y = y_lab) +
      scale_fill_manual(name = Hmisc::capitalize(gsub('_', ' ', var2)),
                        values = col_vec) +
      theme_update() 
    
    
  }
  
  return(g)
  
}

