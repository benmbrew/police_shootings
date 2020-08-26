# this script will read in data from kaggle and clean for modeling
source('functions.R')

# read in all data
dat <- as.data.frame(read.csv('../data/fatal-police-shootings-data.csv', 
                              stringsAsFactors = FALSE,
                              na.strings = c("NA" = "")))
poverty <- read.csv('../data/PercentagePeopleBelowPovertyLevel.csv',
                    stringsAsFactors = FALSE,
                    na.strings = c("NA" = ""))
high_school <- read.csv('../data/PercentOver25CompletedHighSchool.csv',
                        stringsAsFactors = FALSE,
                        na.strings = c("NA" = ""))
median_income <- read.csv('../data/MedianHouseholdIncome2015.csv',
                          stringsAsFactors = FALSE,
                          na.strings = c("NA" = ""))
race <- read.csv('../data/ShareRaceByCity.csv',
                 stringsAsFactors = FALSE,
                 na.strings = c("NA" = ""))

# make names lower case and replace space with underscore
names(dat) <- tolower(names(dat))
names(poverty) <- tolower(names(poverty))
names(high_school) <- tolower(names(high_school))
names(median_income) <- tolower(names(median_income))
names(race) <- tolower(names(race))

# convert date
dat$date <- as.Date(dat$date, format = '%Y-%m-%d')

# recode 
dat$Weapon <- as.factor(ifelse(is.na(dat$armed), 'unknown', 
                                  ifelse(grepl('undetermined', dat$armed), 'unknown',
                                         ifelse(grepl('unknown weapon', dat$armed), 'unknown',
                                                ifelse(grepl('unarmed', dat$armed), 'unarmed',
                                                       ifelse(grepl('nail|bean-bag|pellet|BB', dat$armed), 'fake_gun',
                                                              ifelse(grepl('gun', dat$armed), 'real_gun',
                                                                     ifelse(grepl('knife|ax|bayonet|box_cutter|chain|glass|^hatchet$|^machete$|pick|samurai|scissors|sharp|spear', dat$armed), 'sharp_object',
                                                                            'misc'))))))))

dat$armed <- ifelse(is.na(dat$armed), 'unknown',
                    ifelse(grepl('undetermined', dat$armed), 'unknown',
                           ifelse(grepl('unknown weapon', dat$armed), 'unknown', dat$armed)))

# recode flee to remove NA
dat$flee <- ifelse(is.na(dat$flee), 'unknown', dat$flee)

# get cumulative sum of unarmed and armed deaths by race 
dat$unarmed_ind <- ifelse(is.na(dat$Weapon), 0, 
                          ifelse(dat$Weapon == 'unarmed', 1, 0))

# recode NA in race to unknown 
dat$race <- ifelse(is.na(as.character(dat$race)), 'unknown', dat$race)


# get cumulative sum of unarmed ind
dat$cum_sum_unarmed <- NA
unique_race <- unique(dat$race)
for(i in 1:length(unique(dat$race))){
  this_race <- unique_race[i]
  dat$cum_sum_unarmed[dat$race == this_race] <- cumsum(dat$unarmed_ind[dat$race == this_race])
}
dat$unarmed_ind <- NULL

##########
# combine data
##########

# first make all cities lower case 
dat$city <- as.character(tolower(dat$city))
high_school$city <- as.character(tolower(enc2utf8(high_school$city)))
median_income$city <- as.character(tolower(enc2utf8(median_income$city)))
poverty$city <- as.character(tolower(enc2utf8(poverty$city)))
race$city <- as.character(tolower(enc2utf8(race$city)))

# remove "city", "cdp", "town", "borough", "village"
remove_string <- 'city|cdp|town|borough|village'
high_school$city <- trimws(gsub(remove_string, '', high_school$city), which = 'both')
median_income$city <- trimws(gsub(remove_string, '', median_income$city), which = 'both')
poverty$city <- trimws(gsub(remove_string, '', poverty$city), which = 'both')
race$city <- trimws(gsub(remove_string, '', race$city), which = 'both')

# remove city and township from dat
remove_string <- 'city|township'

# get city strings 
demo_city <- unique(race$city)
dat_city<- unique(trimws(gsub(remove_string, '', dat$city), which = 'both'))

fuzzy_geo <- stringdistmatrix(a = dat_city,
                              b = demo_city)
x <- apply(fuzzy_geo, 1, function(x){
  # get the index of the best match(es)
  #best_match <- which.min(x)
  the_min <- min(x)
  best_match <- which(x  == the_min)
  # extract the best match from geo_unique_2011
  best_names <- demo_city[best_match]
  # paste together the best names
  best_names <- paste0(best_names, collapse = ';')
})
# matching
fuzzy_dict <- data_frame(name_dat_city = dat_city,
                         name_demo_city = x)
# add 2011 old
fuzzy_dict$old_dat_city <- dat_city

# keep only the 2016 name and the original 2011 names
fuzzy_dict$name_dat_city <- NULL

# change name of variable you want to join (2016) into "Geography"
colnames(fuzzy_dict)[1] <- 'city'

# get new city name
high_school <- get_city(high_school)
median_income <- get_city(median_income)
poverty <- get_city(poverty)
race <- get_city(race)

# join demo data
demo_1 <- inner_join(high_school, median_income, by = c('city', 'state'))
demo_2 <- inner_join(demo_1, race, by = c('city', 'state'))
demo_all <- inner_join(demo_2, poverty, by = c('city', 'state'))
rm(demo_1, demo_2, fuzzy_dict, fuzzy_geo, high_school, median_income, poverty, 
   race, dat_city, demo_city, remove_string, this_race, unique_race, x)

# remove duplicates
demo_all<- demo_all[!duplicated(demo_all[c(1,2)]),]

# join demo_all and dat by city and state
dat_demo <- inner_join(dat, demo_all, by = c('city', 'state'))
rm(demo_all)

# save data objects
saveRDS(dat, '../data/cleaned_shootings_data.rda')
saveRDS(dat_demo, '../data/cleaned_shootings_data_with_demographics.rda')

