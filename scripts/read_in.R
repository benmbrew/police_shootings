# this script will read in data from kaggle and clean for modeling

# set libraries
library(tidyverse)
library(readr)

# read in all data
deaths <- read_csv('../data/PoliceKillingsUS.csv')
poverty <- read_csv('../data/PercentagePeopleBelowPovertyLevel.csv')
high_school <- read_csv('../data/PercentOver25CompletedHighSchool.csv')
median_income <- read_csv('../data/MedianHouseholdIncome2015.csv')
race <- read_csv('../data/ShareRaceByCity.csv')

# make names lower case and replace space with underscore
names(deaths) <- tolower(names(deaths))
names(poverty) <- tolower(names(poverty))
names(high_school) <- tolower(names(high_school))
names(median_income) <- tolower(names(median_income))
names(race) <- tolower(names(race))





# homogenize data for joining
names(deaths)
names(poverty)
names(high_school)
names(median_income)
names(race)
