# ##### Directories 
root <- '/home/joebrew/Documents/bcndata'
data_dir <- paste0(root, '/data/accidents_de_cotxe')
code_dir <- paste0(root, '/code')

##### Libraries
library(readr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(rgdal)
library(rgeos)
library(RColorBrewer)
library(ggmap)
library(weatherData)
# require(RCurl)
# options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

##### Theme
source(paste0(root, '/lib/theme.R'))
source(paste0(root, '/lib/helpers.R'))

##### Data read in
football <- read_csv(paste0(data_dir, '/football_games/games_dirty.csv'))

# Clean up date
football$new_date <- NA
for (i in 1:nrow(football)){
  old_date <- football$date[i]
  old_date_split <- strsplit(old_date, ' ')
  year <- old_date_split[[1]][3]
  cleaned_year <- substr(year, 1, 4)
  new_date <- paste0(
    old_date_split[[1]][1], ' ',
    old_date_split[[1]][2], ' ',
    cleaned_year)
  football$new_date[i] <- new_date
}
football$date <- football$new_date
football$new_date <- NULL

# Clean up location
football$location <- 
  gsub('[show]', '', football$location, fixed = TRUE)

# Read in the already cleaned football
ac <-  read_csv(paste0(data_dir, '/football_games/games.csv'))

# Bind together
football <- rbind(ac, football)

# Clean up date
football$date <- as.Date(football$date, format = '%d %B %Y')

# Write new csv
write_csv(football, paste0(data_dir, '/football_games/games_2010-2014.csv'))

