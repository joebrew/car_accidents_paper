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
library(Hmisc)
library(knitr)
# library(htmlTable)
# library(leaflet)
library(raster)

# ACCIDENTS
# Read in accidents data
accidents <- read_delim('paris/car_accidents/paris_car_accidents.csv', delim = ';')
# Get number by date
accidents <- 
  accidents %>%
  group_by(date = Date) %>%
  summarise(accidents = n())

# FOOTBALL GAMES
# Read in football games data
football <- read_csv('paris/football_games/games_2012_2013.csv')
# Organize so that it matches the same format as the barcelona data
football <- 
  football %>%
  mutate(game_day = TRUE,
         location_bi = ifelse(home, 'home', 'away'),
         outcome = ifelse(outcome == 'lose', 'loss', outcome),
         score = NA,
         location = NA) 


# Join together football and accidents
paris <- 
  data.frame(date = seq(min(accidents$date),
                        max(accidents$date),
                        1))
paris <- 
  paris %>%
  left_join(football, by = 'date') %>%
  left_join(accidents, by = 'date') %>%
  mutate(game_day = ifelse(is.na(game_day), FALSE, game_day),
         dataset = 'par') %>%
  dplyr::select(date, game_day, location_bi, outcome, score,
                location, dataset, accidents)

# Get rid of anything extra
rm(accidents, football)