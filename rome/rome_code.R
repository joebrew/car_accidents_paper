#########################################################################################
library(reshape2)
library(dplyr)
library(tidyr)
library(lubridate)

# This script cleans the AS Roma football match gamesa
homeFolder <- '/home/benbrew/Documents'
projectFolder <- paste0(homeFolder, '/car_accidents_paper')
footballFolder <- paste0(projectFolder, '/rome/football_games')
trafficFolder <- paste0(projectFolder, '/rome/car_accidents')

# Load raw footballa
football <- read.csv(paste0(footballFolder, '/as_roma_games.csv'), header = FALSE)

# git rid of unnecssary columns
football <- football[, c(1:4,9)]

# rename columns
colnames(football) <- c('date', 'home_team', 'score', 'away_team', 'outcome')

# create indicator for home or away
football$home <- ifelse(football$home_team == 'AS Roma', TRUE, FALSE)

# format footballe
football$date <- parse_date_time(x = football$date,
                            orders = c("d m y", "d B Y", "m/d/y"))
football$date <- as.Date(football$date, format = 'Y%-%m-%d')

# remove unneeded columns
football$home_team <- NULL
football$away_team <- NULL
football$score <- NULL

# organize football so it matches accidents 
football <- football %>%
  mutate(game_day = TRUE,
         location_bi = ifelse(home, 'home', 'away'),
         outcome = ifelse(outcome == 'L', 'loss',
                          ifelse(outcome == 'W', 'win', 'tie')),
         score = NA,
         location = NA) 



##################################################################################
# Read in rome traffic data 
jan_june_2013 <- read.csv(paste0(trafficFolder, '/jan_june_2013.csv'), sep = ';')
july_dec_2013 <- read.csv(paste0(trafficFolder, '/july_dec_2013.csv'), sep = ';')
july_dec_2014 <- read.csv(paste0(trafficFolder, '/july_dec_2014.csv'), sep = ';')

# combine all 3 data sets
accidents <- rbind(jan_june_2013, july_dec_2013, july_dec_2014)

# convert date 
temp <- strsplit(as.character(accidents$DataOraIncidente), " ")
temp.2 <- lapply(temp, function(x) x[1])
temp.3 <- unlist(cbind(temp.2))
accidents$DataOraIncidente <- as.factor(temp.3)
accidents$DataOraIncidente <- as.Date(accidents$DataOraIncidente, format = '%d/%m/%Y')

# group by dates and get counts for accidents
accidents <- accidents %>%
  group_by(date = DataOraIncidente) %>%
  summarise(accidents = n())

# Join accidents and football
# Join together football and accidents
rome <- data.frame(date = seq(min(accidents$date),
                        max(accidents$date),
                        1))

rome <- 
  rome %>%
  left_join(football, by = 'date') %>%
  left_join(accidents, by = 'date') %>%
  mutate(game_day = ifelse(is.na(game_day), FALSE, game_day),
         dataset = 'rome') %>%
  dplyr::select(date, game_day, location_bi, outcome, score,
                location, dataset, accidents)

# Get rid of anything extra
rm(accidents, football, jan_june_2013, july_dec_2013, july_dec_2014)
