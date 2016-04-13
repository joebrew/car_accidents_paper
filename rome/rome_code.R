#########################################################################################
library(reshape2)
library(dplyr)
library(tidyr)
library(lubridate)
library(XML)
library(readr)

# This script cleans the AS Roma football match gamesa
footballFolder <- 'rome/football_games'
trafficFolder <- 'rome/car_accidents'

# Load raw footballa
football <- read.csv(paste0(footballFolder, '/as_roma_games.csv'), header = FALSE)
football_time <- read.csv(paste0(footballFolder, '/as_roma_times.csv'), header = FALSE)

# git rid of unnecssary columns
football <- football[, c(1:4,9)]
football_time <- football_time[, 1:4]

# rename columns
colnames(football) <- c('date', 'home_team', 'score', 'away_team', 'outcome')
colnames(football_time) <- c('date', 'time', 'home', 'other_team')

# create indicator for home or away
football$home <- ifelse(football$home_team == 'AS Roma', TRUE, FALSE)

# format footballe
football$date <- parse_date_time(x = football$date,
                            orders = c("d m y", "d B Y", "m/d/y"))
football$date <- as.Date(football$date, format = 'Y%-%m-%d')
football_time$date <- as.Date(football_time$date, format = '%d/%m/%Y')
football_time$time <- paste0(football_time$time, ':00')

# remove unneeded columns
football$home_team <- NULL
football$away_team <- NULL
football$score <- NULL
football_time$home <- NULL
football_time$other_team <- NULL

# join football and football_time
football <- left_join(football, football_time,
                      by = 'date')

# organize football so it matches accidents 
football <- football %>%
  mutate(game_day = TRUE,
         location_bi = ifelse(home, 'home', 'away'),
         outcome = ifelse(outcome == 'L', 'loss',
                          ifelse(outcome == 'W', 'win', 'tie')),
         time = time,
         score = NA,
         location = NA) 

##################################################################################
# Read in rome traffic data 
jan_june_2013 <- read.csv(paste0(trafficFolder, '/jan_june_2013.csv'), sep = ';')
july_dec_2013 <- read.csv(paste0(trafficFolder, '/july_dec_2013.csv'), sep = ';')
july_dec_2014 <- read.csv(paste0(trafficFolder, '/july_dec_2014.csv'), sep = ';')

# Read in the annoyingly formatted early 2014 data
if('jan_june_2014.csv' %in% dir(trafficFolder)){
  jan_june_2014 <- read_csv(paste0(trafficFolder, '/jan_june_2014.csv')) 
} else {
  jan_june_2014 <- xmlParse(paste0(trafficFolder, '/xmlincidentigengiu2014.xml'))
  x <- xmlToList(jan_june_2014)  
  y <- x[['data']]
  # Function to turn to dataframe
  ydf <- function(row_number){
    obj <- y[[row_number]]
    df <- data.frame(matrix(NA, ncol = length(names(obj)),
                            nrow = 1))
    names(df) <- names(obj)
    df[1,] <- as.character(unlist(obj))
    df <- df[,names(df) %in% names(jan_june_2013)]
    return(df)
  }
  # Loop through each element of the xml
  # extracting data into a dataframe
  results_list <- list()
  for (i in 1:length(y)){
    print(i)
    results_list[[i]] <- 
      ydf(i)
  }
  # Combine all results into one dataframe
  rbf <- plyr::rbind.fill
  jan_june_2014 <- do.call('rbf', results_list)
  
  # Reformat the DataOraIncidente column to match the others
  jan_june_2014$DataOraIncidente <-
    paste0(
      format(as.Date(substr(jan_june_2014$DataOraIncidente, 1, 10)), '%m/%d/%Y'),
      ' ',
      gsub(':', '.', substr(jan_june_2014$DataOraIncidente, 12, 
                            nchar(jan_june_2014$DataOraIncidente)))
    )
  
  # Write a csv (so as to not have to repeat the slow xml parsing)
  write_csv(jan_june_2014, paste0(trafficFolder, '/jan_june_2014.csv'))
}

# Subset all 3 datasets to only include available columns
# from the jan_june_2014 data
jan_june_2013 <- jan_june_2013[,names(jan_june_2014)]
july_dec_2013 <- july_dec_2013[,names(jan_june_2014)]
july_dec_2014 <- july_dec_2014[,names(jan_june_2014)]

# combine all 4 data sets
accidents <- rbind(jan_june_2013, 
                   july_dec_2013, 
                   jan_june_2014, 
                   july_dec_2014)

# convert date 
temp <- strsplit(as.character(accidents$DataOraIncidente), " ")
temp.date <- lapply(temp, function(x) x[1])
temp.time <- lapply(temp, function(x) x[2])
temp.date <- unlist(cbind(temp.date))
temp.time <- unlist(cbind(temp.time))
accidents$DataOraIncidente <- as.factor(temp.date)
accidents$DataOraIncidente <- as.Date(accidents$DataOraIncidente, format = '%d/%m/%Y')
accidents$time <- as.factor(temp.time)
accidents$time <- gsub('.', ':', accidents$time, fixed = TRUE)

# remove those without a data
accidents <- accidents[!is.na(accidents$DataOraIncidente),]

# group by date/time and get counts for accidents
accidents_tod <- accidents %>%
  group_by(date = DataOraIncidente,
           accident_time = time) %>%
  summarise(accidents = n())

# Group by date only and get counts for accidents
accidents_day <- 
  accidents %>%
  group_by(date = DataOraIncidente) %>%
  summarise(accidents = n())

# Join accidents and football
# DAILY COUNTS
rome <- 
  data.frame(date = seq(min(accidents_day$date),
                        max(accidents_day$date),
                        1)) %>%
  left_join(football, by = 'date') %>%
  left_join(accidents_day, by = 'date') %>%
  mutate(game_day = ifelse(is.na(game_day), FALSE, game_day),
         dataset = 'rome') %>%
  dplyr::select(date, game_day, location_bi, outcome, score,
                location, dataset, accidents)

# Also join accidents and football at the TIME 
# specific level
# !!! (NOT DOING FOR NOW)

# Get rid of anything extra
rm(accidents, football, football_time, jan_june_2013, july_dec_2013, 
   july_dec_2014, jan_june_2014)
rm(temp, temp.date, temp.time, trafficFolder,
   footballFolder, accidents_day, accidents_tod)
