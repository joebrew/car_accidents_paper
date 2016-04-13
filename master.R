library(Hmisc)

# Read in Barcelona data
source('barcelona/barcelona_code.R')

# Read in Paris data
source('paris/paris_code.R')

# Read in rome data
source('rome/rome_code.R')

# Combine all datasets
df <- rbind(barcelona,
            paris,
            rome)

# Fill the "outcome" column with
# non-game occurrences too
df$outcome[is.na(df$outcome)] <- 
  'no game'

# Relevel outcome so that no game is the baseline
# (ie, reference class)
df$outcome <- factor(capitalize(df$outcome),
                     levels = c('No game',
                                'Loss', 
                                'Tie',
                                'Win'))

# Scale the number of accidents so that they are
# weighted equally (ie, just because Rome has)
# more accidents doesn't mean that we want to weight
# Rome more, so we set everything based on "average"
df <-
  df %>%
  group_by(dataset) %>%
  mutate(scaler = mean(accidents, na.rm = TRUE),
         days_under_observation = n()) %>%
  ungroup %>%
  mutate(accidents_scaled = accidents / scaler) %>%
  dplyr::select(-scaler)
# now we have a column called accidents_scaled
# 1 = average number of accidents (for that city)
# <1 = below average
# >1 = above average
# we've also created a column called days_under_observation
# which just shows how long we've observed each city 
# (in case we want to weight each equally)

# Model for number of accidents
fit <- lm(accidents_scaled ~ outcome, 
          data = df)

# Visual
temp <-
  df %>%
  group_by(outcome) %>%
  summarise(y = mean(accidents_scaled, na.rm = TRUE))

# cols <- brewer.pal(4, 'Pastel2')
cols <- 
  colorRampPalette(c('lightblue', 'darkgreen'))(4)
ggplot(data = temp,
       aes(x = outcome, y = y * 100)) +
  geom_bar(stat = 'identity', alpha = 0.5,
           fill = cols) +
  xlab('Game status') +
  ylab('Scaled accidents (% of average)') +
  geom_label(aes(label = paste0(round(y * 100, digits = 2), '%'))) +
  ggtitle('Car accidentology by home team football game outcomes')

##################### 
# Model for and visualization for Paris

# Fill the "outcome" column with
# non-game occurrences too
paris$outcome[is.na(paris$outcome)] <- 
  'no game'

# Relevel outcome so that no game is the baseline
# (ie, reference class)
paris$outcome <- factor(capitalize(paris$outcome),
                     levels = c('No game',
                                'Loss', 
                                'Tie',
                                'Win'))

# Scale accidents
paris <-
  paris %>%
  group_by(dataset) %>%
  mutate(scaler = mean(accidents, na.rm = TRUE),
         days_under_observation = n()) %>%
  ungroup %>%
  mutate(accidents_scaled = accidents / scaler) %>%
  dplyr::select(-scaler)

# Model for number of accidents
fit <- lm(accidents_scaled ~ outcome, 
          data = paris)

# Visual
temp <-
  paris %>%
  group_by(outcome) %>%
  summarise(y = mean(accidents_scaled, na.rm = TRUE))

# cols <- brewer.pal(4, 'Pastel2')
cols <- 
  colorRampPalette(c('lightblue', 'darkgreen'))(4)
ggplot(data = temp,
       aes(x = outcome, y = y * 100)) +
  geom_bar(stat = 'identity', alpha = 0.5,
           fill = cols) +
  xlab('Game status') +
  ylab('Scaled accidents (% of average)') +
  geom_label(aes(label = paste0(round(y * 100, digits = 2), '%'))) +
  ggtitle('Car accidentology by home team football game outcomes')


##################### 
# Model for and visualization for barcelona

# Fill the "outcome" column with
# non-game occurrences too
barcelona$outcome[is.na(barcelona$outcome)] <- 
  'no game'

# Relevel outcome so that no game is the baseline
# (ie, reference class)
barcelona$outcome <- factor(capitalize(barcelona$outcome),
                        levels = c('No game',
                                   'Loss', 
                                   'Tie',
                                   'Win'))

# Scale accidents
barcelona <-
  barcelona %>%
  group_by(dataset) %>%
  mutate(scaler = mean(accidents, na.rm = TRUE),
         days_under_observation = n()) %>%
  ungroup %>%
  mutate(accidents_scaled = accidents / scaler) %>%
  dplyr::select(-scaler)

# Model for number of accidents
fit <- lm(accidents_scaled ~ outcome, 
          data = barcelona)

# Visual
temp <-
  barcelona %>%
  group_by(outcome) %>%
  summarise(y = mean(accidents_scaled, na.rm = TRUE))

# cols <- brewer.pal(4, 'Pastel2')
cols <- 
  colorRampPalette(c('lightblue', 'darkgreen'))(4)
ggplot(data = temp,
       aes(x = outcome, y = y * 100)) +
  geom_bar(stat = 'identity', alpha = 0.5,
           fill = cols) +
  xlab('Game status') +
  ylab('Scaled accidents (% of average)') +
  geom_label(aes(label = paste0(round(y * 100, digits = 2), '%'))) +
  ggtitle('Car accidentology by home team football game outcomes')

##################### 
# Model for and visualization for rome

# Fill the "outcome" column with
# non-game occurrences too
rome$outcome[is.na(rome$outcome)] <- 
  'no game'

# Relevel outcome so that no game is the baseline
# (ie, reference class)
rome$outcome <- factor(capitalize(rome$outcome),
                            levels = c('No game',
                                       'Loss', 
                                       'Tie',
                                       'Win'))

# Scale accidents
rome <-
  rome %>%
  group_by(dataset) %>%
  mutate(scaler = mean(accidents, na.rm = TRUE),
         days_under_observation = n()) %>%
  ungroup %>%
  mutate(accidents_scaled = accidents / scaler) %>%
  dplyr::select(-scaler)

# Model for number of accidents
fit <- lm(accidents_scaled ~ outcome, 
          data = rome)

# Visual
temp <-
  rome %>%
  group_by(outcome) %>%
  summarise(y = mean(accidents_scaled, na.rm = TRUE))

# cols <- brewer.pal(4, 'Pastel2')
cols <- 
  colorRampPalette(c('lightblue', 'darkgreen'))(4)
ggplot(data = temp,
       aes(x = outcome, y = y * 100)) +
  geom_bar(stat = 'identity', alpha = 0.5,
           fill = cols) +
  xlab('Game status') +
  ylab('Scaled accidents (% of average)') +
  geom_label(aes(label = paste0(round(y * 100, digits = 2), '%'))) +
  ggtitle('Car accidentology by home team football game outcomes')

#####################################################################################################
# Models with day of week as confounder.

# Add day of week to each data set 
barcelona$day <- weekdays(barcelona$date)
paris$day <- weekdays(paris$date)
rome$day <- weekdays(rome$date)

# combine data sets 
df <- rbind(barcelona,
            paris,
            rome)

# Relevel outcome so that no game is the baseline
# (ie, reference class)
df$day <- factor(capitalize(df$day),
                       levels = c('Sunday',
                                  'Monday', 
                                  'Tuesday',
                                  'Wednesday',
                                  'Thursday',
                                  'Friday',
                                  'Saturday'))

# model with all cities
fit <- lm(accidents_scaled ~ outcome + day, 
          data = df)

# Visual
temp <-
  df %>%
  group_by(day) %>%
  summarise(y = mean(accidents_scaled, na.rm = TRUE))

# cols <- brewer.pal(4, 'Pastel2')
cols <- 
  colorRampPalette(c('lightblue', 'darkgreen'))(7)
ggplot(data = temp,
       aes(x = day, y = y * 100)) +
  geom_bar(stat = 'identity', alpha = 0.5,
           fill = cols) +
  xlab('Day of Week') +
  ylab('Scaled accidents (% of average)') +
  geom_label(aes(label = paste0(round(y * 100, digits = 2), '%'))) +
  ggtitle('Car accidentology by Day of Week')


# Visual
temp <-
  df %>%
  group_by(outcome, day) %>%
  summarise(y = mean(accidents_scaled, na.rm = TRUE))

# cols <- brewer.pal(4, 'Pastel2')
ggplot(data = temp, aes(day, y * 100, fill = outcome)) +
  geom_bar(stat = 'identity') +
  xlab('Day of Week') +
  ylab('Scaled accidents (% of average)') +
  ggtitle('Car accidentology by Day of Week and Outcome')

##########
# Paris
# Relevel outcome so that no game is the baseline
# (ie, reference class)
paris$day <- factor(capitalize(paris$day),
                 levels = c('Sunday',
                            'Monday', 
                            'Tuesday',
                            'Wednesday',
                            'Thursday',
                            'Friday',
                            'Saturday'))

# model with all cities
fit <- lm(accidents_scaled ~ outcome + day, 
          data = paris)

# Visual
temp <-
  paris %>%
  group_by(day) %>%
  summarise(y = mean(accidents_scaled, na.rm = TRUE))

# cols <- brewer.pal(4, 'Pastel2')
cols <- 
  colorRampPalette(c('lightblue', 'darkgreen'))(7)
ggplot(data = temp,
       aes(x = day, y = y * 100)) +
  geom_bar(stat = 'identity', alpha = 0.5,
           fill = cols) +
  xlab('Day of Week') +
  ylab('Scaled accidents (% of average)') +
  geom_label(aes(label = paste0(round(y * 100, digits = 2), '%'))) +
  ggtitle('Car accidentology by Day of Week')


# Visual
temp <-
  paris %>%
  group_by(outcome, day) %>%
  summarise(y = mean(accidents_scaled, na.rm = TRUE))

# cols <- brewer.pal(4, 'Pastel2')
ggplot(data = temp, aes(day, y * 100, fill = outcome)) +
  geom_bar(stat = 'identity') +
  xlab('Day of Week') +
  ylab('Scaled accidents (% of average)') +
  ggtitle('Car accidentology by Day of Week and Outcome')


##########
# barcelona
# Relevel outcome so that no game is the baseline
# (ie, reference class)
barcelona$day <- factor(capitalize(barcelona$day),
                    levels = c('Sunday',
                               'Monday', 
                               'Tuesday',
                               'Wednesday',
                               'Thursday',
                               'Friday',
                               'Saturday'))

# model with all cities
fit <- lm(accidents_scaled ~ outcome + day, 
          data = barcelona)

# Visual
temp <-
  barcelona %>%
  group_by(day) %>%
  summarise(y = mean(accidents_scaled, na.rm = TRUE))

# cols <- brewer.pal(4, 'Pastel2')
cols <- 
  colorRampPalette(c('lightblue', 'darkgreen'))(7)
ggplot(data = temp,
       aes(x = day, y = y * 100)) +
  geom_bar(stat = 'identity', alpha = 0.5,
           fill = cols) +
  xlab('Day of Week') +
  ylab('Scaled accidents (% of average)') +
  geom_label(aes(label = paste0(round(y * 100, digits = 2), '%'))) +
  ggtitle('Car accidentology by Day of Week')


# Visual
temp <-
  barcelona %>%
  group_by(outcome, day) %>%
  summarise(y = mean(accidents_scaled, na.rm = TRUE))

# cols <- brewer.pal(4, 'Pastel2')
ggplot(data = temp, aes(day, y * 100, fill = outcome)) +
  geom_bar(stat = 'identity') +
  xlab('Day of Week') +
  ylab('Scaled accidents (% of average)') +
  ggtitle('Car accidentology by Day of Week and Outcome')


##########
# rome
# Relevel outcome so that no game is the baseline
# (ie, reference class)
rome$day <- factor(capitalize(rome$day),
                        levels = c('Sunday',
                                   'Monday', 
                                   'Tuesday',
                                   'Wednesday',
                                   'Thursday',
                                   'Friday',
                                   'Saturday'))

# model with all cities
fit <- lm(accidents_scaled ~ outcome + day, 
          data = rome)

# Visual
temp <-
  rome %>%
  group_by(day) %>%
  summarise(y = mean(accidents_scaled, na.rm = TRUE))

# cols <- brewer.pal(4, 'Pastel2')
cols <- 
  colorRampPalette(c('lightblue', 'darkgreen'))(7)
ggplot(data = temp,
       aes(x = day, y = y * 100)) +
  geom_bar(stat = 'identity', alpha = 0.5,
           fill = cols) +
  xlab('Day of Week') +
  ylab('Scaled accidents (% of average)') +
  geom_label(aes(label = paste0(round(y * 100, digits = 2), '%'))) +
  ggtitle('Car accidentology by Day of Week')


# Visual
temp <-
  rome %>%
  group_by(outcome, day) %>%
  summarise(y = mean(accidents_scaled, na.rm = TRUE))

# cols <- brewer.pal(4, 'Pastel2')
ggplot(data = temp, aes(day, y * 100, fill = outcome)) +
  geom_bar(stat = 'identity') +
  xlab('Day of Week') +
  ylab('Scaled accidents (% of average)') +
  ggtitle('Car accidentology by Day of Week and Outcome')

###########################################################################################
# Models and visualization looking at home and away games seperately
df_home <- df[which(df$location_bi == 'home'),]
df_away <- df[which(df$location_bi == 'away'),]

# Model for number of accidents for home team
fit <- lm(accidents_scaled ~ outcome, 
          data = df_home)

# Visual
temp_home <-
  df_home %>%
  group_by(outcome) %>%
  summarise(y = mean(accidents_scaled, na.rm = TRUE))

# cols <- brewer.pal(4, 'Pastel2')
cols <- 
  colorRampPalette(c('lightblue', 'darkgreen'))(3)
ggplot(data = temp_home,
       aes(x = outcome, y = y * 100)) +
  geom_bar(stat = 'identity', alpha = 0.5,
           fill = cols) +
  xlab('Game status') +
  ylab('Scaled accidents (% of average)') +
  geom_label(aes(label = paste0(round(y * 100, digits = 2), '%'))) +
  ggtitle('Car accidentology by outcome of home games')

# Model for number of accidents for away team
fit <- lm(accidents_scaled ~ outcome, 
          data = df_away)

# Visual
temp_away <-
  df_away %>%
  group_by(outcome) %>%
  summarise(y = mean(accidents_scaled, na.rm = TRUE))

# cols <- brewer.pal(4, 'Pastel2')
cols <- 
  colorRampPalette(c('lightblue', 'darkgreen'))(3)
ggplot(data = temp_away,
       aes(x = outcome, y = y * 100)) +
  geom_bar(stat = 'identity', alpha = 0.5,
           fill = cols) +
  xlab('Game status') +
  ylab('Scaled accidents (% of average)') +
  geom_label(aes(label = paste0(round(y * 100, digits = 2), '%'))) +
  ggtitle('Car accidentology by outcome of away games')


# model with all cities and while controlling for outcome when team is at home
fit <- lm(accidents_scaled ~ outcome + day, 
          data = df_home)

# Visual
temp_home <-
  df_home %>%
  group_by(day) %>%
  summarise(y = mean(accidents_scaled, na.rm = TRUE))

# cols <- brewer.pal(4, 'Pastel2')
cols <- 
  colorRampPalette(c('lightblue', 'darkgreen'))(7)
ggplot(data = temp_home,
       aes(x = day, y = y * 100)) +
  geom_bar(stat = 'identity', alpha = 0.5,
           fill = cols) +
  xlab('Day of Week') +
  ylab('Scaled accidents (% of average)') +
  geom_label(aes(label = paste0(round(y * 100, digits = 2), '%'))) +
  ggtitle('Car accidentology by Day of Week for home games')


# Visual
temp_home <-
  df_home %>%
  group_by(outcome, day) %>%
  summarise(y = mean(accidents_scaled, na.rm = TRUE))

# cols <- brewer.pal(4, 'Pastel2')
ggplot(data = temp_home, aes(day, y * 100, fill = outcome)) +
  geom_bar(stat = 'identity') +
  xlab('Day of Week') +
  ylab('Scaled accidents (% of average)') +
  ggtitle('Car accidentology by Day of Week and Outcome for Home Team')

#############
# Paris
# Models and visualization looking at home and away games seperately
paris_home <- paris[which(paris$location_bi == 'home'),]
paris_away <- paris[which(paris$location_bi == 'away'),]

# Model for number of accidents for home team
fit <- lm(accidents_scaled ~ outcome, 
          data = paris_home)

# Visual
temp_home <-
  paris_home %>%
  group_by(outcome) %>%
  summarise(y = mean(accidents_scaled, na.rm = TRUE))

# cols <- brewer.pal(4, 'Pastel2')
cols <- 
  colorRampPalette(c('lightblue', 'darkgreen'))(3)
ggplot(data = temp_home,
       aes(x = outcome, y = y * 100)) +
  geom_bar(stat = 'identity', alpha = 0.5,
           fill = cols) +
  xlab('Game status') +
  ylab('Scaled accidents (% of average)') +
  geom_label(aes(label = paste0(round(y * 100, digits = 2), '%'))) +
  ggtitle('Car accidentology by outcome of home games')

# Model for number of accidents for away team
fit <- lm(accidents_scaled ~ outcome, 
          data = paris_away)

# Visual
temp_away <-
  paris_away %>%
  group_by(outcome) %>%
  summarise(y = mean(accidents_scaled, na.rm = TRUE))

# cols <- brewer.pal(4, 'Pastel2')
cols <- 
  colorRampPalette(c('lightblue', 'darkgreen'))(3)
ggplot(data = temp_away,
       aes(x = outcome, y = y * 100)) +
  geom_bar(stat = 'identity', alpha = 0.5,
           fill = cols) +
  xlab('Game status') +
  ylab('Scaled accidents (% of average)') +
  geom_label(aes(label = paste0(round(y * 100, digits = 2), '%'))) +
  ggtitle('Car accidentology by outcome of away games')


# model with all cities and while controlling for outcome when team is at home
fit <- lm(accidents_scaled ~ outcome + day, 
          data = paris_home)

# Visual
temp_home <-
  paris_home %>%
  group_by(day) %>%
  summarise(y = mean(accidents_scaled, na.rm = TRUE))

# cols <- brewer.pal(4, 'Pastel2')
cols <- 
  colorRampPalette(c('lightblue', 'darkgreen'))(5)
ggplot(data = temp_home,
       aes(x = day, y = y * 100)) +
  geom_bar(stat = 'identity', alpha = 0.5,
           fill = cols) +
  xlab('Day of Week') +
  ylab('Scaled accidents (% of average)') +
  geom_label(aes(label = paste0(round(y * 100, digits = 2), '%'))) +
  ggtitle('Car accidentology by Day of Week for home games')


# Visual
temp_home <-
  paris_home %>%
  group_by(outcome, day) %>%
  summarise(y = mean(accidents_scaled, na.rm = TRUE))

# cols <- brewer.pal(4, 'Pastel2')
ggplot(data = temp_home, aes(day, y * 100, fill = outcome)) +
  geom_bar(stat = 'identity') +
  xlab('Day of Week') +
  ylab('Scaled accidents (% of average)') +
  ggtitle('Car accidentology by Day of Week and Outcome for Home Team')

#############
# barcelona
# Models and visualization looking at home and away games seperately
barcelona_home <- barcelona[which(barcelona$location_bi == 'home'),]
barcelona_away <- barcelona[which(barcelona$location_bi == 'away'),]

# Model for number of accidents for home team
fit <- lm(accidents_scaled ~ outcome, 
          data = barcelona_home)

# Visual
temp_home <-
  barcelona_home %>%
  group_by(outcome) %>%
  summarise(y = mean(accidents_scaled, na.rm = TRUE))

# cols <- brewer.pal(4, 'Pastel2')
cols <- 
  colorRampPalette(c('lightblue', 'darkgreen'))(3)
ggplot(data = temp_home,
       aes(x = outcome, y = y * 100)) +
  geom_bar(stat = 'identity', alpha = 0.5,
           fill = cols) +
  xlab('Game status') +
  ylab('Scaled accidents (% of average)') +
  geom_label(aes(label = paste0(round(y * 100, digits = 2), '%'))) +
  ggtitle('Car accidentology by outcome of home games')

# Model for number of accidents for away team
fit <- lm(accidents_scaled ~ outcome, 
          data = barcelona_away)

# Visual
temp_away <-
  barcelona_away %>%
  group_by(outcome) %>%
  summarise(y = mean(accidents_scaled, na.rm = TRUE))

# cols <- brewer.pal(4, 'Pastel2')
cols <- 
  colorRampPalette(c('lightblue', 'darkgreen'))(3)
ggplot(data = temp_away,
       aes(x = outcome, y = y * 100)) +
  geom_bar(stat = 'identity', alpha = 0.5,
           fill = cols) +
  xlab('Game status') +
  ylab('Scaled accidents (% of average)') +
  geom_label(aes(label = paste0(round(y * 100, digits = 2), '%'))) +
  ggtitle('Car accidentology by outcome of away games')


# model with all cities and while controlling for outcome when team is at home
fit <- lm(accidents_scaled ~ outcome + day, 
          data = barcelona_home)

# Visual
temp_home <-
  barcelona_home %>%
  group_by(day) %>%
  summarise(y = mean(accidents_scaled, na.rm = TRUE))

# cols <- brewer.pal(4, 'Pastel2')
cols <- 
  colorRampPalette(c('lightblue', 'darkgreen'))(7)
ggplot(data = temp_home,
       aes(x = day, y = y * 100)) +
  geom_bar(stat = 'identity', alpha = 0.5,
           fill = cols) +
  xlab('Day of Week') +
  ylab('Scaled accidents (% of average)') +
  geom_label(aes(label = paste0(round(y * 100, digits = 2), '%'))) +
  ggtitle('Car accidentology by Day of Week for home games')


# Visual
temp_home <-
  barcelona_home %>%
  group_by(outcome, day) %>%
  summarise(y = mean(accidents_scaled, na.rm = TRUE))

# cols <- brewer.pal(4, 'Pastel2')
ggplot(data = temp_home, aes(day, y * 100, fill = outcome)) +
  geom_bar(stat = 'identity') +
  xlab('Day of Week') +
  ylab('Scaled accidents (% of average)') +
  ggtitle('Car accidentology by Day of Week and Outcome for Home Team')

#############
# rome
# Models and visualization looking at home and away games seperately
rome_home <- rome[which(rome$location_bi == 'home'),]
rome_away <- rome[which(rome$location_bi == 'away'),]

# Model for number of accidents for home team
fit <- lm(accidents_scaled ~ outcome, 
          data = rome_home)

# Visual
temp_home <-
  rome_home %>%
  group_by(outcome) %>%
  summarise(y = mean(accidents_scaled, na.rm = TRUE))

# cols <- brewer.pal(4, 'Pastel2')
cols <- 
  colorRampPalette(c('lightblue', 'darkgreen'))(3)
ggplot(data = temp_home,
       aes(x = outcome, y = y * 100)) +
  geom_bar(stat = 'identity', alpha = 0.5,
           fill = cols) +
  xlab('Game status') +
  ylab('Scaled accidents (% of average)') +
  geom_label(aes(label = paste0(round(y * 100, digits = 2), '%'))) +
  ggtitle('Car accidentology by outcome of home games')

# Model for number of accidents for away team
fit <- lm(accidents_scaled ~ outcome, 
          data = rome_away)

# Visual
temp_away <-
  rome_away %>%
  group_by(outcome) %>%
  summarise(y = mean(accidents_scaled, na.rm = TRUE))

# cols <- brewer.pal(4, 'Pastel2')
cols <- 
  colorRampPalette(c('lightblue', 'darkgreen'))(3)
ggplot(data = temp_away,
       aes(x = outcome, y = y * 100)) +
  geom_bar(stat = 'identity', alpha = 0.5,
           fill = cols) +
  xlab('Game status') +
  ylab('Scaled accidents (% of average)') +
  geom_label(aes(label = paste0(round(y * 100, digits = 2), '%'))) +
  ggtitle('Car accidentology by outcome of away games')


# model with all cities and while controlling for outcome when team is at home
fit <- lm(accidents_scaled ~ outcome + day, 
          data = rome_home)

# Visual
temp_home <-
  rome_home %>%
  group_by(day) %>%
  summarise(y = mean(accidents_scaled, na.rm = TRUE))

# cols <- brewer.pal(4, 'Pastel2')
cols <- 
  colorRampPalette(c('lightblue', 'darkgreen'))(7)
ggplot(data = temp_home,
       aes(x = day, y = y * 100)) +
  geom_bar(stat = 'identity', alpha = 0.5,
           fill = cols) +
  xlab('Day of Week') +
  ylab('Scaled accidents (% of average)') +
  geom_label(aes(label = paste0(round(y * 100, digits = 2), '%'))) +
  ggtitle('Car accidentology by Day of Week for home games')


# Visual
temp_home <-
  rome_home %>%
  group_by(outcome, day) %>%
  summarise(y = mean(accidents_scaled, na.rm = TRUE))

# cols <- brewer.pal(4, 'Pastel2')
ggplot(data = temp_home, aes(day, y * 100, fill = outcome)) +
  geom_bar(stat = 'identity') +
  xlab('Day of Week') +
  ylab('Scaled accidents (% of average)') +
  ggtitle('Car accidentology by Day of Week and Outcome for Home Team')
