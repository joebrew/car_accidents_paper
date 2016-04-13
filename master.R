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

