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
library(htmlTable)
library(leaflet)
library(raster)
library(htmlwidgets)
# require(RCurl)
# options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

##### Theme
source(paste0(root, '/lib/theme.R'))
source(paste0(root, '/lib/helpers.R'))

##### Data read in
csvs <- dir(data_dir)
csvs <- csvs[grepl('.csv', csvs)]
csvs <- paste0(data_dir, '/', csvs)
csvs <- csvs[grepl('ACCIDENTS', csvs)]
results_list <- list()
hh <- c()
for (i in 1:length(csvs)){
  if(i == 5){
    temp <- read.csv(csvs[i], sep = ';', stringsAsFactors=FALSE, skip = 3, header = FALSE,  fileEncoding="latin1")
    names(temp) <- hh
  } else {
    temp <- read.csv(csvs[i], stringsAsFactors=FALSE, fileEncoding="latin1")
    if(i == i){
      hh <- names(temp)
    }
  }
  temp$year <- as.numeric(gsub('ACCIDENTS_GU_BCN_|.csv', '', csvs[i]))
  results_list[[i]] <- temp
}
accidents <- do.call('rbind', results_list)
rm(temp, results_list, csvs, hh, i)

# Get a date
accidents$date <-
  as.Date(paste(accidents$NK.Any,
                 accidents$Mes.de.any,
                 accidents$Dia.de.mes,
                 sep = '-'))

# Numeric coordiantes
accidents$y <- as.numeric(gsub(',', '.', accidents$Coordenada.UTM..Y.))
accidents$x <- as.numeric(gsub(',', '.', accidents$Coordenada.UTM..X. ))

# Remove those non-geocoded accidents
accidents <- accidents[accidents$x != -1 & accidents$y != -1,]

# Get year, month, etc
accidents$year <- as.numeric(format(accidents$date, '%Y'))
accidents$year_month <- paste0(format(accidents$date, '%Y'),
                                      '-',
                                      format(accidents$date, '%m'))
accidents$month <- as.numeric(format(accidents$date, '%m'))

##### Geographic data
mapa <- readOGR(paste0(root, '/data/geo/barris'), 'BCN_Barri_SHP')

##### Convert both accidents and mapa to lat/lng

# Accidents
coordinates(accidents) <- ~x+y
proj4string(accidents) <- CRS(proj4string(mapa))
accidents <- spTransform(accidents, CRS("+proj=longlat +datum=WGS84"))

# Mapa
mapa <- spTransform(mapa, CRS("+proj=longlat +datum=WGS84"))

# Simple map of all accidents
par(mar = c(0,0,0,0))
par(oma = c(0,0,0,0))
jpeg(filename = '/home/joebrew/Documents/bcndata.github.io/img/2015-11-30-Car-accidents-in-Barcelona/static.jpeg',
     width = 4000, height = 4000, units = 'px', quality = 100, bg = 'black')
plot(mapa, border = 'black', bg = 'black')
points(accidents$x, accidents$y, col = 'white', pch = 16, cex = 0.55)
deaths <- accidents[accidents$Número.de.morts > 0,]
points(deaths$x, deaths$y, col = 'red', pch = 16, cex = 0.55)
dev.off()


##### FOOTBALL GAMES

# READ 2012/13 FOOTBALL DATA (compiled from wikipedia)
football <- read_csv(paste0(data_dir, '/football_games/games_2010-2014.csv'))

# Make some win/loss columns
football$outcome <- 
  ifelse((football$home == 'Barcelona' &
            as.numeric(substr(football$score,1,1)) >
            as.numeric(substr(football$score, 3,3))) | 
           (football$away == 'Barcelona' &
              as.numeric(substr(football$score,1,1)) <
              as.numeric(substr(football$score, 3,3))),
         'win',
         ifelse((as.numeric(substr(football$score,1,1)) ==
                   as.numeric(substr(football$score, 3,3))),
                'tie',
                'loss'))

# Flag any football game day
football$game_day <- TRUE

# Flag home vs. away
football$location_bi <- ifelse(football$location == 'Barcelona',
                               'home', 'away')

# Flag which days occur on football games, after, etc.
temp <- 
  accidents %>%
  data.frame %>%
  left_join(football %>%
              dplyr::select(date, outcome, game_day))

# Does game day cause accidents?
temp_bi <-
  temp %>%
  group_by(date, game_day) %>%
  summarise(accidents = n()) %>%
  group_by(game_day) %>%
  summarise(days = length(unique(date)),
            accidents = sum(accidents)) %>%
  mutate(daily_rate = accidents / days) %>%
  arrange(desc(daily_rate)) %>%
  mutate(status = ifelse(is.na(game_day), 'No game', 'Game day')) %>%
  dplyr::select(status, days, accidents, daily_rate)
temp_bi$daily_rate <- round(temp_bi$daily_rate, digits = 2)
names(temp_bi) <- c('Status', 'Days', 'Accidents', 'Daily Rate')
htmlTable(temp_bi, rnames = FALSE, useViewer = FALSE)

# Significant ?
temp_model <-
  temp %>%
  group_by(date) %>%
  summarise(accidents = n(),
            game_day = first(game_day)) %>%
  mutate(status = ifelse(is.na(game_day), 'No game', 'Game day')) %>%
  dplyr::select(date, accidents, status)
  
fit <- lm(accidents ~ status, data = temp_model)
confint(fit)

# Does winning or losing matter?
wl_agg <- 
  temp %>%
  group_by(date, outcome) %>%
  summarise(accidents = n()) %>%
  group_by(outcome) %>%
  summarise(days = length(unique(date)),
            accidents = sum(accidents)) %>%
  mutate(daily_rate = accidents / days) %>%
  arrange(desc(daily_rate)) %>%
  mutate(outcome = ifelse(is.na(outcome), 'No game', outcome)) %>%
  mutate(outcome = capitalize(outcome))
wl_agg$daily_rate <- round(wl_agg$daily_rate, digits = 2)
names(wl_agg) <- gsub('_', ' ', names(wl_agg))
names(wl_agg) <- capitalize(names(wl_agg))
htmlTable(wl_agg,  rnames = FALSE, useViewer = FALSE)

# Significant?
temp_model <- 
  temp %>%
  group_by(date) %>%
  summarise(accidents = n(),
            outcome = first(outcome)) %>%
  mutate(outcome = ifelse(is.na(outcome), 'No game', outcome)) %>%
  mutate(outcome = capitalize(outcome)) %>%
  dplyr::select(date, accidents, outcome)

fit <- lm(accidents ~ outcome, data = temp_model)
confint(fit)

# Deaths
deaths <- 
  temp %>%
  group_by(date, outcome) %>%
  summarise(accidents = n(),
            deaths = sum(Número.de.morts)) %>%
  group_by(outcome) %>%
  summarise(days = length(unique(date)),
            accidents = sum(accidents),
            deaths = sum(deaths)) %>%
  mutate(daily_rate = deaths / days) %>%
  arrange(desc(daily_rate)) %>%
  mutate(outcome = ifelse(is.na(outcome), 'No game', outcome)) %>%
  mutate(outcome = capitalize(outcome))

# ENGLISH
ggplot(data = deaths, aes(x = outcome, y = daily_rate)) +
  geom_bar(stat = 'identity', fill = 'gold', alpha = 0.7) +
  xlab('Outcome of game') +
  ylab('Daily car accident deaths') +
  bcn_data_theme
ggsave('/home/joebrew/Documents/bcndata.github.io/img/2015-11-30-Car-accidents-in-Barcelona/deaths.JPG')


# CATALA
ct <- deaths
ct$outcome <- c('Empatat', 'Sense partit', 'Perdut', 'Guanyat')
ct$outcome <- factor(ct$outcome, levels = ct$outcome)
ggplot(data = ct, aes(x = outcome, y = daily_rate)) +
  geom_bar(stat = 'identity', fill = 'gold', alpha = 0.6) +
  xlab('Resultat') +
  ylab('Morts per accidents de cotxe per dia') +
  bcn_data_theme
ggsave('/home/joebrew/Documents/bcndata.github.io/img/2015-11-30-Car-accidents-in-Barcelona/deaths_ca.JPG')

# ESPANOL
esp <- deaths
esp$outcome <- c('Empatado', 'Sin partido', 'Perdido', 'Ganado')
esp$outcome <- faespor(esp$outcome, levels = esp$outcome)
ggplot(data = esp, aes(x = outcome, y = daily_rate)) +
  geom_bar(stat = 'identity', fill = 'gold', alpha = 0.6) +
  xlab('Resultado') +
  ylab('Muertos por accidentes de coche por día') +
  bcn_data_theme
ggsave('/home/joebrew/Documents/bcndata.github.io/img/2015-11-30-Car-accidents-in-Barcelona/deaths_es.JPG')


##### ACCIDENTS IN GENERAL

# English
ggplot(data = wl_agg, aes(x = Outcome, y = `Daily rate`)) +
  geom_bar(stat = 'identity', fill = 'gold', alpha = 0.6) +
  xlab('Outcome of game') +
  ylab('Daily accidents') +
  bcn_data_theme
ggsave('/home/joebrew/Documents/bcndata.github.io/img/2015-11-30-Car-accidents-in-Barcelona/football.JPG')


# Catala
wl_agg_ca <- wl_agg
outcome_levels <- c('Perdut', 'Sense partit', 'Empatat', 'Guanyat')
wl_agg_ca$Outcome <- 
  outcome_levels
wl_agg_ca$Outcome <- factor(
  wl_agg_ca$Outcome,
  levels = outcome_levels 
)
ggplot(data = wl_agg_ca, aes(x = Outcome, y = `Daily rate`)) +
  geom_bar(stat = 'identity', fill = 'gold', alpha = 0.6) +
  xlab('Resultat') +
  ylab('Accidents per dia') +
  bcn_data_theme
ggsave('/home/joebrew/Documents/bcndata.github.io/img/2015-11-30-Car-accidents-in-Barcelona/football_ca.JPG')



# Espanol
wl_agg_ca <- wl_agg
outcome_levels <- c('Peder', 'No partido', 'Empate', 'Ganar')
wl_agg_ca$Outcome <- 
  outcome_levels
wl_agg_ca$Outcome <- factor(
  wl_agg_ca$Outcome,
  levels = outcome_levels 
)
ggplot(data = wl_agg_ca, aes(x = Outcome, y = `Daily rate`)) +
  geom_bar(stat = 'identity', fill = 'gold', alpha = 0.6) +
  xlab('Resultado') +
  ylab('Accidentes por día') +
  bcn_data_theme
ggsave('/home/joebrew/Documents/bcndata.github.io/img/2015-11-30-Car-accidents-in-Barcelona/football_es.JPG')


# wl <- temp %>%
#   group_by(date, outcome) %>%
#   summarise(accidents = n()) %>%
#   mutate(outcome = ifelse(is.na(outcome), 'No game', outcome)) 
# ggplot(data = wl,
#        aes(x = factor(outcome), y = accidents)) +
#   geom_jitter(alpha = 0.7) +
#   geom_violin(alpha = 0.5, fill = 'gold') +
#   xlab('Outcome of game') +
#   ylab('Number of car accidents')

# # All time map football game
# ggplot(data = temp_model,
#        aes(x = date, y = accidents, color = outcome)) +
#   geom_point(size = 4) +
#   geom_line(aes(color = NULL), alpha = 0.1)

# Games by day of week
# temp %>%
#   filter(game_day == TRUE) %>%
#   group_by(Dia.de.setmana) %>%
#   summarise(n = n())

##### WEATHER
if(!'weather.csv' %in% dir(data_dir)){
  years <- 2010:2014
  results_list <- list()
  for (y in 1:length(years)){
    weath <- getWeatherForDate(station_id = 'BCN',
                               start_date = paste0(years[y], '-01-01'),
                               end_date = paste0(years[y], '-12-31'),
                               opt_all_columns = TRUE)
    results_list[[y]] <- weath
    }
  weather <- do.call('rbind', results_list)
  write_csv(weather, paste0(data_dir, '/weather.csv'))
} else {
  weather <- read_csv(paste0(data_dir, '/weather.csv'))
}

# Join weather to accidents
temp <- accidents %>%
  data.frame %>%
  group_by(date) %>%
  summarise(accidents = n()) %>%
  left_join(weather %>%
              mutate(date = as.Date(Date))) %>%
  mutate(max_temp_bin = round(Max_TemperatureF, digits = -1),
         rainy = PrecipitationIn > 0)

# Temperature
# ggplot(data = temp,
#        aes(x= factor(max_temp_bin),
#            y = accidents)) +
#   geom_jitter(alpha = 0.2) +
#   geom_violin(fill = 'gold', alpha = 0.2) +
#   ylab('Accidents') +
#   xlab('Maximum temperature (F)') +
#   ggtitle('Daily car accidents in Barcelona by temperature')
# ggsave('/home/joebrew/Documents/bcndata.github.io/img/2015-11-30-Car-accidents-in-Barcelona/weather.JPG')

# temp$rainy <- ifelse(temp$rainy,
#                      'Some rain', 
#                      'No rain')
# ggplot(data = temp,
#        aes(x = rainy,
#            y = accidents)) +
#   geom_jitter(alpha = 0.2) +
#   geom_violin(fill = 'gold', alpha = 0.2) +
#   ylab('Accidents') +
#   xlab('Raininess') +
#   ggtitle('Daily car accidents in Barcelona by raininess')
# ggsave('/home/joebrew/Documents/bcndata.github.io/img/2015-11-30-Car-accidents-in-Barcelona/rainy.JPG')


# GIF
# source(paste0(root_dir, '/code/accidents_de_cotxe/make_gif.R'))

# ##### GRID APPROACH TO GETTING WORST INTERSECTION
# temp <- expand.grid(
#   x = seq(min(accidents$x), max(accidents$x), length = 100),
#   y = seq(min(accidents$y), max(accidents$y), length = 100)
# )
# 
# # Placeholders for accidents within 10, 100, 1000 meters
# temp$within_0010 <- temp$within_0100 <- temp$within_1000 <- NA
# 
# coordinates(temp) <- ~x+y
# proj4string(temp) <- proj4string(mapa)
# 
# # Loop through each grid
# for (i in 1:nrow(temp)){
#   this_point <- temp[i,]
#   distance <- get_distance(lon1 = this_point$x,
#                            lat1 = this_point$y, 
#                            lon2 = accidents$x,
#                            lat2 = accidents$y)
#   temp$within_0010[i] <- length(which(distance <= 0.01))
#   temp$within_0100[i] <- length(which(distance <= 0.1))
#   temp$within_1000[i] <- length(which(distance <= 1))
#   cat(paste0('Finished ', i, ' of ', nrow(temp), '\n'))
# }
# 
# # Organize by within_0010
# temp <- temp[order(temp$within_0010),]
# 
# # Visualize
# cols <- rev(colorRampPalette(brewer.pal(9, 'Spectral'))(max(temp$within_0010)))
# plot(mapa)
# points(temp, col = adjustcolor(cols[temp$within_0010], alpha.f = 0.2), 
#        pch = 16, cex = 0.6)

##### GET WORST INTERSECTION
temp <- accidents %>%
  data.frame %>%
  # round
  mutate(x = round(x, digits = 3),
         y = round(y, digits = 3)) %>%
  group_by(x,y) %>%
  summarise(n = n(),
            deaths = sum(Número.de.morts),
            victims = sum(Número.de.víctimes),
            vehicles = sum(Número.de.vehicles.implicats)) 
temp <- temp[rev(order(temp$n)),]
worst <- temp[1,]
paste0(worst$y, ', ', worst$x)

# See worst intersection
x = get_map(location = c(lon = worst$x - 0.0004, lat = worst$y + 0.0005),
            zoom = 18,
            maptype = 'toner',
            source = 'stamen',
            color = 'bw')
ggmap(x)

# Expand temp to get all points in Barcelona
temp_ex <- expand.grid(x = seq(min(temp$x) - 0.05,
                               max(temp$x) + 0.05,
                               by = 0.001),
                       y = seq(min(temp$y) - 0.05,
                               max(temp$y) + 0.05,
                               by = 0.001))

# Join temp_ex to temp
temp_ex <- left_join(temp_ex, temp)
temp_ex$n[is.na(temp_ex$n)] <- 0

# Define color
# cols <- rev(colorRampPalette(brewer.pal(9, 'Spectral'))(max(temp$n)))
cols <- colorRampPalette(brewer.pal(9, 'Oranges'))(max(temp$n))
temp_ex$color <- ifelse(temp_ex$n == 0,
                        'blue',
                        cols[temp_ex$n])

# Make spatial
coordinates(temp_ex) <- ~x+y
proj4string(temp_ex) <- proj4string(mapa)

# Keep only those within the Barcelona polygons
temp_ex <- temp_ex[!is.na(over(temp_ex, polygons(mapa))),]

# Visualize
plot(mapa, border = NA)
points(temp_ex, col = temp_ex$color, pch = 16, cex = 0.4)

#####
# NUMBER OF ACCIDENTS BY YEAR / MONTH
#####
temp <- accidents %>%
  data.frame %>%
  group_by(Date = year) %>%
  summarise(n = n(),
            deaths = sum(Número.de.morts),
            victims = sum(Número.de.víctimes),
            vehicles = sum(Número.de.vehicles.implicats))

# ENGLISH / Catalan
ggplot(data = temp, aes(x = Date, y = n)) +
  geom_bar(stat = 'identity', alpha = 0.5, fill = 'gold') +
  # geom_point() +
  # theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab('') +
  ylab('Accidents') +
  # ggtitle('Accidents per year') +
  bcn_data_theme
    # geom_bar(stat = 'identity', data = temp, aes(x = Date, y = deaths))
ggsave('/home/joebrew/Documents/bcndata.github.io/img/2015-11-30-Car-accidents-in-Barcelona/by_year.JPG')
ggsave('/home/joebrew/Documents/bcndata.github.io/img/2015-11-30-Car-accidents-in-Barcelona/by_year_ca.JPG')

# SPANISH
ggplot(data = temp, aes(x = Date, y = n)) +
  geom_bar(stat = 'identity', alpha = 0.5, fill = 'gold') +
  # geom_point() +
  # theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab('') +
  ylab('Accidentes') +
  # ggtitle('Accidents per year') +
  bcn_data_theme
# geom_bar(stat = 'identity', data = temp, aes(x = Date, y = deaths))
ggsave('/home/joebrew/Documents/bcndata.github.io/img/2015-11-30-Car-accidents-in-Barcelona/by_year_es.JPG')


#####
# TIME OF DAY
#####


# TIME OF DAY
tod <- 
  accidents %>%
  data.frame %>%
  group_by(date, Hora.de.dia) %>%
  summarise(n = n())

# Join tod to expanded grid of same (in order to get 0 counts)
tod_ex <- expand.grid(date = seq(min(tod$date),
                                 max(tod$date),
                                 by = 1),
                      Hora.de.dia = seq(min(tod$Hora.de.dia),
                                        max(tod$Hora.de.dia),
                                        by = 1))
tod <- left_join(tod_ex, tod)
tod$n[is.na(tod$n)] <- 0

# ggplot(data = tod, aes(x = Hora.de.dia, y = n / 5)) +
#   geom_point() +
#   geom_smooth() +
#   xlab('Time of day') +
#   ylab('Accidents') +
#   bcn_data_theme +
#   ggtitle('Don\'t drive at 3pm')
# rm(tod)

ggplot(data = tod, aes(x = factor(Hora.de.dia), y = n)) +
  geom_jitter(alpha = 0.01) +
  geom_violin(border = NA, fill = 'gold', alpha = 0.6) +
  xlab('Hour of day') +
  ylab('Daily accidents')

# Mean only
tod_agg <- tod %>%
  group_by(Hora.de.dia) %>%
  summarise(p010 = quantile(n, probs = 0.1),
            p020 = quantile(n, probs = 0.2),
            p030 = quantile(n, probs = 0.3),
            p040 = quantile(n, probs = 0.4),
            p050 = quantile(n, probs = 0.5),
            p060 = quantile(n, probs = 0.6),
            p070 = quantile(n, probs = 0.7),
            p080 = quantile(n, probs = 0.8),
            p090 = quantile(n, probs = 0.9),
            pmean = mean(n))

# ggplot(data = tod_agg,
#        aes(x = Hora.de.dia, y = p050)) +
#   geom_line() +
#   geom_ribbon(aes(x = Hora.de.dia, ymin = p040, ymax = p060), alpha = 0.2) + 
#   geom_ribbon(aes(x = Hora.de.dia, ymin = p030, ymax = p070), alpha = 0.2) + 
#   geom_ribbon(aes(x = Hora.de.dia, ymin = p020, ymax = p080), alpha = 0.2) + 
#   geom_ribbon(aes(x = Hora.de.dia, ymin = p010, ymax = p090), alpha = 0.2) 
  

tod_agg$Hora.de.dia <- paste0(tod_agg$Hora.de.dia, ':00')
tod_agg$Hora.de.dia <- factor(tod_agg$Hora.de.dia, 
                              levels = tod_agg$Hora.de.dia)

# ENGLISH

ggplot(data = tod_agg,
       aes(x = as.numeric(Hora.de.dia), y = pmean)) +
  geom_area(fill = 'gold', alpha = 0.6) +
  xlab('Time of day') +
  ylab('Daily accidents') +
  bcn_data_theme +
  scale_x_discrete(labels = tod_agg$Hora.de.dia) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave('/home/joebrew/Documents/bcndata.github.io/img/2015-11-30-Car-accidents-in-Barcelona/hora.JPG')


# CATALAN
ggplot(data = tod_agg,
       aes(x = as.numeric(Hora.de.dia), y = pmean)) +
  geom_area(fill = 'gold', alpha = 0.6) +
  xlab('Hora del dia') +
  ylab('Accidents per dia') +
  bcn_data_theme +
  scale_x_discrete(labels = tod_agg$Hora.de.dia) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave('/home/joebrew/Documents/bcndata.github.io/img/2015-11-30-Car-accidents-in-Barcelona/hora_ca.JPG')

# ESPANOL
ggplot(data = tod_agg,
       aes(x = as.numeric(Hora.de.dia), y = pmean)) +
  geom_area(fill = 'gold', alpha = 0.6) +
  xlab('Hora del día') +
  ylab('Accidentes por día') +
  bcn_data_theme +
  scale_x_discrete(labels = tod_agg$Hora.de.dia) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave('/home/joebrew/Documents/bcndata.github.io/img/2015-11-30-Car-accidents-in-Barcelona/hora_es.JPG')


#####
# DAY OF WEEK
#####

dow_dict <- data.frame(ca = c('Dilluns', 'Dimarts', 'Dimecres', 'Dijous',
                              'Divendres', 'Dissabte', 'Diumenge'),
                       en = c('Monday', 'Tuesday', 'Wednesday', 'Thursday',
                              'Friday', 'Saturday', 'Sunday'),
                       es = c('Lunes', 'Martes', 'Miércoles', 'Jueves',
                              'Viernes', 'Sábado', 'Domingo'))

dow <- 
  accidents %>% 
  data.frame %>%
  group_by(Descripció.dia.setmana) %>%
  summarise(n = n())

# CATALAN
dow$Descripció.dia.setmana <- 
  factor(as.character(dow$Descripció.dia.setmana),
         levels = dow_dict$ca,
         labels = dow_dict$ca)

ggplot(data = dow, aes(x = Descripció.dia.setmana, y = n / (365.25 * 4))) +
  geom_bar(stat = 'identity', fill = 'gold', alpha = 0.6) +
  bcn_data_theme +
  # ggtitle('Stay off the road on Friday') +
  xlab('Dia de la setmana') +
  ylab('Accidents per dia') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 20))
ggsave('/home/joebrew/Documents/bcndata.github.io/img/2015-11-30-Car-accidents-in-Barcelona/dia_ca.JPG')

# ESPANOL
dow$Descripció.dia.setmana <- 
  factor(as.character(dow$Descripció.dia.setmana),
         levels = dow_dict$ca,
         labels = dow_dict$es)

ggplot(data = dow, aes(x = Descripció.dia.setmana, y = n / (365.25 * 4))) +
  geom_bar(stat = 'identity', fill = 'gold', alpha = 0.6) +
  bcn_data_theme +
  # ggtitle('Stay off the road on Friday') +
  xlab('Dia de la semana') +
  ylab('Accidentes por dia') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 20))
ggsave('/home/joebrew/Documents/bcndata.github.io/img/2015-11-30-Car-accidents-in-Barcelona/dia_es.JPG')


# ENGLISH
dow$Descripció.dia.setmana <- 
  factor(as.character(dow$Descripció.dia.setmana),
         levels = dow_dict$es,
         labels = dow_dict$en)

ggplot(data = dow, aes(x = Descripció.dia.setmana, y = n / (365.25 * 4))) +
  geom_bar(stat = 'identity', fill = 'gold', alpha = 0.6) +
  bcn_data_theme +
  # ggtitle('Stay off the road on Friday') +
  xlab('Day of the week') +
  ylab('Daily accidents') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 20))
ggsave('/home/joebrew/Documents/bcndata.github.io/img/2015-11-30-Car-accidents-in-Barcelona/dia.JPG')


#####
# DAY AND HOUR COMBINED
#####

# ENGLISH
temp <- accidents %>%
  data.frame %>%
  group_by(Dia.de.setmana, Hora.de.dia) %>%
  summarise(n = n())
# New var
temp$day_hour <- 
  paste0(temp$Dia.de.setmana,'-', temp$Hora.de.dia)

# Make a label vector
temp$label <- ifelse(temp$Hora.de.dia == 0,
                     temp$Dia.de.setmana,
                     NA)
label_dict <- data.frame(number = 1:7,
                         day = c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun'))
temp$label <- label_dict$day[temp$label]
temp$label <- as.character(temp$label)
temp$label[is.na(temp$label)] <- ''

# Define rectangles
rect_left <- which(nchar(temp$label) > 2) - 0.5
rect_right <- c((rect_left)[-1], nrow(temp) + 0.5)
low <- min(temp$n, na.rm = TRUE)
high <- max(temp$n, na.rm = TRUE)
rectangles <- data.frame(xmin = rect_left,
                         xmax = rect_right,
                         ymin = 0,
                         ymax = high * 1)
v_rectangles <- data.frame(xmin = 0,
                           xmax = nrow(temp) + 0.5,
                           ymin = 0,
                           ymax = c(0,high * 1))

# Filler colors for rectangles
filler <- rep(c('grey90', 'grey50'), ceiling(0.5 * nrow(rectangles)))
if(length(filler) != nrow(rectangles)){
  filler <- filler[1:(length(filler) - 1)]
}

# Replace labels
for (i in which(nchar(temp$label) > 0)){
  label <- temp$label[i]
  # remove old
  temp$label[i] <- ''
  # Give new
  temp$label[(i+12)] <- label
}

ggplot() +
  theme(axis.text.x = element_text(size = 15, angle = 45, 
                                   # family='Ubuntu Mono',
                                   hjust = 1),
        axis.text.y=element_text(hjust=1, angle=0),
        legend.position = 'none') +
  theme(axis.title.x = element_blank()) +
  scale_x_discrete(labels = temp$label) +
  theme(axis.ticks = element_blank()) +
  geom_rect(data = rectangles, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), 
            fill = filler, alpha = 0.3) +
  geom_area(data = temp, aes(x = factor(day_hour),
                y = n,
                group = 1),
            fill = 'gold', alpha = 0.6) +
  bcn_data_theme +
  xlab('Day of the week') +
  ylab('Accidents') +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank()) 
# theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
ggsave('/home/joebrew/Documents/bcndata.github.io/img/2015-11-30-Car-accidents-in-Barcelona/full_week.JPG')



# CATALAN
temp <- accidents %>%
  data.frame %>%
  group_by(Dia.de.setmana, Hora.de.dia) %>%
  summarise(n = n())
# New var
temp$day_hour <- 
  paste0(temp$Dia.de.setmana,'-', temp$Hora.de.dia)

# Make a label vector
temp$label <- ifelse(temp$Hora.de.dia == 0,
                     temp$Dia.de.setmana,
                     NA)
label_dict <- data.frame(number = 1:7,
                         day = c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun'))
temp$label <- label_dict$day[temp$label]
temp$label <- as.character(temp$label)
temp$label[is.na(temp$label)] <- ''

# Rename in catalan
temp$label <- 
  factor(temp$label,
         levels = unique(temp$label[nchar(temp$label) > 0]),
         labels = dow_dict$ca)
temp$label <- as.character(temp$label)
temp$label[is.na(temp$label)] <- ''

# Define rectangles
rect_left <- which(nchar(temp$label) > 2) - 0.5
rect_right <- c((rect_left)[-1], nrow(temp) + 0.5)
low <- min(temp$n, na.rm = TRUE)
high <- max(temp$n, na.rm = TRUE)
rectangles <- data.frame(xmin = rect_left,
                         xmax = rect_right,
                         ymin = 0,
                         ymax = high * 1)
v_rectangles <- data.frame(xmin = 0,
                           xmax = nrow(temp) + 0.5,
                           ymin = 0,
                           ymax = c(0,high * 1))

# Filler colors for rectangles
filler <- rep(c('grey90', 'grey50'), ceiling(0.5 * nrow(rectangles)))
if(length(filler) != nrow(rectangles)){
  filler <- filler[1:(length(filler) - 1)]
}

# Replace labels
for (i in which(nchar(temp$label) > 0)){
  label <- temp$label[i]
  # remove old
  temp$label[i] <- ''
  # Give new
  temp$label[(i+12)] <- label
}

ggplot() +
  theme(axis.text.x = element_text(size = 15, angle = 45, 
                                   # family='Ubuntu Mono',
                                   hjust = 1),
        axis.text.y=element_text(hjust=1, angle=0),
        legend.position = 'none') +
  theme(axis.title.x = element_blank()) +
  scale_x_discrete(labels = temp$label) +
  theme(axis.ticks = element_blank()) +
  geom_rect(data = rectangles, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), 
            fill = filler, alpha = 0.3) +
  geom_area(data = temp, aes(x = factor(day_hour),
                             y = n,
                             group = 1),
            fill = 'gold', alpha = 0.6) +
  bcn_data_theme +
  xlab('Dia de la setmana') +
  ylab('Accidents') +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank()) 
# theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
ggsave('/home/joebrew/Documents/bcndata.github.io/img/2015-11-30-Car-accidents-in-Barcelona/full_week_ca.JPG')

# ESPANOL
temp <- accidents %>%
  data.frame %>%
  group_by(Dia.de.setmana, Hora.de.dia) %>%
  summarise(n = n())
# New var
temp$day_hour <- 
  paste0(temp$Dia.de.setmana,'-', temp$Hora.de.dia)

# Make a label vector
temp$label <- ifelse(temp$Hora.de.dia == 0,
                     temp$Dia.de.setmana,
                     NA)
label_dict <- data.frame(number = 1:7,
                         day = c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun'))
temp$label <- label_dict$day[temp$label]
temp$label <- as.character(temp$label)
temp$label[is.na(temp$label)] <- ''

# Rename in espanol
temp$label <- 
  factor(temp$label,
         levels = unique(temp$label[nchar(temp$label) > 0]),
         labels = dow_dict$es)
temp$label <- as.character(temp$label)
temp$label[is.na(temp$label)] <- ''

# Define rectangles
rect_left <- which(nchar(temp$label) > 2) - 0.5
rect_right <- c((rect_left)[-1], nrow(temp) + 0.5)
low <- min(temp$n, na.rm = TRUE)
high <- max(temp$n, na.rm = TRUE)
rectangles <- data.frame(xmin = rect_left,
                         xmax = rect_right,
                         ymin = 0,
                         ymax = high * 1)
v_rectangles <- data.frame(xmin = 0,
                           xmax = nrow(temp) + 0.5,
                           ymin = 0,
                           ymax = c(0,high * 1))

# Filler colors for rectangles
filler <- rep(c('grey90', 'grey50'), ceiling(0.5 * nrow(rectangles)))
if(length(filler) != nrow(rectangles)){
  filler <- filler[1:(length(filler) - 1)]
}

# Replace labels
for (i in which(nchar(temp$label) > 0)){
  label <- temp$label[i]
  # remove old
  temp$label[i] <- ''
  # Give new
  temp$label[(i+12)] <- label
}

ggplot() +
  theme(axis.text.x = element_text(size = 15, angle = 45, 
                                   # family='Ubuntu Mono',
                                   hjust = 1),
        axis.text.y=element_text(hjust=1, angle=0),
        legend.position = 'none') +
  theme(axis.title.x = element_blank()) +
  scale_x_discrete(labels = temp$label) +
  theme(axis.ticks = element_blank()) +
  geom_rect(data = rectangles, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), 
            fill = filler, alpha = 0.3) +
  geom_area(data = temp, aes(x = factor(day_hour),
                             y = n,
                             group = 1),
            fill = 'gold', alpha = 0.6) +
  bcn_data_theme +
  xlab('Dia de la semana') +
  ylab('Accidentes') +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank()) 
# theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
ggsave('/home/joebrew/Documents/bcndata.github.io/img/2015-11-30-Car-accidents-in-Barcelona/full_week_es.JPG')



#####
# DEATHS ONLY
#####
temp <- accidents %>%
  data.frame %>%
  filter(Número.de.morts > 0) %>%
  group_by(Hora.de.dia) %>% tally

#####
# LOCATION
#####

# Red for any death, orange for any serious injury
accidents$color <- 
  ifelse(accidents$Número.de.morts > 0, 
         adjustcolor('red', alpha.f = 0.8),
         ifelse(accidents$Número.de.lesionats.greus > 0,
                adjustcolor('darkorange', alpha.f = 0.5),
                ifelse(accidents$Número.de.lesionats.lleus > 0,
                       adjustcolor('blue', alpha.f = 0.3),
                       adjustcolor('green', alpha.f = 0.2))))

# Size
accidents$size <-
  ifelse(accidents$Número.de.morts > 0, 
         0.6,
         ifelse(accidents$Número.de.lesionats.greus > 0,
                0.4,
                ifelse(accidents$Número.de.lesionats.lleus > 0,
                       0.2, 0.1)))

# remove points outside of bbox
accidents <- 
  accidents[which(
    accidents$x >= data.frame(bbox(mapa))['min'][1,] &
      accidents$x <= data.frame(bbox(mapa))['max'][1,] &
      accidents$y >= data.frame(bbox(mapa))['min'][2,] &
      accidents$y <= data.frame(bbox(mapa))['max'][2,]),]

# Order so that green comes first, red last

par(mar = c(0,0,0,0))
jpeg('/home/joebrew/Documents/bcndata.github.io/img/2015-11-30-Car-accidents-in-Barcelona/points2.JPG',
     width = 700,
     height = 500)
plot(mapa, 
     col = adjustcolor('beige', alpha.f = 0.9),
     border = NA)#adjustcolor('black', alpha.f = 0.2))
points(accidents$x, accidents$y, pch = 16,
       cex = accidents$size,
       col = accidents$color)
dev.off()
par(mar = c(4.1, 2.1, 2.1, 1.1))

mapa_f <- fortify(mapa)

ggplot(mapa_f, aes(long, lat,group=group)) + 
  geom_polygon() +
  geom_path(color="white") +
  coord_equal() +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position="none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank()) +
  geom_point(data = accidents, 
             aes(x = x, y = y, color = color, size = 0.2 * size, group = NA), alpha = 0.2)
ggsave('/home/joebrew/Documents/bcndata.github.io/img/2015-11-30-Car-accidents-in-Barcelona/points.JPG')

ggplot(mapa_f, aes(long, lat,group=group)) + 
  geom_polygon(fill = 'grey') +
  geom_path(color="white") +
  coord_equal() +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position="none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank()) +
  #   geom_point(data = accidents, 
  #               aes(x = x, y = y, color = color, size = 0.2 * size, group = NA), alpha = 0.2) +
  stat_density2d(aes(alpha=..level..), geom="polygon", fill = 'red') +
  scale_alpha_continuous(limits=c(0,0.2),breaks=seq(0,0.2,by=0.025))
ggsave('/home/joebrew/Documents/bcndata.github.io/img/2015-11-30-Car-accidents-in-Barcelona/hotspots.JPG')


##### 
# LEAFLET MAP WIDGET
#####

# ENGLISH
sub_data <- accidents#[accidents@data$Número.de.morts > 0,]
sub_data_df <- data.frame(sub_data)
m <- leaflet(mapa) %>%
  # addProviderTiles("Stamen.TonerBackground") %>%
  addProviderTiles('OpenStreetMap.BlackAndWhite') %>%
  setView(lng=mean(sub_data_df$x), lat=mean(sub_data_df$y), zoom=13) %>%
  addMarkers(lng = sub_data_df$x, lat = sub_data_df$y,
             popup = paste0(
               ' \n| DATE: ', format(sub_data$date, '%B %d, %Y'),
               # ' \n| CAUSE: ',sub_data_df$Descripció.causa.vianant,
               ' \n| DEATHS: ', sub_data$Número.de.morts,
               ' \n| SERIOUS INJURIES: ', sub_data$Número.de.lesionats.greus),
             clusterOptions = markerClusterOptions())
m
# saveWidget(m, '/home/joebrew/Documents/bcndata/data/accidents_de_cotxe/widget.html')
saveWidget(m, '/home/joebrew/Documents/bcndata.github.io/img/2015-11-30-Car-accidents-in-Barcelona/widget.html')

# CATALA
sub_data <- accidents#[accidents@data$Número.de.morts > 0,]
sub_data_df <- data.frame(sub_data)
m <- leaflet(mapa) %>%
  # addProviderTiles("Stamen.TonerBackground") %>%
  addProviderTiles('OpenStreetMap.BlackAndWhite') %>%
  setView(lng=mean(sub_data_df$x), lat=mean(sub_data_df$y), zoom=13) %>%
  addMarkers(lng = sub_data_df$x, lat = sub_data_df$y,
             popup = paste0(
               ' \n| DATA: ', format(sub_data$date, '%B %d, %Y'),
               # ' \n| CAUSE: ',sub_data_df$Descripció.causa.vianant,
               ' \n| MORTS: ', sub_data$Número.de.morts,
               ' \n| LESIONATS GREUS: ', sub_data$Número.de.lesionats.greus),
             clusterOptions = markerClusterOptions())
m
# saveWidget(m, '/home/joebrew/Documents/bcndata/data/accidents_de_cotxe/widget.html')
saveWidget(m, '/home/joebrew/Documents/bcndata.github.io/img/2015-11-30-Car-accidents-in-Barcelona/widget_ca.html')


# ESPANOL
sub_data <- accidents#[accidents@data$Número.de.morts > 0,]
sub_data_df <- data.frame(sub_data)
m <- leaflet(mapa) %>%
  # addProviderTiles("Stamen.TonerBackground") %>%
  addProviderTiles('OpenStreetMap.BlackAndWhite') %>%
  setView(lng=mean(sub_data_df$x), lat=mean(sub_data_df$y), zoom=13) %>%
  addMarkers(lng = sub_data_df$x, lat = sub_data_df$y,
             popup = paste0(
               ' \n| FECHA: ', format(sub_data$date, '%B %d, %Y'),
               # ' \n| CAUSE: ',sub_data_df$Descripció.causa.vianant,
               ' \n| MUERTOS: ', sub_data$Número.de.morts,
               ' \n| LESIONADOS GRAVES: ', sub_data$Número.de.lesionats.greus),
             clusterOptions = markerClusterOptions())
m
# saveWidget(m, '/home/joebrew/Documents/bcndata/data/accidents_de_cotxe/widget.html')
saveWidget(m, '/home/joebrew/Documents/bcndata.github.io/img/2015-11-30-Car-accidents-in-Barcelona/widget_es.html')

