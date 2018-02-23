library(tidyverse)
library(ineq)

source("r0_config_201704.R")
source("R_readData/readTrips_player.R")
source("R_readData/readTrips_res.R")
source("R_readData/readTTInfo.R")
source("R_functions/fun_rename.R")


# Drop some of the columns to avoid confussion
trips_play$PATH_NAME <- trips_play$PATH_NAME_INI
trips_play <- trips_play %>% 
  select(-DEP_TIME_F_INI, -ARR_TIME_F_INI, -DEP_TIME_INI, -ARR_TIME_INI, -PATH_NAME_INI, -PATH_REROUTE)

# Remove the rerouting routes
trips_res <- 
  trips_res %>%
  filter(PATH_REROUTE == 0)

# Creates the data base with the forgone choices
trips_play$CHOICE_ID <- 1:length(trips_play$OD)

od_route <- unique(trips_play[,c("OD", "PATH_NAME")])

choices <- 
  trips_play %>% 
  select(SESSION_ID,PLAYER_ID, CHOICE_ID, DEMAND, TREATMENT, OD, PATH_NAME, DEP_TIME, ARR_TIME, TT) %>%
  full_join(od_route, by="OD")

# We create the PERIOD variable 
# It is the period in which the trip started and the informed TT during this period
ini_period <- min(infoTT$PERIOD_INI)
fin_period <- max(infoTT$PERIOD_FIN)

interval <- (infoTT$PERIOD_FIN - infoTT$PERIOD_INI)[1]

infoTT$PERIOD <- getPeriod(infoTT$PERIOD_INI, ini_time = ini_period, fin_time = fin_period, interval)
choices$PERIOD <- getPeriod(choices$DEP_TIME, ini_time = ini_period, fin_time = fin_period, interval)

trips_res <- 
  trips_res %>%
  mutate(DEP_TIME = DEP_TIME + ini_period, ARR_TIME = ARR_TIME + ini_period) 

trips_res$PERIOD <- getPeriod(trips_res$DEP_TIME, ini_time = ini_period, fin_time = fin_period, interval)

############ Computes the mean travel time in each route period
mean_tt <- trips_res %>%
  group_by(SESSION_ID, OD, PATH_NAME, PERIOD) %>%
  summarise(MEAN_TT = mean(TT))


############ Join the variables
choices <- 
  choices %>%
  left_join(infoTT[,c("ROUTE", "TT_INFO", "DEMAND", "PERIOD")], 
            by=c("DEMAND"="DEMAND", "PATH_NAME.y"="ROUTE", "PERIOD"="PERIOD")) %>%
  left_join(mean_tt[,c("SESSION_ID", "PATH_NAME", "PERIOD", "MEAN_TT")], 
            by=c("SESSION_ID"="SESSION_ID", "PATH_NAME.y"="PATH_NAME", "PERIOD"="PERIOD"))

# The MEAN_TT NA's may be explained because the trips that were not finished were removed
na_ids <- unique(choices$CHOICE_ID[is.na(choices$MEAN_TT)])
choices <- choices[!choices$CHOICE_ID %in% na_ids,]

# Obtains chosen route
choices <- choices %>% 
  mutate(CHOSEN=ifelse(PATH_NAME.x == PATH_NAME.y, TRUE,FALSE)) %>%
  select(-PATH_NAME.x) %>%
  rename(PATH_NAME=PATH_NAME.y)


# Order choices by fastest 
choices <- choices %>% 
  arrange(CHOICE_ID, TT_INFO) %>%
  mutate(order_info_tt = rep(1:3, length(choices$CHOICE_ID) / 3))

# check
tail(choices)

choices <- choices %>% 
  arrange(CHOICE_ID, MEAN_TT) %>%
  mutate(order_tt = rep(1:3, length(choices$CHOICE_ID) / 3))

choices_made <-
  choices %>%
  filter(CHOSEN ==TRUE)


#############################################################################

# Number of choices per player
choices_made %>%
  group_by(PLAYER_ID) %>%
  summarise(n_choices = n()) %>%
  ggplot() +
  geom_bar(aes(n_choices))
  
# Number of choices per player per OD
choices_made %>%
  group_by(PLAYER_ID, OD) %>%
  summarise(n_choices = n()) %>%
  ggplot() +
  geom_bar(aes(n_choices, fill=OD))


# Number of choices per player per route and OD
choices_made %>%
  group_by(PLAYER_ID, OD, PATH_NAME) %>%
  summarise(n_choices_route = n()) %>%
  group_by(PLAYER_ID, OD) %>%
  mutate(n_choices_od = sum(n_choices_route)) %>%
  filter(n_choices_od > 1) %>%
  mutate(perc_same = n_choices_route / n_choices_od) %>%
  ggplot() +
  geom_histogram(aes(perc_same))


choices_made %>%
  group_by(PLAYER_ID, OD, PATH_NAME) %>%
  summarise(n_choices_route = n()) %>%
  group_by(PLAYER_ID, OD) %>%
  mutate(n_choices_od = sum(n_choices_route)) %>%
  filter(n_choices_od > 1 , n_choices_route >1) %>%
  mutate(perc_same = n_choices_route / n_choices_od) %>%
  ggplot() +
  geom_histogram(aes(perc_same))

choices_made %>%
  group_by(PLAYER_ID, OD, PATH_NAME) %>%
  summarise(n_choices_route = n()) %>%
  group_by(PLAYER_ID, OD) %>%
  mutate(n_choices_od = sum(n_choices_route)) %>%
  filter(n_choices_od > 1) %>%
  mutate(perc_same = n_choices_route / n_choices_od) %>%
  filter(perc_same > .5) %>%
  ggplot() +
  geom_histogram(aes(perc_same))
