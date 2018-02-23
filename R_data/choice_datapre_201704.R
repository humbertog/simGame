library(tidyverse)

# Read the res_files
source("r0_config_201704.R")
#source("r0_config_201709.R")
source("R_readData/readTrips_player.R")
#source("R_readData/readTrips_res.R")
source("R_readData/readTTInfo.R")
source("R_functions/fun_rename.R")


# Read the alternative attributes
alternative_attributes <- read_csv("R_data/alternative_attributes.csv")


# Read the compliance 
compliance <- read_csv(COMPLIANCE_FILE)
compliance <- compliance %>% select(PLAYER_ID, compliance_rate)

# Drop some of the columns to avoid confussion
trips_play$PATH_NAME <- trips_play$PATH_NAME_INI
trips_play <- trips_play %>% 
  select(-DEP_TIME_F_INI, -ARR_TIME_F_INI, -DEP_TIME_INI, -ARR_TIME_INI, -PATH_NAME_INI, -PATH_REROUTE)

alternative_attributes <- alternative_attributes %>%
  select(-ncross)


# STANDARDIZE VARIABLES
infoTT <- 
  infoTT %>%
  group_by(OD) %>%
  mutate(TT_INFO_mean = mean(TT_INFO), TT_INFO_sd = sd(TT_INFO)) %>%
  mutate(TT_INFO_std = TT_INFO  / TT_INFO_mean ) %>%
  group_by()

# Creates the data base with the forgone choices
trips_play$CHOICE_ID <- 1:length(trips_play$OD)

od_route <- unique(trips_play[,c("OD", "PATH_NAME")])

choices <- 
  trips_play %>% 
  select(CHOICE_ID, PLAYER_ID, DEMAND, TREATMENT, OD, PATH_NAME, DEP_TIME, ARR_TIME, TT) %>%
  full_join(od_route, by="OD")



# We create the PERIOD variable 
# It is the period in which the trip started and the informed TT during this period
ini_period <- min(infoTT$PERIOD_INI)
fin_period <- max(infoTT$PERIOD_FIN)

interval <- (infoTT$PERIOD_FIN - infoTT$PERIOD_INI)[1]


infoTT$PERIOD <- getPeriod(infoTT$PERIOD_INI, ini_time = ini_period, fin_time = fin_period, interval)
choices$PERIOD <- getPeriod(choices$DEP_TIME, ini_time = ini_period, fin_time = fin_period, interval)


# Join the variables
choices <- 
  choices %>%
  left_join(infoTT[,c("ROUTE", "TT_INFO", "TT_INFO_std","TT_INFO_mean", "TT_INFO_sd", "DEMAND", "PERIOD")], 
            by=c("DEMAND"="DEMAND", "PATH_NAME.y"="ROUTE", "PERIOD"="PERIOD")) %>%
  left_join(alternative_attributes, by=c("PATH_NAME.y"="PATH_NAME", "OD"="OD"))

# join the compliance
choices <-
  choices %>%
  left_join(compliance, by= "PLAYER_ID")


# Obtains chosen route
choices <- choices %>% 
  mutate(CHOSEN=ifelse(PATH_NAME.x == PATH_NAME.y, TRUE,FALSE)) %>%
  select(-PATH_NAME.x) %>%
  rename(PATH_NAME=PATH_NAME.y)


rm(od_route, trips_play, COMPLIANCE_FILE, DIR_TRIP_SET, 
        FILE_INFOTT_D2, FILE_INFOTT_D6, FILE_STATS, FILE_TRIPS_PLAYER, FILES_INFOTT, 
        fin_period, i, ini_period, interval, nchar, routes_E_test1, routes_E_test2, routes_E_test3, 
        SESSION_IDS, SESSION_IDS_DEMAND)
