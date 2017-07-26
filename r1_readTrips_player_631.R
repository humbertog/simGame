#####################################################################
# This script processes the trips.csv file. The following actions are done:
# 1. Rename the variables
# 2. Find the name of the routes by matching the followed path with the routes
# 3. Formats the time variables
#
# IN:
# 1. routes_exp20170414.R
# 2. stat.csv (file with the session ids, demand level, n_players, etc)
# 3. trips.csv: (columns: VEH_ID, VEH_TYPE, ORIGIN, DEST, DEP_TIME_F (instants), ARR_TIME_F (instants), PATH)
#
# OUT:
# 1. A file with the same columns + 
#    - TT (in seconds)
#    - SESSION_ID (session ID)
#    - DEMAND (demand level used in the experiment)
#####################################################################
library(tidyverse)
library(lubridate)
source("r0_config.R")
source("r0_routes.R")

DIR <- DIR_TRIP_SET

trips_bdd <- read_delim(FILE_BDD_D6, delim=";")

trips_res_stat <- read_delim(paste(DIR, FILE_STATS, sep="/"), delim=";")

trips_play_s <- read_delim(FILE_PLAYED_BASE_S631, delim=";")

trips_play_s <- trips_play_s %>% rename(SESSION_ID = sessionId, 
                                    PLAYER_ID =player, 
                                    DEP_TIME_F_INI=`initial departure time`, 
                                    DEP_TIME_F=`modified departure time`,
                                    ARR_TIME_F_BASE=`initial arrival time2`,
                                    ARR_TIME_F = `made arrival time`,
                                    PATH = `sections`)


# search BASE route in the BDD
trips_bdd <- 
  trips_bdd %>%
    select(Heure_Depart, Heure_Arrivee, Iti1)

trips_play_s <-
  trips_play_s %>%
    left_join(trips_bdd, by=c("DEP_TIME_F_INI"="Heure_Depart", "ARR_TIME_F_BASE"="Heure_Arrivee"))


# Format dep and arr times
trips_play_s <- trips_play_s %>% 
  mutate(DEP_TIME =-23400 + 
           hour(DEP_TIME_F) * 3600 + 
           minute(DEP_TIME_F) * 60 + 
           second(DEP_TIME_F), 
         ARR_TIME=-23400 + 
           hour(ARR_TIME_F) * 3600 + 
           minute(ARR_TIME_F) * 60 + 
           second(ARR_TIME_F),
         DEP_TIME_INI =-23400 + 
           hour(DEP_TIME_F_INI) * 3600 + 
           minute(DEP_TIME_F_INI) * 60 + 
           second(DEP_TIME_F_INI),
         ARR_TIME_INI =-23400 + 
           hour(ARR_TIME_F_BASE) * 3600 + 
           minute(ARR_TIME_F_BASE) * 60 + 
           second(ARR_TIME_F_BASE)
  )

trips_play_s <- 
  trips_play_s %>%
  mutate(DEP_TIME = as.integer(DEP_TIME),
         ARR_TIME = as.integer(ARR_TIME),
         DEP_TIME_INI = as.integer(DEP_TIME_INI),
         ARR_TIME_INI = as.integer(ARR_TIME_INI)
  )
# Compute the travel time 
trips_play_s <- trips_play_s %>% mutate(TT = ARR_TIME - DEP_TIME)
trips_play_s <- trips_play_s %>% mutate(TT_BASE = ARR_TIME_INI - DEP_TIME_INI)


# OBSERVE THAT THERE IS A PROBLEM WITH THE TT (IN SOME CASES IT IS LESS THAN ONE MINUTE)
# MAYBE IS BECAUSE THE USER REFRESH
trips_play_s %>% 
  arrange(TT) %>% 
  select(SESSION_ID,TT) 

trips_play_s <- 
  trips_play_s %>%
  filter(TT > 360)

# THERE IS ALSO A PROBLEM WITH THE MISSIONS PLAYED AT THE BEGINNING OF THE SESSIONS:
#trips_play_s <-
#  trips_play_s %>%
#  filter(DEP_TIME >600)

# Obtains the names of the routes
trips_play_s$PATH_NAME <- NA
trips_play_s$PATH_NAME_INI <- NA
trips_play_s$ORIGIN <- NA
trips_play_s$DEST <- NA
trips_play_s$DEMAND <- NA

for (i in 1:dim(trips_play_s)[1]) {
  path_temp <- unlist(strsplit(trips_play_s$PATH[i], " ", fixed = FALSE, perl = FALSE, useBytes = FALSE))
  # Pre-process the PATH to obtain only the link ids
  path_temp_ini <- substr(path_temp,1,2)
  path_temp <- path_temp[which(path_temp_ini == "T_")]
  #trips_play_s$PATH[i] <- paste(new_path)
  
  perc_equal <- c()
  # Computes how simmilar are the routes followed to the ones proposed
  for(p in orig_route_l2) {
    inter <- sum(p %in% path_temp)
    perc_equal_t <- inter /  (length(p) + length(path_temp) - inter) 
    perc_equal <- c(perc_equal, perc_equal_t)
  }  
  route_name_idx <- which(perc_equal == max(perc_equal))
  route_name <- names(orig_route_l2)[route_name_idx]
  if (length(route_name) == 1) {
    route_name < names(orig_route_l2)[route_name_idx][1]
  }
  else {
    route_name <- "other"
    warning(paste("Found two (or more) routes that matched the PATH:", i ) )
  }
  
  trips_play_s$PATH_NAME[i] <- route_name
  
  # Set the ini path
  if (!trips_play_s$PATH_NAME[i] %in% c("R_test1_2_r1", "R_test2_2_r1", "R_test1_2_r2",
                                      "R_test2_2_r2", "R_test1_r1", "R_test2_3_r1", 
                                      "R_test1_r2", "R_test2_3_r2", "R_test1_3_r1", 
                                      "R_test2_r1", "R_test1_3_r2", "R_test2_r2", "other")) {
    trips_play_s$PATH_NAME_INI[i] <- trips_play_s$PATH_NAME[i]
  } else {
    if(trips_play_s$PATH_NAME[i] %in% c("R_test1_2_r1", "R_test1_2_r2")) {
      trips_play_s$PATH_NAME_INI[i] <- "R_test1_2"
    } 
    if(trips_play_s$PATH_NAME[i] %in% c("R_test2_2_r1", "R_test2_2_r2")) {
      trips_play_s$PATH_NAME_INI[i] <- "R_test2_2"
    } 
    if(trips_play_s$PATH_NAME[i] %in% c("R_test1_r1", "R_test1_r2")) {
      trips_play_s$PATH_NAME_INI[i] <- "R_test1"
    } 
    if(trips_play_s$PATH_NAME[i] %in% c("R_test2_3_r1", "R_test2_3_r2")) {
      trips_play_s$PATH_NAME_INI[i] <- "R_test2_3"
    } 
    if(trips_play_s$PATH_NAME[i] %in% c("R_test1_3_r1", "R_test1_3_r2")) {
      trips_play_s$PATH_NAME_INI[i] <- "R_test1_3"
    } 
    if(trips_play_s$PATH_NAME[i] %in% c("R_test2_r1", "R_test2_r2")) {
      trips_play_s$PATH_NAME_INI[i] <- "R_test2"
    } 
  }
  
  # Obtain origin and destinations
  trips_play_s$ORIGIN[i] <- path_temp[1]
  trips_play_s$DEST[i] <- path_temp[length(path_temp)]
  
  # Assigns the demand from the stat.csv file
  D <- trips_res_stat %>% 
    filter(SESSION_ID == trips_play_s$SESSION_ID[i]) %>%
    select(DEMAND)
  trips_play_s$DEMAND[i] <- paste("D", D, sep="")
  
  
}

if (sum(is.na(trips_play_s$PATH_NAME)) > 0) warning(paste("PATH_NAME could not be computed for all trips"))


trips_play_s <- 
  trips_play_s %>% 
  mutate(PATH_REROUTE = ifelse(PATH_NAME %in% c("R_test1_2_r1", "R_test2_2_r1", "R_test1_2_r2",
                                                "R_test2_2_r2", "R_test1_r1", "R_test2_3_r1", 
                                                "R_test1_r2", "R_test2_3_r2", "R_test1_3_r1", 
                                                "R_test2_r1", "R_test1_3_r2", "R_test2_r2", "other"),1,0) )



# Obtain origin and destinations 2
trips_play_s$ORIGIN[trips_play_s$ORIGIN == "T_58442383_FRef_F"] <- "T_test3"

# Adds treatment
trips_play_s <- trips_play_s %>%  mutate(TREATMENT = substr(trips_play_s$PLAYER_ID, 9,10))

# Adds the name of the OD
trips_play_s <- trips_play_s %>% mutate(OD=NA)
trips_play_s$OD[trips_play_s$ORIGIN == "T_test1"] <- "OD1_1"
trips_play_s$OD[trips_play_s$ORIGIN == "T_test2"] <- "OD1_2"
trips_play_s$OD[trips_play_s$ORIGIN == "T_test3"] <- "OD2"

trips_play_s$PATH_NAME_BASE <- NA
for (i in 1:dim(trips_play_s)[1]) {
  path_temp <- unlist(strsplit(trips_play_s$Iti1[i], " ", fixed = FALSE, perl = FALSE, useBytes = FALSE))
  # Pre-process the PATH to obtain only the link ids
  path_temp_ini <- substr(path_temp,1,2)
  path_temp <- path_temp[which(path_temp_ini == "T_")]
  #trips_play_s$PATH[i] <- paste(new_path)
  
  perc_equal <- c()
  # Computes how simmilar are the routes followed to the ones proposed
  for(p in orig_route_l2) {
    inter <- sum(p %in% path_temp)
    perc_equal_t <- inter /  (length(p) + length(path_temp) - inter) 
    perc_equal <- c(perc_equal, perc_equal_t)
  }  
  route_name_idx <- which(perc_equal == max(perc_equal))
  route_name <- names(orig_route_l2)[route_name_idx]
  if (length(route_name) == 1) {
    route_name < names(orig_route_l2)[route_name_idx][1]
  }
  else {
    route_name <- "other"
    warning(paste("Found two (or more) routes that matched the PATH:", i ) )
  }
  
  trips_play_s$PATH_NAME_BASE[i] <- route_name
  
  # Set the ini path
  
  }
  
  

