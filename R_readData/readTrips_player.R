#####################################################################
# This script processes the trips.csv file. The following actions are done:
# 1. Rename the variables
# 2. Find the name of the routes by matching the followed path with the routes
# 3. Formats the time variables
#
# IN:
# 1. routes.R
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

source("R_data/dat_routes.R")
source("R_functions/fun_getPathNames.R")
source("R_functions/fun_rename.R")

DIR <- DIR_TRIP_SET
FNAME <- FILE_TRIPS_PLAYER


#trips_res_stat <- read_delim(paste(DIR, FILE_STATS, sep="/"), delim=";")

trips_play <- read_delim(paste(DIR, FNAME, sep="/"), delim=";")

trips_play <- trips_play %>% rename(SESSION_ID = sessionId, 
                      PLAYER_ID =player, 
                      DEP_TIME_F_INI=`initial departure time`, 
                      DEP_TIME_F=`modified departure time`,
                      ARR_TIME_F_INI=`initial arrival time`,
                      ARR_TIME_F = `made arrival time`,
                      SCORE = `score`,
                      PATH = `sections`)



###### Format dep and arr times
# trips_play <- trips_play %>% 
#   mutate(DEP_TIME =-23400 + 
#            hour(DEP_TIME_F) * 3600 + 
#            minute(DEP_TIME_F) * 60 + 
#            second(DEP_TIME_F), 
#          ARR_TIME=-23400 + 
#            hour(ARR_TIME_F) * 3600 + 
#            minute(ARR_TIME_F) * 60 + 
#            second(ARR_TIME_F),
#          DEP_TIME_INI =-23400 + 
#            hour(DEP_TIME_F_INI) * 3600 + 
#            minute(DEP_TIME_F_INI) * 60 + 
#            second(DEP_TIME_F_INI),
#          ARR_TIME_INI =-23400 + 
#            hour(ARR_TIME_F_INI) * 3600 + 
#            minute(ARR_TIME_F_INI) * 60 + 
#            second(ARR_TIME_F_INI)
#   )

trips_play <- 
  trips_play %>%
  mutate(DEP_TIME = as.integer(DEP_TIME_F),
         ARR_TIME = as.integer(ARR_TIME_F),
         DEP_TIME_INI = as.integer(DEP_TIME_F_INI),
         ARR_TIME_INI = as.integer(ARR_TIME_F_INI)
  )

###### Compute the travel time 
trips_play <- trips_play %>% mutate(TT = ARR_TIME - DEP_TIME)


# OBSERVE THAT THERE IS A PROBLEM WITH THE TT (IN SOME CASES IT IS LESS THAN ONE MINUTE)
# MAYBE IT IS BECAUSE THE USER REFRESH
trips_play %>% 
  arrange(TT) %>% 
  select(SESSION_ID,TT) 

trips_play <- 
  trips_play %>%
  filter(TT > 300)



###### Route names 
# Obtains the names of the routes
trips_play$PATH_NAME <- getPathNames(trips_play$PATH , orig_route_l2, simmilarity=.6)
trips_play$PATH_NAME_INI <- getIniPathName(trips_play$PATH_NAME)
if (sum(is.na(trips_play$PATH_NAME)) > 0) warning(paste("PATH_NAME could not be computed for all trips"))

trips_play <- 
  trips_play %>% 
  mutate(PATH_REROUTE = ifelse(PATH_NAME == PATH_NAME_INI, 0,1))

######  Obtains the origins and destinations
trips_play$ORIGIN <- NA
trips_play$DEST <- NA

for(r in unique(trips_play$PATH_NAME)) {
  orig_route_l2[[r]][length(orig_route_l2[[r]])]
  trips_play$ORIGIN[trips_play$PATH_NAME == r] <- orig_route_l2[[r]][1]
  trips_play$DEST[trips_play$PATH_NAME == r] <- orig_route_l2[[r]][length(orig_route_l2[[r]])]
}
trips_play$ORIGIN[trips_play$ORIGIN == "T_58442383_FRef_F"] <- "T_test3"

###### Obtains the OD name 
trips_play$OD <- getODNames(trips_play$ORIGIN, trips_play$DEST)

###### Obtains the demand (Assigns the demand from the config file)
trips_play$DEMAND <- NA

for(s in unique(trips_play$SESSION_ID)) {
  trips_play$DEMAND[trips_play$SESSION_ID == s] <- SESSION_IDS_DEMAND[[as.character(s)]]
}

###### Adds treatment
nchar <- length(strsplit(trips_play$PLAYER_ID[1] , split='')[[1]])

trips_play <- trips_play %>%  mutate(TREATMENT = substr(trips_play$PLAYER_ID, nchar-1,nchar))

###### Renames
trips_play$PATH_NAME <- renameRoutes(trips_play$PATH_NAME)
trips_play$PATH_NAME_INI <- renameRoutes(trips_play$PATH_NAME_INI)
trips_play$OD <- renameOD(trips_play$OD)

###### Removes all other objects
trips_play <- trips_play %>% select(-PATH)

rm(list=c("DIR", "FNAME", "orig_route_l", "orig_route_l2", "r", "s"))
print(paste("Number of rows: ", dim(trips_play)[1]))       
