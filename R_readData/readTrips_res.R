#####################################################################
# This script processes the resXXX files. The following actions are done:
# 1. Name the variables
# 2. Find the name of the routes by matching the followed path with the routes
# 3. Formats the time variables
#
# IN:
# 1. r0_routes.R
# 2. stat.csv (file with the session ids, demand level, n_players, etc)
# 3. resXXX: (columns: VEH_ID, VEH_TYPE, ORIGIN, DEST, DEP_TIME (instants), ARR_TIME (instants), PATH)
#
# OUT:
# 1. A R object with the same columns + 
#    - TT (in seconds)
#    - PATH_NAME
#    - OD
#    - SESSION_ID (session ID)
#    - DEMAND (demand level used in the experiment)
#####################################################################
library(tidyverse)

source("R_data/dat_routes.R")
source("R_functions/fun_getPathNames.R")
source("R_functions/fun_rename.R")

DIR <- DIR_TRIP_SET
SESSION_IDS <- SESSION_IDS

# Loop to read files and assign the session ID's
# The data is then joined into one DF
trips_res_stat <- read_delim(paste(DIR, FILE_STATS, sep="/"), delim=";")
trips_res <- tibble()

for (s in SESSION_IDS) {
  # read the resXXX.csv files
  res_fname <- paste("res", s, "_proc.csv", sep="")
  res_t <- read_csv(paste(DIR, res_fname, sep="/"),
                    col_names=c("VEH_ID", "VEH_TYPE", "ORIGIN", "DEST", "DEP_TIME", "ARR_TIME", "PATH"))
  
  # add columns found in stat.csv
  s_stats <- trips_res_stat %>% filter(SESSION_ID==s) 
  res_t <- res_t %>% mutate(SESSION_ID = s_stats$SESSION_ID, 
                            DEMAND = paste("D", s_stats$DEMAND, sep=""),
                            FIN_TIME = s_stats$PLAY_TIME
                            )
  trips_res <- bind_rows(trips_res, res_t)
}

# Compute the travel time 
trips_res <- trips_res %>% mutate(TT = ARR_TIME - DEP_TIME)

# it is important to remove those trips that were not finished during the game
trips_res <- 
  trips_res %>% 
  filter(ARR_TIME < FIN_TIME) 


###### Route names 
# Obtains the names of the routes
trips_res$PATH_NAME <- getPathNames(trips_res$PATH, orig_route_l2)
trips_res$PATH_NAME_INI <- getIniPathName(trips_res$PATH_NAME)
if (sum(is.na(trips_res$PATH_NAME)) > 0) warning(paste("PATH_NAME could not be computed for all trips"))

trips_res <- 
  trips_res %>% 
  mutate(PATH_REROUTE = ifelse(PATH_NAME == PATH_NAME_INI, 0,1))

###### Obtains the OD name 
trips_res$OD <- getODNames(trips_res$ORIGIN, trips_res$DEST)

# RENAMES
trips_res$PATH_NAME <- renameRoutes(trips_res$PATH_NAME)
trips_res$PATH_NAME_INI <- renameRoutes(trips_res$PATH_NAME_INI)

trips_res$OD <- renameOD(trips_res$OD)

###### 
trips_res <- trips_res %>% select(-PATH)

rm(list=c("res_t", "s_stats", "trips_res_stat", "DIR", "orig_route_l", "orig_route_l2", "s", "res_fname"
))
print(paste("Number of rows: ", dim(trips_res)[1]))   
