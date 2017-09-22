#####################################################################
# This script processes the resXXX files for the BASE scenarios. The following actions are done:
# 1. Name the variables
# 2. Find the name of the routes by matching the followed path with the routes
#
# IN:
# 1. r0_routes.R
# 2. SG_SXXXres_proc.csv: (columns: VEH_ID, VEH_TYPE, ORIGIN, DEST, DEP_TIME (instants), ARR_TIME (instants), PATH)
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
#source("r0_config.R")
source("r0_routes.R")
# 
DIR <- DIR_TRIP_SET_BASE

trips_res_base <- read_csv(paste(DIR, FILE_BASE_SCENARIO, sep="/"), 
                           col_names=c("VEH_ID", "VEH_TYPE", "ORIGIN", "DEST", "DEP_TIME", "ARR_TIME", "PATH"))


### Change type
trips_res_base <- 
  trips_res_base %>%
  mutate(DEP_TIME = as.integer(DEP_TIME),
         ARR_TIME = as.integer(ARR_TIME)
  )

### Computes TT
trips_res_base <- 
  trips_res_base %>% 
  mutate(TT = ARR_TIME - DEP_TIME)

### Computes the FIN_TIME of the session
trips_res_base <- 
  trips_res_base %>% 
  mutate(FIN_TIME = max(ARR_TIME))


# it is important to remove those trips that were not finished during the game
#trips_res_base %>% 
#  ggplot() +
#  geom_point(aes(DEP_TIME, ARR_TIME)) +
  

#trips_res_base <- 
#  trips_res_base %>% 
#  filter(ARR_TIME < FIN_TIME) 


# Obtains the origin
for (i in 1:dim(trips_res_base)[1]) {
  path_temp <- unlist(strsplit(trips_res_base$PATH[i], " ", fixed = FALSE, perl = FALSE, useBytes = FALSE))
  # Pre-process the PATH to obtain only the link ids
  
  origin <- NA
  if (path_temp[1] == "T_test1") {
    origin <- "E_test1"
  } 
  if (path_temp[1] == "T_test2") {
    origin <- "E_test2"
  } 
  if (path_temp[1] %in% c("T_test3","T_58442383_FRef_F")  ) {
    origin <- "E_test3"
  } 
  if (is.na(origin)) {
    print(path_temp[1])
  }
  trips_res_base$ORIGIN[i] <- origin
}
# ONLY USED TO PROCESS res629
# write.csv(trips_res_base, "temp.csv")


### 
# Obtains the names of the routes
trips_res_base <- trips_res_base %>% mutate(PATH_NAME = NA)
for (i in 1:dim(trips_res_base)[1]) {
  path_temp <- unlist(strsplit(trips_res_base$PATH[i], " ", fixed = FALSE, perl = FALSE, useBytes = FALSE))
  # Pre-process the PATH to obtain only the link ids
  path_temp_ini <- substr(path_temp,1,2)
  path_temp <- path_temp[which(path_temp_ini == "T_")]
  #trips_res_base$PATH[i] <- paste(new_path)
  
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
  
  trips_res_base$PATH_NAME[i] <- route_name
}

if (sum(is.na(trips_res_base$PATH_NAME)) > 0) warning(paste("PATH_NAME could not be computed for all trips"))

trips_res_base <- 
  trips_res_base %>% 
  mutate(PATH_REROUTE = ifelse(PATH_NAME %in% c("R_test1_2_r1", "R_test2_2_r1", "R_test1_2_r2",
                                                "R_test2_2_r2", "R_test1_r1", "R_test2_3_r1", 
                                                "R_test1_r2", "R_test2_3_r2", "R_test1_3_r1", 
                                                "R_test2_r1", "R_test1_3_r2", "R_test2_r2", "other"),1,0) )

# Adds the name of the OD
#trips_res_base$ORIGIN[trips_res_base$PATH_NAME %in% c("R_test1", "R_test1_2", "R_test1_3")] <- "E_test1"
#trips_res_base$ORIGIN[trips_res_base$PATH_NAME %in% c("R_test2", "R_test2_2", "R_test2_3")] <- "E_test2"
#trips_res_base$ORIGIN[trips_res_base$PATH_NAME %in% c("R_N1", "R_N2", "R_N3")] <- "E_test3"


trips_res_base <- trips_res_base %>% mutate(OD=NA)
trips_res_base$OD[trips_res_base$ORIGIN == "E_test1"] <- "OD1_1"
trips_res_base$OD[trips_res_base$ORIGIN == "E_test2"] <- "OD1_2"
trips_res_base$OD[trips_res_base$ORIGIN == "E_test3"] <- "OD2"

rm(list=setdiff(ls(), c("trips_res", "trips_play", "trips_res_base")))
