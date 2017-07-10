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
source("r0_config.R")

DIR <- DIR_TRIP_SET

SESSION_IDS <- c(625, 626, 628, 629, 630, 631, 633, 634)

# Loop to read files and assign the session ID's
# The data is then joined into one DF
source("r0_routes.R")

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


# Obtains the names of the routes
trips_res <- trips_res %>% mutate(PATH_NAME = NA)
for (i in 1:dim(trips_res)[1]) {
  path_temp <- unlist(strsplit(trips_res$PATH[i], " ", fixed = FALSE, perl = FALSE, useBytes = FALSE))
  # Pre-process the PATH to obtain only the link ids
  path_temp_ini <- substr(path_temp,1,2)
  path_temp <- path_temp[which(path_temp_ini == "T_")]
  #trips_res$PATH[i] <- paste(new_path)
  
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

  trips_res$PATH_NAME[i] <- route_name
}

if (sum(is.na(trips_res$PATH_NAME)) > 0) warning(paste("PATH_NAME could not be computed for all trips"))

trips_res <- 
  trips_res %>% 
  mutate(PATH_REROUTE = ifelse(PATH_NAME %in% c("R_test1_2_r1", "R_test2_2_r1", "R_test1_2_r2",
                                                "R_test2_2_r2", "R_test1_r1", "R_test2_3_r1", 
                                                "R_test1_r2", "R_test2_3_r2", "R_test1_3_r1", 
                                                "R_test2_r1", "R_test1_3_r2", "R_test2_r2", "other"),1,0) )



# Adds the name of the OD
trips_res <- trips_res %>% mutate(OD=NA)
trips_res$OD[trips_res$ORIGIN == "E_test1"] <- "OD1_1"
trips_res$OD[trips_res$ORIGIN == "E_test2"] <- "OD1_2"
trips_res$OD[trips_res$ORIGIN == "E_test3"] <- "OD2"




# trips_res %>% 
#  filter(ARR_TIME < FIN_TIME) %>%
#  ggplot(aes(DEP_TIME, ARR_TIME)) +
#  geom_point(aes(colour=as.factor(SESSION_ID)),alpha=.3)

# Removes all other objects
rm(list=setdiff(ls(), c("trips_res", "trips_play", "trips_res_base")))

