library(tidyverse)

# Reads the routes
source("r0_routes.R")


# Read files: output of symuvia
KIND <- "Final_Session"

FILE <- "traj_od.csv"
DIR <- "/Users/humberto.gonzalez/simulation_game/exp_20170620"
DIR_FILE <- paste(DIR, KIND, FILE, sep="/")

trips_res <- read_csv(DIR_FILE, col_names = c("veh_id", "veh_type", "ORIGIN", "DESTINATION", "DEP_TIME", "ARR_TIME", "PATH"))


# Compute the travel time 
trips_res <- trips_res %>% mutate(TT = ARR_TIME - DEP_TIME)

# The trips that started during the analysis period of the session
SESS_FIN <- 12600
trips_res <- 
  trips_res %>% 
  filter(DEP_TIME < SESS_FIN) 

# Obtains the names of the routes
trips_res <- trips_res %>% mutate(PATH_NAME = NA)
# this variable is only to check results of naming paths
path_max_p <- c()
for (i in 1:dim(trips_res)[1]) {
#for (i in 2:2) {
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
  
  #print(paste(as.character(max(perc_equal)), route_name))
  path_max_p <- c(path_max_p, max(perc_equal))
  if (length(route_name) == 1 & max(perc_equal) > .8) {
    route_name < names(orig_route_l2)[route_name_idx][1]
  }
  else {
    route_name <- "other"
    warning(paste("Found two (or more) routes that matched the PATH:", i ) )
  }
  
  trips_res$PATH_NAME[i] <- route_name
}

table(trips_res$PATH_NAME)
hist(path_max_p)
#if (sum(is.na(trips_res$PATH_NAME)) > 0) warning(paste("PATH_NAME could not be computed for all trips"))

# Adds the name of the OD
trips_res <- trips_res %>% mutate(OD=NA)
trips_res$OD[trips_res$ORIGIN == "E_test1"] <- "OD1_1"
trips_res$OD[trips_res$ORIGIN == "E_test2"] <- "OD1_2"
trips_res$OD[trips_res$ORIGIN == "E_test3"] <- "OD2"
#

# Filter out the entries with PATH_NAME== other
trips_res <- 
  trips_res %>%
  filter(PATH_NAME != "other", OD != "OD1_2") %>%
  select(-PATH)
  

##############################################################
# Here the travel time dist analysis starts
##############################################################
trips_res$DEP_TIME_INTERVAL <- cut(trips_res$DEP_TIME, breaks=seq(0, 9000, 1800))

trips_res %>%
  ggplot() +
  geom_density(aes(TT, colour=PATH_NAME, fill=PATH_NAME), alpha=.2) +
  xlim(0,3500) + ylim(0, 0.008) + 
  facet_grid(OD ~ .)

ggsave(filename = paste(KIND, "_tt_dist", ".png", sep=""))

trips_res %>%
  ggplot() +
  geom_boxplot(aes(DEP_TIME_INTERVAL, TT, colour=PATH_NAME, fill=PATH_NAME), alpha=.2) + 
  ylim(0, 3000) +
  facet_grid(OD~.)

ggsave(filename = paste(KIND, "_tt_dist_interval", ".png", sep=""))













