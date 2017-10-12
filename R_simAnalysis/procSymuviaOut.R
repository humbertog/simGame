#####################################################################
# Plots the travel time distributions for the different routes
# resulting from the symulation previous to the experiments
# IN: traj_od.csv files
#     these files contain the trajectories of all the trips in the
#     routes that we are interested in.
#     The files are obtained using parser.py and make_res_file.py 
#####################################################################


library(tidyverse)

# Reads the routes
source("R_dat/dat_routes.R")
source("R_functions/fun_getPathNames.R")

# Read files: output of symuvia
KIND <- "D1A2_VP5"

FILE <- "traj_od.csv"
DIR <- "/Users/humberto.gonzalez/simulation_game/exp_20170925/Proposed_cases/SG_Session_09_17"
DIR_FILE <- paste(DIR, KIND, FILE, sep="/")

trips_res <- read_csv(DIR_FILE, col_names = c("veh_id", "veh_type", "ORIGIN", "DESTINATION", "DEP_TIME", "ARR_TIME", "PATH"))


# Compute the travel time 
trips_res <- trips_res %>% mutate(TT = ARR_TIME - DEP_TIME)

# The trips that started during the analysis period of the session
SESS_FIN <- 12600
trips_res <- 
  trips_res %>% 
  filter(DEP_TIME < SESS_FIN) 

trips_res <-get_path_names(trips_res)
table(trips_res$PATH_NAME)

# Filter out the entries with PATH_NAME== other
trips_res <- 
  trips_res %>%
  filter(PATH_NAME != "other", OD != "OD1_2")
  

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













