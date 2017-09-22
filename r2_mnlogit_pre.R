#####################################################################
# We fit a multinomial logistic model to the choices. 
# The variables that we take into accout are:
# - the mean travel time in each of the alternatives
# - the standard deviation of the travel time
# - the treatment to which the player belongs
# The mean tt and the sdcorrespond to those of the travel time distributions
# constructed around the departure time of the chosen alternative.
#####################################################################

library(tidyverse)
library(mlogit)

# Read the res_files
source("r0_config.R")
source("r1_readTrips_res.R")
source("r1_readTrips_player.R")

load("proc_alt.RData")
tab_d <- tab_d %>% 
  select(PATH_NAME=route, total_len, ncross) %>%
  filter(!duplicated(PATH_NAME)) %>% 
  mutate(ncross_km = ncross/ total_len * 1000)


######################################
# Creates the data set with the choices
######################################
# size of interval 
h <- 900

trips_res_play <- tibble()

# Computes the mean and sd of TT for each alternative in a choice
# They are computed arount an interval centered at the departure time of each
# trip
for (i in 1:dim(trips_play)[1]) {
  t_ <- trips_play$DEP_TIME[i]
  db_ <- trips_play$SESSION_ID[i]
  od_ <- trips_play$OD[i]
  tt_ <- trips_play$TT[i]
  chosen_ <- trips_play$PATH_NAME_INI[i]
  treatment_ <- trips_play$TREATMENT[i]
  
  trips_res_play_t <-
    trips_res %>% 
    filter(PATH_REROUTE == 0) %>%
    filter(SESSION_ID==db_, OD==od_) %>%
    filter(DEP_TIME >= t_ - h, DEP_TIME < t_ +0) %>%
    group_by(PATH_NAME) %>%
    summarise(MEAN_TT = mean(TT), SD_TT=sd(TT))
  
  trips_res_play_t <- 
    trips_res_play_t %>% 
    mutate(CHOICE_ID=i, 
           OD = od_,
           CHOSEN_PATH=chosen_,
           TREATMENT=treatment_,
           SESSION_ID = db_,
           CHOICE_TT = tt_,
           CHOICE_DEP_TIME = t_
    )
  
  trips_res_play <- bind_rows(trips_res_play, trips_res_play_t) 
}


trips_res_play <-
  trips_res_play %>%
  mutate(CHOICE=ifelse(PATH_NAME == CHOSEN_PATH, TRUE, FALSE))


trips_res_play <- 
  trips_res_play %>%
  left_join(tab_d)

# OBSERVE THAT THERE IS A PROBLEM WITH THE TT (IN SOME CASES IT IS LESS THAN ONE MINUTE)
# MAYBE IS BECAUSE THE USER REFRESH
trips_res_play %>%
  filter(CHOICE) %>%
  mutate(DEP_TIME_INT=cut(CHOICE_DEP_TIME, seq(1,5400,900))) %>%
  ggplot() + 
  geom_point(aes(MEAN_TT, CHOICE_TT, color=PATH_NAME)) +
  geom_abline(slope=1, intercept=0) 

#

trips_res_play %>%
  ggplot() + 
  geom_density(aes(MEAN_TT, fill=PATH_NAME), alpha=.3) +
  facet_grid(OD ~.)

# check!
not_complete <- 
  trips_res_play %>%
  group_by(CHOICE_ID) %>%
  summarise(N=n()) %>%
  filter(N!=3)

# we remove the choices that are not complete
trips_res_play <- 
  trips_res_play %>% 
  filter(!CHOICE_ID %in% not_complete$CHOICE_ID)

na_cases <- unique(trips_res_play$CHOICE_ID[which(is.na(trips_res_play$SD_TT))])

trips_res_play <- 
  trips_res_play %>% 
  filter(!CHOICE_ID %in% na_cases)


