#####################################################################
# We analyse the route choice distributions along with the observed travel times during the sessions.
# The objectives are:
# 1. See if there is a relation between the tt observed in the routes and the choices
#
# The data needed to do this analysis:
# 1. resXXX.csv files (they contain the full set of trips in each OD)
# 2. trips.csv file (they contain the trips that were controlled by the users)
#####################################################################
library(tidyverse)

# Read the res_files
source("r00_config.R")
source("r0_readTrips_res.R")
source("r0_readTrips_player.R")

# sequence of times to construct the intervals for which the mean tt and
# choices are constructed
seqt <- seq(0, 5400, 600)

# size of interval 
h <- 600

SESSION_IDS <- c(625, 626, 628, 630, 631, 633, 634)

trips_res_play <- tibble()
for (s in SESSION_IDS) {
  for (t in seqt) {
    tinf <- t - h
    tsup <- t + h
    
    # Obtains the mean tt per session, and dep time interval
    trips_res_t <-
      trips_res %>% 
        filter(SESSION_ID == s) %>% 
        filter(!PATH_REROUTE) %>%
        filter(DEP_TIME >= tinf, DEP_TIME<= tsup) %>%
        group_by(SESSION_ID, OD, PATH_NAME) %>%
        summarise(MEAN_TT = mean(TT), SD_TT = sd(TT)) %>%
        mutate(TINF = tinf, TSUP = tsup)
    
    # Obtains the number of times the alt was chosen per session, and dep time interval
    trips_play_t <-
      trips_play %>%
        filter(SESSION_ID == s) %>%
        filter(DEP_TIME >= tinf, DEP_TIME<= tsup) %>%
        group_by(SESSION_ID, OD, PATH_NAME_INI) %>%
        summarise(N=n())
    
    # Joins
    trips_res_play_t <- 
      trips_res_t %>% 
        left_join(trips_play_t, by=c("SESSION_ID", "OD", "PATH_NAME" = "PATH_NAME_INI"))
    
    trips_res_play <- bind_rows(trips_res_play, trips_res_play_t) 
        
  }  
}

# computes the total number of choices by SESSION, OD, TIME INTERVAL
tot_cases <-
  trips_res_play %>% 
    group_by(SESSION_ID,OD, TINF, TSUP) %>%
    summarise(N_TOT= sum(N, na.rm=TRUE) )

# Joins the total number of choices and filters 
choices_tt <- 
  trips_res_play %>% 
  left_join(tot_cases) %>%
  filter(N_TOT > 0) 

choices_tt$N[is.na(choices_tt$N)] <- 0

# Obtains the proportions
choices_tt <- 
  choices_tt %>% 
  mutate(P=N/ N_TOT)


# We join two times the choices_tt to put data in wide format
# We also filter to obtain the base case K to which we compare the 
# proportions: p1/pK, p2/pK
choices_tt_2 <- 
  choices_tt %>% 
    inner_join(choices_tt, by=c("SESSION_ID", "OD", "TINF", "TSUP")) %>%
  filter(PATH_NAME.y %in% c("R_N1", "R_test1", "R_test2"),
         !PATH_NAME.x %in% c("R_N1", "R_test1", "R_test2")
         )


choices_tt_2 <-
  choices_tt_2 %>%
    mutate(TT_DIFF = MEAN_TT.x - MEAN_TT.y,
           P_P = P.x / P.y)

choices_tt_2 %>%
  ggplot(aes(TT_DIFF, log(P_P))) +
  geom_point() +
  geom_smooth(method='lm', se = FALSE) +
  theme_bw() +
  facet_wrap(OD ~ PATH_NAME.x, scales="free")







