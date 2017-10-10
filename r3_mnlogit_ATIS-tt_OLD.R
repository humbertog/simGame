library(tidyverse)

source("r0_routes.R")
source("r1_get_path_names.R")

#################################################
# Obtain the informed travel time from the symulation output
# This version is not appropiate as this is not the way in which 
# the informed travel times was obtained
#################################################
dat_sym2 <- read_csv("/Users/humberto.gonzalez/simulation_game/exp_20170412/data_sims/S2_2/traj_od_S2_2.csv", 
                     col_names = c("veh_id", "veh_type", "ORIGIN", "DESTINATION", "DEP_TIME", "ARR_TIME", "PATH"))

dat_sym6 <- read_csv("/Users/humberto.gonzalez/simulation_game/exp_20170412/data_sims/S2_6/traj_od_S2_6.csv", 
                     col_names = c("veh_id", "veh_type", "ORIGIN", "DESTINATION", "DEP_TIME", "ARR_TIME", "PATH"))


dat_sym2$demand <- "D2"
dat_sym6$demand <- "D6"


dat_sym <- bind_rows(dat_sym2, dat_sym6)
dat_sym %>% ggplot() + geom_histogram(aes(DEP_TIME) ) + facet_grid(. ~ demand)
dat_sym %>% ggplot() + geom_histogram(aes(ARR_TIME) ) + facet_grid(. ~ demand)
# remove the trips that didnt finished
dat_sym <- dat_sym %>% filter(ARR_TIME < 5399)
dat_sym %>% ggplot() + geom_histogram(aes(ARR_TIME) ) + facet_grid(. ~ demand)

# Compute the travel time 
dat_sym <- dat_sym %>% mutate(TT = ARR_TIME - DEP_TIME)

# The trips that started during the analysis period of the session
dat_sym <-get_path_names(dat_sym)
table(dat_sym$PATH_NAME)

# Filter out the entries with PATH_NAME== other
dat_sym <- 
  dat_sym %>%
  filter(PATH_NAME != "other")

# Compute the mean travel time in each period of departure
dat_sym$DEP_TINT1 <- cut(dat_sym$DEP_TIME, seq(0,5400, 900))
table(dat_sym$DEP_TINT1)

atis_mean_tt <- dat_sym %>% group_by(demand, OD, PATH_NAME, DEP_TINT1) %>% 
  summarise(MEAN_EST_TT= mean(TT))

#################################################
# Load the played sessions and paste the atis_mean_tt
#################################################
library(mlogit)
source("r0_config.R")
source("r1_readTrips_player.R")

trips_play$DEP_TINT1 <- cut(trips_play$DEP_TIME, seq(0,5400, 900))

# Computes the mean and sd of TT for each alternative in a choice
# They are computed arount an interval centered at the departure time of each
# trip
trips_res_play <- tibble()
for (i in 1:dim(trips_play)[1]) {
  t_ <- trips_play$DEP_TIME[i]
  tint_ <- trips_play$DEP_TINT1[i]
  db_ <- trips_play$SESSION_ID[i]
  od_ <- trips_play$OD[i]
  demand_ <- trips_play$DEMAND[i]
  tt_ <- trips_play$TT[i]
  chosen_ <- trips_play$PATH_NAME_INI[i]
  treatment_ <- trips_play$TREATMENT[i]
  
  atis_mean_tt_ <-
    atis_mean_tt %>% 
    filter(OD==od_, demand == demand_) %>%
    filter(DEP_TINT1 == tint_) 
    

  atis_mean_tt_ <- 
    atis_mean_tt_ %>% 
    group_by() %>%
    mutate(CHOICE_ID=i, 
           OD = od_,
           CHOSEN_PATH=chosen_,
           TREATMENT=treatment_,
           SESSION_ID = db_,
           CHOICE_TT = tt_,
           CHOICE_DEP_TIME = t_
    )
  
  trips_res_play <- bind_rows(trips_res_play, atis_mean_tt_) 
}

trips_res_play <-
  trips_res_play %>%
  mutate(CHOICE=ifelse(PATH_NAME == CHOSEN_PATH, TRUE, FALSE))

# Joins with the alternative characteristics
load("proc_alt.RData")
tab_d <- tab_d %>% 
  select(PATH_NAME=route, total_len, ncross) %>%
  filter(!duplicated(PATH_NAME)) %>% 
  mutate(ncross_km = ncross/ total_len * 1000)

trips_res_play <- 
  trips_res_play %>%
  left_join(tab_d)


# OBSERVE THAT THERE IS A PROBLEM WITH THE TT (IN SOME CASES IT IS LESS THAN ONE MINUTE)
# MAYBE IS BECAUSE THE USER REFRESH
trips_res_play %>%
  ggplot() + 
  geom_density(aes(MEAN_EST_TT, fill=PATH_NAME), alpha=.3) +
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


#################################################
# mnlogit model
#################################################
# OD and treatment
od <- "OD2"
treat <- "t3"

data_model <- 
  trips_res_play %>%
  filter(OD==od) %>%
  filter(TREATMENT==treat) %>%
  as.data.frame()


# Models
f0 <- mFormula(CHOICE ~ 1 )
f1 <- mFormula(CHOICE ~  -1+ I(MEAN_EST_TT/60) )  
f1.1 <- mFormula(CHOICE ~  1+ I(MEAN_EST_TT/60) )  

f2 <- mFormula(CHOICE ~ -1 + I(MEAN_EST_TT/60) + total_len + ncross_km) 

choices_mnl_all <- mlogit.data(data_model, choice="CHOICE", shape="long", alt.var="PATH_NAME", chid.var = "CHOICE_ID", drop.index=TRUE)

reflevel<-"R_N2"
mod_mnlogit0 <- mlogit(f0  , data=choices_mnl_all, reflevel=reflevel )
mod_mnlogit1 <- mlogit(f1  , data=choices_mnl_all, reflevel=reflevel)
mod_mnlogit1.1 <- mlogit(f1.1  , data=choices_mnl_all, reflevel=reflevel)
# I cant estimate a nested model!!!!!!!!!!!!!!!
#mod_mnlogit1.4 <- mlogit(f1  , data=choices_mnl_all, 
#                         nests = list(periph=c("R_test1_2"), inter=c("R_test1", "R_test1_3")))
mod_mnlogit1.2 <- mlogit(f1  , data=choices_mnl_all, 
                         rpar = c('I(MEAN_EST_TT/60)'="n"), reflevel=reflevel)

mod_mnlogit1.2.1 <- mlogit(f1.1  , data=choices_mnl_all, 
                           rpar = c('I(MEAN_EST_TT/60)'="n"), reflevel=reflevel)
#
mod_probit0 <- mlogit(f0  , data=choices_mnl_all, probit = TRUE, reflevel=reflevel)
mod_probit1 <- mlogit(f1  , data=choices_mnl_all, probit = TRUE, reflevel=reflevel)
mod_probit1.1 <- mlogit(f1.1  , data=choices_mnl_all, probit = TRUE, reflevel=reflevel)

summary(mod_mnlogit0)
summary(mod_mnlogit1)
summary(mod_mnlogit1.1)
summary(mod_mnlogit1.2)
summary(mod_mnlogit1.2.1)

#
summary(mod_probit0)
summary(mod_probit1)
summary(mod_probit1.1)
