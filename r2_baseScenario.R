#####################################################################
# Compares the flows and travel times of the base scenario (the simulation without players)
# and the played sessions.
# 1. Obtain the flow in both the base and played scenarios
# 2. Obtains the tt of both the base and played scenarios
# 3. Plotting
#####################################################################
library(tidyverse)


###  Read the res_files
source("r0_config.R")
source("r1_readTrips_base.R")
source("r1_readTrips_res.R")
source("r1_readTrips_player.R")
source("r1_readTrips_player_631.R")


########################################################################
# Compares the base case with the played case (ONLY PLAYED)
########################################################################
trips_play_s <-
  trips_play_s %>%
    mutate(CHG_PATH=ifelse(PATH_NAME==PATH_NAME_BASE,0,1)) %>%
    mutate(BEFORE=ifelse(DEP_TIME < 3000,1,0)) %>%
    mutate(REROUTE=ifelse(PATH_NAME==PATH_NAME_INI,0,1))

trips_play_s %>%
  ggplot() +
  geom_point(aes(DEP_TIME_F_INI, DEP_TIME_F)) +
  geom_abline(slope = 1, intercept=0)
# the departure times in the played case are higher -> lower tt's?

trips_play_s %>%
  ggplot() +
  geom_point(aes(TT_BASE, TT, colour=as.factor(CHG_PATH), shape=as.factor(BEFORE))) +
  geom_abline(slope = 1, intercept=0)
# The travel times of the played are lower! Check the ones with greater improvement

###
table(trips_play_s$PATH_NAME_INI)
table(trips_play_s$PATH_NAME_BASE)

table(trips_play_s$PATH_NAME_BASE,trips_play_s$PATH_NAME)

trips_play_s %>%
  filter(REROUTE==0) %>%
  select(OD,PATH_NAME_INI, PATH_NAME_BASE) %>%
  gather(CASE,PATH, -OD) %>%
  ggplot() +
  geom_bar(aes(PATH,fill=CASE),  position="dodge")
  
  
trips_play_s %>%
  select(PATH_NAME, TT,REROUTE)  %>%
  rename(PATH=PATH_NAME) %>%
  mutate(CASE="play") %>%
  bind_rows(trips_play_s %>%
              select(PATH_NAME_BASE, TT_BASE,REROUTE)  %>%
              rename(PATH=PATH_NAME_BASE, TT=TT_BASE) %>%
              mutate(CASE="base")) %>%
  ggplot() +
  geom_boxplot(aes(PATH, TT, colour=CASE))
  
  
#
trips_play_s %>%
  select(PATH_NAME_INI, TT)  %>%
  rename(PATH=PATH_NAME_INI) %>%
  mutate(CASE="play") %>%
  bind_rows(trips_play_s %>%
              select(PATH_NAME_BASE, TT_BASE)  %>%
              rename(PATH=PATH_NAME_BASE, TT=TT_BASE) %>%
              mutate(CASE="base")) %>%
  ggplot() +
  geom_boxplot(aes(PATH, TT, colour=CASE))


trips_play_s %>%
  filter(abs(TT-TT_BASE) > 300,  PATH_NAME_BASE == PATH_NAME) %>%
  select(OD, PATH_NAME_INI, PATH_NAME_BASE, PATH_NAME, DEP_TIME_F, DEP_TIME_F_INI,TT, TT_BASE) %>%
  mutate(TTDIFF=TT_BASE-TT)
  
trips_play_s %>%
#  filter(DEP_TIME <2400) %>%
  select(OD, PATH_NAME_INI, PATH_NAME_BASE, PATH_NAME, DEP_TIME_F, DEP_TIME_F_INI,TT, TT_BASE) %>%
  mutate(TTDIFF=TT_BASE-TT) %>%
  arrange(TTDIFF)

########################################################################
# Compares the base case with the played case (ALL TRIPS)
########################################################################
# 
session_id <- 631

# Select the wanted columns and cases
trips_res_s <- 
  trips_res %>% 
  filter(SESSION_ID == session_id) %>%
  select(OD, PATH_NAME, DEP_TIME, ARR_TIME, TT, PATH_REROUTE) %>%
  mutate(DB_ID = "played")

trips_res_base_s <-
  trips_res_base %>%
  select(OD, PATH_NAME, DEP_TIME, ARR_TIME, TT, PATH_REROUTE) %>%
  mutate(DB_ID = "base")


trips_res %>%
  group_by(PATH_NAME) %>% 
  summarise(max(DEP_TIME), max(ARR_TIME))

trips_res_base %>%
  group_by(PATH_NAME) %>% 
  summarise(max(DEP_TIME),max(ARR_TIME))

#######
# We add a column with the initial path


# trips_res_s <-
#   trips_res_s %>% 
#     mutate(PATH_NAME_ACTUAL = PATH_NAME) %>%
#     mutate(PATH_NAME=ifelse(PATH_NAME_ACTUAL %in% c("R_test1_2_r1", "R_test1_2_r2"), "R_test1_2", PATH_NAME)) %>%
#     mutate(PATH_NAME=ifelse(PATH_NAME_ACTUAL %in% c("R_test1_r1", "R_test1_r2"), "R_test1", PATH_NAME)) %>%
#     mutate(PATH_NAME=ifelse(PATH_NAME_ACTUAL %in% c("R_test2_3_r1", "R_test2_3_r2"), "R_test2_3", PATH_NAME)) %>%
#     mutate(PATH_NAME=ifelse(PATH_NAME_ACTUAL %in% c("R_test2_r1", "R_test2_r2"), "R_test2", PATH_NAME))
# 





dim(trips_res_s)
dim(trips_res_base_s)


# Bind the data sets
trips_play_base <- 
  bind_rows(trips_res_s, trips_res_base_s) %>%
 # filter(PATH_REROUTE == 0) %>%
  filter(DEP_TIME > 900 , DEP_TIME < 4000)

# CHECK
trips_play_base %>% 
  filter(PATH_REROUTE == 0, DEP_TIME < 600) %>%
  ggplot(aes(DEP_TIME)) +
  geom_density(aes(colour=DB_ID))

trips_play_base %>% 
  filter(PATH_REROUTE == 0, DEP_TIME < 600) %>%
  ggplot(aes(TT)) +
  geom_density(aes(colour=DB_ID))

trips_play %>% 
  filter(SESSION_ID == 631) %>%
  group_by(PATH_NAME) %>%
  summarise(min(DEP_TIME), max(DEP_TIME))

trips_play %>% 
  filter(SESSION_ID==631) %>%
  mutate(TREATMENT=ifelse(TREATMENT=="t1", "Treatment 1:NI", ifelse(TREATMENT=="t2", "Treatment 2:CMI", "Treatment 3:TTI"))) %>%
  ggplot(aes(PATH_NAME_INI)) + 
  geom_bar(aes(colour=OD, fill=OD)) + 
  facet_grid(. ~ TREATMENT) +
  theme_bw() +
  labs(x="route name", y="count", fill = "OD", colour="OD") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 


######################################
# ROUTE TRIP DISTRIBUTION
######################################
trips_play_base %>%
  ggplot() +
  geom_bar(aes(OD,colour=DB_ID, fill=DB_ID), position="dodge")

trips_play_base %>%
  ggplot() +
  geom_bar(aes(PATH_NAME,colour=DB_ID, fill=DB_ID), position="dodge")

trips_play_base %>%
  ggplot() +
  geom_boxplot(aes(PATH_NAME, TT, colour=DB_ID), position="dodge")

######################################
# TT distributions
######################################


# All OD's
trips_play_base %>%
  ggplot() +
  geom_density(aes(TT, fill=DB_ID), alpha=.3) 
  
trips_play_base %>%
  ggplot() +
  geom_density(aes(TT, fill=DB_ID), alpha=.3) +
  facet_grid(OD ~ .)

trips_play_base %>%
  ggplot() +
  geom_density(aes(TT, fill=DB_ID), alpha=.3) +
  facet_grid(PATH_NAME ~ .)


# Why if R_N1 is used more it is faster?
trips_play_base %>%
  ggplot() +
  geom_histogram(aes(x = DEP_TIME, fill=DB_ID), alpha=.5, position = "dodge") +
  facet_grid(PATH_NAME ~ .)

######################################
# FLOW: number of cars entering the route in an interval of time
######################################
###  Cut intervals
intervals <- seq(0,5400, 900)
trips_play_base <- trips_play_base %>% 
  mutate(DEP_TIME_INTERVAL=cut(DEP_TIME, intervals))

flow_interval <- 
  trips_play_base %>% 
  group_by(DB_ID,PATH_NAME, DEP_TIME_INTERVAL) %>%
  summarise(FLOW = n(), MEAN_TT= mean(TT))
  

# check nrows: 
table(trips_play_base$DEP_TIME_INTERVAL)

# Flow
flow_interval %>%
  ggplot(aes(DEP_TIME_INTERVAL, FLOW)) +
  geom_point(aes(colour=DB_ID), alpha=.5) +
  geom_line(aes(group = DB_ID, colour = DB_ID)) + 
  facet_grid(PATH_NAME ~ .)

# Mean TT
flow_interval %>%
  ggplot(aes(DEP_TIME_INTERVAL, MEAN_TT)) +
  geom_point(aes(colour=DB_ID), alpha=.5) +
  geom_line(aes(group = DB_ID, colour = DB_ID)) + 
  facet_grid(PATH_NAME ~ .)

# Flow vs TT
flow_interval %>%
  filter() %>%
  ggplot(aes(FLOW, MEAN_TT)) +
  geom_point(aes(colour=DB_ID), alpha=.5) +
  facet_grid(PATH_NAME ~ DEP_TIME_INTERVAL)


######################################
# Check departure times
######################################
trips_play_base %>%
  ggplot() +
  geom_density(aes(DEP_TIME, fill=DB_ID), alpha=.3) +
  facet_grid(PATH_NAME ~ .)

























######################################
# Computes the lagged flow
######################################
t_ant <- 15 * 60
t_post <- 10 * 60 

trips_play_base$FLOW_ANT <- NA
trips_play_base$TT_POST <- NA
for (i in 1:dim(trips_play_base)[1]) {
  t <- trips_play_base$DEP_TIME[i]
  path_ <- trips_play_base$PATH_NAME[i]
  db_ <- trips_play_base$DB_ID[i]
  
  dant <- trips_play_base %>% 
    filter(DB_ID == db_, PATH_NAME == path_) %>% 
    filter(DEP_TIME >= t - t_ant, DEP_TIME <= t)
  
  dpost <- trips_play_base %>% 
    filter(DB_ID == db_, PATH_NAME == path_) %>% 
    filter(DEP_TIME >= t, DEP_TIME <= t + t_post)
  
  trips_play_base$FLOW_ANT[i] <- dim(dant)[1]
  trips_play_base$TT_POST[i] <- mean(dpost$TT)
}

trips_play_base %>%
  ggplot(aes(DEP_TIME, FLOW_ANT)) +
  geom_line(aes(colour=DB_ID)) +
  facet_grid(PATH_NAME ~ .)
  

trips_play_base %>%
  ggplot(aes(DEP_TIME, TT_POST)) +
  geom_line(aes(colour=DB_ID)) +
  facet_grid(PATH_NAME ~ .)

trips_play_base %>%
  group_by(DB_ID) %>%
  summarise(min(DEP_TIME))

# make the summaries AND UPDATE THE EXCEL FILE WITH THE MAXIMIZATION TABLE
trips_play_base %>%
  filter(DB_ID == "base") %>%
  ggplot(aes(FLOW_ANT, TT_POST)) +
  geom_point(aes(colour=DB_ID), alpha=.5) +
  #geom_line(aes(group = DB_ID, colour = DB_ID)) + 
  facet_grid(PATH_NAME ~ .)





