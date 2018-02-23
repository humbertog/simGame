#####################################################################
# We study the number of times that the INFORMED minimum and ACTUAL 
# minimum travel time routes were chosen. 
# 1. For each played mission, we take the informed travel time
# of all the three alternatives in a choice problem.
# 2. Then, we sort the alternatives in ascending order 
# 3. Finally we see in which place the chosen alternative is.
#####################################################################
library(tidyverse)

source("r0_config_201709.R")
source("R_readData/readTrips_player.R")
source("R_readData/readTrips_res.R")
source("R_readData/readTTInfo.R")
source("R_functions/fun_rename.R")


# Drop some of the columns to avoid confussion
trips_play$PATH_NAME <- trips_play$PATH_NAME_INI
trips_play <- trips_play %>% 
  select(-DEP_TIME_F_INI, -ARR_TIME_F_INI, -DEP_TIME_INI, -ARR_TIME_INI, -PATH_NAME_INI, -PATH_REROUTE)

# Remove the rerouting routes
trips_res <- 
  trips_res %>%
  filter(PATH_REROUTE == 0)

# Creates the data base with the forgone choices
trips_play$CHOICE_ID <- 1:length(trips_play$OD)

od_route <- unique(trips_play[,c("OD", "PATH_NAME")])

choices <- 
  trips_play %>% 
  select(SESSION_ID,PLAYER_ID, CHOICE_ID, DEMAND, TREATMENT, OD, PATH_NAME, DEP_TIME, ARR_TIME, TT) %>%
  full_join(od_route, by="OD")

# We create the PERIOD variable 
# It is the period in which the trip started and the informed TT during this period
ini_period <- min(infoTT$PERIOD_INI)
fin_period <- max(infoTT$PERIOD_FIN)

interval <- (infoTT$PERIOD_FIN - infoTT$PERIOD_INI)[1]

infoTT$PERIOD <- getPeriod(infoTT$PERIOD_INI, ini_time = ini_period, fin_time = fin_period, interval)
choices$PERIOD <- getPeriod(choices$DEP_TIME, ini_time = ini_period, fin_time = fin_period, interval)

trips_res <- 
  trips_res %>%
  mutate(DEP_TIME = DEP_TIME + ini_period, ARR_TIME = ARR_TIME + ini_period) 

trips_res$PERIOD <- getPeriod(trips_res$DEP_TIME, ini_time = ini_period, fin_time = fin_period, interval)

############ Computes the mean travel time in each route period
mean_tt <- trips_res %>%
  group_by(SESSION_ID, OD, PATH_NAME, PERIOD) %>%
  summarise(MEAN_TT = mean(TT))


############ Join the variables
choices <- 
  choices %>%
  left_join(infoTT[,c("ROUTE", "TT_INFO", "DEMAND", "PERIOD")], 
            by=c("DEMAND"="DEMAND", "PATH_NAME.y"="ROUTE", "PERIOD"="PERIOD")) %>%
  left_join(mean_tt[,c("SESSION_ID", "PATH_NAME", "PERIOD", "MEAN_TT")], 
            by=c("SESSION_ID"="SESSION_ID", "PATH_NAME.y"="PATH_NAME", "PERIOD"="PERIOD"))

# The MEAN_TT NA's may be explained because the trips that were not finished were removed
na_ids <- unique(choices$CHOICE_ID[is.na(choices$MEAN_TT)])
choices <- choices[!choices$CHOICE_ID %in% na_ids,]


# Obtains chosen route
choices <- choices %>% 
  mutate(CHOSEN=ifelse(PATH_NAME.x == PATH_NAME.y, TRUE,FALSE)) %>%
  select(-PATH_NAME.x) %>%
  rename(PATH_NAME=PATH_NAME.y)


# Order choices by fastest 
choices <- choices %>% 
  arrange(CHOICE_ID, TT_INFO) %>%
  mutate(order_info_tt = rep(1:3, length(choices$CHOICE_ID) / 3))

# check
tail(choices)

choices <- choices %>% 
  arrange(CHOICE_ID, MEAN_TT) %>%
  mutate(order_tt = rep(1:3, length(choices$CHOICE_ID) / 3))



########### extra
choices$PATH_NAME_SHORT <- unlist(lapply(strsplit(choices$PATH_NAME, "_"), function(x) x[2]))




####################################################################################
# Minimization rate
####################################################################################
# By Path
choices %>% 
  group_by(OD, PATH_NAME, TREATMENT, order_tt) %>%
  summarise(n=n(), minimization_n=sum(CHOSEN)) %>%
  mutate(minimization_rate=minimization_n/n) %>%
  filter(order_tt==1)


# By OD
choices %>% 
  group_by(OD, TREATMENT, order_tt) %>%
  summarise(n=n(), minimization_n=sum(CHOSEN)) %>%
  mutate(minimization_rate=minimization_n/n) %>%
  filter(order_tt==1)

# Overall
choices %>% 
  group_by(TREATMENT, order_tt) %>%
  summarise(n=n(), minimization_n=sum(CHOSEN)) %>%
  mutate(minimization_rate=minimization_n/n) %>%
  filter(order_tt==1)


####################################################################################
# Differences in TT
####################################################################################

fastest_rest <- 
  choices %>%
  filter(order_tt==1) %>%
  select(OD, PATH_NAME, order_tt, SESSION_ID, DEMAND, PERIOD, MEAN_TT) %>%
  distinct(OD, PATH_NAME, order_tt, SESSION_ID, DEMAND, PERIOD, MEAN_TT) %>%
  full_join(
    choices %>%
      select(OD, PATH_NAME, order_tt, SESSION_ID, DEMAND, PERIOD, MEAN_TT) %>%
      distinct(OD,PATH_NAME, order_tt, SESSION_ID, DEMAND, PERIOD, MEAN_TT),
    by=c("OD", "SESSION_ID", "DEMAND", "PERIOD")
  )

fastest_rest <- 
  fastest_rest %>%
  mutate(tt_diff = MEAN_TT.y - MEAN_TT.x) %>%
  filter(order_tt.y != 1)

fastest_rest %>%
  mutate(diff_type = ifelse(order_tt.y == 2, "second fastest", "slow")) %>%
  add_row(OD="O2D1", tt_diff=0, diff_type="second fastest") %>%
  ggplot() +
  geom_density(aes(tt_diff, fill=OD), alpha=.5) +
  facet_grid(diff_type ~.) +  
  ylim(0,0.0075) +
  xlim(0,740) +
  xlab("difference in travel time (seconds)") +
  #ggtitle("Compliance by route") +
  theme_bw() 

ggsave("./plots/tt_diff.png",  width = 16, height = 10, units = "cm", dpi = 300, limitsize = TRUE)

fastest_rest %>%
  group_by(OD, order_info_tt.y) %>%
  summarise(mean(info_tt_diff))






