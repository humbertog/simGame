#####################################################################
# We study the number of times that the minimum INFORMED travel time route was chosen. 
# 1. For each played mission, we take the informed travel time
# of all the three alternatives in a choice problem.
# 2. Then, we sort the alternatives in ascending order 
# 3. Finally we see in which place the chosen alternative is.
#####################################################################
library(tidyverse)

source("r0_config.R")
source("R_readData/readTrips_player.R")
source("R_readData/readTrips_res.R")
source("R_readData/readTTInfo.R")

# Drop some of the columns to avoid confussion
trips_play$PATH_NAME <- trips_play$PATH_NAME_INI
trips_play <- trips_play %>% 
  select(-DEP_TIME_F_INI, -ARR_TIME_F_INI, -DEP_TIME_INI, -ARR_TIME_INI, -PATH_NAME_INI, -PLAYER_ID, -PATH_REROUTE)

# Creates the data base with the forgone choices
trips_play$CHOICE_ID <- 1:length(trips_play$OD)

od_route <- unique(trips_play[,c("OD", "PATH_NAME")])

choices <- 
  trips_play %>% 
  select(CHOICE_ID, DEMAND, TREATMENT, OD, PATH_NAME, DEP_TIME, ARR_TIME, TT) %>%
  full_join(od_route, by="OD")



# We create the PERIOD variable 
# It is the period in which the trip started and the informed TT during this period
cut_breaks <- seq(0, max(infoTT$PERIOD_FIN)-min(infoTT$PERIOD_INI) , 600)

infoTT$PERIOD_INI_F <- infoTT$PERIOD_INI - min(infoTT$PERIOD_INI)
infoTT$PERIOD_FIN_F <- infoTT$PERIOD_FIN - min(infoTT$PERIOD_INI)
infoTT$PERIOD <- cut(infoTT$PERIOD_INI_F, cut_breaks, right=FALSE,include.lowest = TRUE)

choices$PERIOD <- cut(choices$DEP_TIME, cut_breaks, right=FALSE,include.lowest = TRUE)


# Join the variables
choices <- 
  choices %>%
  left_join(infoTT[,c("ROUTE", "TT_INFO", "DEMAND", "PERIOD")], 
            by=c("DEMAND"="DEMAND", "PATH_NAME.y"="ROUTE", "PERIOD"="PERIOD"))


# Obtains chosen route
choices <- choices %>% 
  mutate(CHOSEN=ifelse(PATH_NAME.x == PATH_NAME.y, TRUE,FALSE)) %>%
  select(-PATH_NAME.x) %>%
  rename(PATH_NAME=PATH_NAME.y)



# Order choices by fastest 

choices <- choices %>% 
  arrange(CHOICE_ID, TT_INFO) %>%
  mutate(order = rep(1:3, length(choices$CHOICE_ID) / 3))

# check
tail(choices)

############################
# Minimization rate
############################
# 
minimization_rat <- choices %>% 
  group_by(OD, TREATMENT, order) %>%
  summarise(minimization_n=sum(CHOSEN)) %>%
  group_by(OD, TREATMENT) %>%
  mutate(n=sum(minimization_n)) %>%
  mutate(minimization_rate=minimization_n/n)
  
print(minimization_rat, n=100)

# Number of times the route was fastes, second fastest and slowest 
choices %>%
  group_by(OD, PATH_NAME, order) %>%
  summarise(n_min=n()) 








