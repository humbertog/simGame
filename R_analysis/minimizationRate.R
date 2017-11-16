#####################################################################
# We study the number of times that the INFORMED minimum and ACTUAL 
# minimum travel time routes were chosen. 
# 1. For each played mission, we take the informed travel time
# of all the three alternatives in a choice problem.
# 2. Then, we sort the alternatives in ascending order 
# 3. Finally we see in which place the chosen alternative is.
#####################################################################
library(tidyverse)

source("r0_config_201704.R")
source("R_readData/readTrips_player.R")
#source("R_readData/readTrips_res.R")
source("R_readData/readTTInfo.R")
source("R_functions/fun_rename.R")

# Drop some of the columns to avoid confussion
trips_play$PATH_NAME <- trips_play$PATH_NAME_INI
trips_play <- trips_play %>% 
  select(-DEP_TIME_F_INI, -ARR_TIME_F_INI, -DEP_TIME_INI, -ARR_TIME_INI, -PATH_NAME_INI, -PLAYER_ID, -PATH_REROUTE)

#alternative_attributes <- alternative_attributes %>%
#  select(-ncross)

# Creates the data base with the forgone choices
trips_play$CHOICE_ID <- 1:length(trips_play$OD)

od_route <- unique(trips_play[,c("OD", "PATH_NAME")])

choices <- 
  trips_play %>% 
  select(SESSION_ID, CHOICE_ID, DEMAND, TREATMENT, OD, PATH_NAME, DEP_TIME, ARR_TIME, TT) %>%
  full_join(od_route, by="OD")



# We create the PERIOD variable 
# It is the period in which the trip started and the informed TT during this period
ini_period <- min(infoTT$PERIOD_INI)
fin_period <- max(infoTT$PERIOD_FIN)

interval <- (infoTT$PERIOD_FIN - infoTT$PERIOD_INI)[1]


infoTT$PERIOD <- getPeriod(infoTT$PERIOD_INI, ini_time = ini_period, fin_time = fin_period, interval)
choices$PERIOD <- getPeriod(choices$DEP_TIME, ini_time = ini_period, fin_time = fin_period, interval)


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


# Formats

infoTT$ROUTE <- renameRoutes(infoTT$ROUTE)
infoTT$OD <- renameOD(infoTT$OD)

choices$PATH_NAME <- renameRoutes(choices$PATH_NAME)
choices$OD <- renameOD(choices$OD)


# Order choices by fastest 

choices <- choices %>% 
  arrange(CHOICE_ID, TT_INFO) %>%
  mutate(order = rep(1:3, length(choices$CHOICE_ID) / 3))

# check
tail(choices)

mean_tt <- 
  choices %>%
  filter(CHOSEN == TRUE) %>%
  select(-CHOSEN,-order) %>% 
  group_by(SESSION_ID, OD, PATH_NAME, PERIOD) %>%
  summarise(mean_tt = mean(TT))

choices <- 
  choices %>%
  left_join(mean_tt, by=c("SESSION_ID","OD","PATH_NAME","PERIOD"))

choices <- choices %>% 
  arrange(CHOICE_ID, mean_tt) %>%
  mutate(order_tt = rep(1:3, length(choices$CHOICE_ID) / 3))


############################
# Minimization rate
############################


minimization_rate <- 
  choices %>% 
  filter(TREATMENT == 't3') %>%
  group_by(OD, PATH_NAME, order_tt) %>%
  summarise(n=n(), minimization_n=sum(CHOSEN)) %>%
  mutate(minimization_rate=minimization_n/n)

minimization_rate_od <- 
  minimization_rate %>%
  group_by(OD,order_tt) %>%
  summarise(n_od=sum(n), minimization_n_od=sum(minimization_n)) %>%
  mutate(minimization_rate_od = minimization_n_od / n_od)


############################
# Compliance rate
############################
#### 
# ROUTE
compliance_rate <- 
  choices %>% 
  filter(TREATMENT == 't3') %>%
  group_by(OD, PATH_NAME,order) %>%
  summarise(n=n(), compliance_n=sum(CHOSEN)) %>%
  mutate(compliance_rate=compliance_n/n)


compliance_rate %>%
  filter(order == 1)

#OD
compliance_rate_od <- 
compliance_rate %>%
  group_by(OD,order) %>%
  summarise(n_od=sum(n), compliance_n_od=sum(compliance_n)) %>%
  mutate(compliance_rate_od = compliance_n_od / n_od)


compliance_rate_od %>%
  ggplot() +
  geom_col(aes(as.factor(order), compliance_rate_od, fill=OD), position="dodge")

  
# overall
compliance_rate %>%
  group_by(order) %>%
  summarise(n_all=sum(n), compliance_n_all=sum(compliance_n)) %>%
  mutate(compliance_rate_all = compliance_n_all / n_all)

# interpretacion: de n veces que la ruta j fue la de minimo tiempo informado,
# m veces se escogio
  
############################
# Minimum time coincides with minimum informed travel time
############################
# many NA's
na_ids <- unique(choices$CHOICE_ID[is.na(choices$mean_tt)])
not_na_choices <- choices[!choices$CHOICE_ID %in% na_ids,]

not_na_choices %>%
  ggplot() +
  geom_point(aes(TT_INFO, mean_tt))

not_na_choices %>%
  filter(order_tt ==1) %>%
  group_by(OD, order_tt, order) %>%
  summarise(n=n()) %>%
  group_by(OD) %>%
  mutate(n_tot=sum(n)) %>%
  mutate(perc=n/n_tot)


# TODO: USE EXTERNAL INFORMATION TO COMPUTE THE ACTUALN TRAVEL TIMES ON EACH ROUTE
# TODO: COUNT THE NUMBER OF TIMES THAT THE FASTEST ROUTE WAS CHOSEN WHEN IT WAS INFORMED TO BE THE FASTEST

not_na_choices %>%
  filter(TREATMENT == 't3') %>% 
  group_by(order_tt, order) %>%
  summarise(n=sum(CHOSEN)) %>%
  group_by() %>%
  mutate(n_tot=sum(n)) %>%
  mutate(perc=n/n_tot)
  




