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

minimization_rate_od %>%
  ggplot() +
  geom_col(aes(as.factor(order_tt), minimization_rate_od, fill=OD), position="dodge")

############################
# Compliance rate
############################
#### 
# ROUTE
compliance_rate <- 
  choices %>% 
  filter(TREATMENT == 't3') %>%
  group_by(OD, PATH_NAME,order_info_tt) %>%
  summarise(n=n(), compliance_n=sum(CHOSEN)) %>%
  mutate(compliance_rate=compliance_n/n)


compliance_rate %>%
  filter(order_info_tt == 1)

#OD
compliance_rate_od <- 
  compliance_rate %>%
  group_by(OD,order_info_tt) %>%
  summarise(n_od=sum(n), compliance_n_od=sum(compliance_n)) %>%
  mutate(compliance_rate_od = compliance_n_od / n_od)

# chisq tests
compliance_rate_od %>%
  filter(OD!="O3D2") %>%
  select(order_info_tt, compliance_n_od) %>%
  spread(order_info_tt, compliance_n_od) %>%
  group_by() %>%
  select(-OD) %>%
  chisq.test(simulate.p.value = FALSE)
  
compliance_rate_od %>%
  filter(OD!="O2D1") %>%
  select(order_info_tt, compliance_n_od) %>%
  spread(order_info_tt, compliance_n_od) %>%
  group_by() %>%
  select(-OD) %>%
  chisq.test(simulate.p.value = FALSE)

compliance_rate_od %>%
  filter(OD!="O1D1") %>%
  select(order_info_tt, compliance_n_od) %>%
  spread(order_info_tt, compliance_n_od) %>%
  group_by() %>%
  select(-OD) %>%
  chisq.test(simulate.p.value = FALSE)

# overall
compliance_rate_overall <- 
compliance_rate %>%
  group_by(order_info_tt) %>%
  summarise(n_all=sum(n), compliance_n_all=sum(compliance_n)) %>%
  mutate(compliance_rate_all = compliance_n_all / n_all)

ggplot() +
  geom_col(data=compliance_rate_od, aes(as.factor(order_info_tt), compliance_rate_od, fill=OD), position="dodge") +
  geom_col(data=as.data.frame(compliance_rate_overall), aes(as.factor(order_info_tt), compliance_rate_all), alpha=0, colour="black") +
  xlab("route rank (1=fastest, 2=second fastest, 3=slow)") +
  ylab("compliance rate") +
  theme_bw() 

# interpretacion: de n veces que la ruta j fue la de minimo tiempo informado,
# m veces se escogio

### See compliance by player
# We count the proportion of the decisions that players were complaint
compliance_rate_player <- 
  choices %>% 
  filter(TREATMENT == 't3', CHOSEN==TRUE) %>%
  group_by(PLAYER_ID) %>%
  summarise(n=n(), compliance_n=sum(order_info_tt ==1)) %>%
  mutate(compliance_rate=compliance_n/n)
  
  
compliance_rate_player %>% 
  ggplot() +
  geom_histogram(aes(compliance_rate), binwidth=.2, center=.1) +
  xlab("compliance rate") +
  ylab("count") +
  theme_bw() 
  
# count number of players which comply and dont:   x > 0.5 and x < 0.5
sum(compliance_rate_player$compliance_rate >= 0.6)
sum(compliance_rate_player$compliance_rate <= 0.4)
sum(compliance_rate_player$compliance_rate > 0.4 & compliance_rate_player$compliance_rate < 0.6) 

# Number of different decision problems
choices %>% 
  filter(TREATMENT == 't3', CHOSEN==TRUE) %>%
  group_by(PLAYER_ID) %>%
  summarise(Unique_Elements = n_distinct(OD)) %>%
  group_by(Unique_Elements) %>%
  summarise(n=n())


############################
# Minimum time coincides with minimum informed travel time
############################
# many NA's
choices %>%
  distinct(SESSION_ID, OD, PATH_NAME, PERIOD, MEAN_TT, TT_INFO) %>%
  ggplot(aes(MEAN_TT, TT_INFO, colour=OD)) +
  geom_point() +
  #geom_smooth(method="lm", se=FALSE) +
  geom_abline(slope=1, intercept = 0) +
  xlab("incurred travel time ") +
  ylab("informed travel time") +
  theme_bw() 


choices %>%
  distinct(SESSION_ID, OD, PATH_NAME, PERIOD, MEAN_TT, TT_INFO) %>%
  group_by(OD) %>%
  summarise(mean_tt = mean(MEAN_TT),
            RMSE = sqrt(mean( (TT_INFO-MEAN_TT)^2 ))) %>%
  mutate(RMSE / mean_tt)

### Now we see the order
order_mean_info <- 
  choices %>%
    distinct(SESSION_ID, OD, PATH_NAME, PERIOD, MEAN_TT, TT_INFO, order_info_tt, order_tt) 

order_tt <- table(order_mean_info$order_tt, order_mean_info$order_info_tt, order_mean_info$OD) 

sum(diag(order_tt[, ,1])) /sum(order_tt[, ,1])
sum(diag(order_tt[, ,2])) /sum(order_tt[, ,2])
sum(diag(order_tt[, ,3])) /sum(order_tt[, ,3])

choices %>%
  group_by(OD, order_info_tt, order_tt) %>%
  summarise(n=n_distinct(CHOICE_ID)) %>%
  filter(order_info_tt==1) %>%
  group_by(OD) %>%
  mutate(n_tot=sum(n), n/n_tot)
  


choices %>%
  group_by(OD, order_tt, order_info_tt) %>%
  summarise(n=n_distinct(CHOICE_ID)) %>%
  filter(order_info_tt==1) %>%
  group_by(OD) %>%
  mutate(n_tot=sum(n), n/n_tot)

  
  
  
  
  
  
  
  



choices %>%
  filter(order_tt ==1) %>%
  group_by(OD, order_tt, order_info_tt) %>%
  summarise(n=n()) %>%
  group_by(OD) %>%
  mutate(n_tot=sum(n)) %>%
  mutate(perc=n/n_tot)


# TODO: COUNT THE NUMBER OF TIMES THAT THE FASTEST ROUTE WAS CHOSEN WHEN IT WAS INFORMED TO BE THE FASTEST

choices %>%
  filter(TREATMENT == 't3') %>% 
  group_by(order_tt, order_info_tt) %>%
  summarise(n=sum(CHOSEN)) %>%
  group_by() %>%
  mutate(n_tot=sum(n)) %>%
  mutate(perc=n/n_tot)


