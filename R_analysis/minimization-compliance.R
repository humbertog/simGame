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



########### extra
choices$PATH_NAME_SHORT <- unlist(lapply(strsplit(choices$PATH_NAME, "_"), function(x) x[2]))



####################################################################################
# Compliance rate
####################################################################################
#### 
choices %>% 
  filter(TREATMENT == "t3") %>% 
  select(OD) %>%
  table() / 3

### Joint probabilities (all the compliance values can be obtained from here):
chosen_fastest <- 
  choices %>% 
  distinct(SESSION_ID,  PLAYER_ID, CHOICE_ID, DEMAND, TREATMENT, PERIOD, OD) %>%
  mutate(chosen_path = NA, fastest_informed_path=NA, fastest_path=NA)
  
  
for(id in chosen_fastest$CHOICE_ID) {
  chosen_fastest$chosen_path[chosen_fastest$CHOICE_ID == id] <- choices$PATH_NAME[choices$CHOICE_ID == id & choices$CHOSEN == TRUE]
  chosen_fastest$fastest_informed_path[chosen_fastest$CHOICE_ID == id] <- choices$PATH_NAME[choices$CHOICE_ID == id & choices$order_info_tt == 1]
  chosen_fastest$fastest_path[chosen_fastest$CHOICE_ID == id] <- choices$PATH_NAME[choices$CHOICE_ID == id & choices$order_tt == 1]
}

# this table has the number of times that a route was chosen, 
# the number of times a route was informed fastest,
# the joint distribution
joint_chosen_fastest <- 
  chosen_fastest %>%
    filter(TREATMENT == "t3") %>%
    count(OD, chosen_path, fastest_informed_path) %>%
    group_by(OD, chosen_path) %>%
    mutate(n_chosen=sum(n)) %>%
    group_by(OD, fastest_informed_path) %>%
    mutate(n_fastest = sum(n)) %>%
    group_by(OD) %>%
    mutate(n_tot = sum(n)) %>%
    mutate(prob_joint = n / n_tot,
           prob_choice = n_chosen / n_tot,
           prob_fastest = n_fastest / n_tot
           ) 

# compliance route
compliance_route <- 
  joint_chosen_fastest %>%
    filter(chosen_path == fastest_informed_path) %>%
    mutate(compliance_route = prob_joint / prob_fastest) %>%
    group_by() %>%
    select(OD, chosen_path, compliance_route)

# compliance od
joint_chosen_fastest %>%
  filter(chosen_path == fastest_informed_path) %>%
  group_by(OD) %>%
  summarise(compliance_od = sum(prob_joint))

# % fastest informed plots 
joint_chosen_fastest %>%
  distinct(OD, fastest_informed_path, prob_fastest) %>%
  group_by() %>%
  add_row(OD=c("O1D1", "O3D2", "O3D2"), 
          fastest_informed_path=c("O1D1_south", "O3D2_center", "O3D2_south"),
          prob_fastest = c(0,0,0)
          ) %>%
  ggplot() +
  geom_col(aes(fastest_informed_path, prob_fastest, fill=OD)) +
  xlab("route name") +
  ylab("% fastest informed") +
#  ggtitle("% informed as fastest") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 


# ggsave("./plots/perc_fastest_route.png",  width = 12, height = 10, units = "cm", dpi = 300, limitsize = TRUE)

# % chosen
joint_chosen_fastest %>%
  distinct(OD, chosen_path, prob_choice) %>%
  group_by() %>%
  ggplot() +
  geom_col(aes(chosen_path, prob_choice, fill=OD)) +
  xlab("route name") +
  ylab("% choices") +
#  ggtitle("% choices") +
  ylim(0,1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

# ggsave("./plots/perc_chosen_route.png",  width = 12, height = 10, units = "cm", dpi = 300, limitsize = TRUE)


# compliance
compliance_route_temp <- 
compliance_route %>%
  add_row(OD=c("O1D1", "O3D2", "O3D2", "O2D1"), 
          chosen_path=c("O1D1_south", "O3D2_center", "O3D2_south", "O2D1_south"),
          compliance_route = c(0,0,0, 0)
  )

compliance_route_temp2 <- 
  compliance_route_temp %>%
  filter(chosen_path%in%c("O1D1_south", "O3D2_center", "O3D2_south")) %>%
  mutate(compliance_route =1)

ggplot() +
  geom_col(data=compliance_route_temp,aes(chosen_path, compliance_route, fill=OD)) +
  geom_col(data=compliance_route_temp2,aes(chosen_path, compliance_route, fill="NA"), fill="black", alpha=.35, width=.35) + 
  xlab("route name") +
  ylab("compliance") +
  #ggtitle("Compliance by route") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

# ggsave("./plots/compliance_route.png",  width = 12, height = 10, units = "cm", dpi = 300, limitsize = TRUE)
  

####################################################################################
# Differences in informed TT
####################################################################################

fastest_rest <- 
  choices %>%
    filter(order_info_tt==1) %>%
    select(OD, PATH_NAME, order_info_tt, SESSION_ID, DEMAND, PERIOD, TT_INFO) %>%
    distinct(OD, PATH_NAME, order_info_tt, SESSION_ID, DEMAND, PERIOD, TT_INFO) %>%
    full_join(
      choices %>%
        select(OD, PATH_NAME, order_info_tt, SESSION_ID, DEMAND, PERIOD, TT_INFO) %>%
        distinct(OD,PATH_NAME, order_info_tt, SESSION_ID, DEMAND, PERIOD, TT_INFO),
      by=c("OD", "SESSION_ID", "DEMAND", "PERIOD")
    )

fastest_rest <- 
  fastest_rest %>%
    mutate(info_tt_diff = TT_INFO.y - TT_INFO.x) %>%
  filter(order_info_tt.y != 1)

fastest_rest %>%
  mutate(diff_type = ifelse(order_info_tt.y == 2, "second fastest", "slow")) %>%
  ggplot() +
  geom_density(aes(info_tt_diff, fill=OD), alpha=.5) +
  facet_grid(diff_type ~.) +
  xlab("difference in informed travel time (seconds)") +
  ylab("density") +
  #ggtitle("Compliance by route") +
  theme_bw() 

#ggsave("./plots/informed_tt_diff.png",  width = 16, height = 10, units = "cm", dpi = 300, limitsize = TRUE)

fastest_rest %>%
  group_by(OD, order_info_tt.y) %>%
  summarise(mean(info_tt_diff))

####################################################################################
# See compliance by player
####################################################################################

# We count the proportion of the decisions that players were complaint
compliance_rate_player <- 
  choices %>% 
  filter(TREATMENT == 't3', CHOSEN == TRUE) %>%
  group_by(PLAYER_ID) %>%
  summarise(n=n(), compliance_n=sum(order_info_tt == 1)) %>%
  mutate(compliance_rate=compliance_n/n)
  
  
compliance_rate_player %>% 
  ggplot() +
  geom_histogram(aes(compliance_rate), binwidth=.2, center=.1) +
  xlab("compliance rate") +
  ylab("count") +
  theme_bw() 

# ggsave("./plots/compliance_player.png",  width = 16, height = 10, units = "cm", dpi = 300, limitsize = TRUE)

  
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
# Information error
############################
# many NA's
choices %>%
  distinct(SESSION_ID, OD, PATH_NAME, PATH_NAME_SHORT, PERIOD, MEAN_TT, TT_INFO) %>%
  mutate(ROUTE = PATH_NAME_SHORT) %>%
  ggplot(aes(MEAN_TT, TT_INFO, colour=ROUTE)) +
  geom_point() +
  #geom_smooth(method="lm", se=FALSE) +
  geom_abline(slope=1, intercept = 0) +
  facet_grid(.~OD, scale="free") +
  xlab("incurred travel time ") +
  ylab("informed travel time") +
#  ggtitle("Informed vs mean incurred travel time ") +
  theme_bw() 


# ggsave("./plots/informed-incurred_TT.png",  width = 27, height = 10, units = "cm", dpi = 300, limitsize = TRUE)


choices %>%
  distinct(SESSION_ID, OD, PATH_NAME, PERIOD, MEAN_TT, TT_INFO) %>%
  group_by(OD) %>%
  summarise(mean_tt = mean(MEAN_TT),
            RMSE = sqrt(mean( (TT_INFO-MEAN_TT)^2 ))) %>%
  mutate(RMSE / mean_tt)


choices %>%
  distinct(SESSION_ID, OD, PATH_NAME, PERIOD, MEAN_TT, TT_INFO) %>%
  group_by(OD) %>%
  summarise(mean_tt = mean(MEAN_TT),
            RMSE = sqrt(mean( (TT_INFO-MEAN_TT)^2 ))) %>%
  mutate(RMSE / mean_tt)


accuracy_route <-
  choices %>%
    distinct(SESSION_ID, OD, PATH_NAME, PERIOD, MEAN_TT, TT_INFO) %>%
    group_by(OD, PATH_NAME) %>%
    summarise(mean_tt = mean(MEAN_TT),
              RMSE = sqrt(mean( (TT_INFO-MEAN_TT)^2 ))) %>%
    mutate(IE=RMSE / mean_tt)


accuracy_route %>% 
  left_join(compliance_route, by=c("PATH_NAME"="chosen_path")) %>%
  group_by() %>%
  mutate(OD=OD.x) %>%
  ggplot(aes(IE, compliance_route)) +
  geom_point(aes(colour=OD)) +
  geom_smooth(method="lm", se=FALSE, colour= "black") +
  xlab("information error ") +
  ylab("compliance rate") +
#  ggtitle("Information error vs complaince") +
  theme_bw() 

# ggsave("./plots/IE-compliance.png",  width = 16, height = 10, units = "cm", dpi = 300, limitsize = TRUE)


### Now we see the order
order_mean_info <- 
  choices %>%
  distinct(SESSION_ID, OD, PATH_NAME, PERIOD, MEAN_TT, TT_INFO, order_info_tt, order_tt) 

(order_tt <- table(order_mean_info$order_tt, order_mean_info$order_info_tt, order_mean_info$OD) )

sum(diag(order_tt[, ,1])) /sum(order_tt[, ,1])
sum(diag(order_tt[, ,2])) /sum(order_tt[, ,2])
sum(diag(order_tt[, ,3])) /sum(order_tt[, ,3])

###
choices %>% 
  distinct(CHOICE_ID, OD) %>%
  group_by(OD)  %>%
  summarise(n())

choices %>%
  group_by(OD, order_info_tt, order_tt) %>%
  summarise(n=n_distinct(CHOICE_ID)) %>%
  filter(order_info_tt==1) %>%
  group_by(OD) %>%
  mutate(n_tot=sum(n), n/n_tot)

choices %>%
  group_by(OD, order_info_tt, order_tt) %>%
  summarise(n=n_distinct(CHOICE_ID)) %>%
  filter(order_info_tt==1) %>%
  group_by(OD) %>%
  mutate(n_tot=sum(n), n/n_tot)



####################################################################################
# Minimization rate
####################################################################################
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
  geom_col(aes(as.factor(order_tt), minimization_rate_od, fill=OD), position="dodge") +
  xlab("route rank (1=fastest, 2=second fastest, 3=slow)") +
  ylab("minimization rate") +
  theme_bw() 

