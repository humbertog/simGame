
#################################################
# Read data
#################################################
# Reads the files of the 04/2017 experiment
FILE_STATS          <- "stat.csv"
FILE_TRIPS_PLAYER   <- "trips_user.csv"

DIR_TRIP_SET        <- "/Users/humberto.gonzalez/simulation_game/exp_20170412/data_sg/data_tripSet"
SESSION_IDS <- c(625, 626, 628, 629, 630, 631, 633, 634)

source("r1_readTrips_res.R")
source("r1_readTrips_player.R")

trips_play_04 <- trips_play
trips_res_04 <- trips_res

trips_play_04 <- trips_play_04 %>% mutate(exp_n = "04_2017")
trips_res_04 <- trips_res_04 %>% mutate(exp_n = "04_2017")

# Reads the files of the 06/2017 experiment
DIR_TRIP_SET <- "/Users/humberto.gonzalez/simulation_game/exp_20170620/data_sg/data_tripSet"
SESSION_IDS <- c(646)

source("r1_readTrips_res.R")
source("r1_readTrips_player.R")

trips_play_06 <- trips_play
trips_res_06 <- trips_res

trips_play_06 <- trips_play_06 %>% mutate(exp_n = "06_2017")
trips_res_06 <- trips_res_06 %>% mutate(exp_n = "06_2017")

# Joins the two data sets
trips_play <- bind_rows(trips_play_04, trips_play_06)
trips_res <- bind_rows(trips_res_04, trips_res_06)

dim(trips_play)
dim(trips_res)

### Filter out the treatment t2
trips_play <- trips_play %>% filter(TREATMENT != "t2", OD != "OD1_2")
trips_res <- trips_res %>% filter(OD != "OD1_2")

# No paths
trips_play <- trips_play %>% select(-PATH)
trips_res <- trips_res %>% select(-PATH)

# Some errors in the path naming, probably due to the fact that they include many crossings
trips_res <- trips_res %>% filter(PATH_NAME != "R_test2")
  
  
  
#################################################
# Here the analysis starts
#################################################
##### Choice distributions
# All cases
trips_play %>% 
  mutate(TREATMENT=ifelse(TREATMENT=="t1", "Treatment 1:NI", ifelse(TREATMENT=="t2", "Treatment 2:CMI", "Treatment 3:TTI"))) %>%
  ggplot(aes(PATH_NAME_INI)) + 
  geom_bar(aes(colour=OD, fill=OD)) + 
  facet_grid(. ~ TREATMENT) +
  theme_bw() +
  labs(x="route name", y="count", fill = "OD", colour="OD") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

# By experiment
trips_play %>% 
  mutate(TREATMENT=ifelse(TREATMENT=="t1", "Treatment 1:NI", ifelse(TREATMENT=="t2", "Treatment 2:CMI", "Treatment 3:TTI"))) %>%
  ggplot(aes(PATH_NAME_INI)) + 
  geom_bar(aes(colour=OD, fill=OD)) + 
  facet_grid(exp_n ~ TREATMENT) +
  theme_bw() +
  labs(x="route name", y="count", fill = "OD", colour="OD") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 


trips_play %>% 
  mutate(TREATMENT=ifelse(TREATMENT=="t1", "Treatment 1:NI", ifelse(TREATMENT=="t2", "Treatment 2:CMI", "Treatment 3:TTI"))) %>%
  ggplot(aes(PATH_NAME_INI)) + 
  geom_bar(aes(colour=exp_n, fill=exp_n), position="dodge") + 
  facet_grid(TREATMENT ~ OD, scales="free_x") +
  theme_bw() +
  labs(x="route name", y="count", fill = "OD", colour="OD") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

# relative frequency
rel_freq  <- 
  trips_play %>% 
    select(exp_n,OD, TREATMENT, PATH_NAME_INI) %>%
    group_by(exp_n, TREATMENT, OD) %>%
    mutate(n_group=n()) %>%
    group_by(exp_n, TREATMENT, OD, n_group, PATH_NAME_INI) %>%
    summarise(n = n()) %>%
    mutate(rel_freq = n / n_group) %>%
    ungroup()

  
rel_freq %>% 
  mutate(TREATMENT=ifelse(TREATMENT=="t1", "Treatment 1:NI", ifelse(TREATMENT=="t2", "Treatment 2:CMI", "Treatment 3:TTI"))) %>%
  ggplot() + 
  geom_col(aes(x=PATH_NAME_INI, y=rel_freq, colour=exp_n, fill=exp_n), position="dodge", alpha=.3) + 
  facet_grid(TREATMENT ~ OD, scales="free_x") +
  theme_bw() +
  labs(x="route name", y="count", fill = "OD", colour="OD") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
  

### Hypothesis test: are the distributions the same?
choice_counts_t1 <- table(trips_play$exp_n[trips_play$TREATMENT == "t1"], trips_play$PATH_NAME_INI[trips_play$TREATMENT == "t1"])

chisq.test(choice_counts_t1[,4:6], simulate.p.value = FALSE) # reject p-value 0.05629
chisq.test(choice_counts_t1[,1:3], simulate.p.value = FALSE) # reject p-value 0.02065
# they dont appear to be the same 

choice_counts_t3 <- table(trips_play$exp_n[trips_play$TREATMENT == "t3"], trips_play$PATH_NAME_INI[trips_play$TREATMENT == "t3"])

chisq.test(choice_counts_t3[,4:6], simulate.p.value = FALSE) # strong reject p-value 0.006903
chisq.test(choice_counts_t3[,1:3], simulate.p.value = FALSE) # DONT reject p-value 0.5994


##### Travel time distributions
trips_res %>% 
  filter(!PATH_REROUTE) %>%
  ggplot() +
  geom_density(aes(TT, fill=PATH_NAME), alpha=.3) +
  facet_grid(exp_n ~ OD) +
  theme_bw() +
  labs(x="travel time", y="density", fill = "Route name")


trips_res %>% 
  filter(!PATH_REROUTE) %>%
  ggplot() +
  geom_boxplot(aes(PATH_NAME, TT, fill=exp_n), alpha=.3) +
  facet_grid(. ~ OD, scales="free_x") +
  theme_bw() +
  labs(x="travel time", y="density", fill = "Route name")


#################################################
# Maximization rate

od_path <- data.frame(
  OD =c("OD1_1", "OD1_1", "OD1_1", 
        "OD1_2", "OD1_2", "OD1_2", 
        "OD2", "OD2", "OD2"),
  PATH_NAME =c("R_test1", "R_test1_2", "R_test1_3",
               "R_test2", "R_test2_2", "R_test2_3",
               "R_N1", "R_N2", "R_N3"
  ), stringsAsFactors = FALSE
)

###
trips_play_choice <- 
  trips_play %>% 
  #filter(SESSION_ID != 629) %>%
  mutate(ALT1 = NA, ALT2=NA, ALT3=NA, ALT1N=NA, ALT2N=NA, ALT3N=NA)


h <- 900
for (i in 1:dim(trips_play_choice)[1]) {
  t <- trips_play_choice$DEP_TIME[i]
  db_ <- trips_play_choice$SESSION_ID[i]
  od_ <- trips_play_choice$OD[i]
  tt <- trips_play_choice$TT[i]
  
  OD_df_ <- 
    od_path %>%
    filter(OD == od_) %>%
    mutate(PCUM =NA, MEAN=NA)
  
  for (k in  1:3) {
    q_ <- 
      trips_res %>%
      filter(SESSION_ID==db_, 
             PATH_NAME==OD_df_$PATH_NAME[k],
             DEP_TIME >= t - h, 
             DEP_TIME < t + 0
      ) %>%
      select(PATH_NAME, TT)
    
    if (length(q_$TT) <=5) {
      print(i)
    }
    #OD_df_$PCUM[k] <- ecdf(q_$TT)(tt)
    OD_df_$MEAN[k] <- mean(q_$TT)
    
  }
  
  # Order the cummulative probs in ascendent order!
  OD_df_ <- OD_df_ %>% arrange(MEAN)
  
  # 
  trips_play_choice$ALT1N[i] <- OD_df_$PATH_NAME[1]
  trips_play_choice$ALT2N[i] <- OD_df_$PATH_NAME[2] 
  trips_play_choice$ALT3N[i] <- OD_df_$PATH_NAME[3]
  trips_play_choice$ALT1[i] <- OD_df_$MEAN[1] 
  trips_play_choice$ALT2[i] <- OD_df_$MEAN[2] 
  trips_play_choice$ALT3[i] <- OD_df_$MEAN[3]
}

# Times that each route was the fastest
table(trips_play_choice$exp_n, trips_play_choice$ALT1N)

# Number of times that the fastes route was taken
trips_play_choice %>% 
  filter(exp_n == "04_2017") %>%
  mutate(MAXIM1 = PATH_NAME_INI == ALT1N,
         MAXIM2 = PATH_NAME_INI == ALT2N,
         MAXIM3 = PATH_NAME_INI == ALT3N) %>%
  group_by(OD, TREATMENT) %>% 
  summarise(NMAXIM1 = sum(MAXIM1),
            NMAXIM2 = sum(MAXIM2),
            NMAXIM3 = sum(MAXIM3),
            NTOT=n()) %>%
  mutate(PERCMAXIM1 = round(NMAXIM1 / NTOT,3),
         PERCMAXIM2 = round(NMAXIM2 / NTOT,3),
         PERCMAXIM3 = round(NMAXIM3 / NTOT,3)
  )

trips_play_choice %>% 
  filter(exp_n == "06_2017") %>%
  mutate(MAXIM1 = PATH_NAME_INI == ALT1N,
         MAXIM2 = PATH_NAME_INI == ALT2N,
         MAXIM3 = PATH_NAME_INI == ALT3N) %>%
  group_by(OD, TREATMENT) %>% 
  summarise(NMAXIM1 = sum(MAXIM1),
            NMAXIM2 = sum(MAXIM2),
            NMAXIM3 = sum(MAXIM3),
            NTOT=n()) %>%
  mutate(PERCMAXIM1 = round(NMAXIM1 / NTOT,3),
         PERCMAXIM2 = round(NMAXIM2 / NTOT,3),
         PERCMAXIM3 = round(NMAXIM3 / NTOT,3)
  )

#################################################
# Distribution by player

# Numer of decisions by player

trips_play_choice %>% 
    group_by(PLAYER_ID, OD) %>%
    summarise(COUNT=n()) %>%
    ggplot(aes(COUNT)) +
    geom_bar() + 
    facet_grid(.~  OD)


trips_play_choice %>% 
  group_by(PLAYER_ID, OD, PATH_NAME_INI) %>%
  summarise(COUNT = n()) %>%
  group_by(PLAYER_ID, OD) %>%
  summarise(DIFF_CHOICES = n()) %>%
  ggplot() + 
  geom_bar(aes(DIFF_CHOICES)) +
  facet_grid(. ~ OD)
  
  


