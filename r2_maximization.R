#####################################################################
# 
#####################################################################
library(tidyverse)


###  Read the res_files
source("r00_config.R")
source("r0_readTrips_res.R")
source("r0_readTrips_player.R")

######################

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
  filter(SESSION_ID != 629) %>%
  mutate(ALT1 = NA, ALT2=NA, ALT3=NA, ALT1N=NA, ALT2N=NA, ALT3N=NA)


h <- 600


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
             DEP_TIME <= t + h
             ) %>%
      select(PATH_NAME, TT)
    
    if (length(q_$TT) <=5) {
      print(i)
    }
    OD_df_$PCUM[k] <- ecdf(q_$TT)(tt)
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
table(trips_play_choice$ALT1N)

# Number of times that the fastes route was taken

trips_play_choice %>%
  mutate(MAXIM1 = PATH_NAME_INI == ALT1N,
         MAXIM2 = PATH_NAME_INI == ALT2N,
         MAXIM3 = PATH_NAME_INI == ALT3N) %>%
  group_by(OD, TREATMENT) %>% 
  summarise(NMAXIM1 = sum(MAXIM1),
            NMAXIM2 = sum(MAXIM2),
            NMAXIM3 = sum(MAXIM3),
            NTOT=n()) %>%
  mutate(PERCMAXIM1 = NMAXIM1 / NTOT,
         PERCMAXIM2 = NMAXIM2 / NTOT,
         PERCMAXIM3 = NMAXIM3 / NTOT
         )

# For the OD1_1 and the OD2 the share of participants that chose 
# the fastest route is greater for those with travel time information
# (treatment t3). 

trips_play_choice %>% 
  ggplot() +
  geom_point(aes(ALT1, ALT2)) +
  facet_grid( .~OD) 


trips_play_choice %>% 
  ggplot() +
  geom_density(aes(ALT2 - ALT1, fill=OD), alpha=.3) 


# If we plot the difference in mean tt of the fastest and second fastest alternatives
# we can see that the difference of mean tt  in the OD1_1 is larger than
# for the OD1_2, which makes it easier to discriminate













