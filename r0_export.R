#####################################################################
# Export data to use in the LARCH package
#####################################################################
source("r2_mnlogit_pre.R")

# OD and treatment


### OD and treatment
od <- "OD1_1"
treat <- "t3"

data_model <- 
  trips_res_play %>%
  filter(OD==od) %>%
  filter(TREATMENT==treat) 

# Chosen 
data_model$CHOICE <- as.numeric(data_model$CHOICE)
data_model <- data_model %>% 
  mutate(ALT_N=ifelse(PATH_NAME=="R_test1", 1, ifelse(PATH_NAME=="R_test1_2", 2, 3))) %>%
  mutate(NUM_ALTS = 3)


data_model <- data_model %>%
  select(CHOICE_ID, NUM_ALTS, ALT_N, CHOSEN=CHOICE, MEAN_TT, SD_TT, total_len, ncross, ncross_km) %>%
  as.data.frame()



#
write.csv(data_model, file="../exp_20170412/LARCH/choices_OD1_1.csv", row.names = FALSE)

######
od <- "OD1_2"
treat <- "t3"

data_model <- 
  trips_res_play %>%
  filter(OD==od) %>%
  filter(TREATMENT==treat) 

# Chosen 
data_model$CHOICE <- as.numeric(data_model$CHOICE)
data_model <- data_model %>% 
  mutate(ALT_N=ifelse(PATH_NAME=="R_test2", 1, ifelse(PATH_NAME=="R_test2_2", 2, 3))) %>%
  mutate(NUM_ALTS = 3)

data_model <- data_model %>%
  select(CHOICE_ID, NUM_ALTS, ALT_N, CHOSEN=CHOICE, MEAN_TT, SD_TT, total_len, ncross, ncross_km) %>%
  as.data.frame()

#
write.csv(data_model, file="../exp_20170412/LARCH/choices_OD1_2.csv", row.names = FALSE)


######
od <- "OD2"
treat <- "t3"

data_model <- 
  trips_res_play %>%
  filter(OD==od) %>%
  filter(TREATMENT==treat) 

# Chosen 
data_model$CHOICE <- as.numeric(data_model$CHOICE)
data_model <- data_model %>% 
  mutate(ALT_N=ifelse(PATH_NAME=="R_N1", 1, ifelse(PATH_NAME=="R_N2", 2, 3))) %>%
  mutate(NUM_ALTS = 3)

data_model <- data_model %>%
  select(CHOICE_ID, NUM_ALTS, ALT_N, CHOSEN=CHOICE, MEAN_TT, SD_TT, total_len, ncross, ncross_km) %>%
  as.data.frame()

#
write.csv(data_model, file="../exp_20170412/LARCH/choices_OD2.csv", row.names = FALSE)
