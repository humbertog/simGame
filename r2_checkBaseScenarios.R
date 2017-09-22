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

# 
trips_res_base_1 <- trips_res_base

DIR_TRIP_SET_BASE   <- "/Users/humberto.gonzalez/simulation_game/exp_20170412/data_sg/data_SGTrips/SG_S649_BASES631"
FILE_BASE_SCENARIO  <- "SG_S649res_proc.csv"
source("r1_readTrips_base.R")

trips_res_base_2 <- trips_res_base


# Select the wanted columns and cases
trips_res_base_1 <-
  trips_res_base_1 %>%
  select(OD, PATH_NAME, DEP_TIME, ARR_TIME, TT, PATH_REROUTE) %>%
  mutate(DB_ID = "base1")

trips_res_base_2 <-
  trips_res_base_2 %>%
  select(OD, PATH_NAME, DEP_TIME, ARR_TIME, TT, PATH_REROUTE) %>%
  mutate(DB_ID = "base2")


trips_res_base_12 <- bind_rows(trips_res_base_1, trips_res_base_2)


trips_res_base_12 %>%
  ggplot() +
  geom_bar(aes(OD,colour=DB_ID, fill=DB_ID), position="dodge")

trips_res_base_12 %>%
  ggplot() +
  geom_bar(aes(PATH_NAME,colour=DB_ID, fill=DB_ID), position="dodge")

trips_res_base_12 %>%
  ggplot() +
  geom_boxplot(aes(PATH_NAME, TT, colour=DB_ID), position="dodge")
  



