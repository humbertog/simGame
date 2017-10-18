library(tidyverse)

source("r0_config.R")
source("R_readData/readTrips_player.R")
#source("R_readData/readTrips_res.R")
source("R_readData/readTTInfo.R")
source("R_functions/fun_rename.R")

# Reads the alternative attributes
alternative_attributes <- read_csv("R_data/alternative_attributes.csv")


# Drop some of the columns to avoid confussion
trips_play$PATH_NAME <- trips_play$PATH_NAME_INI
trips_play <- trips_play %>% 
  select(-DEP_TIME_F_INI, -ARR_TIME_F_INI, -DEP_TIME_INI, -ARR_TIME_INI, -PATH_NAME_INI, -PLAYER_ID, -PATH_REROUTE)

alternative_attributes <- alternative_attributes %>%
  select(-ncross)


# We create the PERIOD variable 
# It is the period in which the trip started and the informed TT during this period
ini_period <- min(infoTT$PERIOD_INI)
fin_period <- max(infoTT$PERIOD_FIN)

infoTT$PERIOD <- getPeriod(infoTT$PERIOD_INI, ini_time = ini_period, fin_time = fin_period, 600)
trips_play$PERIOD <- getPeriod(trips_play$DEP_TIME, ini_time = ini_period, fin_time = fin_period, 600)


# Join the variables
trips_play <- 
  trips_play %>%
  left_join(infoTT[,c("ROUTE", "TT_INFO", "DEMAND", "PERIOD")], 
            by=c("DEMAND"="DEMAND", "PATH_NAME"="ROUTE", "PERIOD"="PERIOD")) %>%
  left_join(alternative_attributes, by=c("PATH_NAME"="PATH_NAME"))




trips_play$PATH_NAME <- renameRoutes(trips_play$PATH_NAME)
infoTT$ROUTE <- renameRoutes(infoTT$ROUTE)
infoTT$OD <- renameOD(infoTT$OD)
trips_play$OD <- renameOD(trips_play$OD)

##############################################
# Analysis
##############################################

##### The choice distribution
trips_play %>% 
  ggplot() +
  geom_bar(aes(PATH_NAME, fill=OD)) +
  facet_grid(.~TREATMENT) +
  theme_bw() +
  xlab("route") +
  labs(x="route name", y="count", fill = "OD", colour="OD") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

ggsave("choiceDist.png",  width = 20, height = 10, units = "cm",
       dpi = 300, limitsize = TRUE)

# The choice distribution half of experimetn
trips_play %>% 
  mutate(HALF = ifelse(DEP_TIME <25800, 1,2)) %>%
  ggplot() +
  geom_bar(aes(PATH_NAME, fill=OD)) +
  facet_grid(HALF~TREATMENT) +
  theme_bw() +
  xlab("route") +
  labs(x="route name", y="count", fill = "OD", colour="OD") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

ggsave("choiceDist_half.png",  width = 20, height = 10, units = "cm",
       dpi = 300, limitsize = TRUE)

# The informed travel times in each route
# dist
infoTT %>% 
  ggplot() +
  geom_boxplot(aes(ROUTE, TT_INFO, colour=OD)) +
  ylab("informed travel time") +
  xlab("route") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

ggsave("informedTT.png",  width = 20, height = 16, units = "cm",
       dpi = 300, limitsize = TRUE)

# ts
infoTT %>% 
  ggplot() +
  geom_point(aes(PERIOD, TT_INFO, colour=ROUTE, shape=DEMAND)) +
  facet_grid(OD~.) +
  xlab("time period") +
  ylab("informed travel time") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

ggsave("informedTT_period.png",  width = 20, height = 16, units = "cm",
       dpi = 300, limitsize = TRUE)

# Incurred tt
trips_play %>% 
  ggplot() +
  geom_boxplot(aes(PERIOD, TT, colour=PATH_NAME)) +
  facet_grid(OD~.) +
  xlab("time period") +
  ylab("incurred travel time") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

ggsave("incurredTT_period.png",  width = 20, height = 16, units = "cm",
       dpi = 300, limitsize = TRUE)

# The incurred vs the informed travel times
trips_play %>% 
  ggplot() +
  geom_point(aes(TT_INFO, TT))

trips_play %>% 
  ggplot() +
  geom_point(aes(TT_INFO, TT)) +
  geom_abline(intercept = 0, colour="blue") +
  facet_grid(.~PATH_NAME, scales = "free") +
  xlab("informed travel time (seconds)") +
  ylab("incurred travel time (seconds)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
  
ggsave("informedTT-incurredTT_period.png",  width = 20, height = 12, units = "cm",
       dpi = 300, limitsize = TRUE)
