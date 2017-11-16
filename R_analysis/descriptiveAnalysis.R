library(tidyverse)

source("r0_config_201704.R")
source("R_readData/readTrips_player.R")
#source("R_readData/readTrips_res.R") 
source("R_readData/readTTInfo.R")
source("R_functions/fun_rename.R")

# Reads the alternative attributes
alternative_attributes <- read_csv("R_data/alternative_attributes.csv")
alternative_attributes$PATH_NAME <- renameRoutes(alternative_attributes$PATH_NAME)

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

interval <- (infoTT$PERIOD_FIN - infoTT$PERIOD_INI)[1]

infoTT$PERIOD <- getPeriod(infoTT$PERIOD_INI, ini_time = ini_period, fin_time = fin_period, interval)
trips_play$PERIOD <- getPeriod(trips_play$DEP_TIME, ini_time = ini_period, fin_time = fin_period, interval)


# Join the variables
trips_play <- 
  trips_play %>%
  left_join(infoTT[,c("ROUTE", "TT_INFO", "DEMAND", "PERIOD")], 
            by=c("DEMAND"="DEMAND", "PATH_NAME"="ROUTE", "PERIOD"="PERIOD")) %>%
  left_join(alternative_attributes, by=c("PATH_NAME"="PATH_NAME"))

##############################################
# Analysis
##############################################

##### The choice distribution
choice_frequency <- 
  trips_play %>%
  group_by(TREATMENT, OD,PATH_NAME) %>%
  summarise(n=n()) 

choice_frequency <-   
  choice_frequency %>%
  group_by(TREATMENT, OD) %>%
  mutate(n_od=sum(n)) %>%
  mutate(freq=n / n_od)

choice_frequency %>%
  ggplot() +
  geom_col(aes(PATH_NAME, freq, fill=OD)) +
  facet_grid(.~TREATMENT) +
  theme_bw() +
  xlab("route") +
  labs(x="route name", y="count", fill = "OD", colour="OD") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

ggsave("choiceDist.png",  width = 20, height = 10, units = "cm",
       dpi = 300, limitsize = TRUE)


##### Time choice dist
trips_play %>%
  ggplot() +
  geom_histogram(aes(DEP_TIME_F, fill=TREATMENT), binwidth=300,center=hms(25350)) +
  theme_bw() +
  xlab("departure time")
ggsave("timeChoiceDist.png",  width = 20, height = 10, units = "cm",
       dpi = 300, limitsize = TRUE)

# 
trips_play %>%
  ggplot() +
  geom_histogram(aes(DEP_TIME_F, fill=TREATMENT), binwidth=300,center=hms(25350)) +
  facet_grid(.~DEMAND) +
  theme_bw() +
  xlab("departure time")

ggsave("timeChoiceDist-DEMAND.png",  width = 20, height = 10, units = "cm",
       dpi = 300, limitsize = TRUE)


# The choice distribution half of experiment
half_exp <- median(trips_play$DEP_TIME_F)

choice_frequency_half <- 
  trips_play %>%
  mutate(HALF = ifelse(DEP_TIME < half_exp, 1,2)) %>%
  group_by(TREATMENT, HALF, OD, PATH_NAME) %>%
  summarise(n=n()) 

choice_frequency_half <-   
  choice_frequency_half %>%
  group_by(TREATMENT, HALF, OD) %>%
  mutate(n_od=sum(n)) %>%
  mutate(freq=n / n_od)

choice_frequency_half %>% 
  ggplot() +
  geom_col(aes(PATH_NAME, freq, fill=OD)) +
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
scale_breaks <- unique(as.numeric(infoTT$PERIOD_INI_F))
scale_breaks <- scale_breaks[order(scale_breaks)]

infoTT %>% 
  ggplot() +
  geom_step(aes(as.numeric(PERIOD_INI_F), TT_INFO/60, colour=ROUTE, shape=DEMAND)) +
  geom_point(aes(as.numeric(PERIOD_INI_F), TT_INFO/60, colour=ROUTE, shape=DEMAND)) + 
  facet_grid(OD~DEMAND) +
  xlab("time") +
  ylab("informed travel time (minutes)") +
  theme_bw() +
  scale_x_time(breaks = scale_breaks) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

ggsave("informedTT-DEMAND_period.png",  width = 20, height = 16, units = "cm",
       dpi = 300, limitsize = TRUE)

# Incurred tt
trips_play %>% 
  ggplot() +
  geom_boxplot(aes(PERIOD, TT/60, colour=PATH_NAME)) +
  facet_grid(OD~.) +
  xlab("time") +
  ylab("incurred travel time (minutes)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

ggsave("incurredTT_period.png",  width = 20, height = 16, units = "cm",
       dpi = 300, limitsize = TRUE)

trips_play %>% 
  ggplot() +
  geom_boxplot(aes(PERIOD, TT/60, colour=PATH_NAME)) +
  facet_grid(OD~DEMAND) +
  xlab("time") +
  ylab("incurred travel time (minutes)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

ggsave("incurredTT-DEMAND_period.png",  width = 20, height = 16, units = "cm",
       dpi = 300, limitsize = TRUE)

# Mean travel time per period 
trips_play %>% 
  group_by(DEMAND, OD, PATH_NAME, PERIOD) %>%
  summarise(mean_TT = mean(TT)) %>%
  ggplot() +
  geom_col(aes(PERIOD, mean_TT, fill=PATH_NAME), position="dodge") +
  facet_grid(OD~DEMAND) 



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
