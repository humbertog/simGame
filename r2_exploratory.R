#####################################################################
# We analyse the route choice distributions along with the observed travel times during the sessions.
# The objectives are:
# 1. Exploratory analysis of the choice distributions
# 2. Test if the choice distributions are different for different treatments
# 3. A GLM is fitted to the proportion of times a route was chosen
#
# The data needed to do this analysis:
# 1. resXXX.csv files (they contain the full set of trips in each OD)
# 2. trips.csv file (they contain the trips that were controlled by the users)
#####################################################################
library(tidyverse)

# Read the res_files
source("r0_config.R")
source("r1_readTrips_res.R")
source("r1_readTrips_player.R")

### Choice distributions
trips_play %>% ggplot(aes(PATH_NAME_INI)) + 
  geom_bar(aes(colour=OD, fill=OD)) + 
  facet_grid(DEMAND ~ TREATMENT) +
  theme_bw() +
  labs(x="route name", y="count", fill = "OD name", colour="OD name") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

#
trips_play %>% 
  mutate(TREATMENT=ifelse(TREATMENT=="t1", "Treatment 1:NI", ifelse(TREATMENT=="t2", "Treatment 2:CMI", "Treatment 3:TTI"))) %>%
  ggplot(aes(PATH_NAME_INI)) + 
  geom_bar(aes(colour=OD, fill=OD)) + 
  facet_grid(. ~ TREATMENT) +
  theme_bw() +
  labs(x="route name", y="count", fill = "OD", colour="OD") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
  

trips_play %>% 
  filter(TREATMENT == "t2", OD=="OD1_1") %>% 
  select(PATH_NAME_INI) %>%
  table() %>%
  rbind(c(24,24,24)) %>%
  chisq.test(simulate.p.value = FALSE)


### Tests:
# t1 vs DEMAND 
trips_play %>% 
  filter(TREATMENT == "t2") %>% 
  select(DEMAND, PATH_NAME_INI) %>%
  table() %>%
  chisq.test(simulate.p.value = TRUE)
# We can't reject that the samples come from the same distribution

# For the not informed treatment, it makes sense to expect the distributions 
# of chosen routes to be simmilar in both demand scenarios. 
# To test this hypothesis we performed a chi-squared test. 
# The results of this test show that there is not enough evidence to 
# reject the hypothesis that the two distributions are the same.

# t1 vs t2
demand <- "D2"
  trips_play %>% 
  filter(DEMAND == demand) %>%
  filter(TREATMENT %in% c("t1", "t3")) %>% 
  select(TREATMENT, PATH_NAME_INI) %>%
  table() %>%
  chisq.test(simulate.p.value = TRUE)
# We reject that the samples come from the same distribution


trips_play %>% 
  filter(TREATMENT %in% c("t1", "t3")) %>% 
  select(TREATMENT, PATH_NAME_INI) %>%
  table() %>%
  chisq.test(simulate.p.value = FALSE)
# We reject that the samples come from the same distribution

trips_play %>% 
  filter(TREATMENT %in% c("t2", "t3")) %>% 
  select(TREATMENT, PATH_NAME_INI) %>%
  table() %>%
  chisq.test(simulate.p.value = FALSE)
# We reject that the samples come from the same distribution

###########################################
# Distribution of tt for all trips in the ODs
###########################################
# Cut time intervals
intervals <- seq(0,5400, 900)
trips_res <- trips_res %>% mutate(DEP_TIME_INTERVAL=cut(DEP_TIME, intervals))
trips_play <- trips_play %>% mutate(DEP_TIME_INTERVAL=cut(DEP_TIME, intervals))

### TT distributions on each route
# aggregated
trips_res %>% 
  filter(!PATH_REROUTE) %>%
  ggplot() +
  geom_density(aes(TT, fill=PATH_NAME), alpha=.3) +
  facet_grid(OD~.) +
  theme_bw() +
  labs(x="travel time", y="density", fill = "Route name")
  
# trips_res %>% 
#  filter(ARR_TIME < FIN_TIME) %>%
#  ggplot(aes(DEP_TIME, ARR_TIME)) +
#  geom_point(aes(colour=as.factor(SESSION_ID)),alpha=.3)

# disaggregated
trips_res %>% 
  filter(!PATH_REROUTE) %>%
  ggplot() +
  geom_boxplot(aes(DEP_TIME_INTERVAL, TT)) +
  facet_grid(DEMAND ~ PATH_NAME) +
  theme_bw() +
  theme(axis.text.x=element_text(angle=90, hjust=1))


### mean TT for each route. percentage of times it was chosen | treatment
trips_res %>% 
  filter(!PATH_REROUTE) %>%
  group_by(OD,PATH_NAME) %>%
  summarise(MEAN_TT=mean(TT), SD_TT=sd(TT)) %>% 
  left_join(trips_play %>% 
              group_by(OD, PATH_NAME_INI) %>%
              summarise(N_CHOICE=n()),
            by=c("OD", "PATH_NAME"="PATH_NAME_INI"))

# By treatment
choice_tt <-
  trips_res %>% 
  filter(!PATH_REROUTE) %>%
    group_by(OD,PATH_NAME) %>%
    summarise(MEAN_TT=mean(TT)) %>% 
    left_join(trips_play %>% 
                group_by(OD, TREATMENT,PATH_NAME_INI) %>%
                summarise(N_CHOICE=n()),
              by=c("OD", "PATH_NAME"="PATH_NAME_INI"))


choice_tt %>% filter(TREATMENT=="t3")
# See tt_max.xls file

###  We will obtain the proportion of times each route was chosen | demand, dep_time_interval

# Obtains the TT for each (DEMAND, DEP_TIME_INTERVAL,PATH_NAME) 
# and joins it with the count for each (DEMAND, TREATMENT, DEP_TIME_INTERVAL,PATH_NAME)
choice_tt_int_d <-
  trips_res %>% 
  filter(!PATH_REROUTE) %>%
  group_by(OD,  DEMAND, DEP_TIME_INTERVAL,PATH_NAME) %>%
  summarise(MEAN_TT=mean(TT)) %>% 
  inner_join(trips_play %>% 
              group_by(OD, DEMAND, TREATMENT,DEP_TIME_INTERVAL,PATH_NAME_INI) %>%
              summarise(N_CHOICE=n()),
            by=c("OD", "DEMAND", "DEP_TIME_INTERVAL","PATH_NAME"="PATH_NAME_INI"))

# Obtains the number of choices per (OD, DEMAND, TREATMENT, DEP_TIME_INTERVAL)
choice_tt_int_d <- 
  choice_tt_int_d %>% 
    left_join(choice_tt_int_d %>% 
                group_by(OD, DEMAND, TREATMENT, DEP_TIME_INTERVAL) %>% 
                summarise(TOT_CHOICES = sum(N_CHOICE))
    )

# Obtains the proportion of times the route was chosen
choice_tt_int_d <- 
  choice_tt_int_d %>% 
    mutate(CHOICE_PROP=N_CHOICE / TOT_CHOICES)

# Plot
choice_tt_int_d %>% 
  filter(OD=="OD1_2") %>%
  ggplot(aes(MEAN_TT, CHOICE_PROP)) +
  geom_point(aes(colour=TREATMENT))

### Standardize the the TT
choice_tt_int_d <- 
  choice_tt_int_d %>% 
    inner_join(trips_res %>% 
                 filter(PATH_NAME != "reroute") %>%
                 group_by(OD, DEMAND, DEP_TIME_INTERVAL) %>%
                 summarise(OD_MEAN_TT=mean(TT), OD_SD_TT=sd(TT))
    )

choice_tt_int_d <- 
  choice_tt_int_d %>% 
    mutate(STD_TT=(MEAN_TT - OD_MEAN_TT) / OD_SD_TT)

# Plot
choice_tt_int_d %>% 
 # filter(OD=="OD1_2") %>%
  ggplot(aes(STD_TT, CHOICE_PROP)) +
  geom_point(aes(colour=TREATMENT))

###########################################
# GLM model 
###########################################
logit_reg <- glm(CHOICE_PROP ~ STD_TT, 
                 data=choice_tt_int_d, 
                 weights=TOT_CHOICES,  
                 family=binomial(link="logit"))
summary(logit_reg)

# plot
pred_df <- choice_tt_int_d[, "STD_TT"]
pred_df$pred <- predict(logit_reg, choice_tt_int_d[, "STD_TT"],type = "response")


choice_tt_int_d %>% 
  # filter(OD=="OD1_2") %>%
  ggplot(aes(STD_TT, CHOICE_PROP)) +
  geom_point(aes(colour=TREATMENT)) + 
  geom_line(data=pred_df, aes(STD_TT, pred))

# model with one line for each treatment
logit_reg2 <- glm(CHOICE_PROP ~ STD_TT* as.factor(TREATMENT), 
                 data=choice_tt_int_d, 
                 weights=TOT_CHOICES,  
                 family=binomial(link="logit"))
summary(logit_reg2)

# plot
pred_df2 <- choice_tt_int_d[, c("STD_TT", "TREATMENT")]
pred_df2$pred <- predict(logit_reg2, pred_df2, type = "response")


choice_tt_int_d %>% 
  # filter(OD=="OD1_2") %>%
  ggplot(aes(STD_TT, CHOICE_PROP)) +
  geom_point(aes(colour=TREATMENT)) + 
  geom_line(data=pred_df, aes(STD_TT, pred), size=1.2) +
  geom_line(data=pred_df2, aes(STD_TT, pred, colour=TREATMENT))











