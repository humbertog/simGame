#####################################################################
# We fit a multinomial logistic model to the choices_train. 
# The variables that we take into accout are:
# - the INFORMED TRAVEL TIME in each of the alternatives
# - the standard deviation of the travel time
# - the treatment to which the player belongs
# The mean tt and the sdcorrespond to those of the travel time distributions
# constructed around the departure time of the chosen alternative.
#####################################################################

library(tidyverse)
library(mlogit)

# Read the res_files
source("r0_config_201704.R")
source("R_readData/readTrips_player.R")
#source("R_readData/readTrips_res.R")
source("R_readData/readTTInfo.R")
source("R_functions/fun_rename.R")

# Read the alternative attributes
alternative_attributes <- read_csv("R_data/alternative_attributes.csv")

# Read the compliance 
compliance <- read_csv(COMPLIANCE_FILE)
compliance <- compliance %>% select(PLAYER_ID, compliance_rate)

# Drop some of the columns to avoid confussion
trips_play$PATH_NAME <- trips_play$PATH_NAME_INI
trips_play <- trips_play %>% 
  select(-DEP_TIME_F_INI, -ARR_TIME_F_INI, -DEP_TIME_INI, -ARR_TIME_INI, -PATH_NAME_INI, -PATH_REROUTE)

alternative_attributes <- alternative_attributes %>%
  select(-ncross)


# STANDARDIZE VARIABLES
infoTT <- 
  infoTT %>%
  group_by(OD) %>%
  mutate(TT_INFO_mean = mean(TT_INFO), TT_INFO_sd = sd(TT_INFO)) %>%
  mutate(TT_INFO_std = TT_INFO  / TT_INFO_mean ) %>%
  #mutate(TT_INFO_std = (TT_INFO - TT_INFO_mean)  / TT_INFO_sd) %>%
  group_by()

# Creates the data base with the forgone choices_train
trips_play$CHOICE_ID <- 1:length(trips_play$OD)

od_route <- unique(trips_play[,c("OD", "PATH_NAME")])

choices_train <- 
  trips_play %>% 
  select(CHOICE_ID, PLAYER_ID, DEMAND, TREATMENT, OD, PATH_NAME, DEP_TIME, ARR_TIME, TT) %>%
  full_join(od_route, by="OD")



# We create the PERIOD variable 
# It is the period in which the trip started and the informed TT during this period
ini_period <- min(infoTT$PERIOD_INI)
fin_period <- max(infoTT$PERIOD_FIN)

interval <- (infoTT$PERIOD_FIN - infoTT$PERIOD_INI)[1]


infoTT$PERIOD <- getPeriod(infoTT$PERIOD_INI, ini_time = ini_period, fin_time = fin_period, interval)
choices_train$PERIOD <- getPeriod(choices_train$DEP_TIME, ini_time = ini_period, fin_time = fin_period, interval)


# Join the variables
choices_train <- 
  choices_train %>%
  left_join(infoTT[,c("ROUTE", "TT_INFO", "TT_INFO_std","TT_INFO_mean", "TT_INFO_sd", "DEMAND", "PERIOD")], 
            by=c("DEMAND"="DEMAND", "PATH_NAME.y"="ROUTE", "PERIOD"="PERIOD")) %>%
  left_join(alternative_attributes, by=c("PATH_NAME.y"="PATH_NAME"))

# join the compliance
choices_train <-
  choices_train %>%
  left_join(compliance, by= "PLAYER_ID")



# Obtains chosen route
choices_train <- choices_train %>% 
  mutate(CHOSEN=ifelse(PATH_NAME.x == PATH_NAME.y, TRUE,FALSE)) %>%
  select(-PATH_NAME.x) %>%
  rename(PATH_NAME=PATH_NAME.y)


choices_train <-
  choices_train %>%
  mutate(high_compliance = as.factor(ifelse(compliance_rate >= .5, 1, 0)))


#####################################################################
source("r0_config_201709.R")
source("R_readData/readTrips_player.R")
#source("R_readData/readTrips_res.R")
source("R_readData/readTTInfo.R")
source("R_functions/fun_rename.R")
# Read the alternative attributes
alternative_attributes <- read_csv("R_data/alternative_attributes.csv")

# Read the compliance 
compliance <- read_csv(COMPLIANCE_FILE)
compliance <- compliance %>% select(PLAYER_ID, compliance_rate)

# Drop some of the columns to avoid confussion
trips_play$PATH_NAME <- trips_play$PATH_NAME_INI
trips_play <- trips_play %>% 
  select(-DEP_TIME_F_INI, -ARR_TIME_F_INI, -DEP_TIME_INI, -ARR_TIME_INI, -PATH_NAME_INI, -PATH_REROUTE)

alternative_attributes <- alternative_attributes %>%
  select(-ncross)


# STANDARDIZE VARIABLES
infoTT <- 
  infoTT %>%
  group_by(OD) %>%
  mutate(TT_INFO_mean = mean(TT_INFO), TT_INFO_sd = sd(TT_INFO)) %>%
  #mutate(TT_INFO_std = TT_INFO  / TT_INFO_mean ) %>%
  mutate(TT_INFO_std = (TT_INFO - TT_INFO_mean)  / TT_INFO_sd) %>%
  group_by()

# Creates the data base with the forgone choices
trips_play$CHOICE_ID <- 1:length(trips_play$OD)

od_route <- unique(trips_play[,c("OD", "PATH_NAME")])

choices <- 
  trips_play %>% 
  select(CHOICE_ID, PLAYER_ID, DEMAND, TREATMENT, OD, PATH_NAME, DEP_TIME, ARR_TIME, TT) %>%
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
  left_join(infoTT[,c("ROUTE", "TT_INFO", "TT_INFO_std","TT_INFO_mean", "TT_INFO_sd", "DEMAND", "PERIOD")], 
            by=c("DEMAND"="DEMAND", "PATH_NAME.y"="ROUTE", "PERIOD"="PERIOD")) %>%
  left_join(alternative_attributes, by=c("PATH_NAME.y"="PATH_NAME"))

# join the compliance
choices <-
  choices %>%
  left_join(compliance, by= "PLAYER_ID")



# Obtains chosen route
choices <- choices %>% 
  mutate(CHOSEN=ifelse(PATH_NAME.x == PATH_NAME.y, TRUE,FALSE)) %>%
  select(-PATH_NAME.x) %>%
  rename(PATH_NAME=PATH_NAME.y)


choices <-
  choices %>%
  mutate(high_compliance = as.factor(ifelse(compliance_rate >= .5, 1, 0)))

#####################################################################
# MODELS
#####################################################################
od <- "O1D1"
treat <- "t3"

data_model <- 
  choices_train %>%
  filter(OD==od) %>%
  filter(TREATMENT==treat) %>%
  #filter(compliance_rate>.5) %>%
  as.data.frame()

choices_train_mnl <- mlogit.data(data_model, choice="CHOSEN", shape="long", alt.var="PATH_NAME", chid.var = "CHOICE_ID", drop.index=TRUE)

# Formulas
f0 <- mFormula(CHOSEN ~ 1 )
f1 <- mFormula(CHOSEN ~  -1+ TT_INFO_std )  
f1.1 <- mFormula(CHOSEN ~  1 + TT_INFO_std )

f2 <- mFormula(CHOSEN ~ -1 + TT_INFO_std + total_len + ncross_km) 
f2.2 <- mFormula(CHOSEN ~ -1 + (TT_INFO_std + total_len + ncross_km):high_compliance ) 
f2.3 <- mFormula(CHOSEN ~ -1 + TT_INFO_std + (total_len + ncross_km):high_compliance ) 

f3 <- mFormula(CHOSEN ~  TT_INFO_std | -1 + compliance_rate ) 
f4 <- mFormula(CHOSEN ~  TT_INFO_std + total_len + ncross_km | -1 + compliance_rate  ) 

f5 <- mFormula(CHOSEN ~  -1 + TT_INFO_std:compliance_rate  ) 
f5.1  <- mFormula(CHOSEN ~  1 + TT_INFO_std:compliance_rate  ) 
f5.2 <- mFormula(CHOSEN ~  -1 + TT_INFO_std:compliance_rate + total_len + ncross_km  ) 
f5.3 <- mFormula(CHOSEN ~  -1 + (TT_INFO_std:compliance_rate + total_len + ncross_km):high_compliance  ) 
f5.4 <- mFormula(CHOSEN ~  -1 + TT_INFO_std:compliance_rate + (total_len + ncross_km):high_compliance  ) 


# No need to standardize: it can be obtained from the original by multiplying by sd(explanatory) and 
# dividing by 60

### Fit mnlogit
mod_mnlogit5.2 <- mlogit(f5.2  , data=choices_train_mnl)
mod_mnlogit5.3 <- mlogit(f5.3  , data=choices_train_mnl)

summary(mod_mnlogit5.2)
summary(mod_mnlogit5.3)

### Fit random coeff - normal
rand_coeff <- c('TT_INFO_std'="n")
rand_coeff_2 <-c('TT_INFO_std:high_compliance0' = 'n', 'TT_INFO_std:high_compliance1' = 'n')
rand_coeff_3 <- c('TT_INFO_std:compliance_rate'='n')
rand_coeff_4 <-c('high_compliance0:TT_INFO_std:compliance_rate'='n', 'high_compliance1:TT_INFO_std:compliance_rate'='n')

mod_mnlogit5.2_rp <- mlogit(f5.2  , data=choices_train_mnl, rpar = rand_coeff_3)
mod_mnlogit5.3_rp <- mlogit(f5.3  , data=choices_train_mnl, rpar = rand_coeff_4)

summary(mod_mnlogit5.2_rp)
summary(mod_mnlogit5.3_rp)

### Fit probit
mod_probit5.2 <- mlogit(f5.2  , data=choices_train_mnl, probit=TRUE)
mod_probit5.3 <- mlogit(f5.3  , data=choices_train_mnl, probit=TRUE)

summary(mod_probit5.2)
summary(mod_probit5.3)


#####################################################################
choices_test <- 
  choices %>%
  filter(OD==od) %>%
  filter(TREATMENT==treat) %>%
  #filter(compliance_rate>.5) %>%
  as.data.frame()

choices_test_mnl <- mlogit.data(choices_test, choice="CHOSEN", shape="long", alt.var="PATH_NAME", chid.var = "CHOICE_ID", drop.index=TRUE)

pred_mnlogit5.2  <- colMeans(predict(mod_mnlogit5.2, newdata = choices_test_mnl))
pred_mnlogit5.3  <- colMeans(predict(mod_mnlogit5.3, newdata = choices_test_mnl))
pred_mnlogit5.2_rp <- colMeans(predict(mod_mnlogit5.2_rp, newdata = choices_test_mnl))
pred_mnlogit5.3_rp <- colMeans(predict(mod_mnlogit5.3_rp, newdata = choices_test_mnl))
pred_probit5.2 <- colMeans(predict(mod_probit5.2, newdata = choices_test_mnl))
pred_probit5.3  <- colMeans(predict(mod_probit5.3, newdata = choices_test_mnl))

observed <- 
  choices_test %>%
    filter(CHOSEN == TRUE) %>%
    group_by(OD, PATH_NAME) %>%
    summarise(n=n()) %>%
  group_by(OD) %>%
  mutate(N = sum(n), freq=n/N)  %>%
  group_by()


ggplot() +
  geom_col(aes(PATH_NAME, freq, fill="observed"), alpha=.25, data=observed) +
  geom_col(aes(PATH_NAME, freq, colour="mnlogit5.2"), alpha=0 ,data=data.frame(PATH_NAME=names(pred_mnlogit5.2),freq=pred_mnlogit5.2)) +
  geom_col(aes(PATH_NAME, freq, colour="mnlogit5.3"), alpha=0 ,data=data.frame(PATH_NAME=names(pred_mnlogit5.3),freq=pred_mnlogit5.3)) +
  geom_col(aes(PATH_NAME, freq, colour="mnlogit5.2_rp"), alpha=0 ,data=data.frame(PATH_NAME=names(pred_mnlogit5.2_rp),freq=pred_mnlogit5.2_rp)) +
  geom_col(aes(PATH_NAME, freq, colour="mnlogit5.3_rp"), alpha=0 ,data=data.frame(PATH_NAME=names(pred_mnlogit5.3_rp),freq=pred_mnlogit5.3_rp)) +
  geom_col(aes(PATH_NAME, freq, colour="probit5.2"), alpha=0 ,data=data.frame(PATH_NAME=names(pred_probit5.2),freq=pred_probit5.2)) +
  geom_col(aes(PATH_NAME, freq, colour="probit5.3"), alpha=0 ,data=data.frame(PATH_NAME=names(pred_probit5.3),freq=pred_probit5.3)) +
  theme_bw()
ggsave(paste(od, "_pred.png", sep=""),  width = 16, height = 12, units = "cm", dpi = 300, limitsize = TRUE)


ggplot() +
  geom_col(aes(PATH_NAME, freq, fill="observed"), alpha=.25, data=observed) +
  geom_col(aes(PATH_NAME, freq, colour="mnlogit"), alpha=0 ,data=data.frame(PATH_NAME=names(pred_mnlogit5.2),freq=pred_mnlogit5.2)) +
  geom_col(aes(PATH_NAME, freq, colour="mnlogit_rcoeff"), alpha=0 ,data=data.frame(PATH_NAME=names(pred_mnlogit5.2_rp),freq=pred_mnlogit5.2_rp)) +
  geom_col(aes(PATH_NAME, freq, colour="probit"), alpha=0 ,data=data.frame(PATH_NAME=names(pred_probit5.2),freq=pred_probit5.2)) +
  theme_bw()

ggsave(paste(od, "_pred5.2.png", sep=""),  width = 16, height = 12, units = "cm",dpi = 300, limitsize = TRUE)

ggplot() +
  geom_col(aes(PATH_NAME, freq, fill="observed"), alpha=.25, data=observed) +
  geom_col(aes(PATH_NAME, freq, colour="mnlogit5.3"), alpha=0 ,data=data.frame(PATH_NAME=names(pred_mnlogit5.3),freq=pred_mnlogit5.3)) +
  geom_col(aes(PATH_NAME, freq, colour="mnlogit5.3_rp"), alpha=0 ,data=data.frame(PATH_NAME=names(pred_mnlogit5.3_rp),freq=pred_mnlogit5.3_rp)) +
  geom_col(aes(PATH_NAME, freq, colour="probit5.3"), alpha=0 ,data=data.frame(PATH_NAME=names(pred_probit5.3),freq=pred_probit5.3))+
    theme_bw()

#ggsave(paste(od, "_pred5.3.png", sep=""),  width = 16, height = 12, units = "cm",
#       dpi = 300, limitsize = TRUE)


# We see the predictions







