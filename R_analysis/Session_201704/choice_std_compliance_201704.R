#####################################################################
# We fit a multinomial logistic model to the choices. 
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
#source("r0_config_201709.R")
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




#####################################################################
# MODELS
#####################################################################
od <- "O1D1"
treat <- "t3"

data_model <- 
  choices %>%
  filter(OD==od) %>%
  filter(TREATMENT==treat) %>%
  #filter(compliance_rate>.5) %>%
  as.data.frame()

choices_mnl <- mlogit.data(data_model, choice="CHOSEN", shape="long", alt.var="PATH_NAME", chid.var = "CHOICE_ID", drop.index=TRUE)

# Formulas
f0 <- mFormula(CHOSEN ~ 1 )

f1 <- mFormula(CHOSEN ~  -1+ I(TT_INFO/60) )  
f1_std <- mFormula(CHOSEN ~  -1+ TT_INFO_std )  
f1_std_cr <- mFormula(CHOSEN ~  -1 + I(TT_INFO_std * compliance_rate)  ) 


f2_std <- mFormula(CHOSEN ~ -1 + TT_INFO_std + total_len + ncross_km) 
f2_std_cr <- mFormula(CHOSEN ~ -1 + I(TT_INFO_std* compliance_rate) + total_len + ncross_km) 


# No need to standardize: it can be obtained from the original by multiplying by sd(explanatory) and 
# dividing by 60



### Fit mnlogit
mod_mnlogit0 <- mlogit(f0  , data=choices_mnl)

mod_mnlogit1 <- mlogit(f1  , data=choices_mnl)
mod_mnlogit1_std <- mlogit(f1_std  , data=choices_mnl)
mod_mnlogit1_std_cr <- mlogit(f1_std_cr  , data=choices_mnl)
mod_mnlogit1_std_rand_coeff <- mlogit(f1_std  , data=choices_mnl, rpar = c('TT_INFO_std'="t"))
mod_mnlogit1_std_cr_rand_coeff <- mlogit(f1_std_cr  , data=choices_mnl, rpar = c('I(TT_INFO_std * compliance_rate)'="t"))

mod_mnlogit2_std <- mlogit(f2_std  , data=choices_mnl)
mod_mnlogit2_std_cr <- mlogit(f2_std_cr  , data=choices_mnl)


summary(mod_mnlogit0)

summary(mod_mnlogit1)
summary(mod_mnlogit1_std)
summary(mod_mnlogit1_std_cr)
summary(mod_mnlogit1_std_rand_coeff)
summary(mod_mnlogit1_std_cr_rand_coeff)

summary(mod_mnlogit2_std)
summary(mod_mnlogit2_std_cr)



###
obscomp <- data_model$compliance_rate[data_model$CHOSEN]

hist(obscomp, 5)

mean(obscomp*-15.4026)
sd(obscomp*-15.4026)
