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
source("r0_config.R")
source("R_readData/readTrips_player.R")
source("R_readData/readTrips_res.R")
source("R_readData/readTTInfo.R")

# Reads the alternative attributes
alternative_attributes <- read_csv("R_data/alternative_attributes.csv")


# Drop some of the columns to avoid confussion
trips_play$PATH_NAME <- trips_play$PATH_NAME_INI
trips_play <- trips_play %>% 
  select(-DEP_TIME_F_INI, -ARR_TIME_F_INI, -DEP_TIME_INI, -ARR_TIME_INI, -PATH_NAME_INI, -PLAYER_ID, -PATH_REROUTE)

alternative_attributes <- alternative_attributes %>%
  select(-ncross)

# Creates the data base with the forgone choices
trips_play$CHOICE_ID <- 1:length(trips_play$OD)

od_route <- unique(trips_play[,c("OD", "PATH_NAME")])

choices <- 
  trips_play %>% 
    select(CHOICE_ID, DEMAND, TREATMENT, OD, PATH_NAME, DEP_TIME, ARR_TIME, TT) %>%
    full_join(od_route, by="OD")



# We create the PERIOD variable 
# It is the period in which the trip started and the informed TT during this period
cut_breaks <- seq(0, max(infoTT$PERIOD_FIN)-min(infoTT$PERIOD_INI) , 600)

infoTT$PERIOD_INI_F <- infoTT$PERIOD_INI - min(infoTT$PERIOD_INI)
infoTT$PERIOD_FIN_F <- infoTT$PERIOD_FIN - min(infoTT$PERIOD_INI)
infoTT$PERIOD <- cut(infoTT$PERIOD_INI_F, cut_breaks, right=FALSE,include.lowest = TRUE)

choices$PERIOD <- cut(choices$DEP_TIME, cut_breaks, right=FALSE,include.lowest = TRUE)


# Join the variables
choices <- 
  choices %>%
  left_join(infoTT[,c("ROUTE", "TT_INFO", "DEMAND", "PERIOD")], 
            by=c("DEMAND"="DEMAND", "PATH_NAME.y"="ROUTE", "PERIOD"="PERIOD")) %>%
  left_join(alternative_attributes, by=c("PATH_NAME.y"="PATH_NAME"))


# Obtains chosen route
choices <- choices %>% 
  mutate(CHOSEN=ifelse(PATH_NAME.x == PATH_NAME.y, TRUE,FALSE)) %>%
  select(-PATH_NAME.x) %>%
  rename(PATH_NAME=PATH_NAME.y)

#####################################################################
# MODELS
#####################################################################
od <- "OD2"
treat <- "t3"

data_model <- 
  choices %>%
  filter(OD==od) %>%
  filter(TREATMENT==treat) %>%
  as.data.frame()

choices_mnl <- mlogit.data(data_model, choice="CHOSEN", shape="long", alt.var="PATH_NAME", chid.var = "CHOICE_ID", drop.index=TRUE)

# Formulas
f0 <- mFormula(CHOSEN ~ 1 )
f1 <- mFormula(CHOSEN ~  -1+ I(TT_INFO/60) )  
f1.1 <- mFormula(CHOSEN ~  1 + I(TT_INFO/60) )  
f2 <- mFormula(CHOSEN ~ -1 + I(TT_INFO/60) + total_len + ncross_km) 

### Fit mnlogit
mod_mnlogit0 <- mlogit(f0  , data=choices_mnl)
mod_mnlogit1 <- mlogit(f1  , data=choices_mnl)
mod_mnlogit1.1 <- mlogit(f1.1  , data=choices_mnl)
mod_mnlogit2 <- mlogit(f2  , data=choices_mnl)

summary(mod_mnlogit0)
summary(mod_mnlogit1)
summary(mod_mnlogit1.1)
summary(mod_mnlogit2)


### Fit random coeff - normal
mod_mnlogit1_rp <- mlogit(f1  , data=choices_mnl, rpar = c('I(TT_INFO/60)'="n"))
mod_mnlogit1.1_rp <- mlogit(f1.1  , data=choices_mnl, rpar = c('I(TT_INFO/60)'="n"))
mod_mnlogit2_rp <- mlogit(f2  , data=choices_mnl, rpar = c('I(TT_INFO/60)'="n"))

summary(mod_mnlogit1_rp)
summary(mod_mnlogit1.1_rp)
summary(mod_mnlogit2_rp)

### Fit probit
mod_probit0 <- mlogit(f0  , data=choices_mnl, probit=TRUE)
mod_probit1 <- mlogit(f1  , data=choices_mnl, probit=TRUE)
mod_probit1.1 <- mlogit(f1.1  , data=choices_mnl, probit=TRUE)
mod_probit2 <- mlogit(f2  , data=choices_mnl, probit=TRUE)

summary(mod_probit0)
summary(mod_probit1)
summary(mod_probit1.1)
summary(mod_probit2)



