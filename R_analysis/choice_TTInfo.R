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

# Creates the data base with the forgone choices
trips_play$CHOICE_ID <- 1:length(trips_play$OD)

od_route <- unique(trips_play[,c("OD", "PATH_NAME")])

choices <- 
  trips_play %>% 
    select(CHOICE_ID, DEMAND, TREATMENT, OD, PATH_NAME, DEP_TIME, ARR_TIME, TT) %>%
    full_join(od_route, by="OD")



# We create the PERIOD variable 
# It is the period in which the trip started and the informed TT during this period
ini_period <- min(infoTT$PERIOD_INI)
fin_period <- max(infoTT$PERIOD_FIN)

infoTT$PERIOD <- getPeriod(infoTT$PERIOD_INI, ini_time = ini_period, fin_time = fin_period, 900)
choices$PERIOD <- getPeriod(choices$DEP_TIME, ini_time = ini_period, fin_time = fin_period, 900)


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


# Formats

infoTT$ROUTE <- renameRoutes(infoTT$ROUTE)
infoTT$OD <- renameOD(infoTT$OD)

choices$PATH_NAME <- renameRoutes(choices$PATH_NAME)
choices$OD <- renameOD(choices$OD)

#####################################################################
# MODELS
#####################################################################
od <- "O1D1"
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

lapply(list(mod_mnlogit0, mod_mnlogit1, mod_mnlogit1.1, mod_mnlogit2), function(x) logLik(x))
lapply(list(mod_mnlogit0, mod_mnlogit1, mod_mnlogit1.1, mod_mnlogit2), function(x) AIC(x))

### Fit random coeff - normal
mod_mnlogit1_rp <- mlogit(f1  , data=choices_mnl, rpar = c('I(TT_INFO/60)'="n"))
mod_mnlogit1.1_rp <- mlogit(f1.1  , data=choices_mnl, rpar = c('I(TT_INFO/60)'="n"))
mod_mnlogit2_rp <- mlogit(f2  , data=choices_mnl, rpar = c('I(TT_INFO/60)'="n"))

summary(mod_mnlogit1_rp)
summary(mod_mnlogit1.1_rp)
summary(mod_mnlogit2_rp)

lapply(list(mod_mnlogit1_rp, mod_mnlogit1.1_rp, mod_mnlogit2_rp), function(x) logLik(x))
lapply(list(mod_mnlogit1_rp, mod_mnlogit1.1_rp, mod_mnlogit2_rp), function(x) AIC(x))

### Fit probit
mod_probit0 <- mlogit(f0  , data=choices_mnl, probit=TRUE)
mod_probit1 <- mlogit(f1  , data=choices_mnl, probit=TRUE)
mod_probit1.1 <- mlogit(f1.1  , data=choices_mnl, probit=TRUE)
mod_probit2 <- mlogit(f2  , data=choices_mnl, probit=TRUE)

summary(mod_probit0)
summary(mod_probit1)
summary(mod_probit1.1)
summary(mod_probit2)

lapply(list(mod_probit0, mod_probit1, mod_probit1.1, mod_probit2), function(x) logLik(x))
lapply(list(mod_probit0, mod_probit1, mod_probit1.1, mod_probit2), function(x) AIC(x))



# We see the predictions
pred_mnlogit <- predict(mod_mnlogit1, newdata = choices_mnl)
pred_rc <- predict(mod_mnlogit1_rp, newdata = choices_mnl)
pred_probit <- predict(mod_probit1, newdata = choices_mnl)


ggplot() +
  geom_col(aes(route, observed, fill="observed"), alpha=.25, data=data.frame(route=names(mod_mnlogit1$freq), observed=c(mod_mnlogit1$freq / sum(mod_mnlogit1$freq)))) +
  geom_col(aes(route, mean, colour="mnlogit"), alpha=0 ,data=data.frame(route=colnames(pred_mnlogit), mean=colMeans(pred_mnlogit)))+
  geom_col(aes(route, mean, colour="rand_coeff"), alpha=0 ,data=data.frame(route=colnames(pred_rc), mean=colMeans(pred_rc)))+
  geom_col(aes(route, mean, colour="probit"), alpha=0, data=data.frame(route=colnames(pred_probit), mean=colMeans(pred_probit)))+ 
  scale_color_manual(values=c("observed"="red", "mnlogit"="blue","rand_coeff"="green", "probit"="black")) +
  ylab("relative frequency") +
  theme_bw()
  
ggsave(paste(od, "_pred.png", sep=""),  width = 16, height = 12, units = "cm",
       dpi = 300, limitsize = TRUE)




