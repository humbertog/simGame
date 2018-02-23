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

source("./R_data/choice_datapre_201704.R")


choices <-
  choices %>%
  mutate(high_compliance = as.factor(ifelse(compliance_rate >= .5, 1, 0)))

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
f1 <- mFormula(CHOSEN ~  -1+ TT_INFO_std )  
f1.1 <- mFormula(CHOSEN ~  1 + TT_INFO_std )
f1.2 <- mFormula(CHOSEN ~  -1 + TT_INFO_std + TT_INFO_std:high_compliance )

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
mod_mnlogit0 <- mlogit(f0  , data=choices_mnl)
mod_mnlogit1 <- mlogit(f1  , data=choices_mnl)
mod_mnlogit1.1 <- mlogit(f1.1  , data=choices_mnl)
mod_mnlogit1.2 <- mlogit(f1.2  , data=choices_mnl)

mod_mnlogit2 <- mlogit(f2  , data=choices_mnl)
mod_mnlogit2.2 <- mlogit(f2.2  , data=choices_mnl)
mod_mnlogit2.3 <- mlogit(f2.3  , data=choices_mnl)

mod_mnlogit3 <- mlogit(f3  , data=choices_mnl)
mod_mnlogit4 <- mlogit(f4  , data=choices_mnl)

mod_mnlogit5 <- mlogit(f5  , data=choices_mnl)
mod_mnlogit5.1 <- mlogit(f5.1  , data=choices_mnl)
mod_mnlogit5.2 <- mlogit(f5.2  , data=choices_mnl)
mod_mnlogit5.3 <- mlogit(f5.3  , data=choices_mnl)
mod_mnlogit5.4 <- mlogit(f5.4  , data=choices_mnl)


summary(mod_mnlogit0)
summary(mod_mnlogit1)
summary(mod_mnlogit1.1)
summary(mod_mnlogit1.2)

summary(mod_mnlogit2)
summary(mod_mnlogit2.2)
summary(mod_mnlogit2.3)

summary(mod_mnlogit3)
summary(mod_mnlogit4)

summary(mod_mnlogit5)
summary(mod_mnlogit5.1)
summary(mod_mnlogit5.2)
summary(mod_mnlogit5.3)
summary(mod_mnlogit5.4)


lapply(list(mod_mnlogit0, mod_mnlogit1, mod_mnlogit1.1, 
            mod_mnlogit2, mod_mnlogit2.2, mod_mnlogit2.3, 
            mod_mnlogit3, mod_mnlogit4, 
            mod_mnlogit5, mod_mnlogit5.1, mod_mnlogit5.2, mod_mnlogit5.3, mod_mnlogit5.4), 
       function(x) logLik(x))
lapply(list(mod_mnlogit0, mod_mnlogit1, mod_mnlogit1.1, 
            mod_mnlogit2, mod_mnlogit2.2, mod_mnlogit2.3, 
            mod_mnlogit3, mod_mnlogit4, 
            mod_mnlogit5, mod_mnlogit5.1, mod_mnlogit5.2, mod_mnlogit5.3, mod_mnlogit5.4), 
       function(x) AIC(x))




### Fit random coeff - normal
rand_coeff <- c('TT_INFO_std'="n")
rand_coeff_2 <-c('TT_INFO_std:high_compliance0' = 'n', 'TT_INFO_std:high_compliance1' = 'n')
rand_coeff_3 <- c('TT_INFO_std:compliance_rate'='n')
rand_coeff_4 <-c('high_compliance0:TT_INFO_std:compliance_rate'='n', 'high_compliance1:TT_INFO_std:compliance_rate'='n')

#rand_coeff <- c('TT_INFO_std'="n", "compliance_rate:O2D1_north"="n", "compliance_rate:O2D1_south"="n")
#rand_coeff <- c('TT_INFO_std'="n", "compliance_rate:O3D2_north"="n", "compliance_rate:O3D2_south"="n")

mod_mnlogit1_rp <- mlogit(f1  , data=choices_mnl, rpar = rand_coeff)
mod_mnlogit1.1_rp <- mlogit(f1.1  , data=choices_mnl, rpar = rand_coeff)

mod_mnlogit2_rp <- mlogit(f2  , data=choices_mnl, rpar = rand_coeff)
mod_mnlogit2.2_rp <- mlogit(f2.2  , data=choices_mnl, rpar = rand_coeff_2)
mod_mnlogit2.3_rp <- mlogit(f2.3  , data=choices_mnl, rpar = rand_coeff)

mod_mnlogit3_rp <- mlogit(f3  , data=choices_mnl, rpar = rand_coeff)
mod_mnlogit4_rp <- mlogit(f4  , data=choices_mnl, rpar = rand_coeff)

mod_mnlogit5_rp <- mlogit(f5  , data=choices_mnl, rpar = rand_coeff_3)
mod_mnlogit5.1_rp <- mlogit(f5.1  , data=choices_mnl, rpar = rand_coeff_3)
mod_mnlogit5.2_rp <- mlogit(f5.2  , data=choices_mnl, rpar = rand_coeff_3)
mod_mnlogit5.3_rp <- mlogit(f5.3  , data=choices_mnl, rpar = rand_coeff_4)
mod_mnlogit5.4_rp <- mlogit(f5.4  , data=choices_mnl, rpar = rand_coeff_3)


summary(mod_mnlogit1_rp)
summary(mod_mnlogit1.1_rp)

summary(mod_mnlogit2_rp)
summary(mod_mnlogit2.2_rp)
summary(mod_mnlogit2.3_rp)

summary(mod_mnlogit3_rp)
summary(mod_mnlogit4_rp)

summary(mod_mnlogit5_rp)
summary(mod_mnlogit5.1_rp)
summary(mod_mnlogit5.2_rp)
summary(mod_mnlogit5.3_rp)
summary(mod_mnlogit5.4_rp)


lapply(list(mod_mnlogit1_rp, mod_mnlogit1.1_rp, 
            mod_mnlogit2_rp,mod_mnlogit2.2_rp, mod_mnlogit2.3_rp,
            mod_mnlogit3_rp,mod_mnlogit4_rp,
            mod_mnlogit5_rp, mod_mnlogit5.1_rp, mod_mnlogit5.2_rp,mod_mnlogit5.3_rp,mod_mnlogit5.4_rp
), function(x) logLik(x))

lapply(list(mod_mnlogit1_rp, mod_mnlogit1.1_rp, 
            mod_mnlogit2_rp,mod_mnlogit2.2_rp, mod_mnlogit2.3_rp,
            mod_mnlogit3_rp,mod_mnlogit4_rp,
            mod_mnlogit5_rp, mod_mnlogit5.1_rp, mod_mnlogit5.2_rp,mod_mnlogit5.3_rp,mod_mnlogit5.4_rp
), function(x) AIC(x))


### Fit probit
mod_probit0 <- mlogit(f0  , data=choices_mnl, probit=TRUE)

mod_probit1 <- mlogit(f1  , data=choices_mnl, probit=TRUE)
mod_probit1.1 <- mlogit(f1.1  , data=choices_mnl, probit=TRUE)

mod_probit2 <- mlogit(f2  , data=choices_mnl, probit=TRUE)
mod_probit2.2 <- mlogit(f2.2  , data=choices_mnl, probit=TRUE)
mod_probit2.3 <- mlogit(f2.3  , data=choices_mnl, probit=TRUE)

mod_probit3 <- mlogit(f3  , data=choices_mnl, probit=TRUE)
mod_probit4 <- mlogit(f4  , data=choices_mnl, probit=TRUE)

mod_probit5 <- mlogit(f5  , data=choices_mnl, probit=TRUE)
mod_probit5.1 <- mlogit(f5.1  , data=choices_mnl, probit=TRUE)
mod_probit5.2 <- mlogit(f5.2  , data=choices_mnl, probit=TRUE)
mod_probit5.3 <- mlogit(f5.3  , data=choices_mnl, probit=TRUE)
mod_probit5.4 <- mlogit(f5.4  , data=choices_mnl, probit=TRUE)


summary(mod_probit0)

summary(mod_probit1)
summary(mod_probit1.1)

summary(mod_probit2)
summary(mod_probit2.2)
summary(mod_probit2.3)

summary(mod_probit3)
summary(mod_probit4)

summary(mod_probit5)
summary(mod_probit5.1)
summary(mod_probit5.2)
summary(mod_probit5.3)
summary(mod_probit5.4)


lapply(list(mod_probit0, mod_probit1, mod_probit1.1, 
            mod_probit2, mod_probit2.2, mod_probit2.3,
            mod_probit3, mod_probit4,
            mod_probit5, mod_probit5.1, mod_probit5.2, mod_probit5.3, mod_probit5.4
), function(x) logLik(x))
lapply(list(mod_probit0, mod_probit1, mod_probit1.1, 
            mod_probit2, mod_probit2.2, mod_probit2.3,
            mod_probit3, mod_probit4,
            mod_probit5, mod_probit5.1, mod_probit5.2, mod_probit5.3, mod_probit5.4
), function(x) AIC(x))



# We see the predictions
pred_mnlogit <- predict(mod_mnlogit1, newdata = choices_mnl)
pred_rc <- predict(mod_mnlogit1_rp, newdata = choices_mnl)
pred_probit <- predict(mod_probit1, newdata = choices_mnl)
pred_rc5 <- predict(mod_mnlogit5_rp, newdata = choices_mnl)
pred_mnlogit5 <- predict(mod_mnlogit5, newdata = choices_mnl)
pred_probit5 <- predict(mod_probit5, newdata = choices_mnl)

ggplot() +
  geom_col(aes(route, observed, fill="observed"), alpha=.25, data=data.frame(route=names(mod_mnlogit1$freq), observed=c(mod_mnlogit1$freq / sum(mod_mnlogit1$freq)))) +
  geom_col(aes(route, mean, colour="mnlogit"), alpha=0 ,data=data.frame(route=colnames(pred_mnlogit), mean=colMeans(pred_mnlogit)))+
  geom_col(aes(route, mean, colour="rand_coeff"), alpha=0 ,data=data.frame(route=colnames(pred_rc), mean=colMeans(pred_rc)))+
  geom_col(aes(route, mean, colour="probit"), alpha=0, data=data.frame(route=colnames(pred_probit), mean=colMeans(pred_probit)))+ 
  geom_col(aes(route, mean, colour="rand_coeff_compliance"), alpha=0, data=data.frame(route=colnames(pred_rc5), mean=colMeans(pred_rc5)))+ 
  geom_col(aes(route, mean, colour="probit_compliance"), alpha=0, data=data.frame(route=colnames(pred_probit5), mean=colMeans(pred_probit5)))+ 
  scale_color_manual(values=c("observed"="red", "mnlogit"="blue","rand_coeff"="green", "probit"="black", 
                              "rand_coeff_compliance"="yellow", "mnlogit_compliance"="brown",
                              "probit_compliance"="red"
  )) +
  ylab("relative frequency") +
  theme_bw()

ggsave(paste(od, "_pred.png", sep=""),  width = 16, height = 12, units = "cm",
       dpi = 300, limitsize = TRUE)




