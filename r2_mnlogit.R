#####################################################################
# 
#####################################################################
library(tidyverse)
library(mlogit)

# Read the res_files
source("r00_config.R")
source("r0_readTrips_res.R")
source("r0_readTrips_player.R")



######################################
# Creates the data set with the choices
######################################
# size of interval 
h <- 600

trips_play <- 
  trips_play %>% 
  filter(SESSION_ID != 629)

trips_res_play <- tibble()

# Computes the mean and sd of TT for each alternative in a choice
# They are computed arount an interval centered at the departure time of each
# trip
for (i in 1:dim(trips_play)[1]) {
  t_ <- trips_play$DEP_TIME[i]
  db_ <- trips_play$SESSION_ID[i]
  od_ <- trips_play$OD[i]
  tt_ <- trips_play$TT[i]
  chosen_ <- trips_play$PATH_NAME_INI[i]
  treatment_ <- trips_play$TREATMENT[i]
  
  trips_res_play_t <-
    trips_res %>% 
      filter(PATH_REROUTE == 0) %>%
      filter(SESSION_ID==db_, OD==od_) %>%
      filter(DEP_TIME >= t_ - h, DEP_TIME <= t_ + h) %>%
      group_by(PATH_NAME) %>%
      summarise(MEAN_TT = mean(TT), SD_TT = sd(TT))
  
  trips_res_play_t <- 
    trips_res_play_t %>% 
      mutate(CHOICE_ID=i, 
             OD = od_,
             CHOSEN_PATH=chosen_,
             TREATMENT=treatment_
              )
    
  trips_res_play <- bind_rows(trips_res_play, trips_res_play_t) 
}


trips_res_play <-
  trips_res_play %>%
  mutate(CHOICE=ifelse(PATH_NAME == CHOSEN_PATH, TRUE, FALSE))


# check!
sum(table(trips_res_play$CHOICE, trips_res_play$CHOICE_ID)[1,] != 2)

######################################
# mnlogit
######################################
od <- "OD2"
treat <- "t3"

data_model <- 
  trips_res_play %>%
  filter(OD==od) %>%
  #filter(TREATMENT==treat) %>%
  as.data.frame()

choices_mnl <- mlogit.data(data_model, choice="CHOICE", shape="long", alt.var="PATH_NAME", chid.var = "CHOICE_ID", drop.index=TRUE)

f0 <- mFormula(CHOICE ~ 1 )
f1 <- mFormula(CHOICE ~ MEAN_TT )  
f2 <- mFormula(CHOICE ~ MEAN_TT + SD_TT)  
f11 <- mFormula(CHOICE ~ MEAN_TT | TREATMENT)          # I CONSIDER THAT THIS IS THE CORRECT MODEL
f22 <- mFormula(CHOICE ~ MEAN_TT + SD_TT | TREATMENT)  # I CONSIDER THAT THIS IS THE CORRECT MODEL

mod_mnlogit0 <- mlogit(f0  , data=choices_mnl)
mod_mnlogit1 <- mlogit(f1  , data=choices_mnl)
mod_mnlogit2 <- mlogit(f2  , data=choices_mnl)
mod_mnlogit11 <- mlogit(f11  , data=choices_mnl)
mod_mnlogit22 <- mlogit(f22  , data=choices_mnl)

summary(mod_mnlogit0)
summary(mod_mnlogit1)
summary(mod_mnlogit2)
summary(mod_mnlogit11)
summary(mod_mnlogit22)


# check:
colMeans(predict(mod_mnlogit11, choices_mnl))
apply(fitted(mod_mnlogit11, outcome = FALSE), 2, mean)



# Obtain the probabilities:
# ln(p1 / p3) = b0_p1 + b1_p1 * x_i
# ln(p2 / p3) = b0_p1=2 + b1_p2 * x_i
# Pr(Y=1) = 1 / (1 + exp(b0_p1 + b1_p1 * x_i) + exp(b0_p1=2 + b1_p2 * x_i) )

# Pr(Y=1 | x)
chId <- 1
x <- data_model[data_model$CHOICE_ID == chId,c("PATH_NAME", "MEAN_TT", "TREATMENT")]

coef_x_0 <- coef(mod_mnlogit11)[3] * x[1,2]

coef_x_1 <- coef(mod_mnlogit11)[c(1,3,4,6)] * unlist(c(1, x[2,2],ifelse(x[2,3]=="t2",1,0), ifelse(x[2,3]=="t3",1,0) ))

coef_x_2 <- coef(mod_mnlogit11)[c(2,3,5,7)] * unlist(c(1, x[3,2],ifelse(x[3,3]=="t2",1,0), ifelse(x[3,3]=="t3",1,0) ))

# Pr(Y=i | x)
denom <- 1 / (exp(sum(coef_x_0)) + exp(sum(coef_x_1)) + exp(sum(coef_x_2)))
p3 <- exp(sum(coef_x_0))* denom
p1 <- exp(sum(coef_x_1)) * denom
p2 <- exp(sum(coef_x_2))* denom
c(p3, p1,p2)

mlogit.data(data_model, choice="CHOICE", shape="long", alt.var="PATH_NAME", chid.var = "CHOICE_ID", drop.index=TRUE)
predict(mod_mnlogit11, choices_mnl,type="probs")[1,]



