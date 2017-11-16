#####################################################################
# We fit a multinomial logistic model to the choices. 
# The variables that we take into accout are:
# - the mean travel time in each of the alternatives
# - the standard deviation of the travel time
# - the treatment to which the player belongs
# The mean tt and the sdcorrespond to those of the travel time distributions
# constructed around the departure time of the chosen alternative.
#####################################################################
source("r2_mnlogit_pre.R")

od <- "OD1_1"
treat <- c("t3")

data_model <- 
  trips_res_play %>%
  filter(OD==od) %>%
  filter(TREATMENT %in% treat) %>%
  as.data.frame()

#testIds <- sample(unique(data_model$CHOICE_ID), floor(length(unique(data_model$CHOICE_ID)) * .20))


#choices_mnl_train <- mlogit.data(data_model[!data_model$CHOICE_ID %in% testIds,], choice="CHOICE", shape="long", alt.var="PATH_NAME", chid.var = "CHOICE_ID", drop.index=TRUE)
choices_mnl_train <- mlogit.data(data_model, choice="CHOICE", shape="long", alt.var="PATH_NAME", chid.var = "CHOICE_ID", drop.index=TRUE)
#choices_mnl_test <- mlogit.data(data_model[data_model$CHOICE_ID %in% testIds,], choice="CHOICE", shape="long", alt.var="PATH_NAME", chid.var = "CHOICE_ID", drop.index=TRUE)

f0 <- mFormula(CHOICE ~ 1 )
f1 <- mFormula(CHOICE ~  I(MEAN_TT/60) )  
f2 <- mFormula(CHOICE ~ -1 + I(MEAN_TT/60) + total_len + ncross_km)  
f3 <- mFormula(CHOICE ~ -1 + I(MEAN_TT/60) + I(SD_TT/60) + total_len + ncross_km)  
#f11 <- mFormula(CHOICE ~ MEAN_TT | TREATMENT)          # I CONSIDER THAT THIS IS THE CORRECT MODEL
#f22 <- mFormula(CHOICE ~ -1 + I(MEAN_TT/60)  | TREATMENT)  # I CONSIDER THAT THIS IS THE CORRECT MODEL
#f33 <- mFormula(CHOICE ~ MEAN_TT  + total_len + ncross_km | TREATMENT)  # I CONSIDER THAT THIS IS THE CORRECT MODEL

mod_mnlogit0 <- mlogit(f0  , data=choices_mnl_train)
mod_mnlogit1 <- mlogit(f1  , data=choices_mnl_train)
mod_mnlogit2 <- mlogit(f2  , data=choices_mnl_train)
mod_mnlogit3 <- mlogit(f3  , data=choices_mnl_train)
#mod_mnlogit11 <- mlogit(f11  , data=choices_mnl_train)
#mod_mnlogit22 <- mlogit(f22  , data=choices_mnl_train)
#mod_mnlogit33 <- mlogit(f33  , data=choices_mnl_train)

summary(mod_mnlogit0)
summary(mod_mnlogit1)
summary(mod_mnlogit2)
summary(mod_mnlogit3)
#summary(mod_mnlogit11)
#summary(mod_mnlogit22)
#summary(mod_mnlogit33)
lrtest(mod_mnlogit0,mod_mnlogit2)

# check:
colMeans(predict(mod_mnlogit2, choices_mnl_train))
apply(fitted(mod_mnlogit2, outcome = FALSE), 2, mean)

#
observed_choices_TRAIN <- table(data_model$CHOICE[!data_model$CHOICE_ID %in% testIds], data_model$PATH_NAME[!data_model$CHOICE_ID %in% testIds])
(obsfreq_train <- observed_choices_TRAIN[2,] / colSums(observed_choices_TRAIN))

observed_choicesTEST <- table(data_model$CHOICE[data_model$CHOICE_ID %in% testIds], data_model$PATH_NAME[data_model$CHOICE_ID %in% testIds])
(obsfreq_test <- observed_choicesTEST[2,] / colSums(observed_choicesTEST))



colMeans(predict(mod_mnlogit0, choices_mnl_test))
colMeans(predict(mod_mnlogit1, choices_mnl_test))
(predfreq <- colMeans(predict(mod_mnlogit2, choices_mnl_test)))
colMeans(predict(mod_mnlogit3, choices_mnl_test))

#colMeans(predict(mod_mnlogit11, choices_mnl_test))
#colMeans(predict(mod_mnlogit22, choices_mnl_test))

res <-
bind_cols(data.frame(route=names(obsfreq_train)),
          data.frame(null=obsfreq_train),
          data.frame(obs=obsfreq_test), 
          data.frame(pred=predfreq)
          )

res <- 
  res %>%
  gather(type, perc, -route) 

ggplot(res) +
  geom_col(aes(route,perc, fill=type), position="dodge") +
  theme_bw() 
  
  





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
predict(mod_mnlogit11, choices_mnl_train,type="probs")[1,]



