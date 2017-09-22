#####################################################################
# Compare models by prediction accuracy using CV
#####################################################################
source("r2_mnlogit_pre.R")

# OD and treatment
od <- "OD2"
treat <- "t3"

data_model <- 
  trips_res_play %>%
  filter(OD==od) %>%
  filter(TREATMENT==treat) %>%
  as.data.frame()


# Models
f0 <- mFormula(CHOICE ~ 1 )
f1 <- mFormula(CHOICE ~  -1+ I(MEAN_TT/60) )  
f1.1 <- mFormula(CHOICE ~  1+ I(MEAN_TT/60) )  

f2 <- mFormula(CHOICE ~ -1 +I(MEAN_TT/60))  
f3 <- mFormula(CHOICE ~ -1 + I(MEAN_TT/60) + total_len + ncross_km) 

# Choice IDs
# number of choices
choice_ids <- unique(data_model$CHOICE_ID)
length(choice_ids)

# randomize
choice_ids <- choice_ids[sample(length(choice_ids))]

#Create 10 equally size folds
nfolds <- 4
folds <- cut(seq(1, length(choice_ids)), breaks=nfolds, labels=FALSE)

results <- data.frame()

#Perform 10 fold cross validation
for(i in 1:nfolds){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- data_model[data_model$CHOICE_ID %in% choice_ids[testIndexes], ]
  trainData <- data_model[!data_model$CHOICE_ID %in% choice_ids[testIndexes], ]
  
  # Test and training data sets    
  choices_mnl_train <- mlogit.data(trainData, choice="CHOICE", shape="long", alt.var="PATH_NAME", chid.var = "CHOICE_ID", drop.index=TRUE)
  choices_mnl_test <- mlogit.data(testData, choice="CHOICE", shape="long", alt.var="PATH_NAME", chid.var = "CHOICE_ID", drop.index=TRUE)
  
  # Fit
  mod_mnlogit0 <- mlogit(f0  , data=choices_mnl_train)
  mod_mnlogit1 <- mlogit(f1  , data=choices_mnl_train)
  # I cant estimate a nested model!!!!!!!!!!!!!!!
  #mod_mnlogit1.1 <- mlogit(f1  , data=choices_mnl_train, 
  #                         nests = list(periph=c("R_test1_2"), inter=c("R_test1", "R_test1_3")))
  mod_mnlogit1.2 <- mlogit(f1  , data=choices_mnl_train, 
                           rpar = c('I(MEAN_TT/60)'="n"))
  #
  mod_probit1 <- mlogit(f1  , data=choices_mnl_train, probit = TRUE)
  
  # Pred
  pred_mean_mnlogit0 <- colMeans(predict(mod_mnlogit0, choices_mnl_test))
  pred_mean_mnlogit1 <- colMeans(predict(mod_mnlogit1, choices_mnl_test))
  pred_mean_mnlogit1.2 <- colMeans(predict(mod_mnlogit1.2, choices_mnl_test))
  #
  pred_mean_probit1 <- colMeans(predict(mod_probit1, choices_mnl_test))

  # Observed
  observed_dist <- table(testData$CHOICE, testData$PATH_NAME)[2,] / colSums(table(testData$CHOICE, testData$PATH_NAME))
  observed_dist <- data.frame(p=observed_dist)
  observed_dist$route <- row.names(observed_dist)
  observed_dist$type <- "observed"

  # Predicetd
  pred_mean_mnlogit0 <- data.frame(p=pred_mean_mnlogit0)
  pred_mean_mnlogit0$route <- row.names(pred_mean_mnlogit0)
  pred_mean_mnlogit0$type <- "null"

  #
  pred_mean_mnlogit1 <- data.frame(p=pred_mean_mnlogit1)
  pred_mean_mnlogit1$route <- row.names(pred_mean_mnlogit1)
  pred_mean_mnlogit1$type <- "mnl"
  #
  pred_mean_mnlogit1.2 <- data.frame(p=pred_mean_mnlogit1.2)
  pred_mean_mnlogit1.2$route <- row.names(pred_mean_mnlogit1.2)
  pred_mean_mnlogit1.2$type <- "mnl_rc"
  
  #
  pred_mean_probit1 <- data.frame(p=pred_mean_probit1)
  pred_mean_probit1$route <- row.names(pred_mean_probit1)
  pred_mean_probit1$type <- "probit"
  
  resultst <- 
    bind_rows(observed_dist, pred_mean_mnlogit0, pred_mean_mnlogit1, pred_mean_mnlogit1.2, pred_mean_probit1) %>%
    mutate(fold=i)
  results <- results %>% bind_rows(resultst)
  
}
results %>% 
  ggplot() +
  geom_col(aes(route, p, fill=type),position="dodge") +
  facet_grid(. ~ as.factor(fold))
  
# All data:
choices_mnl_all <- mlogit.data(data_model, choice="CHOICE", shape="long", alt.var="PATH_NAME", chid.var = "CHOICE_ID", drop.index=TRUE)

reflevel<-"R_N2"

mod_mnlogit0 <- mlogit(f0  , data=choices_mnl_all, reflevel=reflevel )
mod_mnlogit1 <- mlogit(f1  , data=choices_mnl_all, reflevel=reflevel)
mod_mnlogit1.1 <- mlogit(f1.1  , data=choices_mnl_all, reflevel=reflevel)
# I cant estimate a nested model!!!!!!!!!!!!!!!
#mod_mnlogit1.4 <- mlogit(f1  , data=choices_mnl_all, 
#                         nests = list(periph=c("R_test1_2"), inter=c("R_test1", "R_test1_3")))
mod_mnlogit1.2 <- mlogit(f1  , data=choices_mnl_all, 
                         rpar = c('I(MEAN_TT/60)'="n"), reflevel=reflevel)

mod_mnlogit1.2.1 <- mlogit(f1.1  , data=choices_mnl_all, 
                         rpar = c('I(MEAN_TT/60)'="n"), reflevel=reflevel)
#
mod_probit0 <- mlogit(f0  , data=choices_mnl_all, probit = TRUE, reflevel=reflevel)
mod_probit1 <- mlogit(f1  , data=choices_mnl_all, probit = TRUE, reflevel=reflevel)
mod_probit1.1 <- mlogit(f1.1  , data=choices_mnl_all, probit = TRUE, reflevel=reflevel)

summary(mod_mnlogit0)
summary(mod_mnlogit1)
summary(mod_mnlogit1.1)
summary(mod_mnlogit1.2)
summary(mod_mnlogit1.2.1)

#
summary(mod_probit0)
summary(mod_probit1)
summary(mod_probit1.1)

# The estimated Choleski factor L1 is :
L1 <- matrix(0, 2, 2)
L1[!upper.tri(L1)] <- coef(mod_probit1)[2:4]
# Multiplying L1 by its transpose gives Ω1 :
L1 %*% t(L1) # Covariance of the differences
  