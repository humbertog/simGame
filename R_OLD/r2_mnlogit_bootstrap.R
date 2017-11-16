#####################################################################
# Assess the predictive accuracy of our mnlogit model by performing a bootstrap 
# validation of our model. 
#####################################################################
source("r2_mnlogit_pre.R")

od <- "OD1_1"
treat <- "t3"

data_model <- 
  trips_res_play %>%
  filter(OD==od) %>%
  filter(TREATMENT==treat) %>%
  as.data.frame()


f0 <- mFormula(CHOICE ~ 1 )
f1 <- mFormula(CHOICE ~  I(MEAN_TT/60) )  
f2 <- mFormula(CHOICE ~ -1 +I(MEAN_TT/60))  
f3 <- mFormula(CHOICE ~ -1 + I(MEAN_TT/60) +I(SD_TT/60)+ total_len + ncross_km) 

#f11 <- mFormula(CHOICE ~ MEAN_TT | TREATMENT)          # I CONSIDER THAT THIS IS THE CORRECT MODEL
#f22 <- mFormula(CHOICE ~ MEAN_TT + SD_TT | TREATMENT)  # I CONSIDER THAT THIS IS THE CORRECT MODEL

#####

obs_dists <- data.frame()
pred_dists <- data.frame()
pred_dists_null <- data.frame()

size_boots <- .2
unique_chID <- unique(data_model$CHOICE_ID)

n_boots <- 200
for(i in 1:n_boots){
  if(i %% 50==0) {
    print(i)
  }
  
  test_ids <- sample(unique_chID, floor(length(unique_chID)*size_boots ), replace = TRUE)
  trainData <- data_model[!data_model$CHOICE_ID %in% test_ids, ]
  testData <- data_model[data_model$CHOICE_ID %in% test_ids, ]
  
  # Test and training data sets    
  choices_mnl_train <- mlogit.data(trainData, choice="CHOICE", shape="long", alt.var="PATH_NAME", chid.var = "CHOICE_ID", drop.index=TRUE)
  choices_mnl_test <- mlogit.data(testData, choice="CHOICE", shape="long", alt.var="PATH_NAME", chid.var = "CHOICE_ID", drop.index=TRUE)
  
  mod_mnlogit0 <- mlogit(f0  , data=choices_mnl_train)
  mod_mnlogit1 <- mlogit(f1  , data=choices_mnl_train)
  mod_mnlogit2 <- mlogit(f2  , data=choices_mnl_train, na.action = na.omit,heterosc = FALSE)
  mod_mnlogit3 <- mlogit(f3  , data=choices_mnl_train, na.action = na.omit)
  #mod_mnlogit11 <- mlogit(f11  , data=choices_mnl_train)
  #mod_mnlogit22 <- mlogit(f22  , data=choices_mnl_train)
  
  pred_mean_mnlogit0 <- colMeans(predict(mod_mnlogit0, choices_mnl_test))
  pred_mean_mnlogit1 <- colMeans(predict(mod_mnlogit1, choices_mnl_test))
  pred_mean_mnlogit2 <- colMeans(predict(mod_mnlogit2, choices_mnl_test))
  pred_mean_mnlogit3 <- colMeans(predict(mod_mnlogit3, choices_mnl_test))
  
  # Observed
  observed_dist <- table(testData$CHOICE, testData$PATH_NAME)[2,] / colSums(table(testData$CHOICE, testData$PATH_NAME))
  obs_dists <- rbind(obs_dists, observed_dist)
  names(obs_dists) <- unique(data_model$PATH_NAME)
  
  # Predicetd
  pred_dists_null <- rbind(pred_dists_null, pred_mean_mnlogit0)
  pred_dists <- rbind(pred_dists, pred_mean_mnlogit2)
  names(pred_dists_null) <- unique(data_model$PATH_NAME)
  names(pred_dists) <- unique(data_model$PATH_NAME)

}

chisq_dist <- function(x,y){
  sum((x-y)^2 / (1/2 * (x + y)))
}


err <- data.frame(null_model=c(), prop_model=c())
for (i in 1:n_boots) {
  err_ <- data.frame( null_model=chisq_dist(obs_dists[i,], pred_dists_null[i,]),
                      prop_model=chisq_dist(obs_dists[i,], pred_dists[i,]))
  err <- rbind(err, err_)
}
#err

mean_error <- colMeans(err)

ggplot(err) +
  geom_line(aes(seq(1:n_boots), null_model), stat = "identity", alpha=.8) +
  geom_line(aes(seq(1:n_boots), prop_model), stat = "identity", colour="blue", alpha=.8) +
  geom_hline(yintercept=mean_error[2], colour="red") +
  theme_bw() +
  ylim(0,.7) +
  labs(x="iteration number", y="error")
  
  





