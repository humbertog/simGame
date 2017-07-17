#####################################################################
# Assess the predictive accuracy of our mnlogit model by performing a bootstrap 
# validation of our model. 
#####################################################################
library(tidyverse)
library(mlogit)

# Read the res_files
source("r0_config.R")
source("r1_readTrips_res.R")
source("r1_readTrips_player.R")



######################################
# Creates the data set with the choices
######################################
# size of interval 
h <- 600

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
#chTabl <- table(trips_res_play$CHOICE, trips_res_play$CHOICE_ID)
#sum(chTabl[1,] != 2)
#chTabl[,chTabl[1,] != 2]

#trips_res_play %>% filter(CHOICE_ID == 367)

# we remove the two choices that are not complete
trips_res_play <- 
  trips_res_play %>% 
    filter(!CHOICE_ID %in% c(290, 367,366, 699, 712))

chTabl <- table(trips_res_play$CHOICE, trips_res_play$CHOICE_ID)
sum(chTabl[1,] != 2)

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


f0 <- mFormula(CHOICE ~ 1 )
f1 <- mFormula(CHOICE ~ MEAN_TT )  
f2 <- mFormula(CHOICE ~ MEAN_TT + SD_TT)  
f11 <- mFormula(CHOICE ~ MEAN_TT | TREATMENT)          # I CONSIDER THAT THIS IS THE CORRECT MODEL
f22 <- mFormula(CHOICE ~ MEAN_TT + SD_TT | TREATMENT)  # I CONSIDER THAT THIS IS THE CORRECT MODEL

#####
err_tests <- data.frame()
size_boots <- .20
unique_chID <- unique(data_model$CHOICE_ID)

n_boots <- 500
for(i in 1:n_boots){
  if(i %% 50==0) {
    print(i)
  }
  
  test_ids <- sample(unique_chID, floor(length(unique_chID)*size_boots ), replace = FALSE)
  trainData <- data_model[!data_model$CHOICE_ID %in% test_ids, ]
  testData <- data_model[data_model$CHOICE_ID %in% test_ids, ]
  
  
  # Test and training data sets    
  choices_mnl_train <- mlogit.data(trainData, choice="CHOICE", shape="long", alt.var="PATH_NAME", chid.var = "CHOICE_ID", drop.index=TRUE)
  choices_mnl_test <- mlogit.data(testData, choice="CHOICE", shape="long", alt.var="PATH_NAME", chid.var = "CHOICE_ID", drop.index=TRUE)
  
  mod_mnlogit0 <- mlogit(f0  , data=choices_mnl_train)
  mod_mnlogit1 <- mlogit(f1  , data=choices_mnl_train)
  mod_mnlogit2 <- mlogit(f2  , data=choices_mnl_train, na.action = na.omit)
  mod_mnlogit11 <- mlogit(f11  , data=choices_mnl_train)
  mod_mnlogit22 <- mlogit(f22  , data=choices_mnl_train)
  
  observed_dist <- table(testData$CHOICE, testData$PATH_NAME)[2,] / colSums(table(testData$CHOICE, testData$PATH_NAME))
  
  pred_mean_mnlogit0 <- colMeans(predict(mod_mnlogit0, choices_mnl_test))
  pred_mean_mnlogit1 <- colMeans(predict(mod_mnlogit1, choices_mnl_test))
  pred_mean_mnlogit2 <- colMeans(predict(mod_mnlogit2, choices_mnl_test))
  pred_mean_mnlogit11 <- colMeans(predict(mod_mnlogit11, choices_mnl_test))
  pred_mean_mnlogit22 <- colMeans(predict(mod_mnlogit22, choices_mnl_test))
  
  len_test <- sum(testData$CHOICE)
  err_df <- data.frame(
    chisq.test(rbind(pred_mean_mnlogit0*len_test, observed_dist*len_test), simulate.p.value = TRUE)$p.value,
    chisq.test(rbind(pred_mean_mnlogit1*len_test, observed_dist*len_test), simulate.p.value = TRUE)$p.value,
    chisq.test(rbind(pred_mean_mnlogit2*len_test, observed_dist*len_test), simulate.p.value = TRUE)$p.value,
    chisq.test(rbind(pred_mean_mnlogit11*len_test, observed_dist*len_test), simulate.p.value = TRUE)$p.value,
    chisq.test(rbind(pred_mean_mnlogit22*len_test, observed_dist*len_test), simulate.p.value = TRUE)$p.value
  )
  
  
  err_tests <- rbind(err_tests, err_df)
}

mean_error<-apply(err_tests, 2, mean)
sd_error <- apply(err_tests, 2, sd)
names(mean_error) <- c("0","1","2","11","22")
names(sd_error) <- c("0","1","2","11","22")
mean_error
sd_error
