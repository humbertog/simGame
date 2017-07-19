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

load("proc_alt.RData")
tab_d <- tab_d %>% 
  select(PATH_NAME=route, total_len, ncross) %>%
  filter(!duplicated(PATH_NAME)) %>% 
  mutate(ncross_km = ncross/ total_len * 1000)


######################################
# Creates the data set with the choices
######################################
# size of interval 
h <- 900

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
    filter(DEP_TIME >= t_ - h, DEP_TIME < t_ +0) %>%
    group_by(PATH_NAME) %>%
    summarise(MEAN_TT = mean(TT), SD_TT=sd(TT))
  
  trips_res_play_t <- 
    trips_res_play_t %>% 
    mutate(CHOICE_ID=i, 
           OD = od_,
           CHOSEN_PATH=chosen_,
           TREATMENT=treatment_,
           SESSION_ID = db_,
           CHOICE_TT = tt_,
           CHOICE_DEP_TIME = t_
    )
  
  trips_res_play <- bind_rows(trips_res_play, trips_res_play_t) 
}


trips_res_play <-
  trips_res_play %>%
  mutate(CHOICE=ifelse(PATH_NAME == CHOSEN_PATH, TRUE, FALSE))


trips_res_play <- 
  trips_res_play %>%
  left_join(tab_d)

# OBSERVE THAT THERE IS A PROBLEM WITH THE TT (IN SOME CASES IT IS LESS THAN ONE MINUTE)
# MAYBE IS BECAUSE THE USER REFRESH
trips_res_play %>%
  filter(CHOICE) %>%
  mutate(DEP_TIME_INT=cut(CHOICE_DEP_TIME, seq(1,5400,900))) %>%
  ggplot() + 
  geom_point(aes(MEAN_TT, CHOICE_TT, color=PATH_NAME)) +
  geom_abline(slope=1, intercept=0) 

#

trips_res_play %>%
  ggplot() + 
  geom_density(aes(MEAN_TT, fill=PATH_NAME), alpha=.3) +
  facet_grid(OD ~.)

# check!
not_complete <- 
  trips_res_play %>%
  group_by(CHOICE_ID) %>%
  summarise(N=n()) %>%
  filter(N!=3)

# we remove the choices that are not complete
trips_res_play <- 
  trips_res_play %>% 
  filter(!CHOICE_ID %in% not_complete$CHOICE_ID)

na_cases <- unique(trips_res_play$CHOICE_ID[which(is.na(trips_res_play$SD_TT))])

trips_res_play <- 
  trips_res_play %>% 
  filter(!CHOICE_ID %in% na_cases)




######################################
# mnlogit
######################################
od <- "OD1_1"
treat <- "t3"

data_model <- 
  trips_res_play %>%
  filter(OD==od) %>%
  filter(TREATMENT==treat) %>%
  as.data.frame()


f0 <- mFormula(CHOICE ~ 1 )
f1 <- mFormula(CHOICE ~  I(MEAN_TT/60) )  
f2 <- mFormula(CHOICE ~ -1 + I(MEAN_TT/60) + total_len + ncross_km)  
f3 <- mFormula(CHOICE ~ -1 + I(MEAN_TT/60) +I(SD_TT/60)+ total_len + ncross_km) 

#f11 <- mFormula(CHOICE ~ MEAN_TT | TREATMENT)          # I CONSIDER THAT THIS IS THE CORRECT MODEL
#f22 <- mFormula(CHOICE ~ MEAN_TT + SD_TT | TREATMENT)  # I CONSIDER THAT THIS IS THE CORRECT MODEL

#####
err_tests <- data.frame()
size_boots <- .2
unique_chID <- unique(data_model$CHOICE_ID)

n_boots <- 500
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
  mod_mnlogit2 <- mlogit(f2  , data=choices_mnl_train, na.action = na.omit)
  mod_mnlogit3 <- mlogit(f3  , data=choices_mnl_train, na.action = na.omit)
  #mod_mnlogit11 <- mlogit(f11  , data=choices_mnl_train)
  #mod_mnlogit22 <- mlogit(f22  , data=choices_mnl_train)
  
  observed_dist <- table(testData$CHOICE, testData$PATH_NAME)[2,] / colSums(table(testData$CHOICE, testData$PATH_NAME))
  
  pred_mean_mnlogit0 <- colMeans(predict(mod_mnlogit0, choices_mnl_test))
  pred_mean_mnlogit1 <- colMeans(predict(mod_mnlogit1, choices_mnl_test))
  pred_mean_mnlogit2 <- colMeans(predict(mod_mnlogit2, choices_mnl_test))
  pred_mean_mnlogit3 <- colMeans(predict(mod_mnlogit3, choices_mnl_test))
  
  
  len_test <- sum(testData$CHOICE)
  err_df <- data.frame(
    chisq.test(rbind(pred_mean_mnlogit0*len_test, observed_dist*len_test), simulate.p.value = TRUE)$p.value,
    chisq.test(rbind(pred_mean_mnlogit1*len_test, observed_dist*len_test), simulate.p.value = TRUE)$p.value,
    chisq.test(rbind(pred_mean_mnlogit2*len_test, observed_dist*len_test), simulate.p.value = TRUE)$p.value,
    chisq.test(rbind(pred_mean_mnlogit3*len_test, observed_dist*len_test), simulate.p.value = TRUE)$p.value
    
  )
  
  
  err_tests <- rbind(err_tests, err_df)
}

names(err_tests) <- c("mod0","mod1","mod2","mod3")

apply(err_tests, 2, mean)
apply(err_tests, 2, sd)
apply(err_tests, 2, max)
apply(err_tests, 2, min)

err_tests2 <-
  err_tests %>%
    gather() %>%
    arrange(value) 

ggplot(err_tests2) +
  geom_density(aes(value, colour=key))

ggplot(err_tests2, aes(value, colour=key)) +
  stat_bin(data=subset(err_tests,key=="mod0"),aes(y=cumsum(..count..)),geom="step")+
  stat_bin(data=subset(err_tests,key=="mod1"),aes(y=cumsum(..count..)),geom="step")+
  stat_bin(data=subset(err_tests,key=="mod2"),aes(y=cumsum(..count..)),geom="step")+
  stat_bin(data=subset(err_tests,key=="mod3"),aes(y=cumsum(..count..)),geom="step")


