#####################################################################
# We fit a BAYESIAN multinomial logistic model to the choices. 

#####################################################################
library(rjags)
library(tidyverse)
library(mlogit)

source("./R_data/choice_datapre_201704.R")
choices <- 
  choices %>% 
  arrange(PLAYER_ID)


choices_t %>%
  filter(CHOSEN==TRUE) %>%
  group_by(PLAYER_ID)
  




#########  Data
treat <- "t3"
od <- "O1D1"

y1 <- c(); y2 <- c(); y3 <- c()
itt1 <- c(); itt2 <- c();itt3 <- c()
tlen1 <-c(); tlen2 <- c(); tlen3 <- c()
ncross1 <- c(); ncross2 <- c(); ncross3 <- c()


for (id in unique(choices$CHOICE_ID)) {
  choice_problem <- 
    choices %>%
    filter(CHOICE_ID == id, TREATMENT == treat, OD==od) 
  
  name_center <- paste(od, "center", sep="_")
  name_north <- paste(od, "north", sep="_")
  name_south <- paste(od, "south", sep="_")
  
  y1 <- c(y1, choice_problem$CHOSEN[choice_problem$PATH_NAME == name_center] * 1 )
  y2 <- c(y2, choice_problem$CHOSEN[choice_problem$PATH_NAME == name_north] * 1 )
  y3 <- c(y3, choice_problem$CHOSEN[choice_problem$PATH_NAME == name_south] * 1 )
  
  itt1 <- c(itt1, choice_problem$TT_INFO_std[choice_problem$PATH_NAME == name_center] )
  itt2 <- c(itt2, choice_problem$TT_INFO_std[choice_problem$PATH_NAME == name_north]  )
  itt3 <- c(itt3, choice_problem$TT_INFO_std[choice_problem$PATH_NAME == name_south]  )
  
  tlen1 <- c(tlen1, choice_problem$total_len_std[choice_problem$PATH_NAME == name_center] )
  tlen2 <- c(tlen2, choice_problem$total_len_std[choice_problem$PATH_NAME == name_north]  )
  tlen3 <- c(tlen3, choice_problem$total_len_std[choice_problem$PATH_NAME == name_south]  )
  
  ncross1 <- c(ncross1, choice_problem$ncross_km_std[choice_problem$PATH_NAME == name_center] )
  ncross2 <- c(ncross2, choice_problem$ncross_km_std[choice_problem$PATH_NAME == name_north]  )
  ncross3 <- c(ncross3, choice_problem$ncross_km_std[choice_problem$PATH_NAME == name_south]  )
  
}

choice_problem <- 
  choices %>%
  filter(CHOSEN==TRUE, TREATMENT == treat, OD==od) 


iniidx <- c() ; endidx <- c()
for (id in unique(choice_problem$PLAYER_ID)) {
  iniidx <- c(iniidx, min(which(choice_problem$PLAYER_ID == id)))
  endidx <- c(endidx, max(which(choice_problem$PLAYER_ID == id)))
}

y <- matrix(c(y1,y2,y3), ncol=3)
itt <- matrix(c(itt1,itt2,itt3), ncol=3)
tlen <- matrix(c(tlen1,tlen2,tlen3), ncol=3)
ncross <- matrix(c(ncross1,ncross2,ncross3), ncol=3)

data.jags <- list(y = y, itt=itt, tlen=tlen, ncross=ncross, N=length(y[,1]), iniidx=iniidx, endidx=endidx, Nind=length(iniidx))


# Data for mlogit
data_model <- 
  choices %>%
  filter(OD==od) %>%
  filter(TREATMENT==treat) %>%
  #filter(compliance_rate>.5) %>%
  as.data.frame()

choices_mnl <- mlogit.data(data_model, choice="CHOSEN", shape="long", alt.var="PATH_NAME", chid.var = "CHOICE_ID", id="PLAYER_ID", drop.index=TRUE)
