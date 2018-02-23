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




#########  Data
treat <- "t3"
od <- "O3D2"

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


#####################################################################
# Mixture V_ij = b * ITT_ij  Looking for groups of compliance
#####################################################################

model.jags <- "
model{
for (i in 1:N){ 
y[i,1:3] ~ dmulti(p[i,1:3],1)
pp[i,1] <- p[i,1] /s[i]
pp[i,2] <- p[i,2] /s[i]
pp[i,3] <- p[i,3] /s[i]

s[i] <- p[i,1] + p[i,2] + p[i,3]
#
p[i,1] <- exp(b1[i] * itt[i,1])
p[i,2] <- exp(b1[i] * itt[i,2])
p[i,3] <- exp(b1[i] * itt[i,3])

b1[i] <- muOfClust[ clust[i] + 1 ]

clust[i] ~ dbern( pg )

} 
# Priors
pg ~ dbeta(1,1)

muOfClust[1] ~ dnorm( -1 , 1.0E-1 )
muOfClust[2] ~ dnorm( -19 , 1.0E-1 )
}
"


# Running the model
model <- jags.model(textConnection(model.jags), data = data.jags, n.chains = 4, n.adapt= 20000)

update(model, 20000); # Burnin 

mcmc_samples <- coda.samples(model, 
                             variable.names=c("pg", "muOfClust"), 
                             n.iter=20000)



# Summaries
summary(mcmc_samples)
plot(mcmc_samples)
HPDinterval(mcmc_samples)


plot(as.numeric(mcmc_samples[[1]][,1]),as.numeric(mcmc_samples[[1]][,2]))
plot(as.numeric(mcmc_samples[[1]][,1]),as.numeric(mcmc_samples[[1]][,3]))
plot(as.numeric(mcmc_samples[[1]][,2]),as.numeric(mcmc_samples[[1]][,3]))


mean(mcmc_samples[[1]][,1])

mean(mcmc_samples[[1]][,2:93])
mean(mcmc_samples[[1]][,94:185])
mean(mcmc_samples[[1]][,186:277])

# Non bayesian
f1_std <- mFormula(CHOSEN ~  -1+ TT_INFO_std )  
mod_mnlogit1_std <- mlogit(f1_std  , data=choices_mnl)

summary(mod_mnlogit1_std)

colMeans(predict(mod_mnlogit1_std, newdata = choices_mnl))



#####################################################################
# V_ij = bi * ITT_ij + X
#####################################################################

model.jags <- "
model{
for(i in 1:Nind) {
for(j in iniidx[i]:endidx[i]) {
y[j,1:3] ~ dmulti(p[j,1:3],1)

pp[j,1] <- p[j,1] /s[j]
pp[j,2] <- p[j,2] /s[j]
pp[j,3] <- p[j,3] /s[j]

s[j] <- p[j,1] + p[j,2] + p[j,3]
#
p[j,1] <- exp(b1[i] * itt[j,1] + b2 * tlen[j,1]  + b3 * ncross[j,1])
p[j,2] <- exp(b1[i] * itt[j,2] + b2 * tlen[j,2]  + b3 * ncross[j,2])
p[j,3] <- exp(b1[i] * itt[j,3] + b2 * tlen[j,3]  + b3 * ncross[j,3])
}
b1[i] ~ dnorm(mu,tau)
}
mu ~ dnorm(0, 1.0E-4)
sigma <- 1.0 / sqrt(tau)
tau ~ dgamma(1.0E-3, 1.0E-3)

b2 ~ dnorm(0, 1.0E-4)
b3 ~ dnorm(0, 1.0E-4)

}
"

# Running the model
model <- jags.model(textConnection(model.jags), data = data.jags, n.chains = 4, n.adapt= 20000)

update(model, 100000); # Burnin 

mcmc_samples <- coda.samples(model, 
                             variable.names=c("mu", "sigma","pp"), 
                             n.iter=20000)

# Summaries
summary(mcmc_samples)
plot(mcmc_samples)
HPDinterval(mcmc_samples)

mean(mcmc_samples[[1]][,1])

mean(mcmc_samples[[2]][,2:114])
mean(mcmc_samples[[2]][,115:227])
mean(mcmc_samples[[2]][,228:340])




data_model %>%
  filter(CHOSEN==TRUE) %>%
  group_by(PLAYER_ID) %>%
  summarise(n=n()) %>%
  ggplot() +
  geom_bar(aes(n))


# Non bayesian
f1_std <- mFormula(CHOSEN ~  TT_INFO_std + total_len_std +ncross_km_std  |0 )  
mod_mnlogit1_std <- mlogit(f1_std  , data=choices_mnl, rpar = c(TT_INFO_std="n"), R=100, halton=NA, print.level=0, panel=TRUE)

summary(mod_mnlogit1_std)

colMeans(predict(mod_mnlogit1_std, newdata = choices_mnl))



#####################################################################
# V_ij = bi * ITT_ij 
#####################################################################

model.jags <- "
  model{
      for(i in 1:Nind) {
        for(j in iniidx[i]:endidx[i]) {
          y[j,1:3] ~ dmulti(p[j,1:3],1)

          pp[j,1] <- p[j,1] /s[j]
          pp[j,2] <- p[j,2] /s[j]
          pp[j,3] <- p[j,3] /s[j]

          s[j] <- p[j,1] + p[j,2] + p[j,3]
#
          p[j,1] <- exp(b1[i] * itt[j,1])
          p[j,2] <- exp(b1[i] * itt[j,2])
          p[j,3] <- exp(b1[i] * itt[j,3])
        }
        b1[i] ~ dnorm(mu,tau)
      }
      mu ~ dnorm(0, 1.0E-4)
      sigma <- 1.0 / sqrt(tau)
      tau ~ dgamma(1.0E-3, 1.0E-3)
      
    }
"

# Running the model
model <- jags.model(textConnection(model.jags), data = data.jags, n.chains = 4, n.adapt= 20000)

update(model, 20000); # Burnin 

mcmc_samples <- coda.samples(model, 
                             variable.names=c("mu", "sigma"), 
                             n.iter=20000)
#a<-unlist(as.data.frame(mcmc_samples[[1]]))
#hist(a, 200)

# Summaries
summary(mcmc_samples)
plot(mcmc_samples)
HPDinterval(mcmc_samples)

mean(mcmc_samples[[1]][,1])

mean(mcmc_samples[[1]][,2:93])
mean(mcmc_samples[[1]][,94:185])
mean(mcmc_samples[[1]][,186:277])



data_model %>%
  filter(CHOSEN==TRUE) %>%
  group_by(PLAYER_ID) %>%
  summarise(n=n()) %>%
  ggplot() +
  geom_bar(aes(n))


# Non bayesian
f1_std <- mFormula(CHOSEN ~  TT_INFO_std|0 )  
mod_mnlogit1_std <- mlogit(f1_std  , data=choices_mnl, rpar = c(TT_INFO_std="n"), R=100, halton=NA, print.level=0, panel=TRUE)

summary(mod_mnlogit1_std)

colMeans(predict(mod_mnlogit1_std, newdata = choices_mnl))








#####################################################################
# V_ij = b * ITT_ij 
#####################################################################

model.jags <- "
model{
for (i in 1:N){ 
y[i,1:3] ~ dmulti(p[i,1:3],1)
pp[i,1] <- p[i,1] /s[i]
pp[i,2] <- p[i,2] /s[i]
pp[i,3] <- p[i,3] /s[i]

s[i] <- p[i,1] + p[i,2] + p[i,3]
#
p[i,1] <- exp(b1 * itt[i,1])# + b2 * tlen[i,1] + b3 * ncross[i,1])
p[i,2] <- exp(b1 * itt[i,2])# + b2 * tlen[i,2] + b3 * ncross[i,2] )
p[i,3] <- exp(b1 * itt[i,3])# + b2 * tlen[i,3] + b3 * ncross[i,3] )
} 
b1 ~ dnorm(0, 1.0E-4)
#b1 ~ dlnorm(0,1.0E-4)
# b2 ~ dnorm(0, 1.0E-4)
#b3 ~ dnorm(0, 1.0E-4)

}
"

# Running the model
model <- jags.model(textConnection(model.jags), data = data.jags, n.chains = 4, n.adapt= 20000)

update(model, 20000); # Burnin 

mcmc_samples <- coda.samples(model, 
                             variable.names=c("b1"), 
                             n.iter=20000)



# Summaries
summary(mcmc_samples)
plot(mcmc_samples)
HPDinterval(mcmc_samples)


plot(as.numeric(mcmc_samples[[1]][,1]),as.numeric(mcmc_samples[[1]][,2]))
plot(as.numeric(mcmc_samples[[1]][,1]),as.numeric(mcmc_samples[[1]][,3]))
plot(as.numeric(mcmc_samples[[1]][,2]),as.numeric(mcmc_samples[[1]][,3]))


mean(mcmc_samples[[1]][,1])

mean(mcmc_samples[[1]][,2:93])
mean(mcmc_samples[[1]][,94:185])
mean(mcmc_samples[[1]][,186:277])

# Non bayesian
f1_std <- mFormula(CHOSEN ~  -1+ TT_INFO_std )  
mod_mnlogit1_std <- mlogit(f1_std  , data=choices_mnl)

summary(mod_mnlogit1_std)

colMeans(predict(mod_mnlogit1_std, newdata = choices_mnl))











#####################################################################
# Compliance rate
# V_ij = b * c_i * ITT_ij 

model_cr.jags <- "
  model{
for (i in 1:N){ 
y[i,1:3] ~ dmulti(p[i,1:3],1)

#
p1[i,1] <- p[i,1] /s[i]
p2[i,2] <- p[i,2] /s[i]
p3[i,3] <- p[i,3] /s[i]

s[i] <- p[i,1] + p[i,2] + p[i,3]

p[i,1] <- exp(b * cr[i] * itt[i,1] )
p[i,2] <- exp(b * cr[i] * itt[i,2] )
p[i,3] <- exp(b * cr[i] * itt[i,3] )

cr[i] ~ dbern(pc)

} 
b ~ dnorm(0, 1.0E-3)
pc ~ dbeta(1,1)
}
"


# Running the model
model_cr <- jags.model(textConnection(model_cr.jags), data = data.jags, n.chains = 4, n.adapt= 20000)

update(model_cr, 20000); # Burnin 

mcmc_samples_cr <- coda.samples(model_cr, variable.names=c("b", "pc", "p1", "p2", "p3"), n.iter=20000)

mean(mcmc_samples_cr[[1]][,1])
mean(mcmc_samples_cr[[1]][,368])

mean(mcmc_samples_cr[[1]][,2:123])
mean(mcmc_samples_cr[[1]][,124:245])
mean(mcmc_samples_cr[[1]][,246:367])

summary(mcmc_samples_cr)
plot(mcmc_samples_cr)

choices %>%
  filter(TREATMENT == treat, OD==od, CHOSEN==TRUE) %>%
  ggplot() +
  geom_histogram(aes(compliance_rate), bins = 10)


# Non bayesian
f1_std <- mFormula(CHOSEN ~  -1+ TT_INFO_std )  
mod_mnlogit1_std <- mlogit(f1_std  , data=choices_mnl)

summary(mod_mnlogit1_std)


colMeans(predict(mod_mnlogit1_std, newdata = choices_mnl))



