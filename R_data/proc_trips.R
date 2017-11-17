############################################################
# Processes the trip data
# 
############################################################
library(chron)
library(tidyverse)

process_bdd <- function(bdd_file) {
  # Read csv file
  bdd <- read.csv(bdd_file, sep=";", stringsAsFactors = FALSE)
  dim(bdd)
  head(bdd)
  
  ############################################################
  # Name the routes
  # R_test1 - T_58228870_FRef     
  # R_test1_2 - T_58228871_FRef   
  # R_test1_3 - T_58229465_FRef  
  # R_test2 - T_61618086_FRef
  # R_test2_2 - T_1036736972_FRef
  # R_test2_3 - T_58445242_FRef
  # R_N1 - T_61618126_FRef
  # R_N2 - T_58230106_toRef
  # R_N3 - T_58232207_FRef_F
  
  routes_ids <- data.frame(
    E_test1=unlist(lapply(bdd[,"Iti1"], function(x) strsplit(x, " ", fixed = FALSE, perl = FALSE, useBytes = FALSE)[[1]][3])),
    E_test2=unlist(lapply(bdd[,"Iti1"], function(x) strsplit(x, " ", fixed = FALSE, perl = FALSE, useBytes = FALSE)[[1]][4])),
    E_test3=unlist(lapply(bdd[,"Iti1"], function(x) strsplit(x, " ", fixed = FALSE, perl = FALSE, useBytes = FALSE)[[1]][13])),
    stringsAsFactors = FALSE
  )
  
  routes_ids$route <- NA
  routes_ids$route[routes_ids$E_test1 == 'T_58228870_FRef'] <- 'R_test1'
  routes_ids$route[routes_ids$E_test1 == 'T_58228871_FRef'] <- 'R_test1_2'
  routes_ids$route[routes_ids$E_test1 == 'T_58229465_FRef'] <- 'R_test1_3'
  
  routes_ids$route[routes_ids$E_test2 == 'T_61618086_FRef'] <- 'R_test2'
  routes_ids$route[routes_ids$E_test2 == 'T_1036736972_FRef'] <- 'R_test2_2'
  routes_ids$route[routes_ids$E_test2 == 'T_58445242_FRef'] <- 'R_test2_3'
  
  routes_ids$route[routes_ids$E_test3 == 'T_61618126_FRef'] <- 'R_N1'
  routes_ids$route[routes_ids$E_test3 == 'T_58230106_toRef'] <- 'R_N2'
  routes_ids$route[routes_ids$E_test3 == 'T_58232207_FRef_F' | routes_ids$E_test3 == 'T_58232219_FRef'] <- 'R_N3'
  
  # The number of cars in each route
  table(routes_ids$route)
  routes <- data.frame(path=routes_ids$route, route_nom=NA, od=NA)
  
  routes$route_nom[routes$path == "R_test1_2" | routes$path == "R_test2_2"] <- "OD1_nord"
  routes$route_nom[routes$path == "R_test1" | routes$path == "R_test2_3"] <- "OD1_mid"
  routes$route_nom[routes$path == "R_test1_3" | routes$path == "R_test2"] <- "OD1_sud"
  
  routes$route_nom[routes$path == "R_N1" ] <- "OD2_nord"
  routes$route_nom[routes$path == "R_N2" ] <- "OD2_mid"
  routes$route_nom[routes$path == "R_N3" ] <- "OD2_sud"
  
  routes$od[routes$route_nom %in% c("OD1_nord", "OD1_mid", "OD1_sud")] <- "OD1"
  routes$od[routes$route_nom %in% c("OD2_nord", "OD2_mid", "OD2_sud")] <- "OD2"
  
  bdd$OD[bdd$Origine == "E_test1"] <- "OD_1"
  bdd$OD[bdd$Origine == "E_test2"] <- "OD1_2"
  bdd$OD[bdd$Origine == "E_test3"] <- "OD2"  
  
  table(routes$route_nom)
  bdd <-cbind(bdd, routes)
  
  
  ############################################################
  # Check the times
  ############################################################
  
  class(bdd$Heure_Depart)
  class(bdd$Heure_Arrivee)
  
  
  bdd$Heure_Depart <- chron(times=bdd$Heure_Depart)
  bdd$Heure_Arrivee <- chron(times=bdd$Heure_Arrivee)
  
  bdd$Heure_Depart_sec <- ( hours(bdd$Heure_Depart) * 3600 + minutes(bdd$Heure_Depart) * 60 + seconds(bdd$Heure_Depart) ) - 3600 * 6 - 30 * 60
  bdd$Heure_Arrivee_sec <- ( hours(bdd$Heure_Arrivee) * 3600 + minutes(bdd$Heure_Arrivee) * 60 + seconds(bdd$Heure_Arrivee) ) - 3600 * 6 - 30 * 60
  
  
  # obtain TT
  
  bdd$tt_secs <- minutes(bdd$Heure_Arrivee - bdd$Heure_Depart ) * 60 + seconds(bdd$Heure_Arrivee - bdd$Heure_Depart )
  bdd$tt_mins <- bdd$tt_secs / 60
  
  # cut 
  bdd$period <- cut(bdd$Heure_Depart, c(times("06:30:00"), times("07:00:00"), times("07:10:00"), 
                                        times("07:20:00"), times("07:30:00"), times("07:40:00"),
                                        times("07:50:00"),times("08:00:00")),
                    labels = c("warmup", "00-10", "10-20", "20-30","30-40", "40-50", "50-60"))
  
  #bdd$period <- cut(bdd$Heure_Depart, c(times("06:30:00"), times("07:00:00"), times("07:15:00"), 
  #                                      times("07:30:00"), times("07:45:00"), times("08:00:00")),
  #                  labels = c("warmup", "00-15", "15-30", "30-45","45-60"))
  
  
  bdd$DEP_TIME <- bdd$Heure_Depart_sec
  bdd$ARR_TIME <- bdd$Heure_Arrivee_sec
  bdd$route <- bdd$path
  bdd
}

bdd <- read.csv("/Users/humberto.gonzalez/simulation_game/exp_20170412/results/data_tripSet/Trips2.csv", sep=";", stringsAsFactors = FALSE)
names(bdd)[2] <- "name"

#####  Names of the routes
routes_ids <- data.frame(
  E_test1=unlist(lapply(bdd[,"sections"], function(x) strsplit(x, " ", fixed = FALSE, perl = FALSE, useBytes = FALSE)[[1]][3])),
  E_test2=unlist(lapply(bdd[,"sections"], function(x) strsplit(x, " ", fixed = FALSE, perl = FALSE, useBytes = FALSE)[[1]][4])),
  E_test3=unlist(lapply(bdd[,"sections"], function(x) strsplit(x, " ", fixed = FALSE, perl = FALSE, useBytes = FALSE)[[1]][13])),
  stringsAsFactors = FALSE
)

routes_ids$route <- NA
routes_ids$route[routes_ids$E_test1 == 'T_58228870_FRef'] <- 'R_test1'
routes_ids$route[routes_ids$E_test1 == 'T_58228871_FRef'] <- 'R_test1_2'
routes_ids$route[routes_ids$E_test1 == 'T_58229465_FRef'] <- 'R_test1_3'

routes_ids$route[routes_ids$E_test2 == 'T_61618086_FRef'] <- 'R_test2'
routes_ids$route[routes_ids$E_test2 == 'T_1036736972_FRef'] <- 'R_test2_2'
routes_ids$route[routes_ids$E_test2 == 'T_58445242_FRef'] <- 'R_test2_3'

routes_ids$route[routes_ids$E_test3 == 'T_61618126_FRef'] <- 'R_N1'
routes_ids$route[routes_ids$E_test3 == 'T_58230106_toRef'] <- 'R_N2'
routes_ids$route[routes_ids$E_test3 == 'T_58232207_FRef_F' | routes_ids$E_test3 == 'T_58232219_FRef'] <- 'R_N3'

table(routes_ids$route)
sum(table(routes_ids$route))
dim(routes_ids)

# 
bdd$route <- routes_ids$route

##### treatments
bdd$treatment <- substr(bdd$name, 9,10)
bdd$session <- substr(bdd$name, 1,3)

# check if users ids correspond to the session ids
table(bdd$session, bdd$sessionId)

##### ods
bdd$OD <- NA
bdd$OD[bdd$route %in% c("R_test1", "R_test1_2", "R_test1_3")] <- "OD1_1"
bdd$OD[bdd$route %in% c("R_test2", "R_test2_2", "R_test2_3")] <- "OD1_2"
bdd$OD[bdd$route %in% c("R_N1", "R_N2", "R_N3")] <- "OD2"

bdd$D <- NA
bdd$D[bdd$session %in% c("s01", "s02")] <- "D2"
bdd$D[bdd$session %in% c("s03", "s04")] <- "D6"

##### Time
class(bdd$initial.departure.time)
class(bdd$modified.departure.time)
class(bdd$initial.arrival.time)
class(bdd$made.arrival.time)

bdd$initial.departure.time <- chron(times=bdd$initial.departure.time)
bdd$modified.departure.time <- chron(times=bdd$modified.departure.time)
bdd$initial.arrival.time <- chron(times=bdd$initial.arrival.time)
bdd$made.arrival.time <- chron(times=bdd$made.arrival.time)

bdd$DEP_INST <- ( hours(bdd$modified.departure.time) * 3600 + minutes(bdd$modified.departure.time) * 60 + seconds(bdd$modified.departure.time) ) - 3600 * 6 - 30 * 60
bdd$ARR_INST <- ( hours(bdd$made.arrival.time) * 3600 + minutes(bdd$made.arrival.time) * 60 + seconds(bdd$made.arrival.time) ) - 3600 * 6 - 30 * 60

bdd$DEP_PROG_INST <- ( hours(bdd$initial.departure.time) * 3600 + minutes(bdd$initial.departure.time) * 60 + seconds(bdd$initial.departure.time) ) - 3600 * 6 - 30 * 60
bdd$ARR_PROG_INST <- ( hours(bdd$initial.arrival.time) * 3600 + minutes(bdd$initial.arrival.time) * 60 + seconds(bdd$initial.arrival.time) ) - 3600 * 6 - 30 * 60


# period
bdd$period <- "t730"
bdd$period[bdd$modified.departure.time <= chron(times="07:00:00")] <- "t630"
bdd$period[bdd$modified.departure.time > chron(times="07:00:00") & bdd$modified.departure.time <= chron(times="07:30:00")] <- "t700"
table(bdd$period)


# rank all the choices by the order
bdd <- bdd[order(bdd$name, bdd$sessionId, bdd$modified.departure.time),]

bdd$choice_rank <- NA
u_names <- unique(bdd$name)
for (n in u_names) {
  bdd$choice_rank[bdd$name == n] <- seq(1, sum(bdd$name == n))
}


save(bdd, file="proc_trips.RData")







