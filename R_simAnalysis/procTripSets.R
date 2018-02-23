#####################################################################
# Plots the travel time distributions for the different routes
# resulting from the symulation previous to the experiments
# IN: traj_od.csv files
#     these files contain the trajectories of all the trips in the
#     routes that we are interested in.
#     The files are obtained using parser.py and make_res_file.py 
#####################################################################


library(tidyverse)
library(lubridate)
library(gridExtra)

source("./R_functions/fun_getPathNames.R")

load("./R_data/routes_201802.RData")

file<-"BDD_SG_Calage18"

trips <- read_delim(paste("/Users/humberto.gonzalez/Dropbox/simulation_game/exp_20180215/experiment_design/tripsets/",
                          file, ".csv",sep=""
                          ), 
                    delim=";")
  
  
                    


# Creates a variable with OD (temporal, need to rename ODs)
table(trips$Origine, trips$Destination)
trips$odt <- paste(trips$Origine, trips$Destination, sep = "-")

odt <- table(trips$odt)

selected_odts <- names(odt[odt > 10])
selected_trips <- trips[trips$odt %in% selected_odts,]

# Name the routes
selected_trips$path_name <- getPathNames(path = selected_trips$Iti1, path_list = route_list_201802)

selected_trips <- 
  selected_trips %>%
  select(-Genre, -Categorie_Age, -Iti1)


table(selected_trips$odt, selected_trips$path_name)


# Travel time 
selected_trips$Heure_Depart_mins <- as.numeric(selected_trips$Heure_Depart - 23400) / 60
selected_trips$travel_time <- as.numeric(selected_trips$Heure_Arrivee - selected_trips$Heure_Depart)

p <- list()
i <- 1
for(od in sort(unique(selected_trips$odt))) {
  pt <- selected_trips %>%
    filter(odt == od) %>%
    ggplot(aes(Heure_Depart, travel_time, colour=path_name)) + 
    geom_point(alpha=.3) +
    geom_smooth(se = FALSE, span=.4) +
    ggtitle(od) +theme_bw() +
    theme(legend.direction = "horizontal", legend.position = "bottom",
          axis.title.x=element_blank()) 
  
  p[[i]] <- pt
  i <- i + 1
}

#p1 <- arrangeGrob(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], p[[6]],p[[7]], p[[8]],nrow = 4)

#p2 <- arrangeGrob(p[[9]], p[[10]], p[[11]], p[[12]], p[[13]], p[[14]], p[[15]],nrow = 4)

pall <- arrangeGrob(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], p[[6]],p[[7]], p[[8]],
  p[[9]], p[[10]], p[[11]], p[[12]], p[[13]], p[[14]], p[[15]],nrow = 4)

ggsave(paste("plots_trip_set/plot_", file, ".png", sep="" ), pall, width = 210*2, height = 297, units="mm")


#ggsave(paste("plots_trip_set/plot1_", file, ".png", sep="" ), p1, width = 210, height = 297, units="mm")
#ggsave(paste("plots_trip_set/plot2_", file,".png", sep="" ), p2, width = 210, height = 297, units="mm")

