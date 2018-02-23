#################################################
# Function that obtains the names of the routes from the route_list file
#################################################

getPathNames <- function(path, path_list, simmilarity=.8) {
  # path: a vector with the path in character format
  # path_list: the list with the names of the routes and a vector with the sequence of links in the path
  path_names <- c()
  
  path_max_p <- c()
  for (i in 1:length(path)) {
    #for (i in 2:2) {
    path_temp <- unlist(strsplit(path[i], " ", fixed = FALSE, perl = FALSE, useBytes = FALSE))
    # Pre-process the PATH to obtain only the link ids
    path_temp_ini <- substr(path_temp,1,2)
    path_temp <- path_temp[which(path_temp_ini == "T_")]
    #df_res$PATH[i] <- paste(new_path)
    
    perc_equal <- c()

    # Computes how simmilar are the routes followed to the ones proposed
    for(p in path_list) {
      inter <- sum(p %in% path_temp)
      perc_equal_t <- inter /  (length(p) + length(path_temp) - inter)  
      perc_equal <- c(perc_equal, perc_equal_t)
    }  
    route_name_idx <- which(perc_equal == max(perc_equal))
    route_name <- names(path_list)[route_name_idx]
    
    #print(paste(as.character(max(perc_equal)), route_name))
    #path_max_p <- c(path_max_p, max(perc_equal))
    if (length(route_name) == 1 & max(perc_equal) > simmilarity) {
      route_name < names(path_list)[route_name_idx][1]
    }
    else {
      route_name <- "other"
      
      warning(paste("Found two (or more) routes that matched the PATH:", i ) )
    }
    
    path_names <- c(path_names, route_name)
  }
  path_names
}

getPathNamesOD <- function(path, od, path_list, simmilarity=.8) {
  # path: a vector with the path in character format
  # path_list: the list with the names of the routes and a vector with the sequence of links in the path
  path_names <- c()
  
  path_max_p <- c()
  for (i in 1:length(path)) {
    #for (i in 2:2) {
    path_temp <- unlist(strsplit(path[i], " ", fixed = FALSE, perl = FALSE, useBytes = FALSE))
    # Pre-process the PATH to obtain only the link ids
    path_temp_ini <- substr(path_temp,1,2)
    path_temp <- path_temp[which(path_temp_ini == "T_")]
    #df_res$PATH[i] <- paste(new_path)
    
    perc_equal <- c()
    # Filters by ORIGIN too
    path_list_sub <- list()
    if (od[i] == "E_test1") {
      path_list_sub <- path_list[routes_E_test1]
    } 
    if(od[i] == "E_test2") {
      path_list_sub <- path_list[routes_E_test2] 
    } 
    if(od[i] == "E_test3") {
      path_list_sub <- path_list[routes_E_test3] 
    }
    
    # Computes how simmilar are the routes followed to the ones proposed
    for(p in path_list_sub) {
      inter <- sum(p %in% path_temp)
      perc_equal_t <- inter /  (length(p) + length(path_temp) - inter)  
      perc_equal <- c(perc_equal, perc_equal_t)
    }  
    route_name_idx <- which(perc_equal == max(perc_equal))
    route_name <- names(path_list_sub)[route_name_idx]
    
    #print(paste(as.character(max(perc_equal)), route_name))
    #path_max_p <- c(path_max_p, max(perc_equal))
    if (length(route_name) == 1 & max(perc_equal) > simmilarity) {
      route_name < names(orig_route_l2)[route_name_idx][1]
    }
    else {
      route_name <- "other"
      
      warning(paste("Found two (or more) routes that matched the PATH:", i ) )
    }
    
    path_names <- c(path_names, route_name)
  }
  path_names
}


getIniPathName <- function(path_name) {
  ini_path_names <- path_name
  ini_path_names[ini_path_names %in% c("R_test1_2_r1", "R_test1_2_r2")] <- "R_test1_2"
  ini_path_names[ini_path_names %in% c("R_test2_2_r1", "R_test2_2_r2")] <- "R_test2_2"
  ini_path_names[ini_path_names %in% c("R_test1_r1", "R_test1_r2")] <- "R_test1"
  ini_path_names[ini_path_names %in% c("R_test2_3_r1", "R_test2_3_r2")] <- "R_test2_3"
  ini_path_names[ini_path_names %in% c("R_test1_3_r1", "R_test1_3_r2")] <- "R_test1_3"
  ini_path_names[ini_path_names %in% c("R_test2_r1", "R_test2_r2")] <- "R_test2"
  ini_path_names
}

getODNames <- function(origin, destination) {
  # origin and destination. Two vectors with the origin link and the destination link
  od_names <- rep(NA, length(origin))
  od_names[origin %in% c("E_test1", "T_test1")] <- "OD1_1"
  od_names[origin %in% c("E_test2", "T_test2")] <- "OD1_2"
  od_names[origin %in% c("E_test3", "T_test3")] <- "OD2"
  od_names
}


getODNames_fromPathNames <- function(path_name) {
  # origin and destination. Two vectors with the origin link and the destination link
  od_names <- rep(NA, length(path_name))
  od_names[path_name %in% c("R_test1", "R_test1_2", "R_test1_3")] <- "OD1_1"
  od_names[path_name %in% c("R_test2", "R_test2_2", "R_test2_3")] <- "OD1_2"
  od_names[path_name %in% c("R_N1", "R_N2", "R_N3")] <- "OD2"
  od_names
}