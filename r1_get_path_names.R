library(tidyverse)

get_path_names <- function(df_res) {
  # Obtains the names of the routes
  df_res <- df_res %>% mutate(PATH_NAME = NA)
  # this variable is only to check results of naming paths
  path_max_p <- c()
  for (i in 1:dim(df_res)[1]) {
    #for (i in 2:2) {
    path_temp <- unlist(strsplit(df_res$PATH[i], " ", fixed = FALSE, perl = FALSE, useBytes = FALSE))
    # Pre-process the PATH to obtain only the link ids
    path_temp_ini <- substr(path_temp,1,2)
    path_temp <- path_temp[which(path_temp_ini == "T_")]
    #df_res$PATH[i] <- paste(new_path)
    
    perc_equal <- c()
    # Computes how simmilar are the routes followed to the ones proposed
    for(p in orig_route_l2) {
      inter <- sum(p %in% path_temp)
      perc_equal_t <- inter /  (length(p) + length(path_temp) - inter)  
      perc_equal <- c(perc_equal, perc_equal_t)
    }  
    route_name_idx <- which(perc_equal == max(perc_equal))
    route_name <- names(orig_route_l2)[route_name_idx]
    
    #print(paste(as.character(max(perc_equal)), route_name))
    #path_max_p <- c(path_max_p, max(perc_equal))
    if (length(route_name) == 1 & max(perc_equal) > .8) {
      route_name < names(orig_route_l2)[route_name_idx][1]
    }
    else {
      route_name <- "other"
      warning(paste("Found two (or more) routes that matched the PATH:", i ) )
    }
    
    df_res$PATH_NAME[i] <- route_name
  }
  # Adds the name of the OD
  df_res <- df_res %>% mutate(OD=NA)
  df_res$OD[df_res$ORIGIN == "E_test1"] <- "OD1_1"
  df_res$OD[df_res$ORIGIN == "E_test2"] <- "OD1_2"
  df_res$OD[df_res$ORIGIN == "E_test3"] <- "OD2"
  #
  df_res <- df_res %>% select(-PATH)
  df_res
}



