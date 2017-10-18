library(lubridate)

renameRoutes <- function(routes) {
  renameRoute <- function(x) {
    r <- "no_name"
    if(x == "R_test1") r <- "O1D1_center"
    if(x == "R_test1_2") r <- "O1D1_north"
    if(x == "R_test1_3") r <- "O1D1_south"
    if(x == "R_N1") r <- "O3D2_north"
    if(x == "R_N2") r <- "O3D2_center"
    if(x == "R_N3") r <- "O3D2_south"
    if(x == "R_test2") r <- "O2D1_south"
    if(x == "R_test2_2") r <- "O2D1_north"
    if(x == "R_test2_3") r <- "O2D1_center"
    r
  }
  unlist(lapply(routes, renameRoute))
}

renameOD <- function(ods) {
  renameOD <- function(x) {
    r <- "no_name"
    if(x == "OD1_1") r <- "O1D1"
    if(x == "OD1_2") r <- "O2D1"
    if(x == "OD2") r <- "O3D2"
   
    r
  }
  unlist(lapply(ods, renameOD))
}

getPeriod <- function(time, ini_time, fin_time, interval) {
  cut_breaks <- seq(ini_time, fin_time, interval)
  formated <- seconds_to_period(cut_breaks)
  formated <- sprintf("%02i:%02i", hour(formated), minute(formated))

  cut(time, cut_breaks, right=FALSE,include.lowest = TRUE, 
      labels=paste(formated[1:(length(formated) - 1)], formated[2:(length(formated) )], sep="-"))
}







