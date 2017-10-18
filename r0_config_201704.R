# CONFIG FILE
# DATA SETS:
# 1. resXXX_proc.csv files : All the trips in the OD's played in the SG
# 2. trips.csv : The trips only played by the players. Nothe that there is not an ID that makes possible to cross them with
#               the resXXX_proc.csv files
# 3. SG_SXXXres_proc.csv : Simmilar to the resXXX_proc.csv files, but with less information
#
# OHER FILES : 
# - stat.csv : File with the aggregated stats
#

# TRIP SET 
DIR_TRIP_SET        <- "/Users/humberto.gonzalez/simulation_game/exp_20170412/data_sg/data_tripSet"

SESSION_IDS <- c(625, 626, 628, 629, 630, 631, 633, 634)
#SESSION_IDS <- c(646)


SESSION_IDS_DEMAND <- list("625"="D2", "626"="D2", "628"="D2", "629"="D2", 
                           "630"="D6", "631"="D6", "633"="D6", "634"="D6")

FILE_STATS          <- "stat.csv"
FILE_TRIPS_PLAYER   <- "trips_user.csv"

# INFO FILES
FILE_INFOTT_D2 <- "/Users/humberto.gonzalez/simulation_game/exp_20170412/data_infoTT/SG2_2_HistoricalTrafficData.csv"
FILE_INFOTT_D6 <- "/Users/humberto.gonzalez/simulation_game/exp_20170412/data_infoTT/SG2_6_HistoricalTrafficData.csv"
FILES_INFOTT <- list("D2" = FILE_INFOTT_D2, "D6"=FILE_INFOTT_D6)