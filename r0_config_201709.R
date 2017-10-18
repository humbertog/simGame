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
DIR_TRIP_SET <- "/Users/humberto.gonzalez/simulation_game/exp_20170925/data_tripSet"

SESSION_IDS <- c(662)

SESSION_IDS_DEMAND <- list("662"="D1A2_VP5")

FILE_STATS          <- "stat.csv"
FILE_TRIPS_PLAYER   <- "trips_user.csv"

FILE_INFOTT_D1A2_VP5 <- "/Users/humberto.gonzalez/simulation_game/exp_20170925/data_infoTT/defaultOut_traveltimes.csv"
FILES_INFOTT <- list("D1A2_VP5" = FILE_INFOTT_D1A2_VP5)


