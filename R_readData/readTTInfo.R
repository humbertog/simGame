library(tidyverse)

infod2 <- read_csv(FILE_INFOTT_D2, col_names = c("ROUTE", "PERIOD_INI", "PERIOD_FIN", "TT_INFO"))
infod6 <- read_csv(FILE_INFOTT_D6, col_names = c("ROUTE", "PERIOD_INI", "PERIOD_FIN", "TT_INFO"))

infod2$DEMAND <- "D2"
infod6$DEMAND <- "D6"

infoTT <- bind_rows(infod2, infod6)
# it looks like the times have been converted in seconds
rm(list=c("infod2", "infod6"))


