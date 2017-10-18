library(tidyverse)
source("R_functions/fun_getPathNames.R")

infoTT <- tibble()
for (i in 1:length(FILES_INFOTT)) {
  print(names(FILES_INFOTT)[i])
  infot <- read_csv(FILES_INFOTT[[i]], col_names = c("ROUTE", "PERIOD_INI", "PERIOD_FIN", "TT_INFO"))
  infot$DEMAND <- names(FILES_INFOTT)[i]
  infoTT <- bind_rows(infoTT, infot)
}

# OD names
infoTT$OD <- getODNames_fromPathNames(infoTT$ROUTE)


# it looks like the times have been converted in seconds
rm(list=c("infot"))


