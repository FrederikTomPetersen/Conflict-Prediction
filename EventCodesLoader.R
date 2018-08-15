
my_workspace = "C:/Users/Frederik/Documents/GitHub/Ethnic-Conflict-Prediction/Data/"
setwd(my_workspace)


library(readxl)
EventCodeSort <- read_excel("~/GitHub/Ethnic-Conflict-Prediction/Data/EventCodeSort.xlsx", col_types = c("numeric", "text"))
EventCodes <- EventCodeSort[1]

saveRDS(EventCodeSort, file = "EventCodesNames.rds")
saveRDS(EventCodes, file = "EventCodes.rds")