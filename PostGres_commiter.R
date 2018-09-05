############################################################
#                                                          #
#                    PostGRES SQL                          #
#                                                          #
############################################################

setwd("C:/Users/Frederik/Documents/GitHub/Ethnic-Conflict-Prediction")
source("PostGres_connect.R")

dbExistsTable(con, "gdelt")
# False


dbWriteTable(con, "gdelt_work_dataset", 
             value = Output, append = TRUE, row.names = FALSE)

# query the data from postgreSQL 
df_postgres <- dbGetQuery(con, "SELECT * from gdelt_work_dataset")

rm(con, drv)