############################################################
#                                                          #
#                    PostGRES SQL                          #
#                                                          #
############################################################

setwd("C:/Users/Frederik/Documents/GitHub/Ethnic-Conflict-Prediction")
source("PostGres_connect.R")

dbExistsTable(con, "gdelt")
# False


dbWriteTable(con, "gdelt", 
             value = Output, append = TRUE, row.names = FALSE)

# query the data from postgreSQL 
df_postgres <- dbGetQuery(con, "SELECT * from gdelt")

# compares the two data.frames
identical(Output, df_postgres)

# Basic Graph of the Data
require(ggplot2)
ggplot(df_postgres, aes(x = as.factor(cyl), y = mpg, fill = as.factor(cyl))) + 
  geom_boxplot() + theme_bw()




