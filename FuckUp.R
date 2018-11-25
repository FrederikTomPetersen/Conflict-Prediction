cat("\014")  

######################################################################
###                                                                ###
###            Reloading data in case you broke them               ###
###                                                                ###
###################################################################### 


#Congrats - you effectevily fucked up your main dataset -to obtain it again simply run the following. 
#The script requires that you previously have stored the independent datasets in a Postgres Db defined in the PostGres_connect.R

#assumes that the driver and connection is already loaded - otherwise uncomment
#source("PostGres_connect.R")

#completedataset
completedata <-  dbGetQuery(con, "SELECT * from complete_data")

#individual components
GedAggregated <-  dbGetQuery(con, "SELECT * from ged_aggregated")
GdeltGroup <-  dbGetQuery(con, "SELECT * from gdelt_y_m_d_group")
WDI <- dbGetQuery(con, "SELECT * from wdi")
