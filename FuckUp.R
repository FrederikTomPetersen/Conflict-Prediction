cat("\014")  

######################################################################
###                                                                ###
###            Reloading data in case you broke them               ###
###                                                                ###
###################################################################### 


#Congrats - you effectevily fucked up your main dataset -to obtain it again simply run the following. 
#The script requires that you previously have stored the independent datasets in a Postgres Db defined in the PostGres_connect.R


#assumes that the driver and connection is already loaded - otherwise uncomment
#setwd(Private)
#source("PostGres_connect.R")


GED_disaggregated <- dbGetQuery(con, "SELECT * from ged")

wdi_gdp_capita_2011c_country <- dbGetQuery(con, "SELECT * from wdi_gdp")
wdi_gov_expenditure <- dbGetQuery(con, "SELECT * from wdi_gov_expenditure")
wdi_gov_debt <- dbGetQuery(con, "SELECT * from wdi_gov_debt")
wdi_secondary_male_enrollment <- dbGetQuery(con, "SELECT * from wdi_secondary_male_enrollment")
