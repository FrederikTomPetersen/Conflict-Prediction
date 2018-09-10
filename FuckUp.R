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

# Gdelt1 <- dbGetQuery(con, "SELECT * from gdelt2000_2006")
# Gdelt2 <- dbGetQuery(con, "SELECT * from gdelt2006_2010")
# Gdelt3 <- dbGetQuery(con, "SELECT * from gdelt2011")
# Gdelt4 <- dbGetQuery(con, "SELECT * from gdelt2012")
# Gdelt5 <- dbGetQuery(con, "SELECT * from gdelt2013")
# Gdelt6 <- dbGetQuery(con, "SELECT * from gdelt2014")
# Gdelt7 <- dbGetQuery(con, "SELECT * from gdelt2015")
# Gdelt8 <- dbGetQuery(con, "SELECT * from gdelt2016")


Gdelt <- dbGetQuery(con, "SELECT * from gdelt_group")
GED_disaggregated <- dbGetQuery(con, "SELECT * from ged")

#Gdelt <- dbGetQuery(con, "SELECT * from gdelt_work_dataset")      # 1.268395 observationer
#Gdelt2 <- dbGetQuery(con, "SELECT * from gdelt_work_dataset_nyt") # 21.048124 observationer , koster omkring 20 gb i midlertidig plads at loade
#Gdelt3 <- dbGetQuery(con, "SELECT * from gdelt_y_m_d")            # 27.277184 observationer , koster omkring 30 gb i midlertidig plads at loade



wdi_gdp_capita_2011c_country <- dbGetQuery(con, "SELECT * from wdi_gdp")
wdi_gov_expenditure <- dbGetQuery(con, "SELECT * from wdi_gov_expenditure")
wdi_gov_debt <- dbGetQuery(con, "SELECT * from wdi_gov_debt")
wdi_secondary_male_enrollment <- dbGetQuery(con, "SELECT * from wdi_secondary_male_enrollment")
