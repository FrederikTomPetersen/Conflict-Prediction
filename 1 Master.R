cat("\014")  

######################################################################
###                                                                ###
###                         MASTER SCRIPTS                         ###
###                                                                ###
######################################################################

#Possible workspaces - adjust to your local preferences
DataCave = "C:/Users/Frederik/Documents/SpecialeData/"
Private = "C:/Users/Frederik/Documents/SpecialeScripts/"
GitHub = "C:/Users/Frederik/Documents/GitHub/Ethnic-Conflict-Prediction"
Models = "C:/Users/Frederik/Documents/SpecialeModels/"



# This scripts binds the dataanalysis of my thesis. It aims to control 
# the workflow of the project. it starts out by loading all the necessary packages 
# for the project. Then it defines alle the functions neccesary for the later scripts. 
# After this intial defintions it starts getting the data from different sources. 
# When the datasets are gathered it will automatically begin to tidy the data in order to make af join possible
# after the data are gathered 

setwd(GitHub)

source("2 Packages.R")

source("3 Functions.R")

source("4 PostGres_connect.R")

#source("DataLoad.R")

#source("DataTidy.R")

source("DataGather.R")

#source("DataXplore.R")

#source("DataAnalysis.R")

#source("DataProducts.R")


#####Data_reloader#####
#Congrats - you effectively fucked up your main dataset -to obtain it again simple load it with the FuckUp.R script
#source("FuckUp.R")
