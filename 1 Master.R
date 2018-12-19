cat("\014")  

######################################################################
###                                                                ###
###                         MASTER SCRIPTS                         ###
###                                                                ###
######################################################################

#Definere en række workspaces - kræver tilpasning til egen computer
DataCave = "C:/Users/Frederik/Documents/SpecialeData/"
Private = "C:/Users/Frederik/Documents/SpecialeScripts/"
GitHub = "C:/Users/Frederik/Documents/GitHub/Ethnic-Conflict-Prediction"
Models = "C:/Users/Frederik/Documents/GitHub/Ethnic-Conflict-Prediction/Prediction models/models/"
Latexfigure ="C:/Users/Frederik/OneDrive - Københavns Universitet/Speciale paper/03_figures"
Latextable= "C:/Users/Frederik/OneDrive - Københavns Universitet/Speciale paper/04_tables"
  
# De følgende scripts kører projektet fra ende til anden

setwd(GitHub)

source("2 Packages.R") #Indhenter relevanter pakker for projektet

source("3 Functions.R") # Definere en række funktioner, der anvendes primært i Dataload.R

source("4 PostGres_connect.R") # Forbinder til PostgresSQl - kræver tilretning (!!!)

#source("5 DataLoad.R") # Henter og gemmer data i PostgresSQL

source("6 DataGather.R") # Samler data til et samlet datasæt

source("7 Visulization.R") # foretager en række visalisering af data

source("8 Analyse.R") # Fortager prædiktions analysen

source("9 Importance.R") # Foretage importance analysen

source("10 Tuning.R") # Foretager Tuning analysen
