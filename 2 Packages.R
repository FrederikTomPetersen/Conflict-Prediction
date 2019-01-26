cat("\014")  
######################################################################
###                                                                ###
###                         Load af pakker                         ###
###                                                                ###
###################################################################### 

#Load af pakker. Kræver at pakkerne er installeret gennem eksempelvis install.packages() funktionen
# En række af pakkerne er kun tilgængelige gennem Github

#Indhentning af landekoder
library("states")
library("countrycode")

# Pakker til GDELT og PostgresSQL
library("devtools")
library("gdeltr2")
library("hrbrthemes")
library("tidyverse")
library("countrycode")
library("readxl")
library("data.table")
require("RPostgreSQL")
require("trellisscopejs")

#Genrelle pakker
library("dplyr")
library("tidyverse")
library("data.table")
library("forecast")
library("zoo")
library("xtable")
library("foreign")
library("lubridate")

#Impute af data
library("mice")
library("VIM")
library("simputation")


#Hentning af data
library("WDI") #world bank development indicators
library("psData") #Polity IV data 
library("pwt9")

#Geospatial
library("sf")
library("rgdal")
library("rgeos")

#Visualisering
library("ggplot2")
library("ggthemes")
library("ROCR")
library("pROC")
library("gridExtra")

# Modellering af data
library("xgboost")
library("caret")
library("randomForest")

