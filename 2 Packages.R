cat("\014")  
######################################################################
###                                                                ###
###              Installing and fetching pacakges                  ###
###                                                                ###
###################################################################### 

#Installation af pakker og hentning af relvante pakker 

# devtools::install_github("hadley/devtools")
# devtools::install_github("hadley/dplyr")
# devtools::install_github("hafen/trelliscopejs")
# devtools::install_github("abresler/gdeltr2")
#devtools::install_github("ropensci/rWBclimate")


# install.packages("imputeTS")
# library("imputeTS")
# mice  pakken skal ogs? ind 
#

#States
library("states")
library("countrycode")

#ggdelt and posgres
library("devtools")
library("gdeltr2")
library("hrbrthemes")
library("tidyverse")
library("countrycode")
library("readxl")
library("data.table")
require("RPostgreSQL")
require("trellisscopejs")

#genereal
library("dplyr")
library("tidyverse")
library("data.table")
library("forecast")
library("zoo")
library("xtable")
library("foreign")
library("lubridate")

#imputing
library("mice")
library("VIM")
library("simputation")


#Getting data
library("WDI") #world bank development indicators
library("rWBclimate") # rWBclimate
library("psData") #Polity IV data 
library("pwt9")

#geospatial
library("sf")
library("rgdal")
library("rgeos")

#visualisering
library("ggplot2")
library("ggthemes")
library("ROCR")
library("pROC")
library("gridExtra")
library("grpss")

# regression
library("caret")
library("xgboost")
library("randomForest")
require("caret")


#functional programming
#purrr