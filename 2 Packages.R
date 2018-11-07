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
require("caret")
require("trellisscopejs")

#genereal
library("dplyr")
library("tidyverse")
library("data.table")
library("forecast")
library("zoo")
library("xtable")

#Getting data
library("WDI") #world bank development indicators
library("rWBclimate") # rWBclimate
library("psData") #Polity IV data 
library("pwt9")

#visualisering
library("ggplot2")
library("ggthemes")

# regression
library("caret")
library("xgboost")


#functional programming
#purrr