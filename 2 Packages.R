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

#world bank development indicators
library(WDI)

# rWBclimate
library(rWBclimate)

#visualisering
library(ggplot2)

# regression
library(caret)
library(xgboost)