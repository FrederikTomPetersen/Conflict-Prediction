# Hentning af Fearon & Laitens data fra 2003

library(tidyverse)
library(data.table)
library(foreign)

setwd(DataCave)
direct_link <-  "https://web.stanford.edu/group/ethnic/publicdata/repdata.zip"
download.file(direct_link, basename(direct_link))
unzip(basename(direct_link))


# install.packages("foreign")
# library(foreign)
data <- read.dta("repdata.dta")

fldata <- data %>%
  select("ccode","country","year","pop","lpop","polity2","ef","ethfrac","sdwars","colwars","mtnest","Oil","relfrac")
rm(data)



# Getting mountains
mountains <- fldata %>% 
  distinct(ccode, mtnest)  # vi har 161 obs
dbWriteTable(con, "fl_mountains", 
             value = mountains, append = TRUE, row.names = FALSE)




#Getting ethnic fractionalization
ethnicfractionalization <- fldata %>% 
  distinct(ccode, ethfrac) #345 og 365 optræder to gange (Rusland og Jugoslavien) skal jeg abre tage middelværdien? 
ethnicfractionalization <- ethnicfractionalization[-c(46,56),]
dbWriteTable(con, "fl_ethnicfrac", 
             value = ethnicfractionalization, append = TRUE, row.names = FALSE)



#Getting religious fractionalization
religiousfractionalization <-  fldata %>% 
  distinct(ccode,relfrac)
dbWriteTable(con, "fl_relfrac", 
             value = religiousfractionalization, append = TRUE, row.names = FALSE)

