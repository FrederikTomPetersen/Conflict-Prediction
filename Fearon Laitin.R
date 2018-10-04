# Hentning af Fearon & Laitens data fra 2003

library(tidyverse)
library(data.table)


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




mountains <- fldata %>% 
  distinct(ccode, mtnest)  # vi har 161 obs

mountains <- fldata %>% 
  distinct(ccode, mtnest)

ethnicfractionalization <- fldata %>% 
  distinct(ccode, ethfrac) #345 og 365 optræder to gange (Rusland og Jugoslavien) skal jeg abre tage middelværdien? 

religiousfractionalization <-  fldata %>% 
  distinct(ccode,relfrac)

oil <- fldata %>% 
  distinct(ccode, Oil) # her har vi 184 variable, hvilket udtrykker at et land ikek altid har væres olieeksporterende

colwars <-  fldata %>% 
  distinct(ccode, colwars) #her får jeg 301 observatoner, hvilket udtrykker at der har været koloniale krige og ikek krige gennem tidsperioden 

