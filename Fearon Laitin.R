# Hentning af Fearon & Laitens data fra 2003

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


a <- fldata %>% 
  distinct(ccode)  # vi har 161 lande
rm(a)
