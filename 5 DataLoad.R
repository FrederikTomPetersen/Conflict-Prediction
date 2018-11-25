cat("\014")  

############################################################
#                                                          #
#                       DataLoad.R                         #
#                                                          #
############################################################

# This Script downlaod/fetch each of the individual dataset
# first it will temperaily store it in your datacave before
# saves it to a postgres database. In order for this to work 
# you must fist install a postgres on your computer,
# set up a DB and define the postgres connection as advised the master scripts 


############################################################
#                                                          #
#           Accessing gdelt aggregated on year             #
#                                                          #
############################################################

Countries <-  codelist_panel %>% 
  select(country.name.en,iso2c) %>% 
  distinct(country.name.en,iso2c)

myvars <- c("GLOBALEVENTID", "SQLDATE", "MonthYear", "Actor1Code", "Actor2Code", "Actor1CountryCode", "Actor2CountryCode","Actor1Type1Code","Actor2Type1Code", "Actor1Geo_CountryCode","Actor2Geo_CountryCode", "IsRootEvent", "EventCode", "EventBaseCode", "EventRootCode","QuadClass", "GoldsteinScale", "NumMentions", "AvgTone", "ActionGeo_CountryCode", "Actor1Religion1Code","Actor2Religion1Code", "Actor1EthnicCode","Actor2EthnicCode")
myvars2 <- c("GLOBALEVENTID","year", "month", "Actor1Code", "Actor2Code", "Actor1CountryCode", "Actor2CountryCode","Actor1Type1Code","Actor2Type1Code", "Actor1Geo_CountryCode","Actor2Geo_CountryCode", "IsRootEvent", "EventCode", "EventBaseCode", "EventRootCode","QuadClass", "GoldsteinScale", "NumMentions", "AvgTone", "ActionGeo_CountryCode", "Actor1Religion1Code","Actor2Religion1Code", "Actor1EthnicCode","Actor2EthnicCode","rel", "eth")


All_eventdb_url <-  get_urls_gdelt_event_log()
Event_url_list <- All_eventdb_url$urlData 
Events <- Event_url_list[12:28]

setwd(DataCave)
Gdelt_header  <-  fread("GDELT_HEADER.csv")
Gdelt_header <-  Gdelt_header$`Field Name`
collist <- Gdelt_header[1:57]

#Gdelt_getter_1(Events,1)


############################################################
#                                                          #
#          Accessing gdelt aggregated on month             #
#                                                          #
############################################################

Countries <-  codelist_panel %>% 
  select(country.name.en,iso2c) %>% 
  distinct(country.name.en,iso2c)

myvars <- c("GLOBALEVENTID", "SQLDATE", "MonthYear", "Actor1Code", "Actor2Code", "Actor1CountryCode", "Actor2CountryCode","Actor1Type1Code","Actor2Type1Code", "Actor1Geo_CountryCode","Actor2Geo_CountryCode", "IsRootEvent", "EventCode", "EventBaseCode", "EventRootCode","QuadClass", "GoldsteinScale", "NumMentions", "AvgTone", "ActionGeo_CountryCode", "Actor1Religion1Code","Actor2Religion1Code", "Actor1EthnicCode","Actor2EthnicCode")
myvars2 <- c("GLOBALEVENTID","year", "month", "Actor1Code", "Actor2Code", "Actor1CountryCode", "Actor2CountryCode","Actor1Type1Code","Actor2Type1Code", "Actor1Geo_CountryCode","Actor2Geo_CountryCode", "IsRootEvent", "EventCode", "EventBaseCode", "EventRootCode","QuadClass", "GoldsteinScale", "NumMentions", "AvgTone", "ActionGeo_CountryCode", "Actor1Religion1Code","Actor2Religion1Code", "Actor1EthnicCode","Actor2EthnicCode","rel", "eth")


All_eventdb_url <-  get_urls_gdelt_event_log()
Event_url_list <- All_eventdb_url$urlData 
Events <- Event_url_list[29:115]

setwd(DataCave)
Gdelt_header  <-  fread("GDELT_HEADER.csv")
Gdelt_header <-  Gdelt_header$`Field Name`
collist <- Gdelt_header[1:57]

#Gdelt_getter_2(Events,1)





############################################################
#                                                          #
#          Accessing gdelt aggregated on day               #
#                                                          #
############################################################

Countries <-  codelist_panel %>% 
  select(country.name.en,iso2c) %>% 
  distinct(country.name.en,iso2c)

myvars <- c("GLOBALEVENTID", "SQLDATE", "MonthYear", "Actor1Code", "Actor2Code", "Actor1CountryCode", "Actor2CountryCode","Actor1Type1Code","Actor2Type1Code", "Actor1Geo_CountryCode","Actor2Geo_CountryCode", "IsRootEvent", "EventCode", "EventBaseCode", "EventRootCode","QuadClass", "GoldsteinScale", "NumMentions", "AvgTone", "ActionGeo_CountryCode", "Actor1Religion1Code","Actor2Religion1Code", "Actor1EthnicCode","Actor2EthnicCode")
myvars2 <- c("GLOBALEVENTID","year", "month", "Actor1Code", "Actor2Code", "Actor1CountryCode", "Actor2CountryCode","Actor1Type1Code","Actor2Type1Code", "Actor1Geo_CountryCode","Actor2Geo_CountryCode", "IsRootEvent", "EventCode", "EventBaseCode", "EventRootCode","QuadClass", "GoldsteinScale", "NumMentions", "AvgTone", "ActionGeo_CountryCode", "Actor1Religion1Code","Actor2Religion1Code", "Actor1EthnicCode","Actor2EthnicCode","rel", "eth")

All_eventdb_url <-  get_urls_gdelt_event_log()
Event_url_list <- All_eventdb_url$urlData

#Events <- Event_url_list[116:1845] # slutter 31/12-2017 Nedbydes for at spare plads på CPU og HDD
#Events <-  Event_url_list[116:751] 
#Events <-  Event_url_list[752:1116] #2015
#Events <-  Event_url_list[1117:1482] #2016
#Events <-  Event_url_list[1483:1845] #2017



setwd(DataCave)
Gdelt_header  <-  fread("GDELT_HEADER.csv")
Gdelt_header <-  Gdelt_header$`Field Name`
collist <- Gdelt_header[1:58]


#Gdelt_getter_3(Events,1)

#-----------------------------------------------------------------------------  
#-----------------------------------------------------------------------------

#Denne blok køres efter at al gdelt data er indlæst i postgresdb og grupperet

#variable, der skal beholdes:
vars  <-  c("country", "year", "month", "q1at","q1cnt","q1gs", "q2at","q2cnt","q2gs", "q3at","q3cnt", "q3gs", "q4at","q4cnt","q4gs", "relq1at", "relq1cnt", "relq1gs", "relq2at", "relq2cnt", "relq2gs", "relq3at", "relq3cnt","relq3gs", "relq4at", "relq4cnt", "relq4gs", "ethq1at", "ethq1cnt","ethq1gs", "ethq2at", "ethq2cnt","ethq2gs", "ethq3at", "ethq3cnt", "ethq3gs", "ethq4at", "ethq4cnt", "ethq4gs")


#indhentning af data fra postgresdb
groupy <-  dbGetQuery(con, "SELECT * from gdelt_y_group") %>% 
  select(vars)

groupym <-  dbGetQuery(con, "SELECT * from gdelt_y_m_group") %>% 
  select(vars)

groupymd1 <-  dbGetQuery(con, "SELECT * from gdelt_ymd_group_1") %>% 
  select(vars)

groupymd2 <-  dbGetQuery(con, "SELECT * from gdelt_ymd_group_2") %>% 
  select(vars)

groupymd3 <-  dbGetQuery(con, "SELECT * from gdelt_ymd_group_3") %>% 
  select(vars)

groupymd4 <-  dbGetQuery(con, "SELECT * from gdelt_ymd_group_4") %>% 
  select(vars)

merged <- rbind(groupy, groupym, groupymd1, groupymd2,groupymd3,groupymd4)


dbWriteTable(con, "gdelt_group_complete", 
             value = merged, overwrite = TRUE, row.names = FALSE)

rm(groupy, groupym, groupymd1, groupymd2,groupymd3,groupymd4, vars, merged)



############################################################
#                                                          #
#         Access to Georeferenced event dataset            #
#              Disaggregated version                       #
#                                                          #
############################################################

setwd(DataCave)
direct_link <-  "http://ucdp.uu.se/downloads/ged/ged181-shp.zip"
download.file(direct_link, basename(direct_link))
unzip(basename(direct_link))

ged181 <-  sf::read_sf("ged181.shp", crs = 4326) %>% 
    select(country, region, country_id,year,deaths_a,deaths_b, deaths_civ, deaths_unk,date_start,date_end,gwnob)

ged_disaggregated <-  ged181
rm(ged181)
rm(direct_link)
unlink("ged181-shp.zip")


#Dating
ged_disaggregated <-  ged_disaggregated %>% 
  mutate(date_start = as.Date(date_start),
         date_end= as.Date(date_end),
         mid = (date_start + floor((date_end-date_start)/2))) %>% 
  separate(mid, c("year", "month", "day"), "-") %>% 
  filter(is.na(gwnob)) #sikre at der kun kommer interne konflikter med i data

ged_disaggregated <-  ged_disaggregated %>% 
  mutate(year=as.numeric(year),
         month = as.numeric(month),
         TotalDeaths = (deaths_a + deaths_b + deaths_civ + deaths_unk))




#Creating the aggregated table + tidy

CountryCodelistPanel <-  codelist_panel %>% 
  select(country.name.en, p4n) %>% 
  filter(!is.na(p4n))
ged_base <-  unique(CountryCodelistPanel) %>% 
  group_by(country.name.en,p4n) %>% 
  expand(year= 1989:2017, month = 1:12)
rm(CountryCodelistPanel)

ged_agg <- ged_disaggregated %>% 
  group_by(country_id,year, month) %>% 
  summarize(deaths = sum(as.numeric(TotalDeaths)),
            Incidents = as.numeric(n()),
            sideA = sum(as.numeric(deaths_a)),
            sideB = sum(as.numeric(deaths_b)))%>%  
  arrange(country_id, year, month)

ged_agg <- ged_base %>% 
  left_join(ged_agg, by = c("p4n"="country_id", "year"="year", "month"="month")) 

ged_agg <- ged_agg %>% 
  mutate(deaths = coalesce(deaths,0)
        ,Incidents = coalesce(Incidents,0)
        ,sideA = coalesce(sideA,0)
        ,sideB = coalesce(sideB,0))

ged_agg <- ged_agg %>% 
  group_by(p4n, year)%>% 
    mutate(deaths_running_month= cumsum(deaths)
           ,deathyear = sum(deaths)
           ,deathsuma = sum(sideA)
           ,deathsumb = sum(sideB)
           ,cwy = ifelse(deathyear >=1000 & deathsuma + deathsumb >=200, 1,0)
           ,cwm = ifelse(deaths_running_month>=83 & sideA + sideB >=100/12,1,0)) %>% 
  arrange(country.name.en,year,month)


#Writing to postgres
dbWriteTable(con, "ged_disaggregated", 
             value = ged_disaggregated, overwrite = TRUE, row.names = FALSE)
dbWriteTable(con, "ged_aggregated", 
             value = ged_agg, overwrite = TRUE, row.names = FALSE)
rm(ged_disaggregated,ged_agg)


############################################################
#                                                          #
#                 Ethnic Relevant groups                   #
#                                                          #
#                                                          #
############################################################

#Getting perspective of powershare between ethnic groups. 

setwd(DataCave)
direct_link <-  "https://icr.ethz.ch/data/epr/core/EPR-2018.1.csv"
download.file(direct_link, basename(direct_link))
Basefile <- basename(direct_link)
EPR <-  fread(Basefile)

EPR <- EPR %>% 
  filter(to >=1989)
EPR <- EPR %>% 
  mutate(from = as.Date(from, format="%Y"),
         to = as.Date(to,format="%Y"),
         year = mapply(seq,EPR$from,EPR$to, SIMPLIFY=FALSE)) %>% 
  unnest(year) %>% 
  select(-from,-to)



EPR_sum <-  EPR %>% 
  group_by(gwid,year) %>% 
  summarize(groupscount = n(),
            powerruleprop = sum(size[which(status %in% c("DOMINANT","MONOPOLY"))]),
            powerrulenmb = sum(ifelse(status %in% c("DOMINANT","MONOPOLY"),1,0)),
            powershareprop = sum(size[which(status %in% c("SENIOR PARTNER","JUNIOR PARTNER"))]),
            powersharennb = sum(ifelse(status %in% c("SENIOR PARTNER","JUNIOR PARTNER"),1,0)),
            powerexcludedprop = sum(size[which(status %in% c("POWERLESS","SELF-EXCLUSION","DISCRIMINATED"))]),
            powerexcludednmb = sum(ifelse(status %in% c("POWERLESS","SELF-EXCLUSION","DISCRIMINATED"),1,0)),
            powerirrelevantprop = sum(size[which(status %in% c("IRRELEVANT"))]),
            powerirrelevantnmb = sum(ifelse(status %in% c("IRRELEVANT"),1,0)))

EPR_countries <- EPR %>%
  select(gwid, statename) %>% 
  distinct() %>% 
  group_by(gwid) %>% 
  expand(year= 1989:2017, month = 1:12)

EPR_grouped <- EPR_countries %>% 
  left_join(EPR_sum, by= c("gwid" ="gwid", "year"="year")) 

dbWriteTable(con, "epr_grouped", 
             value = EPR_grouped, overwrite = TRUE, row.names = FALSE)

rm(EPR, EPR_countries,EPR_sum,EPR_grouped)


# Finding total number of refurgees from country

setwd(DataCave)
direct_link <-  "https://icr.ethz.ch/data/epr/er/ER-2018.1.csv"
download.file(direct_link, basename(direct_link))
Basefile <- basename(direct_link)
EPR_ER <-  fread(Basefile)

EPR_ER <- EPR_ER %>% 
  filter(year>=1989) %>% 
  group_by(coo,ccode_coo, year) %>% 
  summarize(refurgescnt = sum(totalrefugees))

dbWriteTable(con, "epr_er", 
             value = EPR_ER, overwrite = TRUE, row.names = FALSE)

rm(EPR_ER)




############################################################
#                                                          #
#               Ethnic fractionalization                   #
#             Fearon Laitin dataset                        #
#                                                          #
############################################################


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


fldata$ccode[fldata$ccode==260] <- 255 
fldata$ccode[fldata$ccode==732] <- 730 

fldata <- fldata

# Getting mountains
mountains <- fldata %>% 
  distinct(ccode, country, mtnest)  # vi har 161 obs

dbWriteTable(con, "fl_mountains", 
             value = mountains, overwrite = TRUE, row.names = FALSE)




#Getting ethnic fractionalization
ethnicfractionalization <- fldata %>% 
  distinct(ccode, ethfrac) #345 og 365 optræder to gange (Rusland og Jugoslavien) skal jeg abre tage middelværdien? 
ethnicfractionalization <- ethnicfractionalization[-c(46,56),]
dbWriteTable(con, "fl_ethnicfrac", 
             value = ethnicfractionalization, overwrite = TRUE, row.names = FALSE)



#Getting religious fractionalization
religiousfractionalization <-  fldata %>% 
  distinct(ccode,relfrac)
dbWriteTable(con, "fl_relfrac", 
             value = religiousfractionalization, overwrite = TRUE, row.names = FALSE)


#oil
oil <- fldata %>% 
  filter(year >= 1989) %>% 
  distinct(ccode, Oil) %>% 
  group_by(ccode) %>% 
  filter(row_number() == 2)

oil <- fldata %>% 
  filter(year >= 1989) %>% 
  distinct(ccode, Oil) %>% 
  group_by(ccode) %>% 
  filter(row_number() == 1)

#Change in oilstatus during time period - apllying latest information
oil$Oil[oil$ccode==99] <- 1
oil$Oil[oil$ccode==471] <- 1
oil$Oil[oil$ccode==651] <- 1
oil$Oil[oil$ccode==679] <- 1
oil$Oil[oil$ccode==690] <- 1


dbWriteTable(con, "fl_oil", 
             value = oil, overwrite = TRUE, row.names = FALSE)

rm(fldata, religiousfractionalization,mountains,ethnicfractionalization,oil)









############################################################
#                                                          #
#                    Colonial history                      #
#                                                          #
#                                                          #
############################################################

setwd(DataCave)
direct_link <-  "http://www.paulhensel.org/Data/colhist.zip"
download.file(direct_link, basename(direct_link))
unzip(basename(direct_link))
colhist <-  fread("ICOW Colonial History 1.1/coldata110.csv")

colbase <-  colhist %>% 
  select(State, Name, ColRuler) %>% 
  distinct() 

col <- colbase %>% mutate(colstyle = ifelse(ColRuler ==  200, 1, ifelse(ColRuler == 220, 2, ifelse(ColRuler==-9,-9, 0)))) %>% 
  transmute(p4n = State, colstyle =colstyle)

dbWriteTable(con, "col_hist", 
             value = col, overwrite = TRUE, row.names = FALSE)

rm(colhist,colbase, col)

############################################################
#                                                          #
#                       POLITY IV                          #
#                                                          #
#                                                          #
############################################################


Countries <-  codelist_panel %>% 
  select(iso2c,p4n) %>%
  filter(!is.na(p4n)) %>% 
  distinct(iso2c,p4n)


#Polity IV
setwd(DataCave)
DSN <- "http://www.systemicpeace.org/inscr/p4v2017.xls"
#download.file(DSN, "PolityIV.sav") # PolityIV server er ustabil så der er risiko for at downloade en beskadiget fil..

PolityIV = read_xls("PolityIV.xls")
PolityIV <-  PolityIV %>%  
  filter(year>=1989) %>% 
  left_join(Countries, by = c("ccode" = "p4n")) %>% 
  select("country", "year", "democ", "autoc", "polity2", "xrreg","xrcomp","xropen","xconst", "iso2c", "ccode")

dbWriteTable(con, "polity_4", 
             value = PolityIV, overwrite = TRUE, row.names = FALSE)
rm(PolityIV, Countries)

############################################################
#                                                          #
#                  Area of countries                       #
#                                                          #
#                                                          #
############################################################



setwd(DataCave)
direct_link <-  "http://thematicmapping.org/downloads/TM_WORLD_BORDERS-0.3.zip"
download.file(direct_link, basename(direct_link))
unzip(basename(direct_link))

countries_area <-  sf::read_sf("TM_WORLD_BORDERS-0.3.shp", crs = 4326) %>% 
  select(ISO2, AREA)

rm(direct_link)
unlink("TM_WORLD_BORDERS-0.3.zip")

dbWriteTable(con, "area", 
             value = countries_area, overwrite = TRUE, row.names = FALSE)
rm(countries_area)









############################################################
#                                                          #
#   Accessing the World Banks development indicators       #
#                                                          #
#                                                          #
############################################################

Country <-  codelist_panel %>% 
  select(country.name.en,iso2c) %>% 
  distinct(country.name.en,iso2c) %>% 
  filter(!is.na(iso2c))

iso2clist <- Country$iso2c
rm(Country)




#####################################
#       Growth and welfare    
#

WDI_GDPcapita2011c = WDI(indicator='NY.GDP.PCAP.PP.KD', start=1989, end=2017,  country = 'all') %>% 
  filter(iso2c %in% iso2clist) #GDP pr capita 2011 constant

WDI_GDP_CD <- WDI(indicator ="NY.GDP.MKTP.CD", start=1989, end=2017,  country = 'all')%>% 
  filter(iso2c %in% iso2clist) # accepter obs loss -  GDP current $

WDI_growth <- WDI(indicator ="NY.GDP.MKTP.KD.ZG", start=1989, end=2017,  country = 'all')%>% 
  filter(iso2c %in% iso2clist) # accepter obs loss - growth annual %


dbWriteTable(con, "wdi_gdp", 
             value = WDI_GDPcapita2011c, overwrite = TRUE, row.names = FALSE)
dbWriteTable(con, "wdi_gdp_cd", 
             value = WDI_GDP_CD, overwrite = TRUE, row.names = FALSE)
dbWriteTable(con, "wdi_growth", 
             value = WDI_growth, overwrite = TRUE, row.names = FALSE)

rm(WDI_GDPcapita2011c,WDI_GDP_CD,WDI_growth)

#####################################
#     Trade
#

WDI_trade <- WDI(indicator ="NE.TRD.GNFS.ZS", start=1989, end=2017,  country = 'all')%>% 
  filter(iso2c %in% iso2clist) #Trade (% of GDP)  # keep og accepter tab af observationer 

WDI_import_gs <- WDI(indicator ="NE.IMP.GNFS.ZS", start=1989, end=2017,  country = 'all')%>% 
  filter(iso2c %in% iso2clist) #Imports of goods and services (% of GDP) - Behold og accepter tab af observationer

dbWriteTable(con, "wdi_trade", 
             value = WDI_trade, overwrite = TRUE, row.names = FALSE)
dbWriteTable(con, "wdi_import_gs", 
             value = WDI_import_gs, overwrite = TRUE, row.names = FALSE)

rm(WDI_trade,WDI_import_gs)


#####################################
#     School enrollmentment         
#

#WDIsearch('enrollment')
WDI_enrollment <- WDI(indicator ="SE.SEC.NENR.MA", start=1989, end=2017,  country = 'all')%>%   
  filter(iso2c %in% iso2clist) #School enrollment, secondary, male (% net)

WDI_enrollment_secondary <-  WDI(indicator = "SE.SEC.ENRR", start =1989, end =2017, country = 'all')%>% 
  filter(iso2c %in% iso2clist) # Behold+ locf

dbWriteTable(con, "wdi_secondary_male_enrollment", 
             value = WDI_enrollment, overwrite = TRUE, row.names = FALSE)
dbWriteTable(con, "wdi_secondary_enrollment", 
             value = WDI_enrollment_secondary, overwrite = TRUE, row.names = FALSE)


rm(WDI_enrollment,WDI_enrollment_secondary)


#####################################
#             Agriculture           #
#####################################
WDIsearch('land')

WDI_arable_land <-  WDI(indicator ="AG.LND.ARBL.ZS", start = 1989, end = 2017,  country='all') %>%   
  filter(iso2c %in% iso2clist)

dbWriteTable(con, "wdi_arable_land", 
             value = WDI_arable_land, overwrite = TRUE, row.names = FALSE)

rm(WDI_arable_land)

# AG.LND.ARBL.ZS = Arable land (% of land area)



###################################
#Population
###################################
#WDIsearch(string = "population", field = "name", short = TRUE)

WDI_population <- WDI(country="all", indicator = 'SP.POP.TOTL', start =1989, end=2017) %>%   
  filter(iso2c %in% iso2clist)

dbWriteTable(con, "wdi_population", 
             value = WDI_population, overwrite = TRUE, row.names = FALSE)

rm(WDI_population)



##################################
#            Countries           #
##################################
setwd(DataCave)
Countries <-  fread("Lande.csv")
dbWriteTable(con, "countries",
             value = Countries, overwrite = TRUE, row.names = FALSE)




