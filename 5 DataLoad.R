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
Events <- Event_url_list[2:28]

setwd(DataCave)
Gdelt_header  <-  fread("GDELT_HEADER.csv")
Gdelt_header <-  Gdelt_header$`Field Name`
collist <- Gdelt_header[1:57]

Gdelt_getter_1(Events,1)


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

Gdelt_getter_2(Events,1)





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
Events <- Event_url_list[116:1845] # slutter 31/12-2017

setwd(DataCave)
Gdelt_header  <-  fread("GDELT_HEADER.csv")
Gdelt_header <-  Gdelt_header$`Field Name`
collist <- Gdelt_header[1:58]


Gdelt_getter_3(Events,1039)

#970
[1] "Dette er download rÃ¦kke nummer 1671"






setwd(DataCave)

#get list of events
All_eventdb_url <-  get_urls_gdelt_event_log()
Event_url_list <- All_eventdb_url$urlData 
Event_1979_2005 <- Event_url_list[2:28]

#get headers for df
Gdelt_header  <-  fread("GDELT_HEADER.csv")
Gdelt_header <-  Gdelt_header$`Field Name`
collist <- Gdelt_header[1:57]

#Preparing the "for loop"
Iterations <- length(Event_1979_2005)
Iterations_left =Iterations


#List of countries in focus:
Countries <-  fread("Lande.csv")
Countries_List <-  Countries$isoAlpha3

Africa = Countries %>% 
  filter(continent == "AF")
Africa_List <-  Africa$isoAlpha3

#interesting events
Eventtypes <-  readRDS("C:/Users/Frederik/Documents/GitHub/Ethnic-Conflict-Prediction/Data/EventCodes.rds")
Eventtypes <- as.numeric(c("1031","1032","1033","1034","104","1041","1042","1043","1044","105","1051","1052","1053","1054","1055","1056","106","107","108","110","111","112","1121","1122","1123","1124","1125","113","114","115","116","120","121","1211","1212","122","122","1222","1223","1224","123","1231","1232","1233","1234","124","1241","1242","1243","124","1245","1246","125","126","127","128","129","130","131","1311","1312","1313","132","1321","1322","1323","1324","133","134","135","136","137","138","1381","1382","1383","1384","1385","139","140","141","1411","1412","1413","1414","142","1421","1422","1423","1424","143","1431","1432","1433","1434","144","1441","1442","1443","1444","145","150","151","152","153","154","155","160","161","162","1621","1622","1623","163","164","165","166","1661","1662","1663","170","171","1711","1712","172","1721","1722","1723","1724","173","174","175","176","181","1821","191","192"))

Output <-  Gdelt_getter(Event_1979_2005, 1)

### wrrting to postgres

dbWriteTable(con, "Table", 
             value = tabletest, overwrite = TRUE, row.names = FALSE)

# query the data from postgreSQL 
Gdelt <- dbGetQuery(con, "SELECT * from gdelt_work_dataset")

rm(All_eventdb_url, Output,Gdelt_Data, Iterations, Tablename, Iterations_left,collist, Basefile, Basename, Event,Event_1979_1981,Event_1979_1985,Event_1979_2005,Event_url_list,Eventtypes)


############################################################



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
    select(country, reagion, country_id,year,deaths_a,deaths_b, deaths_civ, deaths_unk,date_start,date_end,gwnob)
  
ged_disaggregated <-  ged181
rm(ged181)
rm(direct_link)
unlink("ged181-shp.zip")

#Writing to postgres
dbWriteTable(con, "ged_disaggregated", 
             value = ged_disaggregated, overwrite = TRUE, row.names = FALSE)






############################################################
#                                                          #
#   Accessing the World Banks development indicators       #
#     GDP, debt, expenditure and school enrollment         #
#                                                          #
############################################################



#####################################
#    GDP pr capita 2011 Ccnstant    #
#####################################

#GDP pr capita 2011 constant
WDIsearch('gdp.*capita.*constant')
GDP_capita_2011c_country = WDI(indicator='NY.GDP.PCAP.PP.KD', start=1979, end=2017, extra=T, country = 'all')

dbWriteTable(con, "wdi_gdp", 
             value = GDP_capita_2011c_country, overwrite = TRUE, row.names = FALSE)

rm(GDP_capita_2011c_country)




#####################################
####    Goverment expenditure     ###
#####################################

WDIsearch('expenditure')
gov_expenditure =  WDI(indicator ='NE.DAB.TOTL.ZS', start=1979, end=2017, extra=T, country = 'all')   #Expenditure, total (% of GDP)

dbWriteTable(con, "wdi_gov_expenditure", 
             value = gov_expenditure, overwrite = TRUE, row.names = FALSE)
rm( gov_expenditure)




#####################################
####       Goverment debt         ###
#####################################
WDIsearch('debt')
GOV_debt <-  WDI(indicator = 'GC.DOD.TOTL.GD.ZS', start=1979, end=2017, extra=T, country = 'all')   # Central government debt, total (% of GDP)

dbWriteTable(con, "wdi_gov_debt", 
             value = GOV_debt, overwrite = TRUE, row.names = FALSE)
rm(GOV_debt)



#####################################
# secondary male school enrollment  #
#####################################
WDIsearch('enrollment')
WDI_enrollment <- WDI(indicator ='SE.SEC.NENR.MA', start=1979, end=2017, extra=T, country = 'all') #School enrollment, secondary, male (% net)

dbWriteTable(con, "wdi_secondary_male_enrollment", 
             value = WDI_enrollment, overwrite = TRUE, row.names = FALSE)

rm(WDI_enrollment)


#####################################
#             Agriculture           #
#####################################
WDIsearch('land')
WDI_arable_land <-  WDI(indicator ='AG.LND.ARBL.ZS', start = 1979, end = 2018, extra =T, country='all')

dbWriteTable(con, "wdi_arable_land", 
             value = WDI_arable_land, overwrite = TRUE, row.names = FALSE)

rm(WDI_arable_land)
# AG.LND.ARBL.ZS = Arable land (% of land area)


#####################################
# Exports of goods and services (% of GDP)                #
#####################################
WDIsearch('export')
WDI_export_GS <-  WDI(indicator = 'NE.EXP.GNFS.ZS', start = 1979, end = 2018, extra =T, country='all')

dbWriteTable(con, "wdi_export_gs", 
             value = WDI_export_GS, overwrite = TRUE, row.names = FALSE)

rm(WDI_export_GS)
# "NE.EXP.GNFS.ZS"= Exports of goods and services (% of GDP)


#####################################
# Fuels, minerals, and metals                #
#####################################
WDIsearch('Fuels')
WDI_export_FMM <-  WDI(indicator = 'TX.VAL.FMTL.UN.ZS', start = 1979, end = 2017, extra =T, country='all')

dbWriteTable(con, "wdi_export_fmm", 
             value = WDI_export_FMM, overwrite = TRUE, row.names = FALSE)

rm(WDI_export_FMM)
# "TX.VAL.FMTL.UN.ZS"= Fuels, minerals, and metals (% of merchandise exports)

#####################################
# Merchandise exports (BOP): percentage of GDP (%)                #
#####################################
WDIsearch('Merchandise exports')
WDI_export_ME <-  WDI(country= 'all', indicator ='BX.GSR.MRCH.ZS', start = 1979, end = 2018)

dbWriteTable(con, "wdi_export_me", 
             value = WDI_export_ME, overwrite = TRUE, row.names = FALSE)

rm(WDI_export_ME)
# ""BX.GSR.MRCH.ZS""= Merchandise exports (BOP): percentage of GDP (%)

###################################
#Population
###################################
WDIsearch(string = "population", field = "name", short = TRUE)
wdi_population <- WDI(country="all", indicator = 'SP.POP.TOTL', start =1979, end=2017)

dbWriteTable(con, "wdi_population", 
             value = wdi_population, overwrite = TRUE, row.names = FALSE)

rm( wdi_population)


##################################
#            Countries           #
##################################
setwd(DataCave)
Countries <-  fread("Lande.csv")
dbWriteTable(con, "countries",
             value = Countries, overwrite = TRUE, row.names = FALSE)




