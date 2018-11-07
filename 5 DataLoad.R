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

# Gdelt_getter_1(Events,1)


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

# Gdelt_getter_2(Events,1)





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


# Gdelt_getter_3(Events,1039)


  

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
  filter(is.na(gwnob))

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
#   Accessing the World Banks development indicators       #
#                                                          #
#                                                          #
############################################################

Country <-  codelist_panel %>% 
  select(country.name.en,iso2c) %>% 
  distinct(country.name.en,iso2c) %>% 
  filter(!is.na(iso2c))

iso2clist <- Country$iso2c





#####################################
#    GDP pr capita 2011 Ccnstant    #
#####################################

# problem da der n?rmenst ikke er nogen oplysnigner efter 1990


#GDP pr capita 2011 constant
WDIsearch('gdp.*capita.*constant')
WDI_GDPcapita2011c = WDI(indicator='NY.GDP.PCAP.PP.KD', start=1989, end=2017,  country = 'all') %>% 
  filter(iso2c %in% iso2clist)

dbWriteTable(con, "wdi_gdp", 
             value = WDI_GDPcapita2011c, overwrite = TRUE, row.names = FALSE)

rm(WDI_GDPcapita2011c)




#####################################
####    Goverment expenditure     ###
#####################################

WDIsearch('expenditure')
WDI_govexpenditure =  WDI(indicator ='NE.DAB.TOTL.ZS', start=1989, end=2017,  country = 'all') %>%   
filter(iso2c %in% iso2clist)

dbWriteTable(con, "wdi_gov_expenditure", 
             value = WDI_govexpenditure, overwrite = TRUE, row.names = FALSE)
rm(WDI_govexpenditure)




#####################################
####       Goverment debt         ###
#####################################
WDIsearch('debt')
WDI_govdebt <-  WDI(indicator = 'GC.DOD.TOTL.GD.ZS', start=1989, end=2017,  country = 'all') %>%   
  filter(iso2c %in% iso2clist)   # Central government debt, total (% of GDP)

dbWriteTable(con, "wdi_gov_debt", 
             value = WDI_govdebt, overwrite = TRUE, row.names = FALSE)
rm(WDI_govdebt)



#####################################
# secondary male school enrollment  #
#####################################
WDIsearch('enrollment')
WDI_enrollment <- WDI(indicator ="SE.SEC.NENR.MA", start=1989, end=2017,  country = 'all')%>%   
  filter(iso2c %in% iso2clist) #School enrollment, secondary, male (% net)


dbWriteTable(con, "wdi_secondary_male_enrollment", 
             value = WDI_enrollment, overwrite = TRUE, row.names = FALSE)

rm(WDI_enrollment)


#####################################
#             Agriculture           #
#####################################
WDIsearch('land')

WDI_arable_land <-  WDI(indicator ="AG.LND.ARBL.ZS", start = 1989, end = 2018,  country='all') %>%   
  filter(iso2c %in% iso2clist)

dbWriteTable(con, "wdi_arable_land", 
             value = WDI_arable_land, overwrite = TRUE, row.names = FALSE)

rm(WDI_arable_land)

# AG.LND.ARBL.ZS = Arable land (% of land area)


#####################################
# Exports of goods and services (% of GDP)                #
#####################################
WDIsearch('export')
WDI_export_GS <-  WDI(indicator ="NE.EXP.GNFS.ZS", start = 1989, end = 2018,  country='all')%>%   
  filter(iso2c %in% iso2clist)


dbWriteTable(con, "wdi_export_gs", 
             value = WDI_export_GS, overwrite = TRUE, row.names = FALSE)

rm(WDI_export_GS)


# "NE.EXP.GNFS.ZS"= Exports of goods and services (% of GDP)


#####################################
# Fuels, minerals, and metals                #
#####################################

WDIsearch('Fuels')
WDI_export_FMM <-  WDI(indicator ="TX.VAL.FUEL.Zs.UN", start = 1989, end = 2018, country='all')%>%   
  filter(iso2c %in% iso2clist)

dbWriteTable(con, "wdi_export_fmm", 
             value = WDI_export_FMM, overwrite = TRUE, row.names = FALSE)

rm(WDI_export_FMM)
# "TX.VAL.FMTL.UN.ZS"= Fuels, minerals, and metals (% of merchandise exports)

#####################################
# Merchandise exports (BOP): percentage of GDP (%)                #
#####################################
WDIsearch('Merchandise exports')

WDI_export_ME <-  WDI(indicator ="BX.GSR.MRCH.CD", start = 1989, end = 2018, country='all')%>%   
  filter(iso2c %in% iso2clist)
dbWriteTable(con, "wdi_export_me", 
             value = WDI_export_ME, overwrite = TRUE, row.names = FALSE)

rm(WDI_export_ME)


# ""BX.GSR.MRCH.ZS""= Merchandise exports (BOP): percentage of GDP (%)

###################################
#Population
###################################
WDIsearch(string = "population", field = "name", short = TRUE)
WDI_population <- WDI(country="all", indicator = 'SP.POP.TOTL', start =1989, end=2018)%>%   
  filter(iso2c %in% iso2clist)

dbWriteTable(con, "wdi_population", 
             value = wdi_population, overwrite = TRUE, row.names = FALSE)

rm(WDI_population)



###################################
#Remittance
###################################


WDI_remittance_cd <- WDI(country="all", indicator = 'BX.TRF.MGR.CD', start =1989, end=2018)%>%   
  filter(iso2c %in% iso2clist)

dbWriteTable(con, "wdi_remitance_cd", 
             value = WDI_remittance_cd, overwrite = TRUE, row.names = FALSE)

WDI_remittance_gdp <- WDI(country="all", indicator = 'BX.TRF.MGR.DT.GD.ZS', start =1989, end=2018)%>%   
  filter(iso2c %in% iso2clist)

dbWriteTable(con, "wdi_remitance_gdp", 
             value = WDI_remittance_gdp, overwrite = TRUE, row.names = FALSE)


rm(WDI_remittance_gdp,WDI_remittance_cd)

WDIsearch('remit')



##################################
#            Countries           #
##################################
setwd(DataCave)
Countries <-  fread("Lande.csv")
dbWriteTable(con, "countries",
             value = Countries, overwrite = TRUE, row.names = FALSE)




