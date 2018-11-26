cat("\014") 

######################################################################
###                                                                ###
###                 LoTR -  Gathering of the data                  ###
###                                                                ###
###################################################################### 

##############################################
#Creating a base country year month structure
##############################################

CountryCodelistPanel <-  codelist_panel %>% 
  select(country.name.en,iso3c, iso2c, p4n, fips) %>% 
  filter(!is.na(p4n), !is.na(iso3c), !is.na(fips), !is.na(iso2c))

CountryCodelistPanel <-  unique(CountryCodelistPanel)
CountryCodelistPanel <- CountryCodelistPanel

StatesBase <-  CountryCodelistPanel %>% 
  group_by(country.name.en, iso3c,p4n,fips,iso2c) %>% 
  expand(year= 1989:2017, month = 1:12) # giver en tabel med 36936 country-months
rm(CountryCodelistPanel)


# I dette data ligger der en række dubeletter ( colombia, tyskland, korea, bare for at vælge et par stykker. Her skal jeg lave en frasortering!)



##############################################
# Joining conflicts to dataset
##############################################

GedAggregated <-  dbGetQuery(con, "SELECT * from ged_aggregated")
Dataset <- StatesBase %>% 
  left_join(GedAggregated, by = c("p4n" = "p4n", "year" = "year", "month" = "month"))
rm(StatesBase,GedAggregated)


##############################################
#Joining Ethinc Power Relations to dataset
##############################################

epr <-  dbGetQuery(con, "SELECT * from epr_grouped")
Dataset <- Dataset %>% 
  left_join(epr, by = c("p4n" = "gwid", "year" = "year", "month" = "month"))

noinfo <- Dataset %>% 
  filter(is.na(groupscount)) %>% 
  select(country.name.en.x, iso3c,p4n,fips) %>% 
  unique()


Dataset <- Dataset %>% 
  mutate(powerexcludednmb = ifelse(is.na(groupscount),0,powerexcludednmb),
         powerexcludedprop = ifelse(is.na(groupscount),0,powerexcludedprop),
         powersharennb = ifelse(is.na(groupscount),0,powersharennb),
         powershareprop = ifelse(is.na(groupscount),0,powershareprop),
         powerrulenmb = ifelse(is.na(groupscount),0,powerrulenmb),
         powerruleprop = ifelse(is.na(groupscount),0,powerruleprop),
         powerirrelevantnmb = ifelse(is.na(groupscount),1,powerirrelevantnmb),
         powerirrelevantprop = ifelse(is.na(groupscount),1,powerirrelevantprop),
         groupscount= ifelse(is.na(groupscount),1,groupscount))



rm(epr, noinfo)


##############################################
#Joining Ethinc refurgees to dataset
##############################################

epr_er <-  dbGetQuery(con, "SELECT * from epr_er") %>% 
  select(-coo)
Dataset <- Dataset %>% 
  left_join(epr_er, by = c("p4n" = "ccode_coo", "year" = "year"))
Dataset <- Dataset %>% mutate(refurgescnt = ifelse(is.na(refurgescnt),0,refurgescnt))

rm(epr_er)


##############################################
# Joining data from Fearon and Laitin to Dataset
##############################################

fl_mnt <-  dbGetQuery(con, "SELECT * from fl_mountains")
fl_eth <-  dbGetQuery(con, "SELECT * from fl_ethnicfrac")
fl_rel <-  dbGetQuery(con, "SELECT * from fl_relfrac")
fl_oil <-  dbGetQuery(con, "SELECT * from fl_oil")

Dataset <- Dataset %>% 
  left_join(fl_mnt, by = c("p4n" = "ccode")) %>% 
  left_join(fl_eth, by = c("p4n" = "ccode")) %>% 
  left_join(fl_rel, by = c("p4n" = "ccode")) %>% 
  left_join(fl_oil, by = c("p4n" = "ccode"))

noinfo <- Dataset %>% 
  filter(is.na(mtnest)) %>% 
  select(country.name.en.x, iso3c,p4n,fips,iso2c) %>% 
  unique()

rm(fl_mnt, fl_rel, fl_eth, fl_oil, noinfo)


##############################################
#Joining Polity IV to Dataset
##############################################

polity4 <-  dbGetQuery(con, "SELECT * from polity_4")
Dataset <- Dataset %>% 
  left_join(polity4, by = c("iso2c" = "iso2c", "year"="year"))

Dataset <-  Dataset %>% 
  mutate(polity2 = ifelse(is.na(polity2), -66, polity2),
         autoc = ifelse(is.na(autoc), -66, autoc),
         democ = ifelse(is.na(democ), -66, democ)) %>% 
  select(-ccode)

noinfo1 <- Dataset %>% 
  filter(is.na(democ)) %>% 
  select(country.name.en.x, iso3c,p4n,fips) %>% 
  unique()

rm(polity4, noinfo1)



##############################################
#  Adding area to Dataset
##############################################


area <- dbGetQuery(con, "SELECT * from area") %>% 
  select(-geometry)

Dataset <- Dataset %>% 
  left_join(area, by = c("iso2c" = "ISO2")) 
rm(area)

##############################################
#  Adding WDI to Dataset
##############################################

wdi_population <-  dbGetQuery(con, "SELECT * from wdi_population") %>%
  select(-country) %>% 
  group_by(iso2c) %>%
  impute_lm(SP.POP.TOTL ~ year) 


wdi_arable_land <-  dbGetQuery(con, "SELECT * from wdi_arable_land") %>% 
  select(-country) 

wdi_arable_land <- wdi_arable_land %>% 
  group_by(iso2c) %>% 
  na.locf(is.na(wdi_arable_land$AG.LND.ARBL.ZS), fromLast=T)


#human capital ~ school
wdi_secondary_enrollment <- dbGetQuery(con, "SELECT * from wdi_secondary_enrollment") %>% 
  select(-country)
wdi_secondary_enrollment <-  wdi_secondary_enrollment %>% 
  group_by(iso2c) %>% 
  na.locf(is.na(wdi_secondary_enrollment$SE.SEC.ENRR), fromLast=T)


wdi_secondary_enrollment_male <- dbGetQuery(con, "SELECT * from wdi_secondary_male_enrollment") %>% 
  select(-country)
wdi_secondary_enrollment_male <- wdi_secondary_enrollment_male %>% 
  group_by(iso2c) %>% 
  na.locf(is.na(wdi_secondary_enrollment_male$SE.SEC.NENR.MA), fromLast=T)



#Trade
wdi_trade <-  dbGetQuery(con, "SELECT * from wdi_trade") %>% 
  select(-country)
wdi_import_gs <-  dbGetQuery(con, "SELECT * from wdi_import_gs") %>% 
  select(-country)


#Growth and welfare
wdi_gdp <-  dbGetQuery(con, "SELECT * from wdi_gdp") %>% 
  select(-country)
wdi_gdp_cd <-  dbGetQuery(con, "SELECT * from wdi_gdp_cd") %>% 
  select(-country)
wdi_growth <-  dbGetQuery(con, "SELECT * from wdi_growth") %>% 
  select(-country)



wdi <-  wdi_population %>%  
  left_join(wdi_arable_land, by = c("iso2c" = "iso2c", "year" = "year")) %>% 
  left_join(wdi_gdp, by = c("iso2c" = "iso2c", "year" = "year")) %>% 
  left_join(wdi_gdp_cd, by = c("iso2c" = "iso2c", "year" = "year")) %>% 
  left_join(wdi_growth, by = c("iso2c" = "iso2c", "year" = "year")) %>% 
  left_join(wdi_trade, by = c("iso2c" = "iso2c", "year" = "year")) %>% 
  left_join(wdi_import_gs, by = c("iso2c" = "iso2c", "year" = "year")) %>% 
  left_join(wdi_secondary_enrollment, by = c("iso2c" = "iso2c", "year" = "year")) %>% 
  left_join(wdi_secondary_enrollment_male, by = c("iso2c" = "iso2c", "year" = "year")) 


rm(wdi_population,wdi_arable_land,wdi_gdp,wdi_gdp_cd,wdi_growth,wdi_trade,wdi_import_gs,wdi_secondary_enrollment,wdi_secondary_enrollment_male)

wdi <- wdi %>% 
  transmute(
            year = year,
            gdp_p_ca = NY.GDP.PCAP.PP.KD,
            gdp_cd = NY.GDP.MKTP.CD,
            growth = NY.GDP.MKTP.KD.ZG,
            arableland = AG.LND.ARBL.ZS,
            secondary_school = SE.SEC.ENRR,
            secondary_school_male = SE.SEC.NENR.MA,
            trade = NE.TRD.GNFS.ZS,
            population= SP.POP.TOTL,
            import_gs = NE.IMP.GNFS.ZS
  )



Dataset <- Dataset %>% 
  left_join(wdi, by = c("iso2c" = "iso2c", "year"="year")) 

rm(wdi)

##############################################
#  Joining Gdelt to Dataset
##############################################
gdelt <-  dbGetQuery(con, "SELECT * from gdelt_group_complete") 

# I tilfælde af overlappen observationer
gdelt <- gdelt %>%
  group_by(country, year, month) %>%
  filter(row_number() == 1)


Dataset <-  Dataset %>% 
  left_join(gdelt, by = c("iso2c"= "country", "year"="year", "month"="month")) 
rm(gdelt)


##############################################
#  Fixing data types and imputing 0 too no obs data
##############################################


#fixing the NA <- 0
Dataset <- Dataset %>% 
  mutate(deaths = coalesce(deaths,0),
        Incidents = coalesce(Incidents,0),
        sideA = coalesce(sideA,0),
        sideB = coalesce(sideB,0),
        deaths_running_month = coalesce(deaths_running_month,0),
        deathyear = coalesce(deathyear,0),
        deathsuma = coalesce(deathsuma,0),
        deathsumb = coalesce(deathsumb,0),
        cwy = coalesce(cwy,0),
        cwm = coalesce(cwm,0)
        )

Dataset <-  Dataset %>% 
  mutate(
    q1at = coalesce(q1at,0),
    q1cnt  = coalesce(q1cnt,0),
    q1gs  = coalesce(q1gs,0),
    q2at  = coalesce(q2at,0),
    q2cnt  = coalesce(q2cnt,0),
    q2gs  = coalesce(q2gs,0),
    q3at  = coalesce(q3at,0),
    q3cnt  = coalesce(q3cnt,0),
    q3gs  = coalesce(q3gs,0),
    q4at  = coalesce(q4at,0),
    q4cnt = coalesce(q4cnt,0),
    q4gs = coalesce(q4gs,0),
    
    ethq1at = coalesce(ethq1at,0),
    ethq1cnt = coalesce(ethq1cnt,0),
    ethq1gs = coalesce(ethq1gs,0),
    ethq2at = coalesce(ethq2at,0),
    ethq2cnt = coalesce(ethq2cnt,0),
    ethq2gs = coalesce(ethq2gs,0),
    ethq3at = coalesce(ethq3at,0),
    ethq3cnt = coalesce(ethq3cnt,0),
    ethq3gs = coalesce(ethq3gs,0),
    ethq4at = coalesce(ethq4at,0),
    ethq4cnt = coalesce(ethq4cnt,0),
    ethq4gs = coalesce(ethq4gs,0),
    
    relq1at = coalesce(relq1at,0),
    relq1cnt = coalesce(relq1cnt,0),
    relq1gs = coalesce(relq1gs,0),
    relq2at = coalesce(relq2at,0),
    relq2cnt = coalesce(relq2cnt,0),
    relq2gs = coalesce(relq2gs,0),
    relq3at = coalesce(relq3at,0),
    relq3cnt = coalesce(relq3cnt,0),
    relq3gs = coalesce(relq3gs,0),
    relq4at = coalesce(relq4at,0),
    relq4cnt = coalesce(relq4cnt,0),
    relq4gs = coalesce(relq4gs,0)
  )




#Creating means for previous periods

#---------------------------
# 6 months 
Dataset <- rolling_deviation(Dataset, p4n, q1cnt, 6)
Dataset <- rolling_deviation(Dataset, p4n, q2cnt, 6)
Dataset <- rolling_deviation(Dataset, p4n, q3cnt, 6)
Dataset <- rolling_deviation(Dataset, p4n, q4cnt, 6)
Dataset <- rolling_deviation(Dataset, p4n, relq1cnt, 6)
Dataset <- rolling_deviation(Dataset, p4n, relq2cnt, 6)
Dataset <- rolling_deviation(Dataset, p4n, relq3cnt, 6)
Dataset <- rolling_deviation(Dataset, p4n, relq4cnt, 6)
Dataset <- rolling_deviation(Dataset, p4n, ethq1cnt, 6)
Dataset <- rolling_deviation(Dataset, p4n, ethq2cnt, 6)
Dataset <- rolling_deviation(Dataset, p4n, ethq3cnt, 6)
Dataset <- rolling_deviation(Dataset, p4n, ethq4cnt, 6)

Dataset <- rolling_deviation(Dataset, p4n, q1at, 6)
Dataset <- rolling_deviation(Dataset, p4n, q2at, 6)
Dataset <- rolling_deviation(Dataset, p4n, q3at, 6)
Dataset <- rolling_deviation(Dataset, p4n, q4at, 6)
Dataset <- rolling_deviation(Dataset, p4n, relq1at, 6)
Dataset <- rolling_deviation(Dataset, p4n, relq2at, 6)
Dataset <- rolling_deviation(Dataset, p4n, relq3at, 6)
Dataset <- rolling_deviation(Dataset, p4n, relq4at, 6)
Dataset <- rolling_deviation(Dataset, p4n, ethq1at, 6)
Dataset <- rolling_deviation(Dataset, p4n, ethq2at, 6)
Dataset <- rolling_deviation(Dataset, p4n, ethq3at, 6)
Dataset <- rolling_deviation(Dataset, p4n, ethq4at, 6)

Dataset <- rolling_deviation(Dataset, p4n, q1gs, 6)
Dataset <- rolling_deviation(Dataset, p4n, q2gs, 6)
Dataset <- rolling_deviation(Dataset, p4n, q3gs, 6)
Dataset <- rolling_deviation(Dataset, p4n, q4gs, 6)
Dataset <- rolling_deviation(Dataset, p4n, relq1gs, 6)
Dataset <- rolling_deviation(Dataset, p4n, relq2gs, 6)
Dataset <- rolling_deviation(Dataset, p4n, relq3gs, 6)
Dataset <- rolling_deviation(Dataset, p4n, relq4gs, 6)
Dataset <- rolling_deviation(Dataset, p4n, ethq1gs, 6)
Dataset <- rolling_deviation(Dataset, p4n, ethq2gs, 6)
Dataset <- rolling_deviation(Dataset, p4n, ethq3gs, 6)
Dataset <- rolling_deviation(Dataset, p4n, ethq4gs, 6)

#---------------------------
# deaths 

Dataset <- rolling_deviation(Dataset, p4n, deaths, 3)
Dataset <- rolling_deviation(Dataset, p4n, deaths, 6)
Dataset <- rolling_deviation(Dataset, p4n, deaths, 12)

# incidents
Dataset <- rolling_deviation(Dataset, p4n, Incidents, 3)
Dataset <- rolling_deviation(Dataset, p4n, Incidents, 6)
Dataset <- rolling_deviation(Dataset, p4n, Incidents, 12)


#---------------------------------------------------------------------------
#           Oprettelse af laggede variable
#  

Dataset <-  Dataset %>% 
  group_by(p4n) %>% 
  mutate(pop_growth = log(population) - log(lag(population, n = 12)),
         pop = log(population),
         gdp_log = log(gdp_cd),
         refurgescnt = log(refurgescnt)) %>% 
  select(-gdp_cd)



#------------------------------------------------------------------------------
# Forberedelse af data til modellering - 
# I stedet for at lagge alle mine prædikatorer, leader jeg min responsvariable
#

#---------------Lead death ----------------------

complete_data_death <-  Dataset %>%
  group_by(p4n)%>% 
  mutate(deaths = lead(deaths),
         country = country.name.en.x) %>% 
  ungroup() %>% 
  select(-iso3c,-p4n,-fips,-country.name.en.y, -iso2c, -geometry,-country.name.en.x, -country.x,-country.y)

complete <- complete.cases(complete_data_death)
complete_data_death <- complete_data_death[complete,] # 46298 obs

dbWriteTable(con, "complete_data_death", 
             value = complete_data_death, overwrite = TRUE, row.names = FALSE)



#------------------Lead death year-----------------------------

complete_data_deathyear <-  Dataset %>%
  group_by(p4n)%>% 
  mutate(deathyear = lead(deathyear, n =12),
         country = country.name.en.x) %>% 
  ungroup() %>% 
  select(-iso3c,-p4n,-fips,-country.name.en.y, -iso2c, -geometry,-country.name.en.x, -country.x,-country.y)

complete <- complete.cases(complete_data_deathyear)
complete_data_deathyear <- complete_data_deathyear[complete,] # 45000

dbWriteTable(con, "complete_data_deathyear",
             value = complete_data_deathyear, overwrite = T, row.names = F)


#-----------------Lead incidents------------------------

complete_data_incident <-  Dataset %>%
  group_by(p4n)%>% 
  mutate(incidents = lead(Incidents),
         country = country.name.en.x) %>% 
  select(-Incidents) %>% 
  ungroup()  %>% 
  select(-iso3c,-p4n,-fips,-country.name.en.y, -iso2c, -geometry,-country.name.en.x, -country.x,-country.y)

complete <- complete.cases(complete_data_incident)
complete_data_incident <- complete_data_incident[complete,] # 46298

dbWriteTable(con, "complete_data_incident",
             value= complete_data_incident, overwrite =T, row.names=F)

#-------------------lead cw start----------------------------
data_sub <-Dataset %>%
  filter(cwy == 1) %>%
  group_by(p4n, year) %>%
  arrange(p4n, year, month) %>% 
  filter(row_number() == 1) %>%
  group_by(p4n) %>%
  mutate(is_contiguous = (year -1==lag(year))) 

data_sub <- data_sub %>%
  mutate(is_contiguous = case_when(is.na(is_contiguous) ~ FALSE,
                                   TRUE~is_contiguous)) %>%
  mutate(gvar = cumsum(!is_contiguous)) %>%
  group_by(p4n, gvar) %>%
  mutate(minyear = min(year),
         maxyear = max(year)) %>%
  ungroup() %>%
  filter(is_contiguous == F | is.na(is_contiguous)) %>% 
  mutate(cwstart = minyear, cwend = maxyear) %>% 
  select(p4n, year, month,cwstart,cwend)


data_sub <- Dataset %>% 
  left_join(data_sub, by = c("p4n" = "p4n", "year" = "year", "month" = "month")) %>% 
  mutate(cwstart = coalesce(cwstart,0),
         cwend = coalesce(cwend,0))

data_sub <- data_sub %>% 
  mutate(cwstart = ifelse(cwstart>0,1,0),
         cwend = ifelse(cwend>0,1,0)) %>% 
  ungroup()

complete_data_cwstart <-  data_sub %>% 
  mutate(cwstart = lead(cwstart),
         country = country.name.en.x) %>% 
  select(-cwend,-iso3c,-p4n,-fips,-country.name.en.y, -iso2c, -geometry,-country.name.en.x, -country.x,-country.y)

complete <- complete.cases(complete_data_cwstart)
complete_data_cwstart <- complete_data_cwstart[complete,] #46415

dbWriteTable(con, "complete_data_cwstart",
             value = complete_data_cwstart, overwrite = T, row.names=F)

rm(data_sub)

#---------------------------lead cwm---------------------------
complete_data_cwm <-  Dataset %>%
  group_by(p4n)%>% 
  mutate(cwm = lead(cwm),
         country = country.name.en.x) %>% 
  ungroup() %>% 
  select(-iso3c,-p4n,-fips,-country.name.en.y, -iso2c, -geometry,-country.name.en.x, -country.x,-country.y,-deathsuma,-deathsumb)

complete <- complete.cases(complete_data_cwm)
complete_data_cwm <- complete_data_cwm[complete,] # 46298

dbWriteTable(con, "complete_data_cwm",
             value= complete_data_cwm, overwrite = T, row.names=F)

rm(complete_data_cwm,complete_data_cwstart,complete_data_deathyear, complete_data_death,complete_data_incident)


##############################################
#  removing keys
##############################################
Dataset <- ungroup(Dataset)
Dataset <- Dataset %>% 
  select(-iso3c,-p4n,-fips,-country.name.en.y, -iso2c, -geometry,-country.name.en.x, -country.x,-country.y)

Dataset <- Dataset %>% 
  mutate(country = country.name.en.x) %>% 
  select(-country.name.en.x)

complete <- complete.cases(Dataset)
completedata <- Dataset[complete,] # 46416

dbWriteTable(con, "complete_data", 
             value = completedata, overwrite = TRUE, row.names = FALSE)

rm(Dataset, data, Countries)

