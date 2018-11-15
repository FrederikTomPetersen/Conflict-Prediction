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
  select(country.name.en,iso3c, p4n, fips) %>% 
  filter(!is.na(p4n), !is.na(iso3c), !is.na(fips))

CountryCodelistPanel <-  unique(CountryCodelistPanel)
CountryCodelistPanel <- CountryCodelistPanel

StatesBase <-  CountryCodelistPanel %>% 
  group_by(country.name.en, iso3c,p4n,fips) %>% 
  expand(year= 1989:2017, month = 1:12) # giver en tabel med 36936 country-months
rm(CountryCodelistPanel)



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

Dataset <- Dataset %>% 
  left_join(fl_mnt, by = c("p4n" = "ccode")) %>% 
  left_join(fl_eth, by = c("p4n" = "ccode")) %>% 
  left_join(fl_rel, by = c("p4n" = "ccode"))

noinfo <- Dataset %>% 
  filter(is.na(mtnest)) %>% 
  select(country.name.en.x, iso3c,p4n,fips) %>% 
  unique()

rm(fl_mnt, fl_rel, fl_eth, noinfo)


##############################################
#Joining Polity IV to Dataset
##############################################

polity4 <-  dbGetQuery(con, "SELECT * from polity_4")
Dataset <- Dataset %>% 
  left_join(polity4, by = c("p4n" = "ccode", "year"="year"))
rm(polity4)


##############################################
#  Adding WDI to Dataset
##############################################

wdi_arable_land <-  dbGetQuery(con, "SELECT * from wdi_arable_land") %>% 
  select(-country)
wdi_export_fmm <-  dbGetQuery(con, "SELECT * from wdi_export_fmm") %>% 
  select(-country)
wdi_export_gs <-  dbGetQuery(con, "SELECT * from wdi_export_gs") %>% 
  select(-country)
wdi_export_me <-  dbGetQuery(con, "SELECT * from wdi_export_me") %>% 
  select(-country)
wdi_gdp <-  dbGetQuery(con, "SELECT * from wdi_gdp") %>% 
  select(-country)
wdi_population <-  dbGetQuery(con, "SELECT * from wdi_population") %>% 
  select(-country)
wdi_secondary_male_enrollment <-  dbGetQuery(con, "SELECT * from wdi_secondary_male_enrollment") %>% 
  select(-country)


wdi <-  wdi_gdp %>%  
  left_join(wdi_arable_land, by = c("iso2c" = "iso2c", "year" = "year")) %>% 
  left_join(wdi_population, by = c("iso2c" = "iso2c", "year" = "year")) %>% 
  left_join(wdi_export_fmm, by = c("iso2c" = "iso2c", "year" = "year")) %>% 
  left_join(wdi_export_gs, by = c("iso2c" = "iso2c", "year" = "year")) %>% 
  left_join(wdi_export_me, by = c("iso2c" = "iso2c", "year" = "year")) %>% 
  left_join(wdi_secondary_male_enrollment, by = c("iso2c" = "iso2c", "year" = "year"))

rm(wdi_arable_land,wdi_export_fmm,wdi_export_gs,wdi_export_me,wdi_gdp,wdi_population,wdi_secondary_male_enrollment)


wdi <- wdi %>% 
  transmute(iso2c = iso2c,
            year = year,
            gdp = NY.GDP.PCAP.PP.KD,
            arableland = AG.LND.ARBL.ZS,
            secondary_school_male = SE.SEC.NENR.MA,
            fuel = TX.VAL.FUEL.Zs.UN,
            population= SP.POP.TOTL,
            merc_export = BX.GSR.MRCH.CD,
            goodservice_export = NE.EXP.GNFS.ZS
  )

#imputation on wdi

#Mean imputation - kan foretages på variable, hvor jeg forventer en nogenlunde konstant værdi eksempelvis arable land
wdi$arableland[is.na(wdi$arableland)] <- ave(wdi$arableland, 
                                             wdi$iso2c, 
                                             FUN=function(x)mean(x, na.rm = T))[is.na(wdi$arableland)]


# lineær imputation -kan foretages på variable, der har en lineær udvikling
#wdi$population[is.na(wdi$population)] <- impute_lm(wdi, population ~ year | iso2c)
#wdi$population <-  as.numeric(unlist(wdi$population[]))


Dataset <- Dataset %>% 
  left_join(wdi, by = c("fips" = "iso2c", "year"="year")) 

rm(wdi)

##############################################
#  Joining Gdelt to Dataset
##############################################
gdelt <-  dbGetQuery(con, "SELECT * from gdelt_group27") #OBS dataet her skal opdateres
Dataset <-  Dataset %>% 
  left_join(gdelt, by = c("fips"= "country", "year"="year", "month"="month")) 
rm(gdelt)



##############################################
#  removing keys
##############################################
Dataset <- Dataset %>% 
  select(-iso3c,-p4n,-fips,-country.name.en.y,-country, -iso2c, -geometry)


complete <- complete.cases(Dataset)
completedata <- Dataset[complete,] # avvv - impute??!!



##############################################
#  Fixing data types and imputing 0 too no obs data
##############################################


#Data type fixer
DataSet$conflict_incidents <- as.numeric(DataSet$conflict_incidents)

#fixing the NA <- 0
Dataset <- Dataset %>% mutate(deaths = coalesce(deaths,0),
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

DataSet <- DataSet %>% mutate(goldstein = coalesce(goldstein,0))
DataSet <- DataSet %>% mutate(avgtone = coalesce(avgtone,0))
DataSet <- DataSet %>% mutate(q1nm = coalesce(q1nm,0))
DataSet <- DataSet %>% mutate(q1gs = coalesce(q1gs,0))
DataSet <- DataSet %>% mutate(q1at = coalesce(q1at,0))
DataSet <- DataSet %>% mutate(q2nm = coalesce(q2nm,0))
DataSet <- DataSet %>% mutate(q2gs = coalesce(q2gs,0))
DataSet <- DataSet %>% mutate(q2at = coalesce(q2at,0))
DataSet <- DataSet %>% mutate(q3nm = coalesce(q3nm,0))
DataSet <- DataSet %>% mutate(q3gs = coalesce(q3gs,0))
DataSet <- DataSet %>% mutate(q3at = coalesce(q3at,0))
DataSet <- DataSet %>% mutate(q4nm = coalesce(q4nm,0))
DataSet <- DataSet %>% mutate(q4gs = coalesce(q4gs,0))
DataSet <- DataSet %>% mutate(q4at = coalesce(q4at,0))
DataSet <- DataSet %>% mutate(q1cnt = coalesce(q1cnt,0))
DataSet <- DataSet %>% mutate(q2cnt = coalesce(q2cnt,0))
DataSet <- DataSet %>% mutate(q3cnt = coalesce(q3cnt,0))
DataSet <- DataSet %>% mutate(q4cnt = coalesce(q4cnt,0))

DataSet <-  DataSet %>% 
  mutate(country = country.name.en)



NA2mean <- function(x){x <- x %>% 
  group_by(country) %>% 
  replace(x, is.na(x), mean(x, na.rm = TRUE))}

DataSet$gov_debt <- NA2mean(DataSet$gov_debt)


replace(DF, TRUE, lapply(DF, NA2mean))




