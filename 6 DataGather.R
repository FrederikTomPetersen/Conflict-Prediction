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

Dataset <-  Dataset %>% 
  mutate(polity2 = ifelse(is.na(polity2), -66, polity2),
         autoc = ifelse(is.na(autoc), -66, autoc),
         democ = ifelse(is.na(democ), -66, democ))

noinfo1 <- Dataset %>% 
  filter(is.na(democ)) %>% 
  select(country.name.en.x, iso3c,p4n,fips) %>% 
  unique()

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


# lineær imputation kan foretages på variable, der har en lineær udvikling
  
wdi <- wdi %>%
  group_by(iso2c) %>%
  impute_lm(population ~ year) 





Dataset <- Dataset %>% 
  left_join(wdi, by = c("iso2c" = "iso2c", "year"="year")) 

rm(wdi)

##############################################
#  Joining Gdelt to Dataset
##############################################
gdelt <-  dbGetQuery(con, "SELECT * from gdelt_group27") %>% 
  filter(year >=1989)

# I tilfælde af overlappen observationer
gdelt <- gdelt %>%
  group_by(country, year, month) %>%
  filter(row_number() == 1)



Dataset <-  Dataset %>% 
  left_join(gdelt, by = c("iso2c"= "country", "year"="year", "month"="month")) 
rm(gdelt)



##############################################
#  removing keys
##############################################
Dataset <- ungroup(Dataset)
Dataset <- Dataset %>% 
  select(-iso3c,-p4n,-fips,-country.name.en.y,-country, -iso2c, -geometry)

##############################################
#  Fixing data types and imputing 0 too no obs data
##############################################


#Data type fixer
Dataset$conflict_incidents <- as.numeric(Dataset$conflict_incidents)

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
Dataset <-  Dataset %>% 
  mutate(
    q1at = coalesce(q1at,0),
    q1cnt  = coalesce(q1cnt,0),
    q2at  = coalesce(q2at,0),
    q2cnt  = coalesce(q2cnt,0),
    q3at  = coalesce(q3at,0),
    q3cnt  = coalesce(q3cnt,0),
    q4at  = coalesce(q4at,0),
    q4cnt = coalesce(q4cnt,0),
    
    ethq1at = coalesce(ethq1at,0),
    ethq1cnt = coalesce(ethq1cnt,0),
    ethq2at = coalesce(ethq2at,0),
    ethq2cnt = coalesce(ethq2cnt,0),
    ethq3at = coalesce(ethq3at,0),
    ethq3cnt = coalesce(ethq3cnt,0),
    ethq4at = coalesce(ethq4at,0),
    ethq4cnt = coalesce(ethq4cnt,0),
    
    relq1at = coalesce(relq1at,0),
    relq1cnt = coalesce(relq1cnt,0),
    relq2at = coalesce(relq2at,0),
    relq2cnt = coalesce(relq2cnt,0),
    relq3at = coalesce(relq3at,0),
    relq3cnt = coalesce(relq3cnt,0),
    relq4at = coalesce(relq4at,0),
    relq4cnt = coalesce(relq4cnt,0)
  )


#Creating the bases for deviation 
Dataset$date <- as.yearmon(paste(Dataset$year, Dataset$month), "%Y %m")
Dataset$date <-  within(Dataset, Ddte <- sprintf("%d-%02d", year, month))


# Grouped timedependent mean/deviation

# Funktionen skal tage 4 argumenter: 
#     1) Dataframe
#     2) Variable / liste af variabler 
#     3) variable der skal grupperes på
#     4) Tidsperioden, for hvilket gennemsnittet skal udregnes. 


setwd(DataCave)
saveRDS(Dataset,file="data.rds")



grouped_time_mean = function(df,group_var, var, time_in_month){
  
  #df <- Dataset
  #group_var <- "p4n"
  #var <- "q1cnt"
  #time_in_month <- 6
    
  groupvar_q <- enquo(group_var)
  variable_q <- enquo(var)
  varname <- quo_name(variable_q)
  timename <- toString(time_in_month)
  dummy_name <- paste0("t_",timename, "_", varname)
  
  
  df %>% 
    group_by(!! groupvar_q) %>% 
    mutate(
      time2 = date %m-% months(time_in_month),
      !! dummy_name := variable_q - mean(!! variable_q[which(date %within% interval(date-time2))]) %>% 
        select(-"time2")
    )
}
  
a <-  grouped_time_mean(Dataset, p4n, q1cnt, 6)  






maturity <- maturity %m-% months(6)

grouped_mean_imputation <- function(df, group_var, impute_var){
  # Mean imputes a column by groupl
  #
  # Creates a new column imputed_"impute_var": 
  # is 1 if the variable is either imputed, or 
  # still NA (no values in group to impute from).
  # Is 0 if observation is original.
  #
  # Arguments:
  #   df: a dataframe
  #   group_var: the variable to group by.
  #   impute_var: the variable to impute.
  
  
  keys_q <- enquo(group_var)
  values_q <- enquo(impute_var)
  varname <- quo_name(values_q)
  dummy_name <- paste0("imputed_", varname)
  
  df %>%
    group_by(!! keys_q) %>%
    mutate(
      !! dummy_name := case_when(is.na(!! values_q) ~ 1,
                                 T ~ 0), 
      !! varname := case_when(is.na(!! values_q) ~ fixed_mean(!! values_q),
                              T ~ !! values_q)
    ) 
}


















list_of_vars <- c("q1at","q2at","q3at","q4at","q1cnt","q2cnt","q3cnt","q4cnt","relq1at","relq2at","relq3at","relq4at","relq1cnt","relq2cnt","relq3cnt","relq4cnt","ethq1at","ethq2at","ethq3at","ethq4at","ethq1cnt","ethq2cnt","ethq3cnt","ethq4cnt")




meaner_list = function(y,x){
  for (i in x){
    y %>% mutate(paste0("mean",x) := i - mean(i))
  }
}






DataSet <-  DataSet %>% 
  mutate(country = country.name.en)



NA2mean <- function(x){x <- x %>% 
  group_by(country) %>% 
  replace(x, is.na(x), mean(x, na.rm = TRUE))}

DataSet$gov_debt <- NA2mean(DataSet$gov_debt)


replace(DF, TRUE, lapply(DF, NA2mean))



complete <- complete.cases(Dataset)
completedata <- Dataset[complete,] # avvv - impute??!!



