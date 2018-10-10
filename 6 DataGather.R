cat("\014") 

######################################################################
###                                                                ###
###                 LoTR -  Gathering of the data                  ###
###                                                                ###
###################################################################### 

# I mit conflict datasæt har jeg country (navn), country_id(p4n)

# I states har jeg iso3c, gwcode

# I Gdelt har jeg fips


#Creating a base country year month structure
CountryCodelistPanel <-  codelist_panel %>% 
  select(country.name.en,iso3c, p4n, fips) %>% 
  filter(!is.na(p4n), !is.na(iso3c), !is.na(fips))

CountryCodelistPanel <-  unique(CountryCodelistPanel)
CountryCodelistPanel <- CountryCodelistPanel

StatesBase <-  CountryCodelistPanel %>% 
  group_by(country.name.en, iso3c,p4n,fips) %>% 
  expand(year= 2000:2017, month = 1:12) # giver en tabel med 36936 country-months
rm(CountryCodelistPanel)




# joining conflicts to dateset
DataSet <- StatesBase %>% 
  left_join(GedAggregated, by = c("p4n" = "country_id", "year" = "year", "month" = "month"))
rm(StatesBase,GedAggregated)


# joining gdelt to dataset
DataSet <-  DataSet %>% 
  left_join(GdeltGroup, by = c("fips"= "country", "year"="year", "month"="month")) 
rm(GdeltGroup)

#Samling a WDI i en dataframe

DataSet<-  DataSet %>% 
  left_join(WDI, by = c("iso3c" ="isoalpha3", "year" = "year"))
rm(WDI)


#Data type fixer
DataSet$conflict_incidents <- as.numeric(DataSet$conflict_incidents)

#fixing the NA <- 0
DataSet <- DataSet %>% mutate(total_deaths_month = coalesce(total_deaths_month,0))
DataSet <- DataSet %>% mutate(nummentions = coalesce(nummentions,0))
DataSet <- DataSet %>% mutate(conflict_incidents = coalesce(conflict_incidents,0))
DataSet <- DataSet %>% mutate(total_deaths_year = coalesce(total_deaths_year,0))
DataSet <- DataSet %>% mutate(civilwar = coalesce(civilwar,0))
DataSet <- DataSet %>% mutate(civilwar_month = coalesce(civilwar_month,0))
DataSet <- DataSet %>% mutate(cw_month_contribute = coalesce(cw_month_contribute,0))
DataSet <- DataSet %>% mutate(deaths_running_months = coalesce(deaths_running_months,0))
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


a <- DataSet %>% mutate(gov_debt = coalesce(gov_debt, mean(gov_debt)))


a <-  DataSet[missing_debt,]$gov_debt

DataSet <- DataSet[missing_debt, gov_debt := colMeans(gov_debt)]

conflict <-  conflict[, deaths_running_months := cumsum(total_deaths_month), by=list(country, year)]



DataSet_sub <- DataSet %>%  
  filter(year >= 2013)



