cat("\014")  

############################################################
#                                                          #
#                       DataTidy.R                         #
#                                                          #
############################################################

#In this script I will tidy the datasets created in the DataLoad script.
#The tidying should enable join between the dataset creating the completedataset


############################################################
#                                                          #
#                     Tidying GED                          #
#                                                          #
############################################################


conflicts_all <- dbGetQuery(con, "SELECT * from ged_disaggregated")

# Creating approx date
conflicts_all$date_start = as.Date(conflicts_all$date_start) 
conflicts_all$date_end = as.Date(conflicts_all$date_end) 
conflicts_all$date_mid <-  conflicts_all$date_start + floor((conflicts_all$date_end-conflicts_all$date_start)/2) 
conflicts_all <- conflicts_all %>% 
  mutate(
    year = as.numeric(format(date_mid, format = "%Y")),
    month = as.numeric(format(date_mid, format = "%m")),
    day = as.numeric(format(date_mid, format = "%d")))

#creating totaldeaths per feature
conflicts_all$total_deaths <- rowSums(conflicts_all[,c("deaths_a", "deaths_b", "deaths_unk", "deaths_civ")], na.rm=TRUE)

#Creating total deaths per month
conflicts_all <- conflicts_all %>% 
  group_by(country,year,month)
conflict_grouped <- conflicts_all %>% 
  summarise(total_deaths_month = sum(total_deaths),
            conflict_incidents = n()) %>% 
  arrange(country,year,month)

#Creating total deaths per year
a <- conflict_grouped %>% 
  group_by(country,year)
conflict_grouped2 <- a %>% 
  summarise(total_deaths_year = sum(total_deaths_month))

conflict_grouped <- conflict_grouped %>% 
  left_join(conflict_grouped2, by=c("country"="country", "year"="year"))

rm(a, conflict_grouped2)

conflict <- conflict_grouped %>%  
  arrange(country,year,month)

#Running numbers
conflict <-  data.table(conflict)
conflict <-  conflict[, deaths_running_months := cumsum(total_deaths_month), by=list(country, year)]



#Creating base
conflicts_all <- conflicts_all %>% 
  ungroup(country,year,month)
base <- conflicts_all %>% 
  select(country,year, month) %>% 
  expand(country= country, year= 2000:2017, month = 1:12)


#join conflict to base
conflict <- base %>%
  left_join(conflict, by =c("country"="country", "year"="year", "month"="month"))
rm(base, conflict_grouped, conflicts_all,conflict_grouped2, conflict2)


#Civilwar dummies
conflict$civilwar <- ifelse(conflict$total_deaths_year >= 1000,1,0)
conflict$civilwar_month <- ifelse(conflict$deaths_running_months >= 1000,1,0)
conflict$cw_month_contribute <- ifelse(conflict$total_deaths_month >= 83,1,0)


#assigning country_id and region
base_conflict <- dbGetQuery(con, "SELECT * from ged_disaggregated") %>% 
  select(country, country_id, region)

base_conflict_1 <- unique(base_conflict)

conflict <-  conflict %>%
  left_join(base_conflict_1, by= c("country"= "country"))
rm(base_conflict,base_conflict_1)

dbWriteTable(con, "ged_aggregated", 
             value = conflict, overwrite = TRUE, row.names = FALSE)


############################################################
#                                                          #
#                     Tidying GDELT                        #
#                                                          #
############################################################


# Gdelt1 <-  Create_Date(Gdelt1)
# Gdelt1 <- Gdelt1 %>% 
#   mutate(date = as.Date(paste0(year, '.', month, '.', 1), format = "%Y.%m.%d"))
# Gdelt1$EventCode <- as.numeric(Gdelt1$EventCode)
# Gdelt1 <- Event_Classifier(Gdelt1)
# Gdelt1 <-  Gdelt_Keeper(Gdelt1)
# Gdelt1 <- Gdelt1 %>% 
#   filter(!is.na(ActionGeo_CountryCode) | ActionGeo_CountryCode !="") %>% 
#   group_by(ActionGeo_CountryCode, year, month, EventClass)
# Gdelt1 <-  Gdelt1 %>% 
#   summarize(Num_events = n(),
#             tone = mean(AvgTone),
#             Goldstein = mean(GoldsteinScale)) %>% 
#   arrange(ActionGeo_CountryCode, year, month)
# dbWriteTable(con, "gdelt", 
#              value = Gdelt1, append = TRUE, row.names = FALSE)
# rm(Gdelt1)




############################################################
#                                                          #
#                    Tidying GDELT                         #
#                                                          #
############################################################

# #Oprettelse af year, month
# Gdelt <-  Gdelt %>% 
#   mutate(year = as.numeric(substring(MonthYear,1,4)),
#          month = as.numeric(substring(MonthYear,5,6)))
# 
# 
# #Fjernelse af artikler der ikke er tilskrevet noget land
# Gdelt  <- Gdelt[!(is.na(Gdelt$ActionGeo_CountryCode) | Gdelt$ActionGeo_CountryCode==""), ]
# 
# 
# Gdelt <- Gdelt %>% 
#   filter(!is.na(ActionGeo_CountryCode) | ActionGeo_CountryCode !="") %>% 
#   group_by(ActionGeo_CountryCode, year, month, EventClass)
# 
# Gdelt <-  Gdelt %>% 
#   summarize(Num_events = n(),
#             tone = mean(AvgTone),
#             Goldstein = mean(GoldsteinScale)) %>% 
#   arrange(ActionGeo_CountryCode, year, month)





############################################################
#                                                          #
#         Tidying World development indicators             #
#                                                          #
############################################################


# WDI gdp pr capita  
wdi_gdp_capita_2011c_country <- wdi_gdp_capita_2011c_country %>% 
  left_join(Countries, by = c("iso2c" = "countryCode"))
wdi_gdp_capita_2011c_country <-  wdi_gdp_capita_2011c_country %>% 
  filter(countryName %in% Countries$countryName)


#wdi_gov_debt
wdi_gov_debt <- wdi_gov_debt %>% 
  left_join(Countries, by = c("iso2c" = "countryCode"))
wdi_gov_debt <-  wdi_gov_debt %>% 
  filter(countryName %in% Countries$countryName)

#Govermental expenditure
wdi_gov_expenditure <- wdi_gov_expenditure %>% 
  left_join(Countries, by = c("iso2c" = "countryCode"))
wdi_gov_expenditure<-  wdi_gov_expenditure %>% 
  filter(countryName %in% Countries$countryName)


#Secondary male enrollment 
wdi_secondary_male_enrollment <- wdi_secondary_male_enrollment %>% 
  left_join(Countries, by = c("iso2c" = "countryCode"))
wdi_secondary_male_enrollment <-  wdi_secondary_male_enrollment %>% 
  filter(countryName %in% Countries$countryName)

WDI <-  wdi_gdp_capita_2011c_country %>% 
  left_join(wdi_gov_debt, by = c("countryName"="countryName", "year" = "year")) %>% 
  left_join(wdi_gov_expenditure, by = c("countryName"="countryName", "year" = "year")) %>% 
  left_join(wdi_secondary_male_enrollment, by = c("countryName"="countryName", "year" = "year")) %>% 
  transmute(countryName, isoalpha3 =isoAlpha3.x, iso2c =iso2c.x, year, gov_debt = GC.DOD.TOTL.GD.ZS, gov_expenditure =NE.DAB.TOTL.ZS, secondary_male_enrollment= SE.SEC.NENR.MA, gdp_pr_capita = NY.GDP.PCAP.PP.KD) %>% 
  filter(year >= 2000 ) %>% 
  arrange(countryName, year)

dbWriteTable(con, "wdi", 
             value = WDI, overwrite = TRUE, row.names = FALSE)



