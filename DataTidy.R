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


# #Creating year, month, day columns
GED_disaggregated <- GED_disaggregated %>%
  separate(date_start, c("year", "month", "day"), "-")

# 
# #Grouping by country, year, month
GED_disaggregated <-  GED_disaggregated %>%
  group_by(country, year, month)

# 
# #summarizing data
GED_disaggregated <- GED_disaggregated %>%
  summarize(TotalDeaths = sum(deaths_a + deaths_b + deaths_civilians + deaths_unknown),
            Incidents = n(),
            A_dead = sum(deaths_a),
            B_dead = sum(deaths_b))%>%
  arrange(country, year,month)

# #formats
GED_disaggregated$year <- as.numeric(GED_disaggregated$year)
GED_disaggregated$month <- as.numeric(GED_disaggregated$month)
GED_disaggregated$country <- tolower(GED_disaggregated$country)

# 


#selecting from 2000 and forward
GED_disaggregated <- GED_disaggregated %>% 
  filter(year >= 2000) 



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

#Oprettelse af year, month
Gdelt <-  Gdelt %>% 
  mutate(year = as.numeric(substring(MonthYear,1,4)),
         month = as.numeric(substring(MonthYear,5,6)))


#Fjernelse af artikler der ikke er tilskrevet noget land
Gdelt  <- Gdelt[!(is.na(Gdelt$ActionGeo_CountryCode) | Gdelt$ActionGeo_CountryCode==""), ]


Gdelt <- Gdelt %>% 
  filter(!is.na(ActionGeo_CountryCode) | ActionGeo_CountryCode !="") %>% 
  group_by(ActionGeo_CountryCode, year, month, EventClass)

Gdelt <-  Gdelt %>% 
  summarize(Num_events = n(),
            tone = mean(AvgTone),
            Goldstein = mean(GoldsteinScale)) %>% 
  arrange(ActionGeo_CountryCode, year, month)





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

# For all WDI indicators we see a reduction of rows from 15312 --> 12470 observations. Uden tilføjelsen af countrycodes var det kun 10451 obs
# This is due to the removal of regional aggregates 

unique(wdi_gdp_capita_2011c_country$countryName)

# there are 215 unique countries in the dataset 


