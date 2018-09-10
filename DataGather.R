cat("\014") 

######################################################################
###                                                                ###
###                 LoTR -  Gathering of the data                  ###
###                                                                ###
###################################################################### 

#merging gdelt Frames

# Gdelt <-  rbind(Gdelt1, Gdelt2, Gdelt3, Gdelt4,Gdelt5, Gdelt6, Gdelt7, Gdelt8)


# Expanding
GED_disaggregated <-  GED_disaggregated %>% 
  group_by(country) %>% 
  expand(year= 2000:2017, month = 1:12) %>% 
  left_join(GED_disaggregated) %>% 
  arrange(country, year, month)

# #joining on country codes'
setwd(DataCave)
Countries <-  fread("Lande.csv")
Countries$countryName <-  tolower(Countries$countryName)
GED_disaggregated$country <- tolower(GED_disaggregated$country)
GED_disaggregated <- GED_disaggregated %>%
  left_join(Countries, by = c("country" = "countryName"), copy =T)


#Joining GED & GDELT
Samlet <- GED_disaggregated %>% 
  left_join(Gdelt, by = c("year" = "year", "month" = "month", "countryCode" = "ActionGeo_CountryCode"))


# Den ekspander kun indenfor de måneder og år der er tilstede i datasættet ! 
# Går fra 37502 observation til 65027
# Det svarer vel til at jeg har et omtrent 50% tomt datasæt

#Jeg bør vel også expande mit grunddataset til at omfatte den givne periode inden jeg begynder at joine data på. 



#Samling a WDI i en dataframe
WDI_samlet <-  wdi_gdp_capita_2011c_country %>% 
  left_join(wdi_gov_debt, by = c("countryName"="countryName", "year" = "year")) %>% 
  left_join(wdi_gov_expenditure, by = c("countryName"="countryName", "year" = "year")) %>% 
  left_join(wdi_secondary_male_enrollment, by = c("countryName"="countryName", "year" = "year")) %>% 
  transmute(countryName, isoalpha3 =isoAlpha3.x, iso2c =iso2c.x, year, gov_debt = GC.DOD.TOTL.GD.ZS, gov_expenditure =NE.DAB.TOTL.ZS, secondary_male_enrollment= SE.SEC.NENR.MA, gdp_pr_capita = NY.GDP.PCAP.PP.KD) %>% 
  filter(year >= 2000 ) %>% 
  arrange(countryName, year)


lets_reg_it<-  Samlet %>% 
  full_join(WDI_samlet, by = c("isoAlpha3" ="isoalpha3", "year" = "year"))


df <- Samlet


# managing missing values 
df <- df %>% mutate(TotalDeaths = coalesce(TotalDeaths, 0))
df <- df %>% mutate(Incidents = coalesce(Incidents, 0))
df <- df %>% mutate(Num_events = coalesce(Num_events, 0))
df <- df %>% mutate(tone = coalesce(tone, 0))
df <- df %>% mutate(Goldstein = coalesce(Goldstein, 0))




