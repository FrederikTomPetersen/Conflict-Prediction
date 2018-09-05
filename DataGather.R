cat("\014") 

######################################################################
###                                                                ###
###                 LoTR -  Gathering of the data                  ###
###                                                                ###
###################################################################### 

#merging gdelt Frames

Gdelt <-  rbind(Gdelt1, Gdelt2, Gdelt3, Gdelt4,Gdelt5, Gdelt6, Gdelt7, Gdelt8)


# Expanding
GED_disaggregated <-  GED_disaggregated %>% 
  group_by(country) %>% 
  expand(year= 1979:2005, month = 1:12) %>% #Hvis man undlader at angive date range, så får jeg væsentligt mindre data
  left_join(GED_disaggregated) %>% 
  arrange(country, year, month)

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
    subset(select = c(countryName, isoAlpha3.x, iso2c.x, year, GC.DOD.TOTL.GD.ZS, NE.DAB.TOTL.ZS, SE.SEC.NENR.MA, NY.GDP.PCAP.PP.KD)) %>% 
  arrange(countryName, year)


Samlet <-  Samlet %>% 
  full_join(WDI_samlet, by = c("isoAlpha3" ="isoAlpha3.x", "year" = "year"))


df <- Samlet


# managing missing values 
df <- df %>% mutate(TotalDeaths = coalesce(TotalDeaths, 0))
df <- df %>% mutate(Incidents = coalesce(Incidents, 0))
df <- df %>% mutate(Num_events = coalesce(Num_events, 0))
df <- df %>% mutate(tone = coalesce(tone, 0))
df <- df %>% mutate(Goldstein = coalesce(Goldstein, 0))




