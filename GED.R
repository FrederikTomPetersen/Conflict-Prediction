


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

#Creating total death pr incident
conflicts_all$total_deaths <- rowSums(conflicts_all[,c("deaths_a", "deaths_b", "deaths_unk", "deaths_civ")], na.rm=TRUE)


####################################################################





#Running numbers, deaths on uear and month
conflicts_all <- conflicts_all %>%  
  arrange(country,year,month,day)

conflicts_all <-  data.table(conflicts_all)
conflicts_all <-  conflicts_all[, deaths_running_year := cumsum(total_deaths), by=list(country, year)] 
conflicts_all <-  conflicts_all[, deaths_running_months := cumsum(total_deaths), by=list(country, year, month)]



#Udvælgelse af til videre behandling
conflicts_all <- conflicts_all %>% 
  select(country,year,month,day,total_deaths,deaths_running_months,deaths_running_year) %>% 
  arrange(country,year,month,day)

#dækkende årstotal
conflicts_all <-  conflicts_all %>% 
  group_by(country, year)
conflicts_all <- conflicts_all %>% 
  mutate(death_year = sum(total_deaths))

conflicts_all <-  conflicts_all %>% 
  group_by(country, year, month)
conflicts_all <- conflicts_all %>% 
  mutate(death_month = sum(total_deaths))



conflicts_all <- conflicts_all %>% 
  group_by(country,year,month)

conflict_grouped <- conflicts_all %>% 
  expand(year= 2000:2017, month = 1:12) %>% 
  left_join(conflicts_all) %>% 
  summarise(total_deaths = sum(total_deaths),
            conflict_incidents = n()) %>% 
  arrange(country,year,month)



  # cw=mean(civilwar),
  # cw_m=mean(civilwar_month),
  # cw_m_contribute=mean(cw_month_contribute))




# Er der Borgerkrig det givne år? 
conflicts_all <-  conflicts_all %>% 
  mutate(civilwar = if(death_year >= 1000){
    1
  } else {
    0
  }
)

#Er borgerkrigen officielt brudt ud i den givne måned?
conflicts_all <- conflicts_all %>% 
  mutate(civilwar_month = if(deaths_running_months>=1000){
    1
  }else{
    0
  }
)


#Bidrager den givne måned 

conflicts_all <- conflicts_all %>% 
  mutate(cw_month_contribute = if(death_month>=83){
    1
  }else{
    0
  }
)


## Samling af tabel
conflict <-  group_ged(conflicts_all)

conflict <- conflict %>% 
  expand(year= 2000:2017, month = 1:12) %>% 
  group_sum() %>% 
  arrange(country,year,month)



conflicts_all <- conflicts_all %>% 
group_by(country,year,month)

conflict_grouped <- conflicts_all %>% 
  expand(year= 2000:2017, month = 1:12) %>% 
  left_join(conflicts_all) %>% 
  summarise(total_deaths = sum(deaths_a + deaths_b + deaths_civ + deaths_unk),
            civilians_deaths = sum(deaths_civ),
            conflict_incidents = n(),
            cw=mean(civilwar),
            cw_m=mean(civilwar_month),
            cw_m_contribute=mean(cw_month_contribute)) %>% 
  arrange(country,year,month)







states <-  gwstates 
states <-  states %>%
  transmute(country_name, iso3c, gwcode) 

states <- states %>% 
  mutate(year = 0000,
         month = 00)

states <- states %>% 
  group_by(country_name) %>% 
  expand(year= 2000:2017, month = 1:12) %>% 
  left_join(states)




#internal conflicts
conflicts_internal <-  conflicts_all %>% 
  filter(is.na(gwnob))

#external conflicts
conflicts_external <- conflicts_all %>% 
  filter(!is.na(gwnob))
