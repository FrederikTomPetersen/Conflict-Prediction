
#################################################
#                   Functions


#group and summarize functions
Ged_Group = function(x){
  x <- x %>% 
    group_by(country,year,month)
  return(x)  
}

Total_Deaths = function(x) {
  x <- x
  for(i in x) {
    x <- x %>% 
    mutate(total_deaths = sum(x$deaths_a[i] + x$deaths_b[i], x$deaths_civ[i], x$deaths_unk[i])) 
  }
  return(x)
}


# nåske noget i stil med dette i for loopet conflicts_all <-  conflicts_all[, deaths_running_year := cumsum(deaths_a +deaths_b + deaths_civ + deaths_unk), by=list(country, year)] 



Ged_Sum = function(x){
  x <- x %>% 
    summarize(total_deaths = sum(deaths_a+deaths_b+deaths_civ+deaths_unk),
              civilians_deaths = sum(deaths_civ),
              conflict_incidents = n(),
              cw=mean(civilwar),
              cw_m=mean(civilwar_month),
              cw_m_contribute=mean(cw_month_contribute)) %>% 
    arrange(country, year, month)
}

#################################################




workspace ="C:/Users/ftp/Desktop/Speciale/" 
gis = "C:/Users/ftp/Desktop/Speciale/gis/"

#crs = 4326 for world
setwd(gis)
conflicts_all <-  sf::read_sf("ged181.shp", crs = 4326) %>% 
  select(country,year,deaths_a,deaths_b, deaths_civ, deaths_unk,date_start,date_end,gwnob)



# Creating approx date
conflicts_all$date_start = as.Date(conflicts_all$date_start) 
conflicts_all$date_end = as.Date(conflicts_all$date_end) 
conflicts_all$date_mid <-  conflicts_all$date_start + floor((conflicts_all$date_end-conflicts_all$date_start)/2) 
conflicts_all <- conflicts_all %>% 
  mutate(
    year = as.numeric(format(date_mid, format = "%Y")),
    month = as.numeric(format(date_mid, format = "%m")))


#internal conflicts
conflicts_internal <-  conflicts_all %>% 
  filter(is.na(gwnob))

#external conflicts
conflicts_external <- conflicts_all %>% 
  filter(!is.na(gwnob))

conflicts_all <- Total_Deaths(conflicts_all)


####################################################################





#Running numbers, deaths on uear and month
conflicts_all <-  data.table(conflicts_all)
conflicts_all <-  conflicts_all[, deaths_running_year := cumsum(deaths_a +deaths_b + deaths_civ + deaths_unk), by=list(country, year)] 
conflicts_all <-  conflicts_all[, deaths_running_months := cumsum(deaths_a +deaths_b + deaths_civ + deaths_unk), by=list(country, year, month)]



#Udvælgelse af til videre behandling
conflicts_all <- conflicts_all %>% 
  select(country,year,month,deaths_a,deaths_b, deaths_civ, deaths_unk,deaths_running_months,deaths_running_year) %>% 
  arrange(country,year,month)

#dækkende årstotal
conflicts_all <-  conflicts_all %>% 
  group_by(country, year)
conflicts_all <- conflicts_all %>% 
  mutate(death_year = sum(deaths_a + deaths_b, deaths_civ, deaths_unk))

conflicts_all <-  conflicts_all %>% 
  group_by(country, year, month)
conflicts_all <- conflicts_all %>% 
  mutate(death_month = sum(deaths_a + deaths_b, deaths_civ, deaths_unk))



conflicts_all <-  conflicts_all %>% 
  group_by(country, year, month)

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




