
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

dbWriteTable(con, "ged_aggregated", 
             value = conflict, overwrite = TRUE, row.names = FALSE)








