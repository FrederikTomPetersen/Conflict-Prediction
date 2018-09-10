cat("\014")  

################################################################################################
##                                       Visualizing                                         ##
################################################################################################




#####################################################################
###                        Indexing                               ###
#####################################################################

Samlet <- df


Samlet$indexarticles <- Samlet$Num_events

index <-  Samlet %>%
  drop_na(Num_events) %>% 
  filter(year == 2000, month == 1) %>% 
  select(country, numevents_index = Num_events, death_index = TotalDeaths, Goldstein_index = Goldstein, tone_index = tone)

# Samlet <- Samlet %>% 
#   drop_na(Num_events, TotalDeaths)

#Tone
Samlet %>%
  select(country, year, month, TotalDeaths,tone) %>% 
  left_join(index) %>% 
  group_by(country) %>% 
  transmute(year, month, Tone = coalesce(tone / tone_index,0), Deaths = coalesce(TotalDeaths / death_index)) %>% 
  filter(country == "afghanistan", year <= 2011, year >=2005) %>%
  ungroup() %>% 
  transmute(Tone, Deaths, date = lubridate::make_date(year, month, 1)) %>% 
  gather(key, value, Tone, Deaths) %>% 
  ggplot(aes(date, value, colour = key)) +
  geom_line() + 
  scale_y_log10()


#Numevents
Samlet %>% 
  select(country, year, month, Num_events,TotalDeaths) %>% 
  left_join(index) %>% 
  group_by(country) %>% 
  transmute(year,month, Gdelt_Events = coalesce(Num_events / numevents_index, 0), Deaths = coalesce(TotalDeaths / death_index,0)) %>% 
  filter(country == "afghanistan", year <= 2011, year >=2005) %>%
  ungroup() %>% 
  transmute(Gdelt_Events, Deaths, date = lubridate::make_date(year, month, 1)) %>% 
  gather(key, value, Gdelt_Events, Deaths) %>% 
  ggplot(aes(date, value, colour = key)) +
  geom_line() + 
  scale_y_log10()


# giver det mening her at lave noget smoothing på antallet af begivenheder, sådan at de tre månders foregående begivenheder spiller ind? 
#Jeg tænker bare at min kurve svinger sygt meget!
