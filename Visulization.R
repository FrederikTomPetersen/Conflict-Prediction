cat("\014")  

################################################################################################
##                                       Visualizing                                         ##
################################################################################################




#####################################################################
###                        Indexing                               ###
#####################################################################


df_index <- DataSet_sub

df_index$indexarticles <-  df_index$nummentions

index <-  df_index %>% 
  drop_na(nummentions) %>% 
  filter(year == 2013, month == 4) %>% 
  select(country,
         numevents_index = nummentions,
         death_month_index = total_deaths_month,
         Goldstein_index = goldstein,
         tone_index = avgtone,
         q1nm_index = q1nm,
         q2nm_index= q2nm,
         q3nm_index =q3nm,
         q4nm_index = q4nm,
         q1cnt_index = q1cnt,
         q2cnt_index= q2cnt,
         q3cnt_index =q3cnt,
         q4cnt_index = q4cnt)


#Tone
df_index %>%
  #select(country, year, month, total_deaths_month,avgtone) %>% 
  left_join(index) %>% 
  group_by(country) %>% 
  transmute(year, month, Tone = coalesce(avgtone / tone_index,0), Deaths = total_deaths_month/death_month_index) %>% 
  filter(country == "Afghanistan", year <= 2018, year >= 2013) %>%
  ungroup() %>% 
  transmute(Tone, Deaths, date = lubridate::make_date(year, month, 1)) %>% 
  gather(key, value, Tone, Deaths) %>% 
  ggplot(aes(date, value, colour = key)) +
  geom_line() + 
  scale_y_log10()


#Numevents
df_index %>% 
  select(country, year, month, nummentions,total_deaths_month) %>% 
  left_join(index) %>% 
  group_by(country) %>% 
  transmute(year,month, Gdelt_Events = coalesce(nummentions / numevents_index, 0), Deaths = total_deaths_month/death_month_index) %>% 
  filter(country == "Afghanistan", year <= 2018, year >=2013) %>%
  ungroup() %>% 
  transmute(Gdelt_Events, Deaths, date = lubridate::make_date(year, month, 1)) %>% 
  gather(key, value, Gdelt_Events, Deaths) %>% 
  ggplot(aes(date, value, colour = key)) +
  geom_line() + 
  scale_y_log10()






# Q number of mentions
df_index %>% 
  left_join(index) %>% 
  group_by(country) %>% 
  transmute(year, month, Deaths = total_deaths_month/death_month_index, Q1NM = q1nm/q1nm_index, Q2NM = q2nm/q2nm_index, Q3NM = q3nm/q3nm_index, Q4NM = q4nm/q4nm_index) %>% 
  filter(country =="Afghanistan", year <= 2018, year >=2013) %>% 
  ungroup() %>% 
  transmute(Deaths, Q1NM, Q2NM,Q3NM,Q4NM, date = lubridate::make_date(year,month,1)) %>% 
  gather(key,value, Deaths, Q3NM, Q4NM) %>% 
  ggplot(aes(date, value, colour=key)) +
  geom_line()



df_index %>% 
  left_join(index) %>% 
  group_by(country) %>% 
  transmute(year, month, Deaths = total_deaths_month/death_month_index, Q1CNT = q1cnt/q1cnt_index, Q2CNT = q2cnt/q2cnt_index, Q3CNT = q3cnt/q3cnt_index, Q4CNT = q4cnt/q4cnt_index) %>% 
  filter(country =="Afghanistan", year <= 2018, year >=2013) %>% 
  ungroup() %>% 
  transmute(Deaths, Q1CNT, Q2CNT,Q3CNT,Q4CNT, date = lubridate::make_date(year,month,1)) %>% 
  gather(key,value, Deaths, Q3CNT, Q4CNT) %>% 
  ggplot(aes(date, value, colour=key)) +
  geom_line()

             
# giver det mening her at lave noget smoothing på antallet af begivenheder, sådan at de tre månders foregående begivenheder spiller ind? 
#Jeg tænker bare at min kurve svinger sygt meget!

CNT
cnt




