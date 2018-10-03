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
         q4cnt_index = q4cnt,
         q1at_index = q1at,
         q2at_index= q2at,
         q3at_index =q3at,
         q4at_index = q4at)


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


#lande hvor indexet er flot

# Sudan
# DR Congo (Zaire)
# Afghanistan
# Pakistan
# Mozambique
# Mali


# Q number of political events
df_index %>% 
  left_join(index) %>% 
  group_by(country) %>% 
  transmute(year, month, Deaths = total_deaths_month/death_month_index, Verbal_Cooperation_Count = q1cnt/q1cnt_index, Material_Cooperation_Count = q2cnt/q2cnt_index, Verbal_Conflict_Count = q3cnt/q3cnt_index, Material_Conflict_Count = q4cnt/q4cnt_index) %>% 
  filter(country =="Afghanistan", year <= 2018, year >=2013) %>% 
  ungroup() %>% 
  transmute(Deaths, Verbal_Cooperation_Count, Material_Cooperation_Count,Verbal_Conflict_Count,Material_Conflict_Count, date = lubridate::make_date(year,month,1)) %>% 
  gather(key,value, Deaths,Verbal_Conflict_Count) %>% 
  ggplot(aes(date, value, colour=key)) +
  geom_line() +
  scale_color_manual(labels=c("# døde", "# Verbalt modarbjde"), values= c("red", "green")) +
  labs(title = "Indeks 2013 - Afghanistan\n", y = "indeks", x="dato", color="Signaturforklaring\n") +
  theme_bw() +
  theme(legend.position="right")+
  theme(plot.title = element_text(hjust = 0.5))





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
         q4cnt_index = q4cnt,
         q1at_index = q1at,
         q2at_index= q2at,
         q3at_index =q3at,
         q4at_index = q4at)


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


#lande hvor indexet er flot

# Sudan
# DR Congo (Zaire)
# Afghanistan
# Pakistan
# Mozambique
# Mali


# Q number of political events
df_index %>% 
  left_join(index) %>% 
  group_by(country) %>% 
  transmute(year, month, Deaths = total_deaths_month/death_month_index, Verbal_Cooperation_Count = q1cnt/q1cnt_index, Material_Cooperation_Count = q2cnt/q2cnt_index, Verbal_Conflict_Count = q3cnt/q3cnt_index, Material_Conflict_Count = q4cnt/q4cnt_index) %>% 
  filter(country =="Afghanistan", year <= 2018, year >=2013) %>% 
  ungroup() %>% 
  transmute(Deaths, Verbal_Cooperation_Count, Material_Cooperation_Count,Verbal_Conflict_Count,Material_Conflict_Count, date = lubridate::make_date(year,month,1)) %>% 
  gather(key,value, Deaths,Material_Conflict_Count) %>% 
  ggplot(aes(date, value, colour=key)) +
  geom_line() +
  scale_color_manual(labels=c("# døde", "# Materielt modarbjde"), values= c("red", "green")) +
  labs(title = "Indeks 2013 - Afghanistan\n", y = "indeks", x="dato", color="Signaturforklaring\n") +
  theme_bw() +
  theme(legend.position="right")+
  theme(plot.title = element_text(hjust = 0.5))



df_index %>% 
  left_join(index) %>% 
  group_by(country) %>% 
  transmute(year, month, Deaths = total_deaths_month/death_month_index, Verbal_Cooperation_Count = q1cnt/q1cnt_index, Material_Cooperation_Count = q2cnt/q2cnt_index, Verbal_Conflict_Count = q3cnt/q3cnt_index, Material_Conflict_Count = q4cnt/q4cnt_index) %>% 
  filter(country =="Afghanistan", year <= 2018, year >=2013) %>% 
  ungroup() %>% 
  transmute(Deaths, Verbal_Cooperation_Count, Material_Cooperation_Count,Verbal_Conflict_Count,Material_Conflict_Count, date = lubridate::make_date(year,month,1)) %>% 
  gather(key,value, Deaths,Material_Conflict_Count, Verbal_Conflict_Count) %>% 
  ggplot(aes(date, value, colour=key)) +
  geom_line() +
  scale_color_manual(labels=c("# døde", "# Materielt modarbjde", "Verbalt modarbjede"), values= c("red", "green", "blue")) +
  labs(title = "Indeks 2013 - Afghanistan\n", y = "indeks", x="dato", color="Signaturforklaring\n") +
  theme_bw() +
  theme(legend.position="right")+
  theme(plot.title = element_text(hjust = 0.5))





#Q Average tone
df_index %>% 
  left_join(index) %>% 
  group_by(country) %>% 
  transmute(year, month, Deaths = total_deaths_month/death_month_index, Q1at = q1at/q1at_index, Q2at = q2at/q2at_index, Q3at = q3at/q3at_index, Q4at = q4at/q4at_index) %>% 
  filter(country =="Afghanistan", year <= 2018, year >=2013) %>% 
  ungroup() %>% 
  transmute(Deaths, Q1at, Q2at,Q3at,Q4at, date = lubridate::make_date(year,month,1)) %>% 
  gather(key,value, Deaths,Q1at, Q2at, Q3at, Q4at) %>% 
  ggplot(aes(date, value, colour=key)) +
  geom_line()



list <-  df_index %>% 
  filter(total_deaths_month[which(year==2013 & month == 4)]>0) %>% 
  distinct(country)
             
# giver det mening her at lave noget smoothing på antallet af begivenheder, sådan at de tre månders foregående begivenheder spiller ind? 
#Jeg tænker bare at min kurve svinger sygt meget!







