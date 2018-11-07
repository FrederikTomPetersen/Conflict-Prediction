cat("\014")  

################################################################################################
##                                       Visualizing                                         ##
################################################################################################

# Udviklingen i antallet af døde relaterede til instrastatslige kamphandlinger

ged <-  dbGetQuery(con, "SELECT * from ged_aggregated")

plotdata <- ged %>% 
  group_by(year) %>% 
  summarise(deathyear = sum(deaths))


plotdata %>% 
  ggplot(aes(x = year, y = deathyear/1000)) +
  geom_line()+
  geom_point() + 
  labs( y = "Kamprelaterede døde - i tusinder", x="Årstal") +
  theme_classic() +
  theme(legend.position="right",panel.background = element_blank())+
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major.y = element_line(color = "grey80"))+
  scale_color_gradient()+
  scale_shape_manual(15) +
  scale_x_continuous(breaks=c(1989,1995,2000,2005,2010,2015,2017)) +
  scale_y_continuous(breaks= c(25,50,100,200,300,400,500,600))
setwd(Latexfigure)
ggsave(filename = "udviklingidde.pdf" )
  
rm(ged,plotdata)



####################################################################
# Descriptive data between conflict countries and peace countries  #
####################################################################
Country <-  codelist_panel %>% 
  select(country.name.en,iso2c) %>% 
  distinct(country.name.en,iso2c) %>% 
  filter(!is.na(iso2c))
iso2clist <- Country$iso2c

CountryCodelistPanel <-  codelist_panel %>% 
  select(p4n, fips) %>% 
  filter(!is.na(p4n), !is.na(fips)) %>% 
  distinct(p4n, fips)

GedAggregated <-  dbGetQuery(con, "SELECT * from ged_aggregated")

country_conflict <- GedAggregated %>% 
  filter(cwy==1) %>% 
  distinct(p4n) %>% 
  left_join(CountryCodelistPanel, by=c("p4n"="p4n"))

country_peace <-  GedAggregated %>% 
  distinct(p4n) %>% 
  anti_join(country_conflict, by=c("p4n"="p4n"))



WDI_growth = WDI(indicator='NY.GDP.MKTP.KD.ZG', start=1989, end=2017,  country = 'all') %>% 
  filter(iso2c %in% iso2clist)

WDI_GDP = WDI(indicator='NY.GDP.PCAP.CD', start=1989, end=2017,  country = 'all') %>% 
  filter(iso2c %in% iso2clist)


GinC <- WDI_growth %>% 
  filter(iso2c %in% country_conflict$fips & !is.na(NY.GDP.MKTP.KD.ZG)) 
GinC <- mean(GinC$NY.GDP.MKTP.KD.ZG) %>% 
  round(digits = 2)

GinP <- WDI_growth %>% 
  filter(!iso2c %in% country_conflict$fips & !is.na(NY.GDP.MKTP.KD.ZG)) 
GinP <- mean(GinP$NY.GDP.MKTP.KD.ZG) %>% 
  round(digits = 2)

GDPinC <-  WDI_GDP %>% 
  filter(iso2c %in% country_conflict$fips & !is.na(NY.GDP.PCAP.CD)) 
GDPinC <- mean(GDPinC$NY.GDP.PCAP.CD) %>% 
  round(digits = 0)

GDPinP <-  WDI_GDP %>% 
  filter(!iso2c %in% country_conflict$fips & !is.na(NY.GDP.PCAP.CD)) 
GDPinP <- mean(GDPinP$NY.GDP.PCAP.CD) %>% 
  round(digits = 0)



table <-  matrix(c(GDPinC,GinC,GDPinP,GinP), ncol=2)
colnames(table) <- c('BNP','Vækst' )
rownames(table) <- c('Lande med konflikt i perioden', 'Lande med fred i perioden')
setwd(Latextable)
print(xtable(table, type = "latex"), file = "tabel1.tex")

rm(GinP,GinC,GDPinC,GDPinP,table,country_conflict,country_peace, GedAggregated,WDI_GDP,WDI_growth,Countries,CountryCodelistPanel,Country)








#####################################################################
#                           TIMELINE
#####################################################################

GedAggregated <-  dbGetQuery(con, "SELECT * from ged_aggregated")


timeline <-GedAggregated %>%
  filter(cwy == 1) %>%
  group_by(p4n, year) %>%
  arrange(p4n, year, month) %>% 
  filter(row_number() == 1) %>%
  group_by(p4n) %>%
  mutate(is_contiguous = (year -1==lag(year))) 

timeline <- timeline %>%
  mutate(is_contiguous = case_when(is.na(is_contiguous) ~ FALSE,
                                   TRUE~is_contiguous)) %>%
  mutate(gvar = cumsum(!is_contiguous)) %>%
  group_by(p4n, gvar) %>%
  mutate(minyear = min(year),
         maxyear = max(year)) %>%
  ungroup() %>%
  filter(is_contiguous == F | is.na(is_contiguous))



timeline$country.name.en = as.factor(timeline$country.name.en)
timeline$country.name.en = fct_reorder(timeline$country.name.en, -timeline$minyear)


ggplot(timeline) +
  geom_segment(aes(x=minyear, xend=maxyear, y=country.name.en, yend=country.name.en), size=0.8) +
  geom_point(mapping=aes(x=minyear, y=country.name.en), size=2, shape=21, fill="white") +
  geom_point(mapping=aes(x=maxyear, y=country.name.en), size=2, shape=21, fill="white")+
  theme_classic() +
  theme(legend.position="right",panel.background = element_blank())+
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major.y = element_line(color = "grey80"))+
  theme(text = element_text(size=8, lineheight=10)) +
  labs(y = "", x="") +
  scale_color_gradient() +
  theme(axis.text=element_text(size=8),
          axis.title=element_text(size=10,face="bold")) +
  scale_x_continuous(breaks=c(1989,1995,2000,2005,2010,2015,2017)) 

setwd(Latexfigure)
ggsave(filename = "timeline.pdf" )
rm(GedAggregated, timeline)





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







