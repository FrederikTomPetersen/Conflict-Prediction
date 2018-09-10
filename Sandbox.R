cat("\014")  

########################################################################################
##                                                                                    ##
##                                      THE SANDBOX                                   ##
##                                                                                    ##
########################################################################################


# all good playgrounds must have a sandbox
#Here castles will be build and destroyed - enjoy


#  1) indeksering af dødstal og artikler/tone for at lave ggplot hvor begge optræder på samme akse
# Jeg skal skrive et commit til postgres ind efter hvert dataload  --> posgres-comitter dør




# Jeg kunne lave noget usuperviseret learning på datasættet for at se om der er clusters i min afhængeige variable , både aggregeret i rå format. 
# jeg kan desuden også se på usuperviseret i GDELT rå dataet. er der en sammenhæng mellem hvilke typer og toner ? 

#Jeg er vel mere ude i et klassifikationsproblem end jeg er ude i en kontinuerligt regrsseion problem. 


#10 fold cross validation tager 9/10 vind i train til at køre sit train på. 
#Derefter sammenligner man hvilken af de ti iterationer der er bedst og den bedste er så den modellering, der erbedst ? 
#CV bruges i stor stil til at evaluerer modeller 
# vi får retunerede en de optiamle  hyperparameters 

# check list 
# 1 .Never fit our model building on validation data
# 2. Check thta the test data has never been used
# 3. Ensure the model has converged ( do not supress warnings
# 4. Am i using a static model for times series? 



# hvordan skal jeg sample trian og test når jeg arbejde med tidsserie. 
#Jeg bliver vel nødt til at se om historisk data kan forudsige fremtidig data, i stedet for at tager data fra hele tidslinjen og så prøve at gætte data i midten. 
# solution could be block sampling  or forward moving sampling 








#####################################################################
###                         simple plotting                       ###
#####################################################################

library(ggplot2)

ggplot(data=df, aes(df$tone)) +
  geom_histogram(bins = 300) 

ggplot(data=df, aes(df$Goldstein)) +
  geom_histogram(bins = 400)



scale_fill_gradient2(
  low = "red", hight = "blue",
  mid = "white", midpoint = 25)


#####################################################################
###                   Getting Climate Data                        ###
#####################################################################

?get_model_temp

country_temp <- get_model_temp(Country_2l,"mavg", 1980, 2010)[1:5]
country_perception <- get_model_precip(Country_2l,"mavg", 1980, 2010)

USA_temp <- get_model_temp("USA","mavg", 1980, 2010)

temp_stats <- get_ensemble_stats(Country_2l, "mavg", ppt_days)








#####################################################################
###                        Indexing                              ###
#####################################################################



Samlet$indexarticles <- Samlet$Num_events

index <-  Samlet %>%
  drop_na(Num_events) %>% 
  filter(year == 2000, month == 1) %>% 
  select(country, numevents_index = Num_events, death_index = TotalDeaths)

Samlet %>%
  select(country, year, month, Num_events, TotalDeaths) %>% 
  left_join(index) %>% 
  group_by(country) %>% 
  transmute(year, month, index = coalesce(Num_events / numevents_index, 0), index2 = coalesce(TotalDeaths / death_index)) %>% 
  filter(country == "afghanistan") %>%
  ungroup() %>% 
  transmute(index, index2, date = lubridate::make_date(year, month, 1)) %>% 
  gather(key, value, index, index2) %>% 
  ggplot(aes(date, value, colour = key)) +
  geom_line() + 
  scale_y_log10()





#####################################################################
###               Tidying the Gdeltdataset                        ###
#####################################################################


Gdelt_sort <-  df_postgres %>% 
  mutate(year = as.numeric(substring(MonthYear,1,4)),
         month = as.numeric(substring(MonthYear,5,6)))


Gdelt_sort  <- Gdelt_sort[!(is.na(Gdelt_sort$ActionGeo_CountryCode) | Gdelt_sort$ActionGeo_CountryCode==""), ]


Gdelt_sort <- Gdelt_sort %>% 
  filter(!is.na(ActionGeo_CountryCode) | ActionGeo_CountryCode !="") %>% 
  group_by(ActionGeo_CountryCode, year, month)

Gdelt_count <-  Gdelt_sort %>% 
  summarize(Num_events = n(),
            tone = mean(AvgTone),
            Goldstein = mean(GoldsteinScale)) %>% 
  arrange(ActionGeo_CountryCode, year, month)





Gdelt1 <- gdelt_tidier(Gdelt1) %>% 
  mutate(date = as.Date(paste0(year, '.', month, '.', 1), format = "%Y.%m.%d"))
Gdelt1 <-  Gdelt_Keeper(Gdelt1)








## plotting the tone - 
Gdelt %>%
  filter(ActionGeo_CountryCode %in% c("AF", "BF", "BW")) %>%
  mutate(
    date = as.Date(paste0(year, '.', month, '.', 1), format = "%Y.%m.%d")
  ) %>%
  arrange(ActionGeo_CountryCode, date) %>%
  ggplot(aes(x = date, y = tone, color = ActionGeo_CountryCode)) +
  geom_line() 





range(Gdelt$tone)

a <- Gdelt %>% 
  filter(year<= 2013)
range(a$tone)


#####Getting the countries right #############

#load in of countries
Countries <-  fread("Lande.csv")
Africa = Countries #%>% 
#  filter(continent == "AF")
#Africa_code <-  Africa$countryName

#Tolower
Africa$countryName <-  tolower(Africa$countryName)
GED_disaggregated$country <- tolower(GED_disaggregated$country)

#Join to harmonize
GED_disaggregated_test <- GED_disaggregated %>% 
  left_join(Africa, by = c("country" = "countryName"), copy =T)




Gdelt_sub <-  Gdelt_count %>% 
  filter(ActionGeo_CountryCode %in% c("EZ", "AG", "AC"))


Gdelt_sub <-  Gdelt_sub %>% 
  left_join(Africa, by = c("ActionGeo_CountryCode" = "countryCode"))


# Gdelt_sub <-  Gdelt_count %>% 
#   filter(ActionGeo_CountryCode %in% Africa_code)





#Jitterplot  
Article_plot <- ggplot(data = Gdelt_sub) +
  geom_point(aes(x = year,  y = Num_events), alpha = 0.1) +
  geom_point(data=GED_disaggregated, aes(x= year, y =TotalDeaths))

Article_plot + facet_grid(ActionGeo_CountryCode ~.) 











#############################################################################
###                           GGPlot Samlet                             ####





## KRISTIAN: GDEL
Samlet %>%
  filter(ActionGeo_CountryCode %in% c("AF", "FR", "EG")) %>%
  mutate(
    date = as.Date(paste0(year, '.', month, '.', 1), format = "%Y.%m.%d")
  ) %>%
  group_by(country, year, month) %>% 
  summarize(TotalDeaths = sum())
arrange(ActionGeo_CountryCode, date) %>%
  ggplot() +
  geom_line(aes(x = date, y = tone, color = ActionGeo_CountryCode)) +
  geom_line(aes(x = date, y = TotalDeaths, color = TotalDeaths))
