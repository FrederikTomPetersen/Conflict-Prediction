cat("\014")  

########################################################################################
##                                                                                    ##
##                                      THE SANDBOX                                   ##
##                                                                                    ##
########################################################################################


# all good playgrounds must have a sandbox
#Here castles will be build and destroyed - enjoy

#codes for creating a subset of the dataset - primarily used for datavizulaization 
subset_codes <- c("BC", "BH", "BN", "CD", "CF", "CG", "SG","UG") 

#to be made :

#  1) indeksering af dødstal og artikler/tone for at lave ggplot hvor begge optræder på samme akse
# Jeg skal skrive et commit til postgres ind efter hvert dataload  --> posgres-comitter dør







# Jeg kunne avle noget usuperviseret learning på datasættet for at se om der er clusters i min afhængeige variable , både aggregeret i rå format. 
# jeg kan desudenn ogsåse på usuperviseret i GDELT rå dataet. er der en sammenhæng mellem hvilke typer og toner ? 

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
###      Fordeling af eventypes i positive og negative            ###
#####################################################################



Demand_help <-as.numeric(c("1031","1032","1033","1034"))
Demand_political_change <- as.numeric(c("104","1041","1042","1043","1044","105","1051","1052","1053","1054","1055","1056"))
Disaprove <- as.numeric(c("110","111","112","1121","1122","1123","1124","1125","113","114","115","116"))
Rejction_of_demand <- as.numeric(c("120","121","1211","1212","122","122","1222","1223","1224","123","1231","1232","1233","1234","124","1241","1242","1243","124","1245","1246","125","126","127","128","129"))
Threaten <-  as.numeric(c("130","131","1311","1312","1313","132","1321","1322","1323","1324","133","134","135","136","137","138","1381","1382","1383","1384","1385","139"))
Protest <- as.numeric(c("140","141","1411","1412","1413","1414","142","1421","1422","1423","1424","143","1431","1432","1433","1434","144","1441","1442","1443","1444","145"))
Exhibit_force <- as.numeric(c("150","151","152","153","154","155"))
Reduce_relations <- as.numeric(c("160","161","162","1621","1622","1623","163","164","165","166","1661","1662","1663"))
Coerce <- as.numeric(c("170","171","1711","1712","172","1721","1722","1723","1724","173","174","175","176"))
Non_lethalViolence <-  as.numeric(c("181","1821"))
Occupy_block <- as.numeric(c("191","192"))


Countries_sub <-  Countries %>% 
  filter(continent %in% c("AF","SA","AS","OC")) %>% 
  select(isoAlpha3)


Gdelt1$EventCode <-  as.numeric(Gdelt1$EventCode)



#####################################################################
###                     Make datatype correct                     ###
#####################################################################

Datayper = function(x) {
  output <- x
  x$EventCode <- as.numeric(x$EventCode)
  return(output)
}



#####################################################################
###                     Event_Classifiere                         ###
#####################################################################


Event_Classifier = function(x){
  output <- x %>% 
    mutate(EventClass = case_when(EventCode %in%  Demand_help ~ "Demand_help", 
                                  EventCode %in%  Demand_political_change ~ "Demand_political_change",
                                  EventCode %in%  Disaprove ~ "Disaprove",
                                  EventCode %in%  Rejction_of_demand ~ "Rejction_of_demand",
                                  EventCode %in%  Threaten ~ "Threaten",
                                  EventCode %in%  Protest ~ "Protest",
                                  EventCode %in%  Exhibit_force ~ "Exhibit_force",
                                  EventCode %in%  Reduce_relations ~ "Reduce_relations",
                                  EventCode %in%  Coerce ~ "Coerce",
                                  EventCode %in%  Non_lethalViolence ~ "Non_lethalViolence",
                                  EventCode %in%  Occupy_block ~ "Occupy_block",
                                  TRUE ~ "NULL"))
  return(output)
}

abc <-Event_Classifier(Gdelt1)


df %>% mutate(g = case_when(a == 2 | a == 5 | a == 7 | (a == 1 & b == 4) ~ 2,
                            a == 0 | a == 1 | a == 4 | a == 3 |  c == 4 ~ 3,
                            TRUE ~ NA_real_))





event_classifier = function(x){
  data <- x
if (data$Eventcode %in% Demand_help) {
  classifier = "Demand_help"
 # mutate ( ?????)
} else if (data$Eventcode %in% Demand_political_change) {
  classifier = "Demand_political_change"
}else if (data$Eventcode %in% Disaprove) {
  classifier = "Disaprove"
}else if (Eventcode %in% Rejction_of_demand) {
  classifier = "Rejction_of_demand"
}else if (data$Eventcode %in% Threaten) {
  classifier = "Threaten"
}else if (data$Eventcode %in% Protest) {
  classifier = "Protest"
}else if (data$Eventcode %in% Exhibit_force) {
  classifier = "Exhibit_force"
}else if (data$Eventcode %in% Reduce_relations) {
  classifier = "Reduce_relations"
}else if (data$Eventcode %in% Coerce) {
  classifier = "Coerce"
}else if (data$Eventcode %in% Non_lethalViolence) {
  classifier = "Non_lethalViolence"
}else if (data$Eventcode %in% Occupy_block) {
  classifier = "Occupy_block"
}
return(x)
}

a <- event_classifier(Gdelt1)






#####################################################################
###              Fordeling af nyhedsdataets tone                  ###
#####################################################################



Gdelt_gather <-  


library(ggplot2)

ggplot(data=Table, aes(Table$AvgTone)) +
  geom_histogram(bins = 300) 

ggplot(data=Gdelt_Data, aes(Gdelt_Data$GoldsteinScale)) +
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
###                        Expanding data                         ###
#####################################################################

#Expanding data
year = 1960:2018
month = 1:12
Country_test <-  Countries$countryName

dataframe_test <-  data.frame(Country_test, year, month)


#et forsøg
library(stringi)
res <- as.data.frame(t(stri_list2matrix(test)))
colnames(res) <- unique(unlist(sapply(test, names)))
res
library(data.table)
rbindlist(r1, fill=TRUE)
data
r <- list(structure(c(21, 1), .Names = c("AA", "AB")), structure(c(19, 4), .Names = c("AA", "AB")), structure(c(23, 1), .Names = c("AA","AB")), structure(c(15, 3, 6), .Names = c("AA", "AB", "BB")))
r1 <- lapply(r, as.data.frame.list)



#forsøg 2
test <- list(structure(c(Country_test, year, month), .Names = c("country", "year", "month")))


#Forsøg 4
df <-  tibble(
  Country = Country_test,
  year = 1960:2017,
  month = 1:12
)


#Forsøg 4
df <- tibble(
  year   = c(2010, 2010, 2010, 2010, 2012, 2012, 2012),
  qtr    = c(   1,    2,    3,    4,    1,    2,    3),
  return = rnorm(7)
)

df <- df %>% expand(year, month)
df <- df %>% expand(year = full_seq(year,1), month)


Country_list  <-  Countries$countryName
Country_2l  <-  Countries$isoAlpha3


data <- tibble(
  Country = Country_test,
  Year = year,
  Month = month
)


#oprindelig forsøg
df <-  Samlet %>% 
  group_by(country) %>% 
  expand(year,month) %>% 
  left_join(Samlet)









#####################################################################
###                        Datatidying                            ###
#####################################################################

#In this script i will tidy my different datasets in order to make
#joins possible. This requies that the key variables "country", and 
# date (year, month, day) are present and unique in each dataset.




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





Gdelt2 <- gdelt_tidier(Gdelt2) %>% 
  mutate(date = as.Date(paste0(year, '.', month, '.', 1), format = "%Y.%m.%d"))
Gdelt2 <-  Gdelt_Keeper(Gdelt2)











## KRISTIAN: GDEL
Gdelt_count %>%
  filter(ActionGeo_CountryCode %in% c("AF", "FR", "EG")) %>%
  mutate(
    date = as.Date(paste0(year, '.', month, '.', 1), format = "%Y.%m.%d")
  ) %>%
  arrange(ActionGeo_CountryCode, date) %>%
  ggplot(aes(x = date, y = tone, color = ActionGeo_CountryCode)) +
  geom_line() +
  geom_line(data=GED_disaggregated_test(aes))


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
