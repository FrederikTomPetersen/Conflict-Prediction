cat("\014")  

########################################################################################
##                                                                                    ##
##                                      THE SANDBOX                                   ##
##                                                                                    ##
########################################################################################

# all good playgrounds must have a sandbox
#Here castles will be build and destroyed - enjoy


#  1) indeksering af dødstal og artikler/tone for at lave ggplot hvor begge optræder på samme akse





#             Samling af WDI tabeller

wdi_arable_land <-  dbGetQuery(con, "SELECT * from wdi_arable_land")
wdi_export_FFM <-  dbGetQuery(con, "SELECT * from wdi_export_fmm")
wdi_export_GS <-  dbGetQuery(con, "SELECT * from wdi_export_gs")
wdi_export_me <-  dbGetQuery(con, "SELECT * from wdi_export_me")
wdi_gdp <-  dbGetQuery(con, "SELECT * from wdi_gdp")
wdi_gov_expenditure <-  dbGetQuery(con, "SELECT * from wdi_gov_expenditure")
wdi_population <-  dbGetQuery(con, "SELECT * from wdi_population")
wdi_sse <-  dbGetQuery(con, "SELECT * from wdi_secondary_male_enrollment")









#################       Samlet tabel Gdelt      ##########################################

vars  <-  c("country", "year", "month", "q1at","q1cnt", "q2at","q2cnt", "q3at","q3cnt", "q4at","q4cnt", "relq1at", "relq1cnt", "relq2at", "relq2cnt", "relq3at", "relq3cnt", "relq4at", "relq4cnt", "ethq1at", "ethq1cnt", "ethq2at", "ethq2cnt", "ethq3at", "ethq3cnt", "ethq4at", "ethq4cnt")
group1 <-  dbGetQuery(con, "SELECT * from gdelt_y_group") %>% 
  select(vars)
group2 <-  dbGetQuery(con, "SELECT * from gdelt_y_m_group") %>% 
  select(vars)
group3 <-  dbGetQuery(con, "SELECT * from gdelt_y_m_d_group") %>% 
  select(vars)
merged <- rbind(group1, group2, group3)

dbWriteTable(con, "gdelt_group27", 
             value = merged, append = TRUE, row.names = FALSE)
rm(group1, group2, group3, merged)

grouped <-  dbGetQuery(con, "SELECT * from gdelt_group27")
  

##############################




WDIsearch(string = "population", field = "name", short = TRUE)
WDIsearch(string = "gdp", field = "name", short = TRUE, cache = NULL)
population <- WDI(country="all", indicator = "SP.POP.TOTL", start =1979, end=2017)




# IN.EC.POP.TOTL - Population (Thousands)
# 
# SP.POP.0024.TO.ZS - Population 0-24 (% of total population)
# 
# SP.POP.GROW - Population growth (annual %)
# 
# SP.POP.TOTL.MA.ZS - Population, male (% of total)
# 
# SP.POP.TOTL - Population, total
# 
# SP.RUR.TOTL.ZS - Rural population (% of total population)
# 
# SP.URB.TOTL - Urban population

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


country_temp <- get_model_temp(Country_2l,"mavg", 1980, 2010)[1:5]
country_perception <- get_model_precip(Country_2l,"mavg", 1980, 2010)

USA_temp <- get_model_temp("USA","mavg", 1980, 2010)

temp_stats <- get_ensemble_stats(Country_2l, "mavg", ppt_days)


#####################################################################
###                        Indexing                              ###
####################################################################





#####################################################################
###               Tidying the Gdeltdataset                        ###
#####################################################################
Gdelt1 <- gdelt_tidier(Gdelt1) %>% 
  mutate(date = as.Date(paste0(year, '.', month, '.', 1), format = "%Y.%m.%d"))
Gdelt1 <-  Gdelt_Keeper(Gdelt1)



##############################################
## plotting the tone - 
Gdelt %>%
  filter(ActionGeo_CountryCode %in% c("AF", "BF", "BW")) %>%
  mutate(
    date = as.Date(paste0(year, '.', month, '.', 1), format = "%Y.%m.%d")
  ) %>%
  arrange(ActionGeo_CountryCode, date) %>%
  ggplot(aes(x = date, y = tone, color = ActionGeo_CountryCode)) +
  geom_line() 



#####Getting the countries right #############

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

####################################################  
#Running numbers
conflict <- conflict %>%  
  arrange(country,year,month)
conflict <-  data.table(conflict)
conflict <-  conflict[, deaths_running_year := cumsum(total_deaths), by=list(country, year)] 
conflict <-  conflict[, deaths_running_months := cumsum(total_deaths), by=list(country, year, month)]


#############################################3

install.packages("rJava")
library(rJava)
install.packages("psData")
library("psData")

