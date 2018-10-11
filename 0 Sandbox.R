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

?save
save(DataSet_sub,file="data.rds")






vars  <-  c("country", "year", "month", "q1at","q1cnt", "q2at","q2cnt", "q3at","q3cnt", "q4at","q4cnt", "relq1at", "relq1cnt", "relq2at", "relq2cnt", "relq3at", "relq3cnt", "relq4at", "relq4cnt", "ethq1at", "ethq1cnt", "ethq2at", "ethq2cnt", "ethq3at", "ethq3cnt", "ethq4at", "ethq4cnt")


group1 <-  dbGetQuery(con, "SELECT * from gdelt_y_group") %>% 
  select(vars)
group2 <-  dbGetQuery(con, "SELECT * from gdelt_y_m_group") %>% 
  select(vars)
group3 <-  dbGetQuery(con, "SELECT * from gdelt_y_m_d_group") %>% 
  select(vars)


merged <- rbind(group1, group2, group3)


a <- WDIsearch(string = "population", field = "name", short = TRUE)
View(a)
WDIsearch(string = "gdp", field = "name", short = TRUE, cache = NULL)

test <- WDI(country="all", indicator = "indícator.name.xx.11", start =1950, end=2017)

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












###############################################################################3
#Quading it up()




    
QuadClasser = function(x){
    x <-  x %>% 
    mutate(q1nm = ifelse(QuadClass==1, NumMentions, 0),
           q1at = ifelse(QuadClass==1, GoldsteinScale,0),
           q1gs = ifelse(QuadClass==1, AvgTone,0),
           q2nm = ifelse(QuadClass==2, NumMentions, 0),
           q2at = ifelse(QuadClass==2, GoldsteinScale,0),
           q2gs = ifelse(QuadClass==2, AvgTone,0),
           q3nm = ifelse(QuadClass==3, NumMentions, 0),
           q3at = ifelse(QuadClass==3, GoldsteinScale,0),
           q3gs = ifelse(QuadClass==3, AvgTone,0),
           q4nm = ifelse(QuadClass==4, NumMentions, 0),
           q4at = ifelse(QuadClass==4, GoldsteinScale,0),
           q4gs = ifelse(QuadClass==4, AvgTone,0)
           )
return(x)
}
b <- QuadClasser(Table)  
    


QuadClasser = function(x,y) {
x <- x %>% 
  if(y == 1) {
    mutate(q1nm = x$NumMentions,
           q1at = x$GoldsteinScale,
           q1gs = x$AvgTone)
  } else if (y == 2) {
    mutate(q2nm = x$NumMentions,
           q2at = x$GoldsteinScale,
           q2gs = x$AvgTone)
  } else if (y == 3){
    mutate(q3nm = x$NumMentions,
           q3at = x$GoldsteinScale,
           q3gs = x$AvgTone)
  } else if (y == 4){
    mutate(q4nm = x$NumMentions,
           q4at = x$GoldsteinScale,
           q4gs = x$AvgTone)
  } 
}  
  
q1 = function(x){
  x <- x %>% 
    filter(QuadClass == 1) %>% 
    mutate(q1nm = coalesce(x$NumMentions,0),
           q1at = coalesce(x$GoldsteinScale, 0),
           q1gs = coalesce(x$AvgTone, 0))
return(x)  
}
  
  
  a <-  q1(Table)
  
  
  
  





 if(QuadClass = 1) {
  mutate(q1nm = x$NumMentions,
        q1at = x$GoldsteinScale,
        q1gs = x$AvgTone)
} else if (QuadClass = 2) {
  mutate(q2nm = x$NumMentions,
         q2at = x$GoldsteinScale,
         q2gs = x$AvgTone)
} else if (QuadClass = 3){
  mutate(q3nm = x$NumMentions,
         q3at = x$GoldsteinScale,
         q3gs = x$AvgTone)
} else if (QuadClass == 4){
  mutate(q4nm = x$NumMentions,
         q4at = x$GoldsteinScale,
         q4gs = x$AvgTone)
} 
}








approximate_or_distance = function(x) {
  x <-  x %>% 
    mutate(AoD = 
             if(x$QuadClass <= 2){
               1
             } else {
               2
             }
    )
}




###################################################

QuadClasser2 = function(x){
  x <-  x %>% 
    mutate(
           q1nm = ifelse(QuadClass==1, NumMentions, 0),
           q1at = ifelse(QuadClass==1, GoldsteinScale,0),
           q1gs = ifelse(QuadClass==1, AvgTone,0)) %>%
      filter(QuadClass == 1)
  return(x)
  x <-  x %>%      
     mutate(      
           q2nm = ifelse(QuadClass==2, NumMentions, 0),
           q2at = ifelse(QuadClass==2, GoldsteinScale,0),
           q2gs = ifelse(QuadClass==2, AvgTone,0)) %>% 
       filter(QuadClass == 1)
  return(x)
  x <-  x %>%     
      mutate(     
           q3nm = ifelse(QuadClass==3, NumMentions, 0),
           q3at = ifelse(QuadClass==3, GoldsteinScale,0),
           q3gs = ifelse(QuadClass==3, AvgTone,0)) %>% 
        filter(QuadClass == 3)
  return(x)
  x <-  x %>%
      mutate(
           q4nm = ifelse(QuadClass==4, NumMentions, 0),
           q4at = ifelse(QuadClass==4, GoldsteinScale,0),
           q4gs = ifelse(QuadClass==4, AvgTone,0)) %>% 
        filter(QuadClass == 4)
  return(x)
}




QuadClasser = function(x){
  x <-  x %>% 
    mutate(q1nm = ifelse(QuadClass==1, NumMentions, 0),
           q1at = ifelse(QuadClass==1, GoldsteinScale,0),
           q1gs = ifelse(QuadClass==1, AvgTone,0),
           q2nm = ifelse(QuadClass==2, NumMentions, 0),
           q2at = ifelse(QuadClass==2, GoldsteinScale,0),
           q2gs = ifelse(QuadClass==2, AvgTone,0),
           q3nm = ifelse(QuadClass==3, NumMentions, 0),
           q3at = ifelse(QuadClass==3, GoldsteinScale,0),
           q3gs = ifelse(QuadClass==3, AvgTone,0),
           q4nm = ifelse(QuadClass==4, NumMentions, 0),
           q4at = ifelse(QuadClass==4, GoldsteinScale,0),
           q4gs = ifelse(QuadClass==4, AvgTone,0)
    )
  return(x)
}



QuadClasser = function(x){
  x <-  x %>% 
    mutate_if(QuadClass==1,  q1nm = ifelse(QuadClass==1, NumMentions, 0),
           q1at = ifelse(QuadClass==1, GoldsteinScale[],0),
           q1gs = ifelse(QuadClass==1, AvgTone,0),
           q2nm = ifelse(QuadClass==2, NumMentions, 0),
           q2at = ifelse(QuadClass==2, GoldsteinScale,0),
           q2gs = ifelse(QuadClass==2, AvgTone,0),
           q3nm = ifelse(QuadClass==3, NumMentions, 0),
           q3at = ifelse(QuadClass==3, GoldsteinScale,0),
           q3gs = ifelse(QuadClass==3, AvgTone,0),
           q4nm = ifelse(QuadClass==4, NumMentions, 0),
           q4at = ifelse(QuadClass==4, GoldsteinScale,0),
           q4gs = ifelse(QuadClass==4, AvgTone,0)
    )
  return(x)
}


a <- Table %>% 
  mutate_if(Table.QuadClass==1, 
            q1nm = ifelse(QuadClass==1, NumMentions, 0),
            q1at = ifelse(QuadClass==1, GoldsteinScale,0),
            q1gs = ifelse(QuadClass==1, AvgTone,0))




a <- Table %>% 
  mutate(   q1nm = ifelse(QuadClass==1, NumMentions[which(Table$QuadClass == 1)], 0),
            q1at = ifelse(QuadClass==1, GoldsteinScale[which(Table$QuadClass == 1)],0),
            q1gs = ifelse(QuadClass==1, AvgTone[which(Table$QuadClass == 1)],0))



install.packages("sqldf")
library("sqldf")


#just don't use "where," it is not needed. the bigger picture is that you need subset methods which r has in spades: subset,[,[[,$'
#Selecting Observations
# first 5 observations
newdata <- mydata[1:5,]

# based on variable values
newdata <- mydata[ which(mydata$gender=='F' 
                         & mydata$age > 65), ]

# or
attach(mydata)
newdata <- mydata[ which(gender=='F' & age > 65),]
detach(mydata)




















#Running numbers
conflict <- conflict %>%  
  arrange(country,year,month)
conflict <-  data.table(conflict)
conflict <-  conflict[, deaths_running_year := cumsum(total_deaths), by=list(country, year)] 
conflict <-  conflict[, deaths_running_months := cumsum(total_deaths), by=list(country, year, month)]

conflicts <- conflict
rm(conflict)


CC <-  complete.cases(DataSet_sub)
complete <-  DataSet_sub[CC,]

missing_debt <-  is.na(DataSet$gov_debt)

DataSet_test <- DataSet[missing_debt,] %>%
  mutate(gov_debt = mean(DataSet$gov_debt))
  
  mutate(gov_debt[missing_debt,] = mean(gov_debt))

  
  
  
####################################################  
  
#  Assign mean to missing values
  
  WDI_Tester <-  WDI
  x <- WDI_Tester
  y <- "gov_expenditure"
  
  
Mean_On_Missing_WDI =function(x,y){
  z <- paste0("mean_",y) 
  x[z] <-  NA
  
  notna <- x %>% 
    group_by(countryName) %>% 
    filter(!is.na(y)) %>% 
    mutate(notna[,z] = mean(notna[,y]))
  
  notna[,z] <- mean(notna[,y])
  
  %>% 
    mutate(z = mean(y))
  
  notnaselect <-  notna %>% 
    select(countryName, z) %>% 
    distinct(countryName, z)
  
  x <- x %>% 
    left_join(notnaselect)
  
  x <- x %>% 
    mutate(x$y = ifelse(!is.na(x$y), x$y, x$z)) %>% 
    select(-x$z)
return(x)  
}  

WDI_Tester <- Mean_On_Missing_WDI(WDI_Tester,"gov_expenditure")   
  
  



######################################3
#Dette skal alves om til en funktion der tager to argumenter : df og col 
WDI_Tester <-  WDI

WDI_GROUP_NOTNA <-  WDI %>% 
  group_by(countryName) %>% 
  filter(!is.na(gov_debt)) %>% 
  mutate(govdebtmean = mean(gov_debt)) 

WDI_GROUP_NOTNA_selct <- WDI_GROUP_NOTNA %>% 
  select(countryName, govdebtmean) %>% 
  distinct(countryName,govdebtmean)

WDI_Tester <-  WDI_Tester %>% 
  left_join(WDI_GROUP_NOTNA_selct)
  
WDI_Tester <-  WDI_Tester %>% 
  mutate(gov_debt = ifelse(!is.na(gov_debt),gov_debt,govdebtmean))
#############################################3

install.packages("rJava")
library(rJava)
install.packages("psData")
library("psData")











#########################################################

# religious and ethnic groups







