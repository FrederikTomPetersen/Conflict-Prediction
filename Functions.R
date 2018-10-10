cat("\014")  
################################################################################################
##                            Defining * functions for the project                            ##
################################################################################################

#in this script all functions necessary will be defined
# the script should be runned after the library scripts
# the functions will indepentdently becalled in the sequent scripts where they will come to use



#In order  to get the three gdelt_getters to work a country list must be defined 

Countries <-  codelist_panel %>% 
  select(country.name.en,iso2c) %>% 
  distinct(country.name.en,iso2c)



myvars <- c("GLOBALEVENTID", "SQLDATE", "MonthYear", "Actor1Code", "Actor2Code", "Actor1CountryCode", "Actor2CountryCode","Actor1Type1Code","Actor2Type1Code", "Actor1Geo_CountryCode","Actor2Geo_CountryCode", "IsRootEvent", "EventCode", "EventBaseCode", "EventRootCode","QuadClass", "GoldsteinScale", "NumMentions", "AvgTone", "ActionGeo_CountryCode", "Actor1Religion1Code","Actor2Religion1Code", "Actor1EthnicCode","Actor2EthnicCode")
myvars2 <- c("GLOBALEVENTID","year", "month", "Actor1Code", "Actor2Code", "Actor1CountryCode", "Actor2CountryCode","Actor1Type1Code","Actor2Type1Code", "Actor1Geo_CountryCode","Actor2Geo_CountryCode", "IsRootEvent", "EventCode", "EventBaseCode", "EventRootCode","QuadClass", "GoldsteinScale", "NumMentions", "AvgTone", "ActionGeo_CountryCode", "Actor1Religion1Code","Actor2Religion1Code", "Actor1EthnicCode","Actor2EthnicCode","rel", "eth")

###################################################################
#                      Gdelt_getter 1                            #
###################################################################
# This functions automates the proces for downloading and appending the GDELT dataset
# The function takes the argument x and m 
# x must be a list URL containing the direct link to 


Gdelt_getter_1 = function(x, m) {
  #x = liste af url'er
  #m = startåret 
  for (i in x){
    paste("Download nummer", m)
    Event <- x[m]
    Basefile <- basename(Event)
    Basename <- substring(Basefile,1,4)
    download.file(Event, Basefile)
    unzip(Basefile)
    Tablename <- paste0(Basename,".csv")
    Table <-  fread(Tablename)
    colnames(Table) <-  collist
    Table <- Table %>% 
      filter(EventCode %in% 1:2050,
             ActionGeo_CountryCode %in% Countries$iso2c)  %>% 
      select(myvars)
    Table <- Table %>%  
      mutate(year = as.numeric(substring(MonthYear,1,4)),
             month = as.numeric(substring(MonthYear,5,6)))
    Table <-  rel_eth(Table)
    Table <-  Table %>%
      select(myvars2)
    
    Table <- QuadClasser2(Table)
    
    dbWriteTable(con, "gdelt_y", 
                 value = Table, append = TRUE, row.names = FALSE)
    
    rm(Table)
    unlink(Basefile)
    unlink(paste0(Basename,".csv"))
    print(paste("der er", length(x)-m, " tilbage af", length(x), "iterationer"))
    print(paste("Dette er download række nummer", m))
    m = m + 1
    print(Sys.time())
    Sys.sleep(10)
  }
}


###################################################################
#                          Gdelt_getter 2                           #
###################################################################
# This functions automates the proces for downloading and appending the GDELT dataset - aim for the year month urls
# The function takes the argument x and m 
# x must be a list URL containing the direct link to 


Gdelt_getter_2 = function(x, m) {
  for (i in x){
    paste("Download nummer", m)
    Event <- x[m]
    Basefile <- basename(Event)
    Basename <- substring(Basefile,1,6)
    download.file(Event, Basefile)
    unzip(Basefile)
    Tablename <- paste0(Basename,".csv")
    Table <-  fread(Tablename)
    colnames(Table) <-  collist
    Table <- Table %>% 
      filter(EventCode %in% 1:2050,
             ActionGeo_CountryCode %in% Countries$iso2c)  %>% 
      select(myvars)
    Table <- Table %>%  
      mutate(year = as.numeric(substring(MonthYear,1,4)),
             month = as.numeric(substring(MonthYear,5,6)))
    Table <-  rel_eth(Table)
    Table <-  Table %>%
      select(myvars2)
    Table <- QuadClasser2(Table)
    
    dbWriteTable(con, "gdelt_y_m", 
                 value = Table, append = TRUE, row.names = FALSE)
    rm(Table)
    unlink(Basefile)
    unlink(paste0(Basename,".csv"))
    print(paste("der er", length(x)-m, " tilbage af", length(x), "iterationer"))
    print(paste("Dette er download række nummer", m))
    m = m + 1
    Sys.time()
    Sys.sleep(2)
  }
}

###################################################################
#                          Gdelt_getter3                           #
###################################################################

# collist <- Gdelt_header[1:58]
# Countries_List <-  Countries$isoAlpha3
# Iterations <- length(Event_y_m_d)
# Iterations_left =Iterations
# Event_y_m_d <- Event_url_list[116:2085]

Gdelt_getter_3 = function(x, m) {
  #x = liste af url'er
  #m = startåret 
  for (i in x){
    paste("Download nummer", m)
    Event <- x[m]
    Basefile <- basename(Event)
    Basename <- substring(Basefile,1,8)
    download.file(Event, Basefile)
    unzip(Basefile)
    Tablename <- paste0(Basename,".export.csv")
    Table <-  fread(Tablename)
    colnames(Table) <-  collist
    Table <- Table %>% 
      filter(EventCode %in% 1:2050,
             ActionGeo_CountryCode %in% Countries$iso2c)  %>% 
      select(myvars)
    Table <- Table %>%  
      mutate(year = as.numeric(substring(MonthYear,1,4)),
             month = as.numeric(substring(MonthYear,5,6)))
    Table <-  rel_eth(Table)
    Table <-  Table %>%
      select(myvars2)
    Table <- QuadClasser2(Table)
    
        dbWriteTable(con, "gdelt_y_m_d", 
                 value = Table, append = TRUE, row.names = FALSE)
        
    rm(Table)
    unlink(Basefile)
    unlink(paste0(Basename,".export.csv"))
    print(paste("der er", length(x)-m, " tilbage af", length(x), "iterationer"))
    print(paste("Dette er download række nummer", m))
    m = m + 1
    print(Sys.time())
    Sys.sleep(2)
  }
}




###################################################################
#       Goverment detection (only disaggregated GED data)         #
###################################################################

#GOVDetectFunction
GovDetect = function(df){
  df <- df %>% 
    mutate(
      side_A = tolower(side_a),
      side_b = tolower(side_b),
      lista = str_detect(side_a, "Government"), 
      listb = str_detect(side_b, "Government")) %>% 
    mutate( GOV = case_when(lista == T ~ "A",
                            listb == T ~ "B"))
  return(df)
  rm(lista, listb)
}



#################################################
#         GED tidy function Functions           #


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
      mutate(total_deaths = sum(x$deaths_a[i] + x$deaths_b[i] + x$deaths_civ[i] + x$deaths_unk[i])) 
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




###################################################################
#                     Tidyr Replace NA                            #
###################################################################
tidyr_replace_na   <- function(x) { replace_na(x, as.list(setNames(rep(0, 10), as.list(c(paste0("var", 1:10)))))) }









#EventClassifier 

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



#####################################################################
###                     Make datatype correct                     ###
#####################################################################
#####################################################################
###                      Gdelt_datatyper                          ###
#####################################################################
Datatyper = function(x) {
  output <- x
  x$EventCode <- as.numeric(x$EventCode)
  return(output)
}

#####################################################################
###                      Gdelt_Create_Date                        ###
#####################################################################
Create_Date = function(x) {
  output <- x
  output <- output %>% 
    mutate(year = as.numeric(substring(MonthYear,1,4)),
           month = as.numeric(substring(MonthYear,5,6))) 
  return(output)
}

#####################################################################
###                Appproximate or distance                       ###
#####################################################################

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




#####################################################################
###                      Gdelt_Gdelt_Keeper                       ###
#####################################################################

Gdelt_Keeper = function(x){
  output <- x
  vars_to_keep <- c("GLOBALEVENTID","year","month","Actor1Name","Actor1CountryCode","Actor2Name","Actor2CountryCode","EventCode","GoldsteinScale", "AvgTone", "NumMentions","NumArticles","ActionGeo_CountryCode","ActionGeo_CountryCode","year","month","date","EventClass")
  output <- output[,(colnames(output) %in% vars_to_keep)] 
  return(output)
}
#####################################################################
###                    Gdelt_Gdelt_Tidier                         ###
#####################################################################

Gdelt_Tidier = function(x){
  output <- x
  output$EventCode <- as.numeric(output$EventCode)
  output <-  Create_Date(output)
  output <- Event_Classifier(output)
  
  return(output)
  
}


#####################################################################
###                    Gdelt_processor                         ###
#####################################################################
Gdelt_processor = function(x) {
  x <-  Create_Date(x)
  x <- x %>% 
    mutate(date = as.Date(paste0(year, '.', month, '.', 1), format = "%Y.%m.%d"))
  x$EventCode <- as.numeric(x$EventCode)
  x <- Event_Classifier(x)
  x <-  Gdelt_Keeper(x)
  x <- x %>% 
    filter(!is.na(ActionGeo_CountryCode) | ActionGeo_CountryCode !="") %>% 
    group_by(ActionGeo_CountryCode, year, month, EventClass)
  x <-  x %>% 
    summarize(Num_events = n(),
              tone = mean(AvgTone),
              Goldstein = mean(GoldsteinScale)) %>% 
    arrange(ActionGeo_CountryCode, year, month)
  dbWriteTable(con, "gdelt_group", 
               value = x, append = TRUE, row.names = FALSE)
}


####################################################################
###             Ethnic and relgious identifer                    ###
####################################################################


rel_eth = function(df) {
  x <-  df
  x$Actor1Religion1Code[x$Actor1Religion1Code==""] <- NA
  x$Actor2Religion1Code[x$Actor2Religion1Code==""] <- NA
  x$Actor1EthnicCode[x$Actor1EthnicCode==""] <- NA
  x$Actor2EthnicCode[x$Actor2EthnicCode==""] <- NA
  
  xtate <-x %>% 
    mutate(rel = ifelse(is.na(Actor1Religion1Code) | is.na(Actor2Religion1Code), 0, 1),
           eth = ifelse(is.na(Actor1EthnicCode)| is.na(Actor2EthnicCode), 0, 1))
  return(xtate)
}

####################################################################
###                         QUADCLASSER                          ###
####################################################################

QuadClasser = function(x){
  x <-  x %>% 
    mutate(q1nm = ifelse(QuadClass==1, NumMentions[which(x$QuadClass == 1)], 0),
           q1at = ifelse(QuadClass==1, GoldsteinScale[which(x$QuadClass == 1)],0),
           q1gs = ifelse(QuadClass==1, AvgTone[which(x$QuadClass == 1)],0),
           q2nm = ifelse(QuadClass==2, NumMentions[which(x$QuadClass == 2)], 0),
           q2at = ifelse(QuadClass==2, GoldsteinScale[which(x$QuadClass == 2)],0),
           q2gs = ifelse(QuadClass==2, AvgTone[which(x$QuadClass == 2)],0),
           q3nm = ifelse(QuadClass==3, NumMentions[which(x$QuadClass == 3)], 0),
           q3at = ifelse(QuadClass==3, GoldsteinScale[which(x$QuadClass == 3)],0),
           q3gs = ifelse(QuadClass==3, AvgTone[which(x$QuadClass == 3)],0),
           q4nm = ifelse(QuadClass==4, NumMentions[which(x$QuadClass == 4)], 0),
           q4at = ifelse(QuadClass==4, GoldsteinScale[which(x$QuadClass == 4)],0),
           q4gs = ifelse(QuadClass==4, AvgTone[which(x$QuadClass == 4)],0)
    )
  return(x)
}

####################################################################
###               QUADCLASSER  Version 2                         ###
####################################################################


QuadClasser2 = function(x){
  x <-  x %>% 
    mutate(relq1at = ifelse(x$QuadClass==1 & x$rel==1, AvgTone[which(x$QuadClass == 1 & x$rel==1)], 0),
           relq1gs = ifelse(x$QuadClass==1 & x$rel==1, GoldsteinScale[which(x$QuadClass == 1 & x$rel==1)], 0),
           relq2at = ifelse(x$QuadClass==2 & x$rel==1, AvgTone[which(x$QuadClass == 2 & x$rel==1)], 0),
           relq2gs = ifelse(x$QuadClass==2 & x$rel==1, GoldsteinScale[which(x$QuadClass == 2 & x$rel==1)], 0),
           relq3at = ifelse(x$QuadClass==3 & x$rel==1, AvgTone[which(x$QuadClass == 3 & x$rel==1)], 0),
           relq3gs = ifelse(x$QuadClass==3 & x$rel==1, GoldsteinScale[which(x$QuadClass == 3 & x$rel==1)], 0),
           relq4at = ifelse(x$QuadClass==4 & x$rel==1, AvgTone[which(x$QuadClass == 4 & x$rel==1)], 0),
           relq4gs = ifelse(x$QuadClass==4 & x$rel==1, GoldsteinScale[which(x$QuadClass == 4 & x$rel==1)], 0),
           
           ethq1at = ifelse(x$QuadClass==1 & x$eth==1, AvgTone[which(x$QuadClass == 1 & x$eth==1)], 0),
           ethq1gs = ifelse(x$QuadClass==1 & x$eth==1, GoldsteinScale[which(x$QuadClass == 1 & x$eth==1)], 0),
           ethq2at = ifelse(x$QuadClass==2 & x$eth==1, AvgTone[which(x$QuadClass == 2 & x$eth==1)], 0),
           ethq2gs = ifelse(x$QuadClass==2 & x$eth==1, GoldsteinScale[which(x$QuadClass == 2 & x$eth==1)], 0),
           ethq3at = ifelse(x$QuadClass==3 & x$eth==1, AvgTone[which(x$QuadClass == 3 & x$eth==1)], 0),
           ethq3gs = ifelse(x$QuadClass==3 & x$eth==1, GoldsteinScale[which(x$QuadClass == 3 & x$eth==1)], 0),
           ethq4at = ifelse(x$QuadClass==4 & x$eth==1, AvgTone[which(x$QuadClass == 4 &eth==1)], 0),
           ethq4gs = ifelse(x$QuadClass==4 & x$eth==1, GoldsteinScale[which(x$QuadClass == 4 & x$eth==1)], 0),
      
           q1at = ifelse(x$QuadClass == 1 & x$rel == 0 & x$eth == 0, AvgTone[which(x$QuadClass == 1 & x$rel == 0 & x$eth == 0)],0),
           q1gs = ifelse(x$QuadClass == 1 & x$rel == 0 & x$eth == 0, GoldsteinScale[which(x$QuadClass == 1 & x$rel == 0 & x$eth == 0)],0),
           q2at = ifelse(x$QuadClass == 2 & x$rel == 0 & x$eth == 0, AvgTone[which(x$QuadClass == 2 & x$rel == 0 & x$eth == 0)],0),
           q2gs = ifelse(x$QuadClass == 2 & x$rel == 0 & x$eth == 0, GoldsteinScale[which(x$QuadClass == 2 & x$rel == 0 & x$eth == 0)],0),
           q3at = ifelse(x$QuadClass == 3 & x$rel == 0 & x$eth == 0, AvgTone[which(x$QuadClass == 3 & x$rel == 0 & x$eth == 0)],0),
           q3gs = ifelse(x$QuadClass == 3 & x$rel == 0 & x$eth == 0, GoldsteinScale[which(x$QuadClass == 3 & x$rel == 0 & x$eth == 0)],0),
           q4at = ifelse(x$QuadClass == 4 & x$rel == 0 & x$eth == 0, AvgTone[which(x$QuadClass == 4 & x$rel == 0 & x$eth == 0)],0),
           q4gs = ifelse(x$QuadClass == 4 & x$rel == 0 & x$eth == 0, GoldsteinScale[which(x$QuadClass == 4 & x$rel == 0 & x$eth == 0)],0)
           
    )
  return(x)
}

#Funktionen virker efter hensigten.
