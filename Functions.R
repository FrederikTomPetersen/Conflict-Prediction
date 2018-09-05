cat("\014")  
################################################################################################
##                            Defining * functions for the project                            ##
################################################################################################

#in this script all functions necessary will be defined
# the script should be runned after the library scripts
# the functions will indepentdently becalled in the sequent scripts where they will come to use



###################################################################
#                          Gdelt_getter                           #
###################################################################
# This functions automates the proces for downloading and appending the GDELT dataset
# The function takes the argument x and m 
# x must be a list URL containing the direct link to 


Gdelt_getter = function(x, m) {
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
      filter(Actor1CountryCode %in% Countries_List |Actor2CountryCode %in% Countries_List) %>% 
      filter(EventBaseCode %in% Eventtypes | EventCode %in% Eventtypes)
    Gdelt_Data <- rbind(Gdelt_Data, Table)         
    rm(Table)
    unlink(Basefile)
    unlink(paste0(Basename,".csv"))
    print(paste("der er", length(x)-m, " tilbage af", length(x), "iterationer"))
    Iterations_left = Iterations_left-1
    m = m + 1
    Sys.time()
    Sys.sleep(10)
    
    
  }
  return(Gdelt_Data)
}


###################################################################
#                          Gdelt_getter2                           #
###################################################################
# This functions automates the proces for downloading and appending the GDELT dataset - aim for the year month urls
# The function takes the argument x and m 
# x must be a list URL containing the direct link to 


Gdelt_getter2 = function(x, m) {
  #x = liste af url'er
  #m = startåret 
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
      filter(Actor1CountryCode %in% Countries_List |Actor2CountryCode %in% Countries_List) %>% 
      filter(EventBaseCode %in% Eventtypes | EventCode %in% Eventtypes)
    Gdelt_Data <- rbind(Gdelt_Data, Table)
    dbWriteTable(con, "gdelt_work_dataset_nyt", 
                 value = Table, append = TRUE, row.names = FALSE)
    rm(Table)
    unlink(Basefile)
    unlink(paste0(Basename,".csv"))
    print(paste("der er", length(x)-m, " tilbage af", length(x), "iterationer"))
    Iterations_left = Iterations_left-1
    m = m + 1
    Sys.time()
    Sys.sleep(10)
    
    
  }
  return(Gdelt_Data)
}

###################################################################
#                          Gdelt_getter3                           #
###################################################################

collist <- Gdelt_header[1:58]
Countries_List <-  Countries$isoAlpha3
Iterations <- length(Event_y_m_d)
Iterations_left =Iterations
Event_y_m_d <- Event_url_list[116:2085]



Gdelt_getter3 = function(x, m) {
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
      filter(EventBaseCode %in% Eventtypes | EventCode %in% Eventtypes) %>% 
      filter(Actor1CountryCode %in% Countries_List |Actor2CountryCode %in% Countries_List)
    Table <- Table[!(is.na(Table$Actor1CountryCode) | Table$Actor1CountryCode=="" | is.na(Table$Actor2CountryCode | Table$Actor2CountryCode==""))]
    
    #Gdelt_Data <- rbind(Gdelt_Data, Table)
    dbWriteTable(con, "gdelt_y_m_d123", 
                 value = Table, append = TRUE, row.names = FALSE)
    rm(Table)
    unlink(Basefile)
    unlink(paste0(Basename,".export.csv"))
    print(paste("der er", length(x)-m, " tilbage af", length(x), "iterationer"))
    Iterations_left = Iterations_left-1
    print(paste("Række nummer", m))
    m = m + 1
    Sys.time()
    Sys.sleep(10)
    
    
  }
  return(Gdelt_Data)
}


#Gdelt_getter3(Event_y_m_d,1279)
# Række nummer 1278

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





###################################################################
#                     Tidyr Replace NA                            #
###################################################################
tidyr_replace_na   <- function(x) { replace_na(x, as.list(setNames(rep(0, 10), as.list(c(paste0("var", 1:10)))))) }









#EventClassifier 

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
###                      Gdelt_Gdelt_Keeper                       ###
#####################################################################
Gdelt_Keeper = function(x){
  output <- x
  vars_to_keep <- c("GLOBALEVENTID","year","month","Actor1Name","Actor1CountryCode","Actor2Name","Actor2CountryCode","EventCode","GoldsteinScale", "NumMentions","NumArticles","ActionGeo_CountryCode","ActionGeo_CountryCode","year","month","date","classifier")
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












