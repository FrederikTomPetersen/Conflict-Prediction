
############################################################
#                                                          #
#               Access gdelt eventdatabase v.1              #
#                                                          #
############################################################

#devtools::install_github("hrbrmstr/hrbrthemes")
#devtools::install_github("abresler/gdeltr2")

library("devtools")
library("gdeltr2")
library("hrbrthemes")
library("tidyverse")
library("countrycode")
library("readxl")
library("data.table")

my_workspace = "C:/Users/Frederik/Documents/konflikt/"
setwd(my_workspace)

All_eventdb_url <-  get_urls_gdelt_event_log()
Event_url_list <- All_eventdb_url$urlData 
Event_1979_2005 <- Event_url_list[2:28]


Gdelt_header <-  "https://github.com/FrederikTomPetersen/Ethnic-Conflict-Prediction/tree/master/Data/GDELT_HEADER.csv"
Gdelt_header  <-  read_excel("GDELT_HEADER.xlsx")
collist <-  colnames(Gdelt_header[1:57])


#Forbedredning til "for loop"

#Counter
Iterations <- length(Event_1979_2005)
Iterations_left =Iterations


#Oprettelse af lister til filter:
Countries <-  fread("Lande.csv")
Africa = Countries %>% 
  filter(continent == "AF")
Africa_List <-  Africa$isoAlpha3
Eventtypes <- c("025","024", "142", "141", "145", "140", "130", "123")




Event <- Event_url_list[2]
Basefile <- basename(Event)
Basename <- substring(Basefile,1,4)
download.file(Event, Basefile)
unzip(Basefile)
Tablename <- paste0(Basename,".csv")
Table <-  fread(Tablename)
colnames(Table) <-  collist
Gdelt_Data <- Table %>% 
  filter(Actor1CountryCode %in% Africa_List |Actor2CountryCode %in% Africa_List) %>% 
  filter(EventBaseCode %in% Eventtypes)
Gdelt_Data <-  Gdelt_Data[0,]
colnames(Gdelt_Data) <- collist
rm(Table, Gdelt_header, Countries)


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
      filter(Actor1CountryCode %in% Africa_List |Actor2CountryCode %in% Africa_List) %>% 
      filter(EventBaseCode %in% Eventtypes)
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


Output <-  Gdelt_getter(Event_1979_2005, 1)