
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

Eventtypes <-  readRDS("C:/Users/Frederik/Documents/GitHub/Ethnic-Conflict-Prediction/Data/EventCodes.rds")
Eventtypes <- as.numeric(c("1031","1032","1033","1034","104","1041","1042","1043","1044","105","1051","1052","1053","1054","1055","1056","106","107","108","110","111","112","1121","1122","1123","1124","1125","113","114","115","116","120","121","1211","1212","122","122","1222","1223","1224","123","1231","1232","1233","1234","124","1241","1242","1243","124","1245","1246","125","126","127","128","129","130","131","1311","1312","1313","132","1321","1322","1323","1324","133","134","135","136","137","138","1381","1382","1383","1384","1385","139","140","141","1411","1412","1413","1414","142","1421","1422","1423","1424","143","1431","1432","1433","1434","144","1441","1442","1443","1444","145","150","151","152","153","154","155","160","161","162","1621","1622","1623","163","164","165","166","1661","1662","1663","170","171","1711","1712","172","1721","1722","1723","1724","173","174","175","176","181","1821","191","192"))




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
  filter(EventCode %in% Eventtypes)
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


Output <-  Gdelt_getter(Event_1979_2005, 1)