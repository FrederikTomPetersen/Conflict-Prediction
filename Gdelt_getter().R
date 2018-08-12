

############################################################
#                                                          #
#               Access gdelt eventdatabase v.1             #
#                                                          #
############################################################

#devtools::install_github("hrbrmstr/hrbrthemes")
#devtools::install_github("abresler/gdeltr2")


library("devtools")
library("gdeltr2")
library("hrbrthemes")
library("tidyverse")
library("countrycode")
library("xlsx")

my_workspace = "C:/Users/Frederik/Documents/konflikt/"
setwd(my_workspace)

All_eventdb_url <-  get_urls_gdelt_event_log()
Event_url_list <- All_eventdb_url$urlData 
Event_udvalg_1 <- Event_url_list[1]# er en komprimeret udgave af årene 1970-2014
Event_udvalg_5 <- Event_url_list[5]
Event_udvalg_1_5 <- Event_url_list[2:6]

Gdelt_header <-  "https://www.gdeltproject.org/data/lookups/CSV.header.fieldids.xlsx"
Gdelt_header  <-  read.xlsx(Gdelt_header)
collist <-  colnames(Gdelt_header[1:57])
colnames(table) <- collist


#Forbedredning til "for loop"

#Counter
Iterations <- length(Event_url_list)
Iterations_left =Iterations

#oprettelse af Dataframe
Gdelt_Data <- data.frame(matrix(ncol =57, nrow =0))

#Oprettelse af lister til filter:
Countries <-  read.xlsx("https://github.com/FrederikTomPetersen/Ethnic-Conflict-Prediction/blob/master/Data/Lande.xlsx")
Africa = Countries %>% 
  filter(continent == "AF")
Africa_List <-  Africa$isoAlpha3

Eventtypes <- c("025","024", "142", "141", "145", "140", "130", "123")


Gdelt_getter = function(x, m) {
  #x = liste af url'er
  #m = startåret 
  for (i in Event_udvalg_1_5){
    paste("Download nummer", m)
    Event <- Event_udvalg_1_5[m]
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
    rbind(Gdelt_Data, Table)         
    rm(Table)
    unlink(Basefile)
    unlink(Basename)
    print(paste("der er", Iterations_left, " tilbage af", Iterations, "iterationer"))
    Iterations_left = Iterations_left-1
    m = m + 1
    Sys.time()
    Sys.sleep(60)
    
  }
}

#eksempelvis
Gdelt_getter(Event_udvalg_1_5,1)