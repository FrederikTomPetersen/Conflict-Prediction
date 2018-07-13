

            ############################################################
            #                                                          #
            #     Load af data, der kan have relevans for specialet    #
            #                                                          #
            ############################################################
#Workspace and forudsætninger
            
setwd("C:/Users/Frederik/Documents/konflikt/")
source("scripts/Packages.r")            
set.seed(42)            
Verdenskort <- sf::read_sf("Verdenskort.shp", crs = 4326)

                       
#. 1 Georeferenced Event Data from UCDP - http://ucdp.uu.se/downloads/
DSN_GED181 <-  "http://ucdp.uu.se/downloads/ged/ged181-shp.zip"
Datastore <-"C:/Users/Frederik/Documents/konflikt/RData"
unzip(DSN_GED181,exdir=Datastore)
GED181 <- sf::read_sf("ged181-shp/ged181.shp", crs = 4326)

            
#2 Ethnic Political Relecant
DSN_EPR
Datastore
unzip
GeoEPR <- sf::read_sf("GeoEPR-2018/GeoEPR.shp", crs = 4326)

#3 

DNS_CIESIN <- "http://sedac.ciesin.columbia.edu/geoserver/ows?service=wfs&version=1.0.0&request=GetCapabilities"


#Subnationallevel administrative borders
http://sedac.ciesin.columbia.edu/geoserver/ows?service=wms&version=1.0.0&request=GetCapabilities