

#Getting perspective of powershare between ethnic groups. 

setwd(DataCave)
direct_link <-  "https://icr.ethz.ch/data/epr/core/EPR-2018.1.csv"
download.file(direct_link, basename(direct_link))
Basefile <- basename(direct_link)
EPR <-  fread(Basefile)

EPR <- EPR %>% 
  filter(to >=1989)
EPR <- EPR %>% 
  mutate(from = as.Date(from, format="%Y"),
         to = as.Date(to,format="%Y"),
         year = mapply(seq,EPR$from,EPR$to, SIMPLIFY=FALSE)) %>% 
  unnest(year) %>% 
  select(-from,-to)



EPR_sum <-  EPR %>% 
  group_by(gwid,year) %>% 
  summarize(groupscount = n(),
            powerruleprop = sum(size[which(status %in% c("DOMINANT","MONOPOLY"))]),
            powerrulenmb = sum(ifelse(status %in% c("DOMINANT","MONOPOLY"),1,0)),
            powershareprop = sum(size[which(status %in% c("SENIOR PARTNER","JUNIOR PARTNER"))]),
            powersharennb = sum(ifelse(status %in% c("SENIOR PARTNER","JUNIOR PARTNER"),1,0)),
            powerexcludedprop = sum(size[which(status %in% c("POWERLESS","SELF-EXCLUSION","DISCRIMINATED"))]),
            powerexcludednmb = sum(ifelse(status %in% c("POWERLESS","SELF-EXCLUSION","DISCRIMINATED"),1,0)),
            powerirrelevantprop = sum(size[which(status %in% c("IRRELEVANT"))]),
            powerirrelevantnmb = sum(ifelse(status %in% c("IRRELEVANT"),1,0)))
              
EPR_countries <- EPR %>%
  select(gwid, statename) %>% 
  distinct() %>% 
  group_by(gwid) %>% 
  expand(year= 1989:2017, month = 1:12)

EPR_grouped <- EPR_countries %>% 
  left_join(EPR_sum, by= c("gwid" ="gwid", "year"="year")) 

dbWriteTable(con, "epr_grouped", 
             value = EPR_grouped, overwrite = TRUE, row.names = FALSE)

rm(EPR, EPR_countries,EPR_sum,EPR_grouped)


# Finding total number of refurgees from country

setwd(DataCave)
direct_link <-  "https://icr.ethz.ch/data/epr/er/ER-2018.1.csv"
download.file(direct_link, basename(direct_link))
Basefile <- basename(direct_link)
EPR_ER <-  fread(Basefile)

EPR_ER <- EPR_ER %>% 
  filter(year>=1989) %>% 
  group_by(coo,ccode_coo, year) %>% 
  summarize(refurgescnt = sum(totalrefugees))

dbWriteTable(con, "epr_er", 
             value = EPR_ER, overwrite = TRUE, row.names = FALSE)

rm(EPR_ER)



# Finding number of transnational ethnic groups
setwd(DataCave)
direct_link <-  "https://icr.ethz.ch/data/epr/geoepr/GeoEPR-2018.1.zip"
download.file(direct_link, basename(direct_link))
unzip(basename(direct_link))
Basefile <- basename(direct_link)
Basename <- substring(Basefile,1,6)
Tablename <- paste0(Basename,".shp")
EPR_GEO <- sf::read_sf(Tablename, crs = 4326)


setwd(DataCave)
direct_link <-  "http://thematicmapping.org/downloads/TM_WORLD_BORDERS-0.3.zip"
download.file(direct_link, basename(direct_link))
unzip(basename(direct_link))
Basefile <- basename(direct_link)
Basename <- substring(Basefile,1,20)
Tablename <- paste0(Basename,".shp")
ADM_BORDERS <- sf::read_sf(Tablename, crs = 4326)


abc <-  st_intersection(EPR_GEO, ADM_BORDERS)
sf::write_sf(abc,"intersection.shp")


#test om jeg har oplsyningerne fra AP2017
setwd(DataCave)
data <- read.dta("complete_data.dta")
a <- read.dta("conflict_ethnicity.dta")





