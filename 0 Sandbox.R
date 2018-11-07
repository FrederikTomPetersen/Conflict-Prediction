cat("\014")  



########################################################################################
##                                                                                    ##
##                                      THE SANDBOX                                   ##
##                                                                                    ##
########################################################################################

# all good playgrounds must have a sandbox
#Here castles will be build and destroyed - enjoy


#  1) indeksering af dødstal og artikler/tone for at lave ggplot hvor begge optræder på samme akse


##############################################

#   Samling af WDI tabeller
WDI_GDPcapita2011c <-  dbGetQuery(con, "SELECT * from wdi_gdp")
WDI_arable_land <-  dbGetQuery(con, "SELECT * from wdi_arable_land")
WDI_export_FMM <-  dbGetQuery(con, "SELECT * from wdi_export_fmm")
WDI_export_GS <-  dbGetQuery(con, "SELECT * from wdi_export_gs")
WDI_export_ME <-  dbGetQuery(con, "SELECT * from wdi_export_me")
WDI_gdp <-  dbGetQuery(con, "SELECT * from wdi_gdp")
WDI_gov_expenditure <-  dbGetQuery(con, "SELECT * from wdi_gov_expenditure")
WDI_population <-  dbGetQuery(con, "SELECT * from wdi_population")
WDI_enrollment <-  dbGetQuery(con, "SELECT * from wdi_secondary_male_enrollment")




vars <-  c("iso2c","year", "NY.GDP.PCAP.PP.KD","NE.DAB.TOTL.ZS","SE.SEC.NENR.MA","AG.LND.ARBL.ZS","NE.EXP.GNFS.ZS","TX.VAL.FUEL.Zs.UN","BX.GSR.MRCH.CD", "SP.POP.TOTL")
A_WDi_gather <-  WDI_GDPcapita2011c %>% 
  left_join(WDI_gov_expenditure, by = c("iso2c" = "iso2c", "year" = "year")) %>% 
  left_join(WDI_arable_land, by = c("iso2c" = "iso2c", "year" = "year")) %>% 
  left_join(WDI_population, by = c("iso2c" = "iso2c", "year" = "year")) %>% 
  left_join(WDI_export_FMM, by = c("iso2c" = "iso2c", "year" = "year")) %>% 
  left_join(WDI_export_GS, by = c("iso2c" = "iso2c", "year" = "year")) %>% 
  left_join(WDI_export_ME, by = c("iso2c" = "iso2c", "year" = "year")) %>% 
  left_join(WDI_enrollment, by = c("iso2c" = "iso2c", "year" = "year")) %>% 
  select(vars)




imputed_Data <- mice(A_WDi_gather, m=5, maxit = 5, method = 'rf', seed = 1)




#virker og giver den nærmenste værdi for gruppen
setDT(WDI_arable_land)[, ValueInterp := if(length(na.omit(AG.LND.ARBL.ZS))<2) AG.LND.ARBL.ZS else na.approx(AG.LND.ARBL.ZS, na.rm=TRUE), iso2c]

setDT(WDI_arable_land)[, ValueInterp2 := if(length(na.omit(AG.LND.ARBL.ZS))<2) AG.LND.ARBL.ZS else na.interp(AG.LND.ARBL.ZS), iso2c]

setDT(WDI_arable_land)[, ValueInterp3 := if(length(na.omit(AG.LND.ARBL.ZS))<2) AG.LND.ARBL.ZS else na.interpolation(AG.LND.ARBL.ZS), iso2c]


setDT(WDI_arable_land)[, ValueInterp4 := if(length(na.omit(AG.LND.ARBL.ZS))<2) AG.LND.ARBL.ZS else mice(AG.LND.ARBL.ZS$, m=5, maxit = 2, method = 'norm', seed = 1), iso2c]





md.pattern(WDi_gather)
mice_plot <- aggr(WDi_gather_grouped, col=c('navyblue','yellow'),
                    numbers=TRUE, sortVars=TRUE,
                    labels=names(WDi_gather_grouped), cex.axis=.6,
                    gap=1, ylab=c("Missing data","Pattern"))


imputed_Data <- mice(WDi_gather, m=5, maxit = 2, method = 'rf', seed = 1)
summary(imputed_Data)




 install.packages("imputeTS")
 library("imputeTS")
na.random(mydata)                  # Random Imputation
na.locf(mydata, option = "locf")   # Last Obs. Carried Forward
na.locf(mydata, option = "nocb")   # Next Obs. Carried Backward
na.interpolation(mydata)           # Linear Interpolation
na.interp()
na.seadec(mydata, algorithm = "interpolation") # Seasonal Adjustment then Linear Interpolation




WDi_gather_grouped <- A_WDi_gather %>% 
  select("iso2c", "year", "SP.POP.TOTL","AG.LND.ARBL.ZS")





ara_group <-  WDI_arable_land %>% 
  group_by(iso2c)
ara_impute <- mice(ara_group, m=5, maxit = 10, method = 'rf', seed = 1)
summary(ara_impute)
c <-  complete(ara_impute)

WDI_arable_land <-  dbGetQuery(con, "SELECT * from wdi_arable_land")








########################################################################
#                         Penn World Tables                            #
########################################################################

Country1 <-  codelist_panel %>% 
  select(country.name.en,iso3c) %>% 
  filter(!is.na(iso3c)) %>% 
  distinct(iso3c)

Country2 <- Country1 %>% 
  as.list()
c <-  Country2 %>% unlist

PWT <- pwt9.0 %>% 
  filter(year >= 1979 & isocode %in% c)


vars <- c("hc", "xr", "rgdpe")
PWT <- PWT %>% 
  select(vars)
PWT <-  PWT
  rename(PWT, c("hc"="humancapital", "xr"="exchangerate", "rgdpe" = "GDP_expenditure"))



mice_plot <- aggr(PWT, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(PWT), cex.axis=.4,
                  gap=1, ylab=c("Missing data","Pattern"))






















########################################################################
#                               Functions                              #
########################################################################


#Grouped mean imputation
grouped_mean_imputation <- function(df, group_var, impute_var){

  keys_q <- enquo(group_var)
  values_q <- enquo(impute_var)
  varname <- quo_name(values_q)
  dummy_name <- paste0("imputed_", varname)
  
  df %>%
    group_by(!! keys_q) %>%
    mutate(
      !! dummy_name := case_when(is.na(!! values_q) ~ 1,
                                 T ~ 0), 
      !! varname := case_when(is.na(!! values_q) ~ fixed_mean(!! values_q),
                              T ~ !! values_q)
    ) 
}

# This last bit is just to show that it works
RESULT <- grouped_mean_imputation(WDI, countryName, gov_debt)





# Grouped linear model imputation
grouped_lm_imputation = function(col, df){
  key_q <-  enquo(col)
  
df <- df
interpolationsmodel <- lm(!! col ~ country + year, 
                          data = df, 
                          na.action=na.omit)

df$col[is.na(df$col)] <-  predict(interpolationsmodel, newdata = df)
return(df)
}








###############################################################################################

Countries <-  codelist_panel %>% 
  select(iso2c,p4n) %>%
  filter(!is.na(p4n)) %>% 
  distinct(iso2c,p4n)


#Polity IV
setwd(DataCave)
DSN <- "http://www.systemicpeace.org/inscr/p4v2017.xls"
download.file(DSN, "PolityIV.xls")
PolityIV <- read_excel("p4v2017.xls") %>% 
  filter(year>=1979) %>% 
  left_join(Countries, by = c("ccode" = "p4n")) %>% 
  select("country", "year", "democ", "autoc", "polity2", "iso2c", "ccode")

dbWriteTable(con, "polity_4", 
             value = PolityIV, overwrite = TRUE, row.names = FALSE)
rm(PolityIV)



  
  
  
################### Vizualizing deaths pr year #######################
ged <-  dbGetQuery(con, "SELECT * from ged_aggregated")

plotdata <- ged %>% 
  group_by(year) %>% 
  summarise(deathyear = sum(deaths))

plotdata %>% 
ggplot(aes(x = year, y = deathyear/1000)) +
  geom_line()+
  geom_point() + 
  labs(title = "Antal døde i intrastatslige konflikter", y = "Kamprelaterede døde - i tusinder", x="Årstal") +
  theme_calc() +
  theme(legend.position="right")+
  theme(plot.title = element_text(hjust = 0.5))+
  coord_cartesian() +
  scale_color_gradient()+
  scale_shape_manual(15) +
  scale_x_continuous(breaks=c(1989,1995,2000,2005,2010,2015,2017)) +
  scale_y_continuous(breaks= c(25,50,100,200,300,400,500,600))


















###############################
  WDIsearch('NY.GDP.PCAP.CD')
WDI_GDP <- WDI(indicator ="NY.GDP.PCAP.CD", start=1989, end=2017,  country = 'all')%>%   
  filter(iso2c %in% iso2clist) # 621 af værdierne af NA


a <- is.na(WDI_GDP$NY.GDP.PCAP.CD)
b <- WDI_GDP[a,]

dbWriteTable(con, "wdi_secondary_male_enrollment", 
             value = WDI_enrollment, overwrite = TRUE, row.names = FALSE)

rm(WDI_enrollment)









#################################################3
#growth


CountryCodelistPanel <-  codelist_panel %>% 
  select(country.name.en,iso3c, p4n, fips) %>% 
  filter(!is.na(p4n), !is.na(iso3c), !is.na(fips))


WDIsearch('growth')
WDI_growth = WDI(indicator='NY.GDP.MKTP.KD.ZG', start=1989, end=2017,  country = 'all') %>% 
  filter(iso2c %in% iso2clist)

sort <-  CountryCodelistPanel %>% 
  select(p4n, fips)

gedconflict <- GedAggregated %>% 
  filter(cwy==1) %>% 
  distinct(p4n) %>% 
  left_join(sort, by=c("p4n"="p4n"))

a <-  GedAggregated %>%
  distinct(p4n) %>% 
  right_join(!gedconflict, by=c("p4n"="p4n"))

conflict <- WDI_growth %>% 
  filter(iso2c %in% gedconflict$fips & !is.na(NY.GDP.MKTP.KD.ZG))
growthinconflict <- mean(conflict$NY.GDP.MKTP.KD.ZG)

noconflilct <- WDI_growth %>% 
  filter(!iso2c %in% gedconflict$fips & !is.na(NY.GDP.MKTP.KD.ZG))
growthinpeace <-  mean(noconflilct$NY.GDP.MKTP.KD.ZG)


'%ni%' <- Negate("%in%")




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




#############################################################################
###                           GGPlot Samlet                             ####
## 
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

