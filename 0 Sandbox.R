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

#col hist
setwd(DataCave)
direct_link <-  "http://www.paulhensel.org/Data/colhist.zip"
download.file(direct_link, basename(direct_link))
unzip(basename(direct_link))
colhist <-  fread("ICOW Colonial History 1.1/coldata110.csv")

colbase <-  colhist %>% 
  select(State, Name, ColRuler) %>% 
  distinct() 

col <- colbase %>% mutate(colstyle = ifelse(ColRuler ==  200, 1, ifelse(ColRuler == 220, 2, ifelse(ColRuler==-9,-9, 0)))) %>% 
  transmute(p4n = State, colstyle =colstyle)

dbWriteTable(con, "col_hist", 
             value = col, overwrite = TRUE, row.names = FALSE)

rm(colhist,colbase, col)



# landlocked
setwd(DataCave)
#run link in browser
direct_link <- "http://worldmap.harvard.edu/download/wfs/12432/csv?outputFormat=csv&service=WFS&request=GetFeature&format_options=charset%3AUTF-8&typename=geonode%3Alandlocked_countries_q8r&version=1.0.0"
landlocked <- fread("landlocked_countries_q8r.csv") %>% 
  select(ISO_A2, ISO_A3, TYPE, ADMIN) %>% 
  filter(TYPE == "Sovereign country")

dbWriteTable(con, "landlocked", 
             value = col, landlocked = TRUE, row.names = FALSE)

rm(landlocked)

# pre-colonial ethnic groups 
devtools::install_github("sboysel/murdock")
library(murdock)



#---------------------------------------------------------------------------------------------
#
#                              Construction of density histogram + ROC curve
#

# Funktion til fravelægning af FP,FN, SP, SN
plot_pred_type_distribution <- function(df, threshold) {
  v <- rep(NA, nrow(df))
  v <- ifelse(df$linear >= threshold & df$cwm == 1, "TP", v)
  v <- ifelse(df$linear >= threshold & df$cwm == 0, "FP", v)
  v <- ifelse(df$linear < threshold & df$cwm == 1, "FN", v)
  v <- ifelse(df$linear < threshold & df$cwm == 0, "TN", v)
}
# Tilførelse af FP,FN,SP,SN til datasæt 
test_data$pred_type <- plot_pred_type_distribution(test_data, 0.125)

TP = test_data %>% 
  filter(pred_type =="TP") %>% 
         mutate(TP = n())
FP = test_data %>% 
  filter(pred_type =="FP") %>% 
  mutate(FP = n())
TN = test_data %>% 
  filter(pred_type =="TN") %>% 
  mutate(TN = n())
FN = test_data %>% 
  filter(pred_type =="FN") %>% 
  mutate(FN = n())


#Plotting af densitet histogram med jitter og linje
test_data %>%
  group_by(linear) %>% 
  ggplot(aes(as.factor(cwm),linear)) +
  geom_violin(mapping = NULL, data = test_data, stat = "ydensity",
              position = "dodge", draw_quantiles = NULL, trim = TRUE,
              scale = "area", na.rm = TRUE, show.legend = TRUE,
              inherit.aes = TRUE)+
  geom_jitter(aes(color=pred_type),data= test_data, alpha=0.2, width = 0.4)+
  geom_hline(yintercept=0.125, color="red", alpha=0.6)+
  labs( y = "Prædikteret sandsynlighed for borgerkrig", x="") +
  scale_x_discrete(breaks=c("0","1"),
                     labels=c("ikke-borgerkrig \n y = 0", "Borgerkrig \n y = 1"))
  
setwd(Latexfigure)
ggsave(filename = "Density_histogram.pdf" )










calculate_roc <- function(df, cost_of_fp, cost_of_fn, n=100) {
  tpr <- function(df, threshold) {
    sum(df$linear >= threshold & df$cwm == 1) / sum(df$cwm == 1)
  }
  
  fpr <- function(df, threshold) {
    sum(df$linear >= threshold & df$cwm == 0) / sum(df$cwm == 0)
  }
  
  cost <- function(df, threshold, cost_of_fp, cost_of_fn) {
    sum(df$linear >= threshold & df$cwm == 0) * cost_of_fp + 
      sum(df$linear < threshold & df$cwm == 1) * cost_of_fn
  }
  
  roc <- data.frame(threshold = seq(0,1,length.out=n), tpr=NA, fpr=NA)
  roc$tpr <- sapply(roc$threshold, function(th) tpr(df, th))
  roc$fpr <- sapply(roc$threshold, function(th) fpr(df, th))
  roc$cost <- sapply(roc$threshold, function(th) cost(df, th, cost_of_fp, cost_of_fn))
  
  return(roc)
}

roc <- calculate_roc(test_data, 1, 2, n = 100)

plot_roc(roc, 0.7, 1, 2)







plot_roc <- function(roc, threshold, cost_of_fp, cost_of_fn) {
  library(gridExtra)
  
  norm_vec <- function(v) (v - min(v))/diff(range(v))
  
  idx_threshold = which.min(abs(roc$threshold-threshold))
  
  col_ramp <- colorRampPalette(c("green","orange","red","black"))(100)
  col_by_cost <- col_ramp[ceiling(norm_vec(roc$cost)*99)+1]
  p_roc <- ggplot(roc, aes(fpr,tpr)) + 
    geom_line(color=rgb(0,0,1,alpha=0.3)) +
    geom_point(color=col_by_cost, size=4, alpha=0.5) +
    coord_fixed() +
    geom_line(aes(threshold,threshold), color=rgb(0,0,1,alpha=0.5)) +
    labs(title = sprintf("ROC")) + xlab("FPR") + ylab("TPR") +
    geom_hline(yintercept=roc[idx_threshold,"tpr"], alpha=0.5, linetype="dashed") +
    geom_vline(xintercept=roc[idx_threshold,"fpr"], alpha=0.5, linetype="dashed")
  
  p_cost <- ggplot(roc, aes(threshold, cost)) +
    geom_line(color=rgb(0,0,1,alpha=0.3)) +
    geom_point(color=col_by_cost, size=4, alpha=0.5) +
    labs(title = sprintf("cost function")) +
    geom_vline(xintercept=threshold, alpha=0.5, linetype="dashed")
  
  sub_title <- sprintf("threshold at %.2f - cost of FP = %d, cost of FN = %d", threshold, cost_of_fp, cost_of_fn)
  
  grid.arrange(p_roc, p_cost, ncol=2, sub=textGrob(sub_title, gp=gpar(cex=1), just="bottom"))
}














########################################################################
#                         Variable search in WDI                       #
########################################################################
CountryCodelistPanel <-  codelist_panel %>% 
  select(country.name.en,iso3c, p4n, fips) %>% 
  filter(!is.na(p4n), !is.na(iso3c), !is.na(fips))

CountryCodelistPanel <-  unique(CountryCodelistPanel)
CountryCodelistPanel <- CountryCodelistPanel

StatesBase <-  CountryCodelistPanel %>% 
  group_by(country.name.en, iso3c,p4n,fips) %>% 
  expand(year= 1989:2017, month = 1:12) # giver en tabel med 36936 country-months
rm(CountryCodelistPanel)

#list <- WDIsearch('GDP')

WDI_enrollment_primary_male <- WDI(indicator ="SE.PRM.ENRR.MA", start=1989, end=2017,  country = 'all')%>% 
  filter(iso2c %in% iso2clist) #Behold + locf

WDI_enrollment_primary <-  WDI(indicator = "SE.PRM.ENRR", start =1989, end =2017, country = 'all')%>% 
  filter(iso2c %in% iso2clist) # Behold+ locf



# = Services: contribution to growth of GDP (%)
#NP.IND.TOTL.ZG = Industry: contribution to growth of GDP (%)
#NP.AGR.TOTL.ZG = Agriculture: contribution to growth of GDP (%)
#NE.TRD.GNFS.ZS = Trade (% of GDP)
#NE.IMP.GNFS.ZS = Imports of goods and services (% of GDP)

#NY.GDP.MKTP.CD  = GDP current $
#NY.GDP.MKTPKD.KD.ZG = growth annual %
#NY.GDP.PCAP.CD = GDP pr capita current dollar
#NY.GDP.PCAP.KD.ZG = annual growth pr capita. 

#problemer med download


WDI_ <- WDI(indicator ="NE.IMP.GNFS.ZS", start=1989, end=2017,  country = 'all')%>% 
  filter(iso2c %in% iso2clist) #Behold + locf


test <- StatesBase %>% 
  left_join(WDI_, by = c("iso2c"="iso2c", "year" = "year"))

mice_plot <- aggr(wdi, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(wdi), cex.axis=.5,
                  gap=1, ylab=c("Missing data","Pattern"))

tjek <- test %>% 
  group_by(fips) %>% 
  na.locf(test$NE.IMP.GNFS.ZS, fromLast) 

mice_plot <- aggr(tjek, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(tjek), cex.axis=.5,
                  gap=1, ylab=c("Missing data","Pattern"))


#imputation on wdi

#Mean imputation - kan foretages på variable, hvor jeg forventer en nogenlunde konstant værdi eksempelvis arable land
wdi$arableland[is.na(wdi$arableland)] <- ave(wdi$arableland, 
                                             wdi$iso2c, 
                                             FUN=function(x)mean(x, na.rm = T))[is.na(wdi$arableland)]


# lineær imputation kan foretages på variable, der har en lineær udvikling

wdi <- wdi %>%
  group_by(iso2c) %>%
  impute_lm(population ~ year) 



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
  filter(year >= 1989 & isocode %in% c)


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



###############################################################################################




  
  
  
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



#WDI data der er frasorteret



#####################################
####    Goverment expenditure     ###
#####################################

WDIsearch('expenditure')
WDI_govexpenditure =  WDI(indicator ='NE.DAB.TOTL.ZS', start=1989, end=2017,  country = 'all') %>%   
  filter(iso2c %in% iso2clist)

dbWriteTable(con, "wdi_gov_expenditure", 
             value = WDI_govexpenditure, overwrite = TRUE, row.names = FALSE)
rm(WDI_govexpenditure)




#####################################
####       Goverment debt         ###
#####################################
WDIsearch('debt')
WDI_govdebt <-  WDI(indicator = 'GC.DOD.TOTL.GD.ZS', start=1989, end=2017,  country = 'all') %>%   
  filter(iso2c %in% iso2clist)   # Central government debt, total (% of GDP)

dbWriteTable(con, "wdi_gov_debt", 
             value = WDI_govdebt, overwrite = TRUE, row.names = FALSE)
rm(WDI_govdebt)



#####################################
# Exports of goods and services (% of GDP)                #
#####################################
WDIsearch('export')
WDI_export_GS <-  WDI(indicator ="NE.EXP.GNFS.ZS", start = 1989, end = 2018,  country='all')%>%   
  filter(iso2c %in% iso2clist)


dbWriteTable(con, "wdi_export_gs", 
             value = WDI_export_GS, overwrite = TRUE, row.names = FALSE)

rm(WDI_export_GS)


# "NE.EXP.GNFS.ZS"= Exports of goods and services (% of GDP)


#####################################
# Fuels, minerals, and metals                #
#####################################

WDIsearch('Fuels')
WDI_export_FMM <-  WDI(indicator ="TX.VAL.FUEL.Zs.UN", start = 1989, end = 2018, country='all')%>%   
  filter(iso2c %in% iso2clist)

dbWriteTable(con, "wdi_export_fmm", 
             value = WDI_export_FMM, overwrite = TRUE, row.names = FALSE)

rm(WDI_export_FMM)
# "TX.VAL.FMTL.UN.ZS"= Fuels, minerals, and metals (% of merchandise exports)

#####################################
# Merchandise exports (BOP): percentage of GDP (%)                #
#####################################
WDIsearch('Merchandise exports')

WDI_export_ME <-  WDI(indicator ="BX.GSR.MRCH.CD", start = 1989, end = 2018, country='all')%>%   
  filter(iso2c %in% iso2clist)
dbWriteTable(con, "wdi_export_me", 
             value = WDI_export_ME, overwrite = TRUE, row.names = FALSE)

rm(WDI_export_ME)


# ""BX.GSR.MRCH.ZS""= Merchandise exports (BOP): percentage of GDP (%)



###################################
#Remittance
###################################


WDI_remittance_cd <- WDI(country="all", indicator = 'BX.TRF.MGR.CD', start =1989, end=2018)%>%   
  filter(iso2c %in% iso2clist)

dbWriteTable(con, "wdi_remitance_cd", 
             value = WDI_remittance_cd, overwrite = TRUE, row.names = FALSE)

WDI_remittance_gdp <- WDI(country="all", indicator = 'BX.TRF.MGR.DT.GD.ZS', start =1989, end=2018)%>%   
  filter(iso2c %in% iso2clist)

dbWriteTable(con, "wdi_remitance_gdp", 
             value = WDI_remittance_gdp, overwrite = TRUE, row.names = FALSE)


rm(WDI_remittance_gdp,WDI_remittance_cd)

WDIsearch('remit')













timeline <-completedata %>%
  filter(cwy == 1) %>%
  group_by(country, year) %>%
  arrange(country, year, month) %>% 
  filter(row_number() == 1) %>%
  group_by(country) %>%
  mutate(is_contiguous = (year -1==lag(year))) 

timeline <- timeline %>%
  mutate(is_contiguous = case_when(is.na(is_contiguous) ~ FALSE,
                                   TRUE~is_contiguous)) %>%
  mutate(gvar = cumsum(!is_contiguous)) %>%
  group_by(country, gvar) %>%
  mutate(minyear = min(year),
         maxyear = max(year)) %>%
  ungroup() %>%
  filter(is_contiguous == F | is.na(is_contiguous))



timeline$country = as.factor(timeline$country.name.en)
timeline$country = fct_reorder(timeline$country, -timeline$minyear)


ggplot(timeline) +
  geom_segment(aes(x=minyear, xend=maxyear, y=country, yend=country), size=0.8) +
  geom_point(mapping=aes(x=minyear, y=country), size=2, shape=21, fill="white") +
  geom_point(mapping=aes(x=maxyear, y=country), size=2, shape=21, fill="white")+
  theme_classic() +
  theme(legend.position="right",panel.background = element_blank())+
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major.y = element_line(color = "grey80"))+
  theme(text = element_text(size=8, lineheight=10)) +
  labs(y = "", x="") +
  scale_color_gradient() +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=10,face="bold")) +
  scale_x_continuous(breaks=c(1989,1995,2000,2005,2010,2015,2017)) 















