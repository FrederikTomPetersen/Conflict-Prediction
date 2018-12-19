
#-------------------------------------------------------------------
#       Imporatance of variables in the 4 EGBtree models

setwd(Models)
load("3_egbt_cwstart.rda")
load("3_egbt_cwm.rda")
load("3_egbt_deaths.rda")
load("3_egbt_incidents.rda")

#Sikrer, at jeg ikke får videnskabelige (ulæselige) tal retuneret
options("scipen"=100, "digits"=7)

#--------------------------------
# Variable importance for cwstart 
varimp_cwstart <- caret::varImp(model_egbt_cwstart)$importance
varimp_names_cwstart <- rownames(varimp_cwstart)
rownames(varimp_cwstart) <- NULL
varimp_cwstart <-  cbind(varimp_names_cwstart,varimp_cwstart)
names(varimp_cwstart)[names(varimp_cwstart)=="varimp_names_cwstart"] <- "var"


#--------------------------------
# Variable importance forcwm
varimp_cwm <- caret::varImp(model_egbt_cwm)$importance
varimp_names_cwm <- rownames(varimp_cwm)
rownames(varimp_cwm) <- NULL
varimp_cwm <-  cbind(varimp_names_cwm,varimp_cwm)
names(varimp_cwm)[names(varimp_cwm)=="varimp_names_cwm"] <- "var"

#--------------------------------
# Variable importance for antal doede
varimp_deaths <- caret::varImp(model_egbt_deaths)$importance
varimp_names_deaths <- rownames(varimp_deaths)
rownames(varimp_deaths) <- NULL
varimp_deaths <-  cbind(varimp_names_deaths,varimp_deaths)
names(varimp_deaths)[names(varimp_deaths)=="varimp_names_deaths"] <- "var"

#--------------------------------
# Variable importance for antal kamphandlinger
varimp_incidents <- caret::varImp(model_egbt_incidents)$importance
varimp_names_incidents <- rownames(varimp_incidents)
rownames(varimp_incidents) <- NULL
varimp_incidents <-  cbind(varimp_names_incidents,varimp_incidents)
names(varimp_incidents)[names(varimp_incidents)=="varimp_names_incidents"] <- "var"

#Samsætning af variable importoance til en tabel
importancedf <- varimp_cwstart %>% 
  left_join(varimp_cwm, by = c("var"="var")) %>% 
  left_join(varimp_deaths, by = c("var"="var")) %>% 
  left_join(varimp_incidents, by = c("var"="var"))

#Tilføjelse af meningsfulde kolonne navne
names(importancedf)[names(importancedf)=="Overall.x"] <- "cwstart"
names(importancedf)[names(importancedf)=="Overall.y"] <- "cwm"
names(importancedf)[names(importancedf)=="Overall.x.x"] <- "deaths"
names(importancedf)[names(importancedf)=="Overall.y.y"] <- "incidents"

#Fjernelse af #døde #kamphandlinger #borgerkrigslignende tilstand #udbrud af borgerkrige da disse er responsvariable
complete <-  complete.cases(importancedf)
importancedf <-  importancedf[complete,]


#Dannelse af meningsfulde navne på variablene (fra codebook --> tekst:
importancedf$var <- gsub('rm_6_q1gs', 'Verbalt samarbejde - goldstein (6) ', importancedf$var)
importancedf$var <- gsub('rm_6_q2at', 'Materielt samarbejde - gns. tone (6) ', importancedf$var)
importancedf$var <- gsub('rm_6_q1at', 'Verbalt samarbejde - gns. tone (6) ', importancedf$var)
importancedf$var <- gsub('rm_6_ethq4gs', 'Materielt modarbejde - etnisk - goldstein ', importancedf$var)
importancedf$var <- gsub('rm_6_ethq1cnt', 'Verbalt samarbejde - etnisk - antal (6) ', importancedf$var)
importancedf$var <- gsub('ethq1cnt', 'Verbalt samarbejde - etnisk - antal ', importancedf$var)
importancedf$var <- gsub('rm_6_ethq4at', 'Materielt modarbejde - etnisk - gns. tone (6) ', importancedf$var)
importancedf$var <- gsub('rm_3_deaths', 'Antal døde (3) ', importancedf$var)
importancedf$var <- gsub('rm_3_Incidents', 'Antal kamphandlinger (3) ', importancedf$var)
importancedf$var <- gsub('relq3gs', 'Verbalt modarbejde - religiøs - goldstein ', importancedf$var)
importancedf$var <- gsub('rm_12_deaths', 'Antal døde (12) ', importancedf$var) 
importancedf$var <- gsub('rm_6_deaths', 'Antal døde (6) ', importancedf$var)
importancedf$var <- gsub('rm_6_Incidents', 'Antal kamphandlinger (6) ', importancedf$var)
importancedf$var <- gsub('rm_6_q2cnt', 'Materielt samarbejde - antal (6) ', importancedf$var)
importancedf$var <- gsub('rm_6_q3at', 'Verbalt modarbejde - gns. tone (6) ', importancedf$var)
importancedf$var <- gsub('deaths_running_month', 'Antal døde (1) ', importancedf$var)
importancedf$var <- gsub('deathsuma', 'Antal døde året før - regering ', importancedf$var)
importancedf$var <- gsub('deathsumb', 'Antal døde året før - oprører ', importancedf$var)
importancedf$var <- gsub('sideA', 'Antal døde - regering (1) ', importancedf$var)
importancedf$var <- gsub('rm_12_Incidents', 'Antal kamphandlinger (12)', importancedf$var)
importancedf$var <- gsub('rm_6_ethq1cnt', 'Verbalt samarbejde - etnisk - antal (6)', importancedf$var)
importancedf$var <- gsub('sideB', 'Antal døde - oprører (1) ', importancedf$var)
importancedf$var <- gsub('ethfrac', 'Etnisk fraktionalisering', importancedf$var)
importancedf$var <- gsub('q1at', 'Verbalt samarbejde - gns. tone ', importancedf$var)
importancedf$var <- gsub('q2at', 'Materielt samarbejde - gns. tone ', importancedf$var)
importancedf$var <- gsub('q3at', 'Verbalt modarbejde - gns. tone', importancedf$var)
importancedf$var <- gsub('q4at', 'Materielt modarbejde - gns. tone', importancedf$var)
importancedf$var <- gsub('q3gs', 'Verbalt modarbejde - goldstein', importancedf$var)
importancedf$var <- gsub('q4gs', 'Materielt modarbejde - goldstein', importancedf$var)
importancedf$var <- gsub('q2gs', 'Materielt samarbejde - goldstein', importancedf$var)
importancedf$var <- gsub('gdp_log', 'Bruttonationalprodukt', importancedf$var)

importancedf$var <- gsub('pop_growth', 'Befolkningsvækst', importancedf$var)
importancedf$var <- gsub('secondary_school', 'Sekundær skole, indskrivning %', importancedf$var)
importancedf$var <- gsub('growth', 'Økonomisk vækst', importancedf$var)
importancedf$var <- gsub('powerexcludedprop', 'Andel politisk ekskluderet', importancedf$var)
importancedf$var <- gsub('refurgescnt', 'Antal flygtninge', importancedf$var)
importancedf$var <- gsub('pop', 'Befolkningsstørrelse', importancedf$var)
importancedf$var <- gsub('trade', 'Handel', importancedf$var)
importancedf$var <- gsub('powershareprop', 'Politisk inkluderet', importancedf$var)
importancedf$var <- gsub('mtnest', 'Terræn', importancedf$var)
importancedf$var <- gsub('eth', 'etnisk_', importancedf$var)


#Oprettelse af total importance og gennemsnitlige importance, arrangeret efter mean importance
importancedf <-  importancedf %>% 
  group_by(var) %>% 
  mutate(total_imp = sum(cwstart + cwm + deaths + incidents),
         mean_imp = (cwstart + cwm + deaths + incidents)/4) %>% 
         arrange(desc(mean_imp))


top_importance <-  importancedf[1:20,] # 20 gennemsnitligt mest inflydelsesrige variable

#Fjernelse af konflikt historik som variable
importancenoconflict <- importancedf[!grepl("døde", importancedf$var),]
importancenoconflict <- importancenoconflict[!grepl("kamphandlinger", importancenoconflict$var),]
importancenoconflict <- importancenoconflict[!grepl("incidents", importancenoconflict$var),]
importancenoconflict <- importancenoconflict[!grepl("side", importancenoconflict$var),]

#Tabel uden konflitrelaterede variable 
importancenoconflictnocut <-  importancenoconflict %>% 
  arrange(desc(mean_imp))
importancenoconflict <- importancenoconflictnocut[1:20,]




#-------------------------------------------------------------
#        Grafisk fremstilling af variableimportance: 

# Oprettelse af "nulpunkt" til visualisering
importancenoconflict <-  importancenoconflict %>% mutate(zero = 0)
top_importance <- top_importance %>%   mutate(zero = 0) 


#Omdannelse af variable navne til faktor for bedre oplisningsmuligheder i graf
importancenoconflict$var = as.factor(importancenoconflict$var)
importancenoconflict$var = fct_reorder(importancenoconflict$var, importancenoconflict$mean_imp)

top_importance$var = as.factor(top_importance$var)
top_importance$var = fct_reorder(top_importance$var, top_importance$mean_imp)


#--------------------------------------------------
#         Plotting af importance

setwd(Latexfigure)

#Med konflitk: 
ggplot(top_importance) +
  geom_segment(aes(x=zero, xend=mean_imp, y=var, yend=var), size=0.8) +
  theme_classic() +
  theme(legend.position="right",panel.background = element_blank())+
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major.y = element_line(color = "grey10"))+
  theme(text = element_text(size=10, lineheight=10)) +
  labs(y = "", x ="Gennemsnitlige importance")




ggsave("imp_conflict_mean.png")

#Uden konflikt
ggplot(importancenoconflict) +
  geom_segment(aes(x=zero, xend=mean_imp, y=var, yend=var), size=0.8) +
  theme_classic() +
  theme(legend.position="right",panel.background = element_blank())+
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major.y = element_line(color = "grey10"))+
  theme(text = element_text(size=10, lineheight=10)) +
  labs(y = "", x ="Gennemsnitlige importance")
ggsave("imp_noconflict_mean.png")



# ---------------------------------------------------------------------
#                Forsøg med Ranking af variable til tabel
#

df <-  importancenoconflictnocut # med konflikt

df <-  df %>% 
  ungroup() %>% 
  arrange(desc(cwstart)) %>% 
  mutate(rank_cwstart = row_number()) %>% 
  arrange(desc(cwm)) %>% 
  mutate(rank_cwm = row_number()) %>% 
  arrange(desc(deaths)) %>% 
  mutate(rank_deaths = row_number()) %>% 
  arrange(desc(incidents)) %>% 
  mutate(rank_incidents = row_number(),
         rank = (rank_cwstart+rank_cwm+rank_deaths+rank_incidents)) %>% 
  arrange(rank)
  
df <- df[1:20,] %>% 
  select(var, rank_cwstart, rank_cwm, rank_deaths, rank_incidents, rank) %>% 
  mutate(rank_mean = (rank/4)) %>% 
  select(-rank)


names(df)[names(df)=="rank_cwstart"] <- "Udbrud"
names(df)[names(df)=="rank_cwm"] <- "Borgerkrigstilstand"
names(df)[names(df)=="rank_deaths"] <- "Antal døde"
names(df)[names(df)=="rank_incidents"] <- "Antal kamphandlinger"
names(df)[names(df)=="var"] <- "Variable"
names(df)[names(df)=="rank_mean"] <- "Gennemsnitlige rang"

setwd(Latextable)
print(xtable(df, type = "latex"), file = "importancerank.tex")












