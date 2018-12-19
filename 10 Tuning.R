
#----------------------------------------------------------------------------
#                           Tuning af EGBtree model

#Load af oprindelige model
setwd(Models)
load("3_egbt_cwstart.rda")
plot(model_egbt_cwstart)


#Indhentning af data fra PostGresSQL - database. Alternativ load complete_data_deaths.rds
data <-  dbGetQuery(con, "SELECT * from complete_data_cwstart")


# postgressql kan ikke håndtere faktorvariable, så disse skal redefineres: 
data$Oil <-  as.factor(data$Oil)
data$elct_comp <-  as.factor(data$elct_comp)
data$elct_regulation <-  as.factor(data$elct_regulation)
data$elct_open <-  as.factor(data$elct_open)
data$exe_constraint <-  as.factor(data$exe_constraint)
data$colstyle <-  as.factor(data$colstyle)
data$DIAP <-  as.factor(data$DIAP)
data$PDIAP <-  as.factor(data$PDIAP)
data$SDIAP <-  as.factor(data$SDIAP)
data$cwstart <- as.factor(data$cwstart)

#Oprettelse af træningsdatasæt og testdatasæt - Bloksplit
train_data <-  data %>%
  filter(year <= 2011)
test_data <- data %>% 
  filter(year >=2012)

# Fjernelse af unødvendig data og data, der ikke skal indgå i analysen grundet dets "fremtidskarakter"
train_data <- train_data %>%
  select(-cwy,-deathyear)
test_data <- test_data %>%
  select(-cwy,-deathyear)


# Den oprindelige model: 
# Her kan man se på plottet af modellen 

plot(model_egbt_cwstart)

#som det ses har modellerne forsøgt med 3 forskellige subsample sizes ved to forskellige læringskoeficienter















