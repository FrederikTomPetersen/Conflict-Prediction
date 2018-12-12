
#-----------------------------------------------------------
# Dannelse af forudsigelsesmodeller for udbrud af borgerkrig
#
#
setwd(Models)
load("0_lm_deaths.rda")
load("2_rf_deaths.rda")
load("3_egbt_deaths.rda")


#Indhentning af data fra PostGresSQL - database. Alternativ load complete_data_deaths.rds
data <-  dbGetQuery(con, "SELECT * from complete_data_death")

# postgressql kan ikke håndtere faktor variable, så disse skal redefineres: 
data$Oil <-  as.factor(data$Oil)
data$elct_comp <-  as.factor(data$elct_comp)
data$elct_regulation <-  as.factor(data$elct_regulation)
data$elct_open <-  as.factor(data$elct_open)
data$exe_constraint <-  as.factor(data$exe_constraint)
data$colstyle <-  as.factor( data$colstyle)
data$DIAP <-  as.factor(data$DIAP)
data$PDIAP <-  as.factor(data$PDIAP)
data$SDIAP <-  as.factor(data$SDIAP)
data$cwm <- as.factor(data$cwm)
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


#-------------------------------------------------------
# Oprettelse af forudsigelsesmodeller

#-------------Baseline model--------------
model_baseline_deaths <- lm(formula = deaths ~.-country -year -month, data=train_data)
setwd(Models)
save(model_baseline_deaths, file = "0_lm_deaths.rda")

#Forudsigelse
test_data$deaths_pred_baseline = predict(model_baseline_deaths, newdata = test_data)
test_data$pred_res_baseline = test_data$deaths - test_data$deaths_pred_baseline

library(Metrics)
rmse_baseline <-  rmse(test_data$deaths,test_data$deaths_pred_baseline) # 2084
Rsquared_baseline_death = cor(test_data$deaths, test_data$deaths_pred_baseline)^2 #0,0023


#----------Random forest model----------
library("randomForest")
model_rf_deaths <- randomForest(deaths ~ . -country - year -month, data=train_data ,ntree=250)
setwd(Models)
save(model_rf_deaths, file = "2_rf_deaths.rda")

#Predictions
test_data$deaths_pred_rf = predict(model_rf_deaths, newdata = test_data)
test_data$pred_res_rd_rf = test_data$deaths - test_data$deaths_pred_rf

rmse_rf <-  rmse(test_data$deaths,test_data$deaths_pred_rf) #529
Rsquared_rf_death = cor(test_data$deaths, test_data$deaths_pred_rf)^2 #0,14


#-------------extreme gradient boosted trees -------
library("caret")

fitControl <- trainControl(method = "cv", number = 5)

model_egbt_deaths <- train(deaths ~  . -country - year -month,
                             data = train_data,
                             method = "xgbTree",
                             trControl = fitControl,
                             search = "random")
model_egbt_deaths
setwd(Models)
save(model_egbt_deaths, file="3_egbt_deaths.rda")


#Predictions
test_data$deaths_pred_egbt = predict(model_egbt_deaths, newdata = test_data)
test_data$pred_res_egbt = test_data$deaths - test_data$deaths_pred_egbt

rmse_egbt <-  rmse(test_data$deaths,test_data$deaths_pred_egbt) #105
Rsquared_egbt_death = cor(test_data$deaths, test_data$deaths_pred_egbt)^2 #0,665


#-----------------------------------------------------------
#           Visualiseringer af forudsigelser

deathpointplot <- ggplot(data=test_data) +
  geom_point(aes(x = deaths, y = deaths_pred_baseline, color ="green"), size = 0.25, alpha = 0.2) +
  geom_point(aes(x = deaths, y = deaths_pred_rf, color ="blue"), size= 0.25, alpha=0.2) +
  geom_point(aes(x = deaths, y = deaths_pred_egbt, color ="red"), size= 0.25, alpha=0.2) +
  geom_abline(slope = 1, intercept = 0) +
  scale_color_manual(name ="",
                     values=c("green"="green","blue"="blue", "red" = "red"),
                     labels = c("green" = "Baseline", "blue"="Random Forest", "red" = "EGBtree")) +
  ylab("Forudsagte antal døde") +
  xlab("Antal døde")+
  ylim(-100,1500) +
  xlim(0,1500) +
  theme(legend.position = "",
        legend.title=element_blank(),
        legend.text = element_text(size = 7),
        legend.background = element_rect(fill=NA)) +
  guides(color = guide_legend(override.aes = list(size=2, alpha = 1)))
deathpointplot


kerneldensity <-  ggplot(data=test_data) +
  geom_density(aes(x= pred_res_baseline), bw= 12, color = alpha("red", 0.2), size = 0.3, fill= "red", alpha = 0.5 )+
  geom_density(aes(x= pred_res_rd_rf), bw= 12, color = alpha("blue",0.2), size = 0.3, fill= "blue", alpha = 0.5)+
  geom_density(aes(x= pred_res_egbt), bw= 12, color = alpha("green", 0.2), size = 0.3, fill = "green", alpha = 0.5)+
  stat_density(aes(x=pred_res_baseline, colour="red"), geom="line", bw= 12,position="identity", size = 0, alpha=0.1) +
  stat_density(aes(x=pred_res_rd_rf, colour="blue"), geom="line", bw= 12,position="identity", size = 0, alpha=0.1) +
  stat_density(aes(x=pred_res_egbt, colour="green"), geom="line", bw= 12,position="identity", size = 0, alpha=0.1) +
  scale_color_manual(name = "",
                     values=c("red"="red","blue"="blue", "green" = "green"),
                     labels = c("red" = "Baseline", "green"="Random Forest", "blue" = "EGBtree")) +
  ylab("Densitet") +
  xlab("Fejl i prædiktion") +
  xlim(-250,250) +
  theme(legend.position = "bottom",
      legend.title=element_blank(),
      legend.text = element_text(size = 7),
      legend.background = element_rect(fill=NA)) +
  guides(color = guide_legend(override.aes = list(size=2, alpha = 1)))

setwd(Latexfigure)
pdf("deathsplot.pdf")
grid.arrange(deathpointplot,kerneldensity)
dev.off()


