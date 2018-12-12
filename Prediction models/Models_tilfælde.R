
# 
# setwd(Models)
# load("0_lm_incidents.rda")
# load("2_rf_incidents.rda")
# load("3_egbt_incidents.rda")

#-----------------------------------------------------------
# Dannelse af forudsigelsesmodeller for udbrud af borgerkrig
#
#
#Indhentning af data fra PostGresSQL - database. Alternativ load complete_data_incidents.rds
data <-  dbGetQuery(con, "SELECT * from complete_data_incident")

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
data$cwstart <- as.factor(data$cwstart)
data$cwm <- as.factor(data$cwm)

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
model_baseline_incidents <- lm(formula = incidents ~.-country -year -month, data=train_data)
summary(model_baseline_incidents)
setwd(Models)
save(model_baseline_incidents, file = "0_lm_incidents.rda")
#forudsigelse
test_data$incidents_pred_baseline = predict(model_baseline_incidents, newdata = test_data)
test_data$pred_res_baseline = test_data$incidents - test_data$incidents_pred_baseline

library(Metrics)
rmse_baseline <-  rmse(test_data$incidents,test_data$incidents_pred_baseline) # 3,8
Rsquared_baseline = cor(test_data$incidents, test_data$incidents_pred_baseline)^2 #0,933


#----------Random forest model----------
library("randomForest")
model_rf_incidents <- randomForest(incidents ~ . -country - year -month, data=train_data ,ntree=250, importance=TRUE)
model_rf_incidents
setwd(Models)
save(model_rf_incidents, file = "2_rf_incidents.rda")

#Predictions
test_data$incidents_pred_rf = predict(model_rf_incidents, newdata = test_data)
test_data$pred_res_rd_rf = test_data$incidents - test_data$incidents_pred_rf

rmse_rf <-  rmse(test_data$incidents,test_data$incidents_pred_rf) # 4,785
Rsquared_rf = cor(test_data$incidents, test_data$incidents_pred_rf)^2 #0,933


#-------------extreme gradient boosted trees -------
library("caret")

fitControl <- trainControl(method = "cv", number = 5)

# tune_grid <- expand.grid(nrounds = 150,
#                          max_depth = 2,
#                          eta = 0.15,
#                          gamma = 0.1,
#                          colsample_bytree = 1,
#                          min_child_weight = 0,
#                          subsample = 0.5)

model_egbt_incidents <- train(incidents ~ . -country - year -month,
                           data = train_data,
                           method = "xgbTree",
                           trControl = fitControl,
                           search = "random"
                           )
model_egbt_incidents
setwd(Models)
save(model_egbt_incidents, file="3_egbt_incidents.rda")


#Predictions
test_data$incidents_pred_egbt = predict(model_egbt_incidents, newdata = test_data)
test_data$pred_res_egbt = test_data$incidents - test_data$incidents_pred_egbt

rmse_egbt <-  rmse(test_data$incidents,test_data$incidents_pred_egbt) #4,792
Rsquared_egbt = cor(test_data$incidents, test_data$incidents_pred_egbt)^2 #0,934


#--------------------------------------------------------------------------
#           Visualiseringer af forudsigelser

#punkt plot over prædikteret vs faktiske
point_incidents <- ggplot(data=test_data) +
  geom_point(aes(x = incidents, y = incidents_pred_baseline, color ="red"), size = 0.3, alpha = 0.2) +
  geom_point(aes(x = incidents, y = incidents_pred_rf, color ="green"), size= 0.3, alpha=0.2) +
  geom_point(aes(x = incidents, y = incidents_pred_egbt, color ="blue"), size= 0.3, alpha=0.2) +
  geom_segment(aes(x = 0, xend = 300, y = 0, yend = 300), color="black", size =0.5) + 
  scale_color_manual(name ="",
                     values=c("green"="green","blue"="blue", "red" = "red"),
                     labels = c("red" = "Baseline", "green"="Random Forest", "blue" = "EGBtree")) +
  ylab("Prædikterede antal begivenheder") +
  xlab("Antal begivenheder")+
  xlim(0,300)+
  ylim(0,300)+
  theme(legend.position = "",
        legend.title=element_blank(),
        legend.text = element_text(size = 7),
        legend.background = element_rect(fill=NA)) +
  guides(color = guide_legend(override.aes = list(size=2, alpha = 1)))
point_incidents

kerneldensity <-  ggplot(data=test_data) +
  geom_density(aes(x= pred_res_baseline), bw= 0.8, color = alpha("red", 0.2), size = 0.3, fill= "red", alpha = 0.5 )+
  geom_density(aes(x= pred_res_rd_rf), bw= 0.8, color = alpha("blue",0.2), size = 0.3, fill= "blue", alpha = 0.5)+
  geom_density(aes(x= pred_res_egbt), bw= 0.8, color = alpha("green", 0.2), size = 0.3, fill = "green", alpha = 0.5)+
  stat_density(aes(x=pred_res_baseline, colour="red"), geom="line", bw= 0.8,position="identity", size = 0, alpha=0.1) +
  stat_density(aes(x=pred_res_rd_rf, colour="blue"), geom="line", bw= 0.8,position="identity", size = 0, alpha=0.1) +
  stat_density(aes(x=pred_res_egbt, colour="green"), geom="line", bw= 0.8,position="identity", size = 0, alpha=0.1) +
  scale_color_manual(name ="",
                     values=c("green"="green","blue"="blue", "red" = "red"),
                     labels = c("red" = "Baseline", "green"="Random Forest", "blue" = "EGBtree")) +
  ylab("Densitet") +
  xlab("Fejl i prædiktion") +
  xlim(-15,15) +
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        legend.text = element_text(size = 7),
        legend.background = element_rect(fill=NA)) +
  guides(color = guide_legend(override.aes = list(size=2, alpha = 1)))
kerneldensity



library("gridExtra")
setwd(Latexfigure)
pdf("incidents_figure.pdf")
grid.arrange(point_incidents, kerneldensity)
dev.off()

