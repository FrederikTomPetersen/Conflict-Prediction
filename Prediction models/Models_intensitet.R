

load("my_model1.rda")
load("my_model1.rda")
load("my_model1.rda")


#-----------------------------------------------------------
# Dannelse af forudsigelsesmodeller for udbrud af borgerkrig
#
#
#Indhentning af data fra PostGresSQL - database. Alternativ load complete_data_deaths.rds
data <-  dbGetQuery(con, "SELECT * from complete_data_intensitet")

# postgressql kan ikke håndtere faktor variable, så disse skal redefineres: 
data$Oil <-  as.factor(data$Oil)
data$elct_comp <-  as.factor(data$elct_comp)
data$elct_regulation <-  as.factor(data$elct_regulation)
data$elct_open <-  as.factor(data$elct_open)
data$exe_constraint <-  as.factor(data$exe_constraint)
data$colstyle <-  as.factor( data$colstyle)

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
model_baseline_cwintens <- lm(formula = intensitet ~.-country -year -month, data=train_data)
summary(model_baseline_cwintens)
setwd(Models)
save(model_baseline_cwintens, file = "0_lm_cwintens.rda")
#forudsigelse
test_data$cwintens_pred_baseline = predict(model_baseline_cwintens, newdata = test_data)
test_data$pred_res_baseline = test_data$intensitet - test_data$cwintens_pred_baseline

library(Metrics)
rmse_baseline <-  rmse(test_data$intensitet,test_data$cwintens_pred_baseline)


#----------Random forest model----------
library("randomForest")
model_rf_cwintens <- randomForest(intensitet ~ . -country - year -month, data=train_data ,ntree=250, importance=TRUE)
model_rf_cwintens
setwd(Models)
save(model_rf_cwintens, file = "2_rf_cwintens.rda")

#Predictions
test_data$cwintens_pred_rf = predict(model_rf_cwintens, newdata = test_data)
test_data$pred_res_rd_rf = test_data$intensitet - test_data$cwintens_pred_rf

rmse_rf <-  rmse(test_data$intensitet,test_data$cwintens_pred_rf)


#-------------extreme gradient boosted trees -------
library("caret")

fitControl <- trainControl(method = "cv", number = 5)

model_egbt_cwintens <- train(intensitet ~  . -country - year -month,
                            data = train_data,
                            method = "xgbTree",
                            trControl = fitControl,
                            search = "random")
model_egbt_cwintens
setwd(Models)
save(model_egbt_cwintens, file="3_egbt_cwintens.rda")


#Predictions
test_data$cwintens_pred_egbt = predict(model_egbt_cwintens, newdata = test_data)
test_data$pred_res_egbt = test_data$intensitet - test_data$cwintens_pred_egbt

rmse_egbt <-  rmse(test_data$intensitet,test_data$cwintens_pred_egbt)


#-----------------------------------------------------------
#           Visualiseringer af forudsigelser


ggplot(data=test_data) +
  geom_point(aes(x = intensitet, y = cwintens_pred_baseline, color ="green"), size = 0.2, alpha = 0.2) +
  geom_point(aes(x = intensitet, y = cwintens_pred_rf, color ="blue"), size= 0.2, alpha=0.2) +
  geom_point(aes(x = intensitet, y = cwintens_pred_egbt, color ="red"), size= 0.2, alpha=0.2) +
  geom_abline(slope = 1, intercept = 0) +
  scale_color_manual(name ="",
                     values=c("green"="green","blue"="blue", "red" = "red"),
                     labels = c("green" = "Baseline", "blue"="Random Forest", "red" = "EGBT")) +
  ylab("Forudsagte intensitets faktor") +
  xlab("Intensitets faktor")+
  ylim(-100,100) +
  xlim(0,60) +
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        legend.text = element_text(size = 7),
        legend.background = element_rect(fill=NA)) +
  guides(color = guide_legend(override.aes = list(size=2, alpha = 1)))

setwd(Latexfigure)
ggsave("point_intensitet.pdf")

pdf("point_intensitet.pdf")
grid.arrange(box1, box2, box3)
dev.off()

boxplot(test_data$pred_res_baseline)
boxplot(test_data$pred_res_rf)
boxplot(test_data$pred_res_egbt)

