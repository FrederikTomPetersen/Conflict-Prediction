
#-------------------------------------------------------
# Dannelse af forudsigelsesmodeller for antal døde
#
#

#Indhentning af data fra PostGresSQL - database. Alternativ load complete_data_deaths.rds
data <-  dbGetQuery(con, "SELECT * from complete_data_death")

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
  select(-country, - year, -month, -cwy,-deathyear)
test_data <- test_data %>%
  select(-country, - year, -month, -cwy,-deathyear)


#-------------------------------------------------------
# Oprettelse af forudsigelsesmodeller
# Fitcontrol for modeller
fitcontrol <- trainControl(method = "cv", number = 2)

#---------------------------
# 1) baseline model (lineær)

model_baseline = train(deaths ~., 
                       data=train_data,
                       method = "lm")
setwd(Models)
save(model_baseline, file = "model_baseline_death.rda")

# indskrivning af prædiktion i test_data for sammenligning
test_data$deaths_pred_baseline = predict(model_baseline, newdata = test_data)
test_data$pred_res_baseline = test_data$deaths - test_data$deaths_pred_baseline


#---------------------------
# 1) random forest model
library("randomForest")


model_rf = train(deaths ~., 
                 data=train_data,
                 method = "rf",
                 trControl= fitcontrol,
                 preProcess=c("scale"),
                 linout=T)
setwd(Models)
save(model_rf, file = "model_rf_death.rda")

# indskrivning af prædiktion i test_data for sammenligning
test_data$deaths_pred_rf = predict(model_rf, newdata = test_data)
test_data$pred_res_rf = test_data$deaths - test_data$deaths_pred_rf


#---------------------------
# 1) extreme gradiant boosted trees

model_egbt = train(deaths ~., 
                 data=train_data,
                 method = "rf",
                 trControl= fitcontrol)
setwd(Models)
save(model_egbt, file = "model_egbt_death.rda")

# indskrivning af prædiktion i test_data for sammenligning
test_data$deaths_pred_egbt = predict(model_egbt, newdata = test_data)
test_data$pred_res_egbt = test_data$deaths - test_data$deaths_pred_egbt




#-------------------------------------------------------
# Comparing model performance 

ggplot(data=test_data) +
  geom_point(aes(x = deaths, y = deaths_pred_baseline, color ="green", alpha = "0.2", size = "0.1")) +
  #geom_point(aes(x = deaths, y = deaths_pred_rf, color ="yellow", size= "0.1", alpha="0.2")) +
  #geom_point(aes(x = deaths, y = deaths_pred_egbt, color ="red", size= "0.1", alpha="0.2")) +
  geom_abline(slope = 1, intercept = 0) +
  scale_color_manual(name ="",
                     values=c("green"="green","yellow"="yellow", "red" = "red"),
                     labels = c("green" = "Baseline", "yellow"="Random Forest", "red" = "EGBT")) +
  ylab("Forudsagt antal døde") +
  xlab("Faktisk antal døde")+
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        legend.text = element_text(size = 7),
        legend.background = element_rect(fill=NA)) +
  guides(color = guide_legend(override.aes = list(size=2, alpha = 1)))

setwd(Latexfigure)
ggsave("comp_death.pdf")


box1 <- test_data %>% 
  filter(pred_res_baseline < 10000 & pred_res_baseline > -10000) %>% 
  ggplot() +
      geom_boxplot(aes(y=pred_res_baseline))

box2 <- test_data %>% 
  filter(pred_res_rf < 10000 & pred_res_rf > -10000) %>% 
  ggplot() +
  geom_boxplot(aes(y=pred_res_rf))

box3 <- test_data %>% 
  filter(pred_res_egbt < 10000 & pred_res_egbt > -10000) %>% 
  ggplot() +
  geom_boxplot(aes(y=pred_res_egbt))

pdf("foo.pdf")
grid.arrange(box1, box2, box3)
dev.off()

