
#-----------------------------------------------------------
# Dannelse af forudsigelsesmodeller for udbrud af borgerkrig
#
#
#Indhentning af data fra PostGresSQL - database. Alternativ load complete_data_deaths.rds
data <-  dbGetQuery(con, "SELECT * from complete_data_cwstart")

a <-  train_data %>% 
  filter(cwstart==1)

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

#-------------Baseline model--------------
model_baseline <- glm(cwstart ~  .,family=binomial(link='logit'),data=train_data)
summary(model_baseline)
setwd(Models)
save(model_baseline, file = "0_glm_cwstart.rda")
#forudsigelse
test_data$cwstart_pred_baseline = predict(model_baseline, newdata = test_data, type="response")
test_data$pred_res_baseline = test_data$cwstart - test_data$cwstart_pred_baseline



#----------Random forest model----------
library("randomForest")
model_rf <- randomForest(cwstart ~ ., data=train_data ,ntree=250, importance=TRUE)
model_rf
setwd(Models)
save(model_rf, file = "2_rf_cwstart.rda")

#Predictions
test_data$cwstart_pred_rf = predict(model_rf, newdata = test_data, type="response")
test_data$pred_res_rd = test_data$cwstart - test_data$cwstart_pred_rf


#-------------extreme gradient boosted trees -------
library("caret")

fitControl <- trainControl(method = "cv", number = 5)

model_egbt <- train(cwstart ~  .,
                        data = train_data,
                        method = "xgbTree",
                        trControl = fitControl,
                        search = "random",
                        objective = "binary:logistic")
model_egbt
setwd(Models)
save(model_egbt, file="3_egbt_cwstart.rda")


#Predictions
test_data$cwstart_pred_egbt = predict(model_egbt, newdata = test_data)
test_data$pred_res_egbt = test_data$cwstart - test_data$cwstart_pred_egbt


#-----------------------------------------------------------
#           Visualiseringer af forudsigelser


#-------Densitets histogrammer for de 3 forudsigelser

#- Baseline model
plot_pred_type_distribution <- function(df, threshold) {
  v <- rep(NA, nrow(df))
  v <- ifelse(df$cwstart_pred_baseline >= threshold & df$cwstart == 1, "TP", v)
  v <- ifelse(df$cwstart_pred_baseline >= threshold & df$cwstart == 0, "FP", v)
  v <- ifelse(df$cwstart_pred_baseline < threshold & df$cwstart == 1, "FN", v)
  v <- ifelse(df$cwstart_pred_baseline < threshold & df$cwstart == 0, "TN", v)
}
# Tilførelse af FP,FN,SP,SN til datasæt 
test_data$pred_type <- plot_pred_type_distribution(test_data, 0.1)


#Plotting af densitet histogram med jitter og linje
densitet_baseline_cwstart <- test_data %>%
  ggplot(aes(as.factor(cwstart),cwstart_pred_baseline)) +
  geom_violin(mapping = NULL, data = test_data, stat = "ydensity",
              position = "dodge", draw_quantiles = NULL, trim = TRUE,
              scale = "area", na.rm = TRUE, show.legend = TRUE,
              inherit.aes = TRUE)+
  geom_jitter(aes(color=pred_type),data= test_data, alpha=0.2, width = 0.4)+
  geom_hline(yintercept=0.1, color="red", alpha=0.6)+
  labs( y = "Prædikteret sandsynlighed for borgerkrig", x="") +
  scale_x_discrete(breaks=c("0","1"),
                   labels=c("ikke-borgerkrig \n y = 0", "Borgerkrig \n y = 1"))
densitet_baseline_cwstart

#- Random forest model
plot_pred_type_distribution <- function(df, threshold) {
  v <- rep(NA, nrow(df))
  v <- ifelse(df$cwstart_pred_rf >= threshold & df$cwstart == 1, "TP", v)
  v <- ifelse(df$cwstart_pred_rf >= threshold & df$cwstart == 0, "FP", v)
  v <- ifelse(df$cwstart_pred_rf < threshold & df$cwstart == 1, "FN", v)
  v <- ifelse(df$cwstart_pred_rf < threshold & df$cwstart == 0, "TN", v)
}
# Tilførelse af FP,FN,SP,SN til datasæt 
test_data$pred_type <- plot_pred_type_distribution(test_data, 0.1)


#Plotting af densitet histogram med jitter og linje
densitet_rf_cwstart <- test_data %>%
  ggplot(aes(as.factor(cwstart),cwstart_pred_rf)) +
  geom_violin(mapping = NULL, data = test_data, stat = "ydensity",
              position = "dodge", draw_quantiles = NULL, trim = TRUE,
              scale = "area", na.rm = TRUE, show.legend = TRUE,
              inherit.aes = TRUE)+
  geom_jitter(aes(color=pred_type),data= test_data, alpha=0.2, width = 0.4)+
  geom_hline(yintercept=0.1, color="red", alpha=0.6)+
  labs( y = "Prædikteret sandsynlighed for borgerkrig", x="") +
  scale_x_discrete(breaks=c("0","1"),
                   labels=c("ikke-borgerkrig \n y = 0", "Borgerkrig \n y = 1"))
densitet_rf_cwstart



#- egbt model
plot_pred_type_distribution <- function(df, threshold) {
  v <- rep(NA, nrow(df))
  v <- ifelse(df$cwstart_pred_egbt >= threshold & df$cwstart == 1, "TP", v)
  v <- ifelse(df$cwstart_pred_egbt >= threshold & df$cwstart == 0, "FP", v)
  v <- ifelse(df$cwstart_pred_egbt < threshold & df$cwstart == 1, "FN", v)
  v <- ifelse(df$cwstart_pred_egbt < threshold & df$cwstart == 0, "TN", v)
}
# Tilførelse af FP,FN,SP,SN til datasæt 
test_data$pred_type <- plot_pred_type_distribution(test_data, 0.02)


#Plotting af densitet histogram med jitter og linje
densitet_egbt_cwstart <- test_data %>%
  ggplot(aes(as.factor(cwstart),cwstart_pred_egbt)) +
  geom_violin(mapping = NULL, data = test_data, stat = "ydensity",
              position = "dodge", draw_quantiles = NULL, trim = TRUE,
              scale = "area", na.rm = TRUE, show.legend = TRUE,
              inherit.aes = TRUE)+
  geom_jitter(aes(color=pred_type),data= test_data, alpha=0.2, width = 0.4)+
  geom_hline(yintercept=0.02, color="red", alpha=0.6)+
  labs( y = "Prædikteret sandsynlighed for borgerkrig", x="") +
  scale_x_discrete(breaks=c("0","1"),
                   labels=c("ikke-borgerkrig \n y = 0", "Borgerkrig \n y = 1"))
densitet_egbt_cwstart

pdf("density_cwstart.pdf")
grid.arrange(densitet_baseline_cwstart, densitet_rf_cwstart,densitet_egbt_cwstart)
dev.off()

# ---------------------------------
# simple ROCS 
library("pROC")

ROC_logit <- roc(test_data$cwstart, test_data$cwstart_pred_baseline)
ROC_rf <-  roc(test_data$cwstart, test_data$cwstart_pred_rf)
ROC_egbt <- roc(test_data$cwstart, test_data$cwstart_pred_egbt)
plot(ROC_logit)
plot(ROC_rf)
plot(ROC_egbt)

auc(ROC_logit)
auc(ROC_rf)
auc(ROC_egbt)

#--------------------------------------------
#       Advanced ROC 

#Oprettelse af genereisk plot function: 

plot_roc <- function(roc, threshold, cost_of_fp, cost_of_fn) {
  norm_vec <- function(v) (v - min(v))/diff(range(v))
  idx_threshold = which.min(abs(roc$threshold-threshold))
  col_ramp <- colorRampPalette(c("green","orange","red","black"))(100)
  col_by_cost <- col_ramp[ceiling(norm_vec(roc$cost)*99)+1]
  p_roc <- ggplot(roc, aes(fpr,tpr)) + 
    geom_line(color=rgb(0,0,1,alpha=0.6)) +
    geom_point(color=col_by_cost, size=1, alpha=1) +
    coord_fixed() +
    geom_line(aes(threshold,threshold), color=rgb(0,0,1,alpha=0.5)) +
    xlab("FPR") + 
    ylab("TPR") +
    geom_hline(yintercept=roc[idx_threshold,"tpr"], alpha=0.5, linetype="dashed") +
    geom_vline(xintercept=roc[idx_threshold,"fpr"], alpha=0.5, linetype="dashed")
  p_roc
}


#Udregning af ROC værdi for baselinemodel
calculate_roc <- function(df, cost_of_fp, cost_of_fn, n=100) {
  tpr <- function(df, threshold) {
    sum(test_data$cwstart_pred_baseline >= threshold & test_data$cwstart == 1) / sum(test_data$cwstart == 1)
  }
  
  fpr <- function(df, threshold) {
    sum(test_data$cwstart_pred_baseline >= threshold & test_data$cwstart == 0) / sum(test_data$cwstart == 0)
  }
  
  cost <- function(df, threshold, cost_of_fp, cost_of_fn) {
    sum(test_data$cwstart_pred_baseline >= threshold & test_data$cwstart == 0) * cost_of_fp + 
      sum(test_data$cwstart_pred_baseline < threshold & test_data$cwstart == 1) * cost_of_fn
  }
  
  roc <- data.frame(threshold = seq(0,1,length.out=n), tpr=NA, fpr=NA)
  roc$tpr <- sapply(roc$threshold, function(th) tpr(df, th))
  roc$fpr <- sapply(roc$threshold, function(th) fpr(df, th))
  roc$cost <- sapply(roc$threshold, function(th) cost(df, th, cost_of_fp, cost_of_fn))
  
  return(roc)
}

#Udregning og plot
roc <- calculate_roc(test_data, 1, 4, n = 1000)
plot_roc(roc, 0.1, 1,4)


#Udregning af ROC værdi for random forest model
calculate_roc <- function(df, cost_of_fp, cost_of_fn, n=100) {
  tpr <- function(df, threshold) {
    sum(test_data$cwstart_pred_rf >= threshold & test_data$cwstart == 1) / sum(test_data$cwstart == 1)
  }
  
  fpr <- function(df, threshold) {
    sum(test_data$cwstart_pred_rf >= threshold & test_data$cwstart == 0) / sum(test_data$cwstart == 0)
  }
  
  cost <- function(df, threshold, cost_of_fp, cost_of_fn) {
    sum(test_data$cwstart_pred_rf >= threshold & test_data$cwstart == 0) * cost_of_fp + 
      sum(test_data$cwstart_pred_rf < threshold & test_data$cwstart == 1) * cost_of_fn
  }
  
  roc <- data.frame(threshold = seq(0,1,length.out=n), tpr=NA, fpr=NA)
  roc$tpr <- sapply(roc$threshold, function(th) tpr(df, th))
  roc$fpr <- sapply(roc$threshold, function(th) fpr(df, th))
  roc$cost <- sapply(roc$threshold, function(th) cost(df, th, cost_of_fp, cost_of_fn))
  
  return(roc)
}

#Udregning og plot
roc <- calculate_roc(test_data, 1, 4, n = 1000)
plot_roc(roc, 0.1, 1,4)




#Udregning af ROC værdi for egbt model
calculate_roc <- function(df, cost_of_fp, cost_of_fn, n=100) {
  tpr <- function(df, threshold) {
    sum(test_data$cwstart_pred_egbt >= threshold & test_data$cwstart == 1) / sum(test_data$cwstart == 1)
  }
  
  fpr <- function(df, threshold) {
    sum(test_data$cwstart_pred_egbt >= threshold & test_data$cwstart == 0) / sum(test_data$cwstart == 0)
  }
  
  cost <- function(df, threshold, cost_of_fp, cost_of_fn) {
    sum(test_data$cwstart_pred_egbt >= threshold & test_data$cwstart == 0) * cost_of_fp + 
      sum(test_data$cwstart_pred_egbt < threshold & test_data$cwstart == 1) * cost_of_fn
  }
  
  roc <- data.frame(threshold = seq(0,1,length.out=n), tpr=NA, fpr=NA)
  roc$tpr <- sapply(roc$threshold, function(th) tpr(df, th))
  roc$fpr <- sapply(roc$threshold, function(th) fpr(df, th))
  roc$cost <- sapply(roc$threshold, function(th) cost(df, th, cost_of_fp, cost_of_fn))
  
  return(roc)
}

#Udregning og plot
roc <- calculate_roc(test_data, 1, 4, n = 1000)
plot_roc(roc, 0.1, 1,4)




#-------------------------------------------------------
#                   Feature importance

#RF
importance(model_rf)
varImpPlot(model_rf,type=2, cex=0.5)

