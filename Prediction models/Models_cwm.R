set.seed(1)
#-----------------------------------------------------------
# Dannelse af forudsigelsesmodeller for udbrud af borgerkrig

load("Data_cwm.Rda")

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
model_baseline_cwm <- glm(cwm ~  . -country - year -month,family=binomial(link='logit'),data=train_data)
summary(model_baseline_cwm)
setwd(Models)
save(model_baseline_cwm, file = "0_glm_cwm.rda")
#forudsigelse
test_data$cwm_pred_baseline = predict(model_baseline_cwm, newdata = test_data, type="response")


#----------Random forest model----------
library("randomForest")
model_rf_cwm <- randomForest(cwm ~ . -country - year -month, data=train_data ,ntree=250, importance=TRUE)
model_rf_cwm
setwd(Models)
save(model_rf_cwm, file = "2_rf_cwm.rda")

#Predictions
test_data$cwm_pred_rf = predict(model_rf_cwm, newdata = test_data, type="prob")[,2]


#-------------extreme gradient boosted trees -------
library("caret")

fitControl <- trainControl(method = "cv", number = 5)

model_egbt_cwm <- train(cwm ~  . -country - year -month,
                    data = train_data,
                    method = "xgbTree",
                    trControl = fitControl,
                    search = "random",
                    objective = "binary:logistic")
model_egbt_cwm
setwd(Models)
save(model_egbt_cwm, file="3_egbt_cwm.rda")


#Predictions
test_data$cwm_pred_egbt = predict(model_egbt_cwm, newdata = test_data, type="prob")[,2]

#-----------------------------------------------------------
#           Visualiseringer af forudsigelser

#-----------------------------------------------------------
#     Optimale tærskelværdier for delta findes gennem en analyse af ROC elementerne 
#    
breaks <- c(seq(0, 0.00001, by = 0.000000001),
            seq(0.00001,0.0001, by = 0.0000001),
            seq(0.0001, 0.001, by = 0.000001),
            seq (0.001, 0.01, by = 0.00001),
            seq(0.01,0.1, by = 0.0001),
            seq(0.1,1, by = 0.001)) 
# bruges til at oprette hvor mange varitioner for af delta der skal udregnes

#Oprettelse af genereisk ROC-plot function: 


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
    geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color="blue", size =0.5) + 
    xlab("FPR") + 
    ylab("SPR") +
    xlim(0,1) +
    ylim(0,1) +
    theme(axis.text.x=element_text(size=rel(0.8), angle=90))+
    theme(axis.text.y=element_text(size=rel(0.8)))+
    theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))+
    geom_hline(yintercept=roc[idx_threshold,"tpr"], alpha=0.5, linetype="dashed") +
    geom_vline(xintercept=roc[idx_threshold,"fpr"], alpha=0.5, linetype="dashed")+
    theme(plot.title = element_text(size=12, hjust = 0.5))
  p_roc
}


#Udregning af ROC værdi for baselinemodel
calculate_roc <- function(df, cost_of_fp, cost_of_fn) {
  tpr <- function(df, threshold) {
    sum(test_data$cwm_pred_baseline >= threshold & test_data$cwm == 1) / sum(test_data$cwm == 1)
  }
  
  fpr <- function(df, threshold) {
    sum(test_data$cwm_pred_baseline >= threshold & test_data$cwm == 0) / sum(test_data$cwm == 0)
  }
  
  cost <- function(df, threshold, cost_of_fp, cost_of_fn) {
    sum(test_data$cwm_pred_baseline >= threshold & test_data$cwm == 0) * 1 + 
      sum(test_data$cwm_pred_baseline < threshold & test_data$cwm == 1) * 10
  }
  
  roc <- data.frame(threshold = breaks, tpr=NA, fpr=NA)
  roc$tpr <- sapply(roc$threshold, function(th) tpr(df, th))
  roc$fpr <- sapply(roc$threshold, function(th) fpr(df, th))
  roc$cost <- sapply(roc$threshold, function(th) cost(df, th, 1, 10))
  
  return(roc)
}

#Udregning og plot
roc <- calculate_roc(test_data, 1, 10)
roc_baseline_cvm <- plot_roc(roc, 0.337, 1,10)
roc_baseline_cvm

#Udregning af ROC værdi for Random Forest model
calculate_roc <- function(df, cost_of_fp, cost_of_fn) {
  tpr <- function(df, threshold) {
    sum(test_data$cwm_pred_rf >= threshold & test_data$cwm == 1) / sum(test_data$cwm == 1)
  }
  
  fpr <- function(df, threshold) {
    sum(test_data$cwm_pred_rf >= threshold & test_data$cwm == 0) / sum(test_data$cwm == 0)
  }
  
  cost <- function(df, threshold, cost_of_fp, cost_of_fn) {
    sum(test_data$cwm_pred_rf >= threshold & test_data$cwm == 0) * 1 + 
      sum(test_data$cwm_pred_rf < threshold & test_data$cwm == 1) * 10
  }
  
  roc <- data.frame(threshold = breaks, tpr=NA, fpr=NA)
  roc$tpr <- sapply(roc$threshold, function(th) tpr(df, th))
  roc$fpr <- sapply(roc$threshold, function(th) fpr(df, th))
  roc$cost <- sapply(roc$threshold, function(th) cost(df, th, 1, 10))
  
  return(roc)
}

#Udregning og plot
roc <- calculate_roc(test_data, 1, 10)
roc_rf_cwm <- plot_roc(roc, 0.1290, 1,10)
roc_rf_cwm

#Udregning af ROC værdi for EGBtree 

calculate_roc <- function(df, cost_of_fp, cost_of_fn) {
  tpr <- function(df, threshold) {
    sum(test_data$cwm_pred_egbt >= threshold & test_data$cwm == 1) / sum(test_data$cwm == 1)
  }
  
  fpr <- function(df, threshold) {
    sum(test_data$cwm_pred_egbt >= threshold & test_data$cwm == 0) / sum(test_data$cwm == 0)
  }
  
  cost <- function(df, threshold, cost_of_fp, cost_of_fn) {
    sum(test_data$cwm_pred_egbt >= threshold & test_data$cwm == 0) * 1 + 
      sum(test_data$cwm_pred_egbt < threshold & test_data$cwm == 1) * 10
  }
  
  roc <- data.frame(threshold = breaks, tpr=NA, fpr=NA)
  roc$tpr <- sapply(roc$threshold, function(th) tpr(df, th))
  roc$fpr <- sapply(roc$threshold, function(th) fpr(df, th))
  roc$cost <- sapply(roc$threshold, function(th) cost(df, th, 1, 10))
  
  return(roc)
}

#Udregning og plot
roc <- calculate_roc(test_data, 1, 10)
roc_egbt_cwm <- plot_roc(roc, 0.0396, 1,10)
roc_egbt_cwm

setwd(Latexfigure)
png("roc_cwm.png")
grid.arrange(roc_baseline_cvm, roc_rf_cwm,roc_egbt_cwm, ncol=3, nrow=1)
dev.off()



#-------Densitets histogrammer for de 3 forudsigelser

#- Baseline model
plot_pred_type_distribution <- function(df, threshold) {
  v <- rep(NA, nrow(df))
  v <- ifelse(df$cwm_pred_baseline >= threshold & df$cwm == 1, "TP", v)
  v <- ifelse(df$cwm_pred_baseline >= threshold & df$cwm == 0, "FP", v)
  v <- ifelse(df$cwm_pred_baseline < threshold & df$cwm == 1, "FN", v)
  v <- ifelse(df$cwm_pred_baseline < threshold & df$cwm == 0, "TN", v)
}
# Tilførelse af FP,FN,SP,SN til datasæt 
test_data$pred_type <- plot_pred_type_distribution(test_data, 0.337)

#Plotting af densitet histogram med jitter og linje
densitet_baseline_cwm <- test_data %>%
  ggplot(aes(as.factor(cwm),cwm_pred_baseline)) +
  geom_violin(mapping = NULL, data = test_data, stat = "ydensity",
              position = "dodge", draw_quantiles = NULL, trim = TRUE,
              scale = "width", na.rm = TRUE, show.legend = TRUE,
              inherit.aes = TRUE)+
  geom_jitter(aes(color=pred_type),data= test_data, alpha=0.2, width = 0.4)+
  geom_hline(yintercept=0.337, color="red", alpha=0.6)+
  labs(y = "Sandsynlighed", x="", colour ="Prædiktionstype") +
  scale_x_discrete(breaks=c("0","1"),
                   labels=c("ikke-borgerkrig \n y = 0", "Borgerkrig \n y = 1")) +
  guides(color = guide_legend(override.aes = list(size=1, alpha = 1))) +
  ggtitle("Baseline model") +
  theme(plot.title = element_text(hjust = 0.5, size =12, face = "plain"))

densitet_baseline_cwm

# -------------------------------------------------
#- Random forest model
plot_pred_type_distribution <- function(df, threshold) {
  v <- rep(NA, nrow(df))
  v <- ifelse(df$cwm_pred_rf >= threshold & df$cwm == 1, "TP", v)
  v <- ifelse(df$cwm_pred_rf >= threshold & df$cwm == 0, "FP", v)
  v <- ifelse(df$cwm_pred_rf < threshold & df$cwm == 1, "FN", v)
  v <- ifelse(df$cwm_pred_rf < threshold & df$cwm == 0, "TN", v)
}
# Tilførelse af FP,FN,SP,SN til datasæt 
test_data$pred_type <- plot_pred_type_distribution(test_data, 0.1290)


#Plotting af densitet histogram med jitter og linje
densitet_rf_cwm <- test_data %>%
  ggplot(aes(as.factor(cwm),cwm_pred_rf)) +
  geom_violin(mapping = NULL, data = test_data, stat = "ydensity",
              position = "dodge", draw_quantiles = NULL, trim = TRUE,
              scale = "width", na.rm = TRUE, show.legend = TRUE,
              inherit.aes = TRUE)+
  geom_jitter(aes(color=pred_type),data= test_data, alpha=0.2, width = 0.4)+
  geom_hline(yintercept=0.1290, color="red", alpha=0.6)+
  labs( y = "Sandsynlighed", x="", colour ="Prædiktionstype") +
  scale_x_discrete(breaks=c("0","1"),
                   labels=c("ikke-borgerkrig \n y = 0", "Borgerkrig \n y = 1")) +
  guides(color = guide_legend(override.aes = list(size=1, alpha = 1, default.unit = ""))) +
  ggtitle("Random forest model") +
  theme(plot.title = element_text(hjust = 0.5, size =12, face = "plain"))
densitet_rf_cwm


# -------------------------------------------------
#- extreme gradient boosting model
plot_pred_type_distribution <- function(df, threshold) {
  v <- rep(NA, nrow(df))
  v <- ifelse(df$cwm_pred_egbt >= threshold & df$cwm == 1, "TP", v)
  v <- ifelse(df$cwm_pred_egbt >= threshold & df$cwm == 0, "FP", v)
  v <- ifelse(df$cwm_pred_egbt < threshold & df$cwm == 1, "FN", v)
  v <- ifelse(df$cwm_pred_egbt < threshold & df$cwm == 0, "TN", v)
}
# Tilførelse af FP,FN,SP,SN til datasæt 
test_data$pred_type <- plot_pred_type_distribution(test_data, 0.0396)


#Plotting af densitet histogram med jitter og linje
densitet_egbt_cwm <- test_data %>%
  ggplot(aes(as.factor(cwm),cwm_pred_egbt)) +
  geom_violin(mapping = NULL, data = test_data, stat = "ydensity",
              position = "dodge", draw_quantiles = NULL, trim = TRUE,
              scale = "width", na.rm = TRUE, show.legend = TRUE,
              inherit.aes = TRUE)+
  geom_jitter(aes(color=pred_type),data= test_data, alpha=0.2, width = 0.4)+
  geom_hline(yintercept=0.0396, color="red", alpha=0.6)+
  labs( y = "Sandsynlighed", x="", colour ="Prædiktionstype") +
  scale_x_discrete(breaks=c("0","1"),
                   labels=c("ikke-borgerkrig \n y = 0", "Borgerkrig \n y = 1")) +
  guides(color = guide_legend(override.aes = list(size=1, alpha = 1))) +
  ggtitle("EXGT model") +
  theme(plot.title = element_text(hjust = 0.5, size =12, face = "plain"))
densitet_egbt_cwm

#Samling af figurer til 1 figur
setwd(Latexfigure)
png("density_cwm.png")
grid.arrange(densitet_baseline_cwm, densitet_rf_cwm,densitet_egbt_cwm, ncol=1, nrow=3)
dev.off()


# ---------------------------------
# Area under curve
library("pROC")

ROC_logit <- roc(test_data$cwm, test_data$cwm_pred_baseline)
ROC_rf <-  roc(test_data$cwm, test_data$cwm_pred_rf)
ROC_egbt <- roc(test_data$cwm, test_data$cwm_pred_egbt)

auc(ROC_logit)
auc(ROC_rf)
auc(ROC_egbt)

