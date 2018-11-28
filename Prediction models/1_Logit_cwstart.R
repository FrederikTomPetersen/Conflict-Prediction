
#getting data
data <-  dbGetQuery(con, "SELECT * from complete_data_cwstart")

#After postgres factors needs to be redefined
data$Oil <-  as.factor(data$Oil)
data$elct_comp <-  as.factor(data$elct_comp)
data$elct_regulation <-  as.factor(data$elct_regulation)
data$elct_open <-  as.factor(data$elct_open)
data$exe_constraint <-  as.factor(data$exe_constraint)
data$colstyle <-  as.factor( data$colstyle)


#Splitting data
train_data <-  data %>%
  filter(year <= 2011)
test_data <- data %>% 
  filter(year >=2012)

#removing data (factors and knowledge not attaindable at observation time)
train_data <- train_data %>%
  select(-country, - year, -month, -cwy,-deathyear)
test_data <- test_data %>%
  select(-country, - year, -month,-cwy,-deathyear)

#Creating the model
model <- glm(as.factor(cwstart) ~  .,family=binomial(link='logit'),data=train_data)
summary(model)
setwd(Models)
save(model, file = "0_glm_cwstart.rda")


#Predictions
test_data$cwstart_pred = predict(model, newdata = test_data, type="response")
test_data$pred_res = test_data$cwstart - test_data$cwstart





#Creating the density histogram
plot_pred_type_distribution <- function(df, threshold) {
  v <- rep(NA, nrow(df))
  v <- ifelse(df$cwstart_pred >= threshold & df$cwstart == 1, "TP", v)
  v <- ifelse(df$cwstart_pred >= threshold & df$cwstart == 0, "FP", v)
  v <- ifelse(df$cwstart_pred < threshold & df$cwstart == 1, "FN", v)
  v <- ifelse(df$cwstart_pred < threshold & df$cwstart == 0, "TN", v)
}
# Tilførelse af FP,FN,SP,SN til datasæt 
test_data$pred_type <- plot_pred_type_distribution(test_data, 0.001)


#Plotting af densitet histogram med jitter og linje
test_data %>%
  ggplot(aes(as.factor(cwstart),cwstart_pred)) +
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


# Creating the ROC CURVE 1

ROC <- roc(test_data$cwstart, test_data$cwstart_pred)
plot(ROC)
dev.print(pdf, 'roc1.pdf')
auc(ROC)
rm(ROC)



calculate_roc <- function(df, cost_of_fp, cost_of_fn, n=100) {
  tpr <- function(df, threshold) {
    sum(test_data$cwstart_pred >= threshold & test_data$cwstart == 1) / sum(test_data$cwstart == 1)
  }
  
  fpr <- function(df, threshold) {
    sum(test_data$cwstart_pred >= threshold & test_data$cwstart == 0) / sum(test_data$cwstart == 0)
  }
  
  cost <- function(df, threshold, cost_of_fp, cost_of_fn) {
    sum(test_data$cwstart_pred >= threshold & test_data$cwstart == 0) * cost_of_fp + 
      sum(test_data$cwstart_pred < threshold & test_data$cwstart == 1) * cost_of_fn
  }
  
  roc <- data.frame(threshold = seq(0,1,length.out=n), tpr=NA, fpr=NA)
  roc$tpr <- sapply(roc$threshold, function(th) tpr(df, th))
  roc$fpr <- sapply(roc$threshold, function(th) fpr(df, th))
  roc$cost <- sapply(roc$threshold, function(th) cost(df, th, cost_of_fp, cost_of_fn))
  
  return(roc)
}


roc <- calculate_roc(test_data, 1, 4, n = 1000)

plot_roc_and_cost <- function(roc, threshold, cost_of_fp, cost_of_fn) {
  
  
  norm_vec <- function(v) (v - min(v))/diff(range(v))
  
  idx_threshold = which.min(abs(roc$threshold-threshold))
  
  col_ramp <- colorRampPalette(c("green","orange","red","black"))(100)
  col_by_cost <- col_ramp[ceiling(norm_vec(roc$cost)*99)+1]
  p_roc <- ggplot(roc, aes(fpr,tpr)) + 
    geom_line(color=rgb(0,0,1,alpha=0.3)) +
    geom_point(color=col_by_cost, size=1, alpha=0.2) +
    coord_fixed() +
    geom_line(aes(threshold,threshold), color=rgb(0,0,1,alpha=0.5)) +
    labs(title = sprintf("ROC-kurve")) + xlab("FPR") + ylab("SPR") +
    geom_hline(yintercept=roc[idx_threshold,"tpr"], alpha=0.5, linetype="dashed") +
    geom_vline(xintercept=roc[idx_threshold,"fpr"], alpha=0.5, linetype="dashed")
  
  p_cost <- ggplot(roc, aes(threshold, cost)) +
    geom_line(color=rgb(0,0,1,alpha=0.3)) +
    geom_point(color=col_by_cost, size=1, alpha=0.2) +
    labs(title = sprintf("cost function")) +
    geom_vline(xintercept=threshold, alpha=0.5, linetype="dashed")
  
  grid.arrange(p_roc, p_cost, ncol=2)
}


plot_roc_and_cost(roc, 0.2, 1, 2)
setwd(Latexfigure)
ggsave(filename = "0_glm_cost_cwstart.pdf" )





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

plot_roc(roc, 0.20, 1, 4)
setwd(Latexfigure)
ggsave(filename = "0_glm_roc_cwstart.pdf" )

auc(test_data$cwstart, test_data$cwstart_pred)




