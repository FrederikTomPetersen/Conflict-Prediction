#----------------------------------------------------------------
#    Tuning af XGBtree algoritmen på "udbruddet af borgerkrig"

#----------------------------------------------------------------
# Funktion der tester antallet af klassifikationstræers effekt på AUC 

#     Oprettelse af tom dataframe
df_nrounds <- data.frame(matrix(ncol = 3, nrow = 0))
x <- c("rounds", "AUCInternal", "AUCexternal")
colnames(df_nrounds) <- x


#Definition af ved hvilke antal klassifikationstræer, der skal udtages værdier for
rounds <- c(seq(1, 10, by = 1),
            seq(11, 100, by = 5),
            seq(101, 200, by = 5),
            seq (201, 300, by = 5),
            seq(301, 400, by = 5))

#Definition af funktion til at teste AUC i test og træningsdatasæt.
#Tager df_nrounds som df, test_data som test, train_data som train, rounds som rounds, og 1 som start.

roundsfunction = function(df,train, test, rounds, m) {
  for (i in rounds) {
    grid <-  expand.grid(max_depth= 1,
                         nrounds= rounds[m],
                         eta= 0.25,
                         gamma = 0,
                         colsample_bytree= 0.8,
                         min_child_weight= 1,
                         subsample= 0.9)
    
    model <- train(cwstart ~  . -country - year -month,
                   data = train,
                   method = "xgbTree",
                   trControl = fitControl,
                   tuneGrid = grid,
                   objective = "binary:logistic")
    
    train$predict = predict(model, newdata = train, type="prob")[,2]
    test$predict = predict(model, newdata = test, type= "prob")[,2]
    ROCinternal <- roc(train$cwstart, train$predict)
    AUCinternal <-  auc(ROCinternal)
    ROCexternal <- roc(test$cwstart, test$predict)
    AUCexternal <-  auc(ROCexternal)
    obs <-  c(rounds[m], AUCinternal, AUCexternal )
    df <-  rbind(df,obs)
    print(paste("Dette var iteration nummer", m))
    m =m+1
  }
  return(df)
}

nroundstable4 <- roundsfunction(df_nrounds, train_data, test_data, rounds, 1)


grid <-  expand.grid(max_depth= 1,
                     nrounds= 441,
                     eta= 0.25,
                     gamma = 0,
                     colsample_bytree= 0.8,
                     min_child_weight= 1,
                     subsample= 0.9)

model <- train(cwstart ~  . -country - year -month,
               data = train_data,
               method = "xgbTree",
               trControl = fitControl,
               tuneGrid = grid,
               objective = "binary:logistic")

test_data$predict2 = predict(model, newdata = test_data, type= "prob")[,2]




#Udregning af ROC værdi for EGBtree 
calculate_roc <- function(df, cost_of_fp, cost_of_fn) {
  tpr <- function(df, threshold) {
    sum(test_data$predict2 >= threshold & test_data$cwstart == 1) / sum(test_data$cwstart == 1)
  }
  fpr <- function(df, threshold) {
    sum(test_data$predict2 >= threshold & test_data$cwstart == 0) / sum(test_data$cwstart == 0)
  }
  cost <- function(df, threshold, cost_of_fp, cost_of_fn) {
    sum(test_data$predict2 >= threshold & test_data$cwstart == 0) * 1 + 
      sum(test_data$predict2 < threshold & test_data$cwstart == 1) * 10
  }
  roc <- data.frame(threshold = breaks, tpr=NA, fpr=NA)
  roc$tpr <- sapply(roc$threshold, function(th) tpr(df, th))
  roc$fpr <- sapply(roc$threshold, function(th) fpr(df, th))
  roc$cost <- sapply(roc$threshold, function(th) cost(df, th, 1, 10))
  return(roc)}


roc <-  calculate_roc(test_data,1,10)

filter <-  roc %>% 
  filter(tpr >=0.375 & cost < 400)


#- Baseline model
plot_pred_type_distribution <- function(df, threshold) {
  v <- rep(NA, nrow(df))
  v <- ifelse(df$predict2 >= threshold & df$cwstart == 1, "TP", v)
  v <- ifelse(df$predict2 >= threshold & df$cwstart == 0, "FP", v)
  v <- ifelse(df$predict2 < threshold & df$cwstart == 1, "FN", v)
  v <- ifelse(df$predict2 < threshold & df$cwstart == 0, "TN", v)
}
# Tilførelse af FP,FN,SP,SN til datasæt 
test_data$pred_type <- plot_pred_type_distribution(test_data, 0.00400)

opgoer <-  test_data %>% 
  group_by(pred_type) %>% 
  summarise(c = n())