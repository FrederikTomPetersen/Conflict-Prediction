set.seed(42)
#-------------------------------------------------------------------------------------
#                           Tuning af EGBtree model

#Load af oprindelige model
setwd(Models)
load("3_egbt_cwstart.rda")
plot(model_egbt_cwstart) # plot af modellens indledene forsøg med "tilfældig tuning
oprindeligeparametre <- model_egbt_cwstart$modelInfo$parameters #oprindelige parametre for model

#load af data
setwd(Datafinal)
load("Data_cwstart.Rda")

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
library("caret")
roundsfunction = function(df,train, test, rounds, m) {
  for (i in rounds) {
    grid <-  expand.grid(max_depth= 1,
                         nrounds= rounds[m],
                         eta= 0.3,
                         gamma = 0,
                         colsample_bytree= 0.6,
                         min_child_weight= 1,
                         subsample= 0.5)
    
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

nroundstable <- roundsfunction(df_nrounds, train_data, test_data, rounds, 1)
#Tabel med AUC værdier for test og træningsdatasæt

# Plot af nrounds 
colnames(nroundstable) <- x

ggplot(data=nroundstable) +
  geom_point(aes(x = rounds, y = AUCInternal, color ="#E69F00"), size = 0.7, alpha = 0.5) +
  geom_point(aes(x = rounds, y = AUCexternal, color ="#56B4E9"), size= 0.7, alpha=0.5) +
  geom_line(aes(x = rounds, y = AUCexternal, color = "#56B4E9"), size = 0.9, alpha = 1)+
  geom_line(aes(x = rounds, y = AUCInternal, color = "#E69F00"), size = 0.9, alpha = 1)+
  
  scale_color_manual(name ="",
                     values=c("#E69F00"="#E69F00","#56B4E9"="#56B4E9"),
                     labels = c("#E69F00" = "AUC træningsdata", "#56B4E9"="AUC testdata")) +
  ylab("AUC") +
  xlab("Antal sekventielle klassifikationstræer")+
  ylim(0.5,1) +
  xlim(0,300) +
  theme(legend.position = "right",
        legend.title=element_blank(),
        legend.text = element_text(size = 7),
        legend.background = element_rect(fill=NA)) +
  guides(color = guide_legend(override.aes = list(size=1, alpha = 1)))


#-------------------------------------------------------------------------

#     Gridsearch af modeller





#som det ses har modellerne forsøgt med 3 forskellige subsample sizes ved to forskellige læringskoeficienter

library("caret")
fitControl <- trainControl(method = "cv", number = 5)

xgboostGrid <-  expand.grid(max_depth=1,
                            nrounds=seq(1,500, by=1),
                            eta=c(0.1 ,0.2,0.25, 0.3, 0.04),
                            gamma = 1,
                            colsample_bytree=c(0.5, 0.6, 0.7, 0.8, 0.9, 1),
                            min_child_weight=1,
                            subsample=c(0.5, 0.6, 0.7, 0.8, 0.9, 1))


model_alternative <- train(cwstart ~  . -country - year -month,
                            data = train_data,
                            method = "xgbTree",
                            trControl = fitControl,
                            tuneGrid = xgboostGrid,
                            objective = "binary:logistic")

# Giver følgende bedste identificerede model:


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

test_data$predict = predict(model, newdata = test_data, type= "prob")[,2]




#Udregning af ROC værdi for tunet EGBtree 

breaks <- c(seq(0, 0.00001, by = 0.000000001),
            seq(0.00001,0.0001, by = 0.0000001),
            seq(0.0001, 0.001, by = 0.000001),
            seq (0.001, 0.01, by = 0.00001),
            seq(0.01,0.1, by = 0.0001),
            seq(0.1,1, by = 0.001)) 

calculate_roc <- function(df, cost_of_fp, cost_of_fn) {
  tpr <- function(df, threshold) {
    sum(test_data$predict >= threshold & test_data$cwstart == 1) / sum(test_data$cwstart == 1)
  }
  fpr <- function(df, threshold) {
    sum(test_data$predict >= threshold & test_data$cwstart == 0) / sum(test_data$cwstart == 0)
  }
  cost <- function(df, threshold, cost_of_fp, cost_of_fn) {
    sum(test_data$predict >= threshold & test_data$cwstart == 0) * 1 + 
      sum(test_data$predict < threshold & test_data$cwstart == 1) * 10
  }
  roc <- data.frame(threshold = breaks, tpr=NA, fpr=NA)
  roc$tpr <- sapply(roc$threshold, function(th) tpr(df, th))
  roc$fpr <- sapply(roc$threshold, function(th) fpr(df, th))
  roc$cost <- sapply(roc$threshold, function(th) cost(df, th, 1, 10))
  return(roc)}


roc <-  calculate_roc(test_data,1,10)

filter <-  roc %>% 
  filter(tpr >= 0.5 & cost < 400)


#- FP, TP, FN, TN
plot_pred_type_distribution <- function(df, threshold) {
  v <- rep(NA, nrow(df))
  v <- ifelse(df$predict >= threshold & df$cwstart == 1, "TP", v)
  v <- ifelse(df$predict >= threshold & df$cwstart == 0, "FP", v)
  v <- ifelse(df$predict < threshold & df$cwstart == 1, "FN", v)
  v <- ifelse(df$predict < threshold & df$cwstart == 0, "TN", v)
}
# Tilførelse af FP,FN,SP,SN til datasæt 
test_data$pred_type <- plot_pred_type_distribution(test_data, 0.00291)

opgoer <-  test_data %>% 
  group_by(pred_type) %>% 
  summarise(c = n())
