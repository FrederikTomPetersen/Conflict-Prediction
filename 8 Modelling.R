
cat("\014")  

################################################################################################
##                                Modellering af prædiktionsmodeller                          ##
################################################################################################
completedata <-  dbGetQuery(con, "SELECT * from complete_data")

data <-  completedata

#når man arbejder med opdeling af datasæt med mere sker det på baggrund af tilfældige talrækker i computeren, der styres af seeded
# for at sikre replikerbarheden af studiet, skal seed fastsættes da split mm eller vil variere hver gang, at studiet reproduceres.
set.seed(1)


#-----------------------------------------------------------------------------------
# Oprettelse af trænings- og testdatasæt 
# Jeg har valgt at lave et split hvor omtrent 80 & af observationerne bruges til at træne modellen

train_data <-  data %>%
  filter(year <= 2011)

test_data <- data %>% 
  filter(year >=2012)


#-------------------------------------
# Oprettelse af "formular" for prædiktionsmodellerne  
# Altså hvilke prædikatorer, der skal bruges til at forudsige responsvariablen

train_data <- train_data %>%
  select(-country, - year, -month, -country.x, -country.y, -cwy,-deathyear)

test_data <- test_data %>%
  select(-country, - year, -month, -country.x, -country.y,-cwy,-deathyear)




formular_outbreak <- cwstart ~  . #inkludere alle variable
formular_incidents <- incidents ~  . #inkludere alle variable
formular_deaths <- deaths ~  . #inkludere alle variable
formular_cwm <- cwm ~  . #inkludere alle variable


#-------------------------------------
# Oprettelse af fit control,
# Fitcontrol er det parametrem der sikre en looping/opdelling i træningsdataet, der fungere som en penalty for at lægge sig tæt op af observationerne
fitControl <- trainControl(method = "cv", number = 1)

# i forhold til resampling methods https://stats.stackexchange.com/questions/17602/caret-re-sampling-methods

# :

#boot - bootstrap
#boot632 -- 0.632 bootstrap
#cv -- cross-validation, probably this refers to K-fold cross-validation.
#LOOCV -- leave-one-out cross validation, also known as jacknife.
#LGOCV -- leave-group-out cross validation, variant of LOOCV for hierarchical data.
#repeatedcv -- is probably repeated random sub-sampling validation, i.e division to train and test data is done in random way.
#oob -- refers to out-of-bag estimation proposed by Breiman, which further is related to bootstrap aggregating. (The file in the link is not a ps file, but a ps.Z file, rename it and then try opening.)



##########################################
#               Linear model             #
##########################################

setwd(Models)
#model 1
lm <-  glm(formular_cwm, data = train_data, na.action = na.exclude, family=binomial(link='logit'))
#save(lm, file = "linear_model_test.rda")
summary(lm)
test_data$linear = predict(lm, newdata = test_data, type="response")
test_data$linear_res = test_data$cwm - test_data$linear

sd(na.omit(test_data$linear_res))
mean(na.omit(test_data$linear_res))

hist(test_data$linear, bins =100)

test_data %>% ggplot()



##########################################
#              randomforest              #
##########################################
#mtry: Number of variables randomly sampled as candidates at each split.


rf_m = train(formular_cw_month,
             data = train,
             method = "rf",
             trControl = fitControl,
             preProcess = c("scale"),
             search = "grid",
             linout = T,
              na.action = na.omit)
print(rf_m)
plot(rf_m)

save(xgb_m, rf_m = "randomforest.rda")


# Grid search: Each axis of the grid is an algorithm parameter, and points in the grid are specific combinations of parameters. Because we are only tuning one parameter, the grid search is a linear search through a vector of candidate values



##########################################
#       XtreameGreadientBoosting         #
##########################################

#mtry: Number of variables randomly sampled as candidates at each split.


xgb_m = train(formular_death,
                  data = train,
                  method = "xgbLinear",
                  trControl = fitControl,
                  preProcess = c("scale"),
                  search = "random", # er det rigtig forstået at hvis jeg her vælger grid så så tager den mine variable i blokke frem for at udvælge dem tilfældigt? 
                  linout = T,
                  na.action = na.exclude)
save(xgb_m, file = "xgb_model.rda")

summary(xgb_m)

test$xgb_m = predict(xgb_m, newdata = test)
test$xgb_res = test$total_deaths_month - test$xgb
mean(test$xgb_res) 
cor(test$TotalDeaths, test$xgb_res)
