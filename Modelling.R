
cat("\014")  

################################################################################################
##                                         Modelling                                          ##
################################################################################################

lets_reg_it <- lets_reg_it %>% 
  filter(continent %in% c("AS","AF","SA")) 

lets_reg_it <-  lets_reg_it %>% 
  replace(lets_reg_it$TotalDeaths, is.na(lets_reg_it$TotalDeaths), 0)


train <- lets_reg_it %>% 
  filter(year <=2012) 
test <-  lets_reg_it %>% 
  filter(year>2012, year <=2013) 

formular <- TotalDeaths ~ Num_events + tone + Goldstein + gov_debt + gov_expenditure + secondary_male_enrollment + gdp_pr_capita
formular2 <- TotalDeaths ~ Num_events * tone + Goldstein + gov_debt + gov_expenditure + secondary_male_enrollment + gdp_pr_capita

#oprettelse af fit control, der sikre en looping i træningsdataet, der gør modelleringen endnu stærkere. 
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)





##########################################
#               Linear model             #
##########################################

setwd(Models)
#model 1
lm <-  lm(formular, data = train, na.action = na.exclude)
save(lm, file = "linear_model.rda")
summary(lm)
test$linear = predict(lm, newdata = test)
test$linear_res = test$TotalDeaths - test$linear

sd(na.omit(test$linear_res))
mean(na.omit(test$linear_res))






##########################################
#              randomforest              #
##########################################

rf_m = train(formular,
             data = train,
             method = "rf",
             trControl = fitControl,
             preProcess = c("scale"),
             search = "grid",
             linout = T)
save(xgb_m, rf_m = "randomforest.rda")






##########################################
#       XtreameGreadientBoosting         #
##########################################


xgb_m = train(formular,
                  data = train,
                  method = "xgbLinear",
                  trControl = fitControl,
                  preProcess = c("scale"),
                  search = "grid",
                  linout = T)
save(xgb_m, file = "xgb_model.rda")

summary(xgb_m)

test$xgb_m = predict(xgb_m, newdata = test)
test$xgb_res = test$TotalDeaths - test$xgb
mean(test$xgb_res) 
cor(test$TotalDeaths, test$xgb_res)
