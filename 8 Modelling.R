
cat("\014")  

################################################################################################
##                                         Modelling                                          ##
################################################################################################

lets_reg_it <- DataSet_sub

train <- lets_reg_it %>% 
  filter(year <=2016) 
test <-  lets_reg_it %>% 
  filter(year>2016, year <=2018) 


formular_death <- total_deaths_month ~ q1nm + q1at + q1gs + q1cnt + q2nm + q2at + q2gs + q2cnt + q3nm + q3at + q3gs + q3cnt + q4nm + q4at + q4gs + q4cnt +gov_debt + gov_expenditure + secondary_male_enrollment + gdp_pr_capita + country
formular_cw_year <- civilwar ~ q1nm + q1at + q1gs + q1cnt + q2nm + q2at + q2gs + q2cnt + q3nm + q3at + q3gs + q3cnt + q4nm + q4at + q4gs + q4cnt +gov_debt + gov_expenditure + secondary_male_enrollment + gdp_pr_capita + country
formular_cw_month <- civilwar_month ~ q1nm + q1at + q1gs + q1cnt + q2nm + q2at + q2gs + q2cnt + q3nm + q3at + q3gs + q3cnt + q4nm + q4at + q4gs + q4cnt +gov_debt + gov_expenditure + secondary_male_enrollment + gdp_pr_capita + country

#oprettelse af fit control, der sikre en looping i træningsdataet, der gør modelleringen endnu stærkere. 
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)





##########################################
#               Linear model             #
##########################################

setwd(Models)
#model 1
lm <-  lm(formular_cw_year, data = train, na.action = na.exclude)
save(lm, file = "linear_model_test.rda")
summary(lm)
test$linear = predict(lm, newdata = test)
test$linear_res = test$total_deaths_month - test$linear

sd(na.omit(test$linear_res))
mean(na.omit(test$linear_res))






##########################################
#              randomforest              #
##########################################

rf_m = train(formular_cw_month,
             data = train,
             method = "rf",
             trControl = fitControl,
             preProcess = c("scale"),
             search = "grid",
             linout = T,
              na.action = na.omit)
save(xgb_m, rf_m = "randomforest.rda")






##########################################
#       XtreameGreadientBoosting         #
##########################################


xgb_m = train(formular_death,
                  data = train,
                  method = "xgbLinear",
                  trControl = fitControl,
                  preProcess = c("scale"),
                  search = "grid",
                  linout = T,
                  na.action = na.exclude)
save(xgb_m, file = "xgb_model.rda")

summary(xgb_m)

test$xgb_m = predict(xgb_m, newdata = test)
test$xgb_res = test$total_deaths_month - test$xgb
mean(test$xgb_res) 
cor(test$TotalDeaths, test$xgb_res)
