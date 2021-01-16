## Elastic net new version. 
## 30-08-2019 

## load library ----------------------------------------------------------------
library(ggplot2) #plot data
library(glmnet) #glm model
library(tidyr) #tidyverse
library(caret)
library(dplyr)

## set seed number -------------------------------------------------------------
set.seed(1)

## load dataset ----------------------------------------------------------------
working_dat <- read.csv("input/experiment_data.csv", stringsAsFactors = FALSE)
working_dat$F2017 <- as.numeric(working_dat$F2017)

## experiment 1: flow percentage change vs. other variables --------------------
## data partitioning 
# split by destination
destination_ind <- working_dat %>%
  group_by(Destination_code) %>%
  summarise(count.n = n())

set.seed(111)
ind.e1 <- sample(2, nrow(destination_ind), replace = T, prob = c(0.8, 0.2))
dest_train <- destination_ind[ind.e1==1,] #98 countries
dest_test <- destination_ind[ind.e1==2,] #22 countries

# determine train and test sets 
train_e1 <- working_dat %>% 
  filter( Destination_code %in% dest_train$Destination_code ) #7355 obs
test_e1 <- working_dat %>% 
  filter( Destination_code %in% dest_test$Destination_code ) #1711 obs 

# determine X and Y exp1 -------------------------------------------------------
# train
#TS1
trn.1.1 <- train_e1 %>%
  select(T2013, T2014, T2015, T.mean.TS1, T.med.TS1, T.max.TS1, T.min.TS1,
         T.range.TS1, T.diff.TS1, T.sd.TS1, T.var.TS1, T.sl.TS1, 
         T.pc.13.14, T.pc.14.15, F.pc.13.14, F.pc.14.15)
#TS2  
trn.1.2 <- train_e1 %>%
  select(T2014, T2015, T2016, T.mean.TS2, T.med.TS2, T.max.TS2, T.min.TS2,
         T.range.TS2, T.diff.TS2, T.sd.TS2, T.var.TS2, T.sl.TS2, 
         T.pc.14.15, T.pc.15.16, F.pc.14.15, F.pc.15.16)
#TS3     
trn.1.3 <- train_e1 %>%
  select(T2015, T2016, T2017, T.mean.TS3, T.med.TS3, T.max.TS3, T.min.TS3,
         T.range.TS3, T.diff.TS3, T.sd.TS3, T.var.TS3, T.sl.TS3, 
         T.pc.15.16, T.pc.16.17, F.pc.15.16, F.pc.16.17)

#test
#TS1
tst.1.1 <- test_e1 %>%
  select(T2013, T2014, T2015, T.mean.TS1, T.med.TS1, T.max.TS1, T.min.TS1,
         T.range.TS1, T.diff.TS1, T.sd.TS1, T.var.TS1, T.sl.TS1, 
         T.pc.13.14, T.pc.14.15, F.pc.13.14, F.pc.14.15)
#TS2  
tst.1.2 <- test_e1 %>%
  select(T2014, T2015, T2016, T.mean.TS2, T.med.TS2, T.max.TS2, T.min.TS2,
         T.range.TS2, T.diff.TS2, T.sd.TS2, T.var.TS2, T.sl.TS2, 
         T.pc.14.15, T.pc.15.16, F.pc.14.15, F.pc.15.16)
#TS3     
tst.1.3 <- test_e1 %>%
  select(T2015, T2016, T2017, T.mean.TS3, T.med.TS3, T.max.TS3, T.min.TS3,
         T.range.TS3, T.diff.TS3, T.sd.TS3, T.var.TS3, T.sl.TS3, 
         T.pc.15.16, T.pc.16.17, F.pc.15.16, F.pc.16.17)


## Elastic net implementation --------------------------------------------------
# Tune the model from the train data in Time Window 1 -> get Alpha and Lambda 
mod.ctrl <- trainControl(method = "cv", number = 10, verboseIter = T)

set.seed(122)
trn.1.1.enet <- train( F.pc.14.15 ~ .,
                      trn.1.1,
                      method = 'glmnet',
                      tuneLength = 20,
                      trControl = mod.ctrl)

#Final values of parameter
# RMSE was used to select the optimal model using the smallest value.
# The final values used for the model were alpha = 0.90526316
# and lambda = 0.007793364.

# define the best Alpha and Lambda 
bestAlpha <- tune.1.1.enet$bestTune$alpha
bestLambda <- tune.1.1.enet$bestTune$lambda

# Train and test model: experiment 1 // Time Window 1 --------------------------
set.seed(122)
trn.1.1.enet <- train( F.pc.14.15 ~ .,
                       trn.1.1,
                       method = 'glmnet',
                       tuneGrid = data.frame(alpha = bestAlpha ,  
                                             lambda = bestLambda), #set to be constant    
                       trControl = mod.ctrl)
# coefficients 
enet_coef_TS1 <- coef(trn.1.1.enet$finalModel,trn.1.1.enet$bestTune$lambda)

# obtain training error [MSE] 
MSE.trn.1.1.enet <- (trn.1.1.enet$results$RMSE)^2

# predict and obtain testing error 
pred.1.1.enet <- predict(trn.1.1.enet, tst.1.1)
SE.tst.1.1.enet <- tst.1.1$F.pc.14.15 - pred.1.1.enet
MSE.tst.1.1.enet <- mean((tst.1.1$F.pc.14.15 - pred.1.1.enet)^2) 

# Model importance
varImp(trn.1.1.enet, scale = T)

## Train and test model: experiment 1 // Time Window 2--------------------------
set.seed(122)
trn.1.2.enet <- train( F.pc.15.16 ~ .,
                        trn.1.2,
                        method = 'glmnet',
                        tuneGrid = data.frame(alpha = bestAlpha ,  
                                              lambda = bestLambda), #set to be constant    
                        trControl = mod.ctrl)

# coefficients
enet_coef_TS2 <- coef(trn.1.2.enet$finalModel,trn.1.2.enet$bestTune$lambda)

# obtain training error [MSE] 
MSE.trn.1.2.enet <- (trn.1.2.enet$results$RMSE)^2

# predict and obtain testing error 
pred.1.2.enet <- predict(trn.1.2.enet, tst.1.2)
SE.tst.1.2.enet <- tst.1.2$F.pc.15.16 - pred.1.2.enet
MSE.tst.1.2.enet <- mean((tst.1.2$F.pc.15.16 - pred.1.2.enet)^2) 

## Train and test model: experiment 1 // Time Window 3 -------------------------
set.seed(122)
trn.1.3.enet <- train( F.pc.16.17 ~ .,
                       trn.1.3,
                       method = 'glmnet',
                       tuneGrid = data.frame(alpha = bestAlpha ,  
                                             lambda = bestLambda), #set to be constant    
                       trControl = mod.ctrl)

# coefficients
enet_coef_TS3 <- coef(trn.1.3.enet$finalModel,trn.1.3.enet$bestTune$lambda)

# obtain training error [MSE] 
MSE.trn.1.3.enet <- (trn.1.3.enet$results$RMSE)^2

# predict and obtain testing error 
pred.1.3.enet <- predict(trn.1.3.enet, tst.1.3)
SE.tst.1.3.enet <- tst.1.3$F.pc.16.17 - pred.1.3.enet
MSE.tst.1.3.enet <- mean((tst.1.3$F.pc.16.17 - pred.1.3.enet)^2)

## Summarize the results of experiment 1 ---------------------------------------
ex1.error.enet <- data.frame(c('Time.Window', 'train', 'test'),
                             c('1', MSE.trn.1.1.enet, MSE.tst.1.1.enet), 
                             c('2', MSE.trn.1.2.enet, MSE.tst.1.2.enet),
                             c('3', MSE.trn.1.3.enet, MSE.tst.1.3.enet))

ex1.pred.values <- data.frame(pred.1.1.enet, pred.1.2.enet, pred.1.3.enet)

# collect error by destination 
ex1_err_dest_enet <- data.frame(test_e1$Destination_code, test_e1$Origin_code, 
                                SE.tst.1.1.enet, SE.tst.1.2.enet, SE.tst.1.3.enet) 
ex1_err_dest_enet$MSE.enet <- ((SE.tst.1.1.enet + SE.tst.1.2.enet + SE.tst.1.3.enet)^2)/3
                                
# summarize coefficients
enet_coef_Ex1 <- cbind(enet_coef_TS1, enet_coef_TS2, enet_coef_TS3)
as.data.frame(enet_coef_Ex1)
#------------------------------------------------------------------------------#
## Experiment 2: Source market conditions --------------------------------------
# determine x and y for experiment 2 -------------------------------------------
# divide into three windows  
ex2_TS1 <- working_dat %>%
  select(Destination_code, Origin_code, dest_income, org_income, 
         same_level_inc, visa_access, sub_reg_dest, sub_reg_org,
         same_region,
         T2013, T2014, T2015, T.mean.TS1, T.med.TS1, T.max.TS1, T.min.TS1,
         T.range.TS1, T.diff.TS1, T.sd.TS1, T.var.TS1, T.sl.TS1, 
         T.pc.13.14, T.pc.14.15, F.pc.13.14, F.pc.14.15)

ex2_TS2 <- working_dat %>% 
  select(Destination_code, Origin_code, dest_income, org_income, 
         same_level_inc, visa_access, sub_reg_dest, sub_reg_org,
         same_region,
         T2014, T2015, T2016, T.mean.TS2, T.med.TS2, T.max.TS2, T.min.TS2,
         T.range.TS2, T.diff.TS2, T.sd.TS2, T.var.TS2, T.sl.TS2, 
         T.pc.14.15, T.pc.15.16, F.pc.14.15, F.pc.15.16) 

ex2_TS3 <- working_dat %>%
  select(Destination_code, Origin_code, dest_income, org_income, 
         same_level_inc, visa_access, sub_reg_dest, sub_reg_org,
         same_region,
         T2015, T2016, T2017, T.mean.TS3, T.med.TS3, T.max.TS3, T.min.TS3,
         T.range.TS3, T.diff.TS3, T.sd.TS3, T.var.TS3, T.sl.TS3, 
         T.pc.15.16, T.pc.16.17, F.pc.15.16, F.pc.16.17)

# partitioning 
# Group 1: Low income
# train set: different income level of destination and origin: 917 observations 
trn.Low.ts1 <- ex2_TS1 %>%
  filter (same_level_inc == "FALSE" & dest_income == "L")
trn.Low.ts2 <- ex2_TS2 %>%
  filter (same_level_inc == "FALSE" & dest_income == "L")
trn.Low.ts3 <- ex2_TS3 %>%
  filter (same_level_inc == "FALSE" & dest_income == "L")

# test set: same income level of destination and origin: 211 observations 
tst.Low.ts1 <- ex2_TS1 %>%
  filter (same_level_inc == "TRUE" & dest_income == "L")
tst.Low.ts2 <- ex2_TS2 %>%
  filter (same_level_inc == "TRUE" & dest_income == "L")
tst.Low.ts3 <- ex2_TS3 %>%
  filter (same_level_inc == "TRUE" & dest_income == "L")

# variables definition 
# TS1 
x_trn_Low_ts1 <- trn.Low.ts1[,10:25]
x_tst_Low_ts1 <- tst.Low.ts1[,10:25]
# TS2 
x_trn_Low_ts2 <- trn.Low.ts2[,10:25]
x_tst_Low_ts2 <- tst.Low.ts2[,10:25]
# TS3 
x_trn_Low_ts3 <- trn.Low.ts3[,10:25]
x_tst_Low_ts3 <- tst.Low.ts3[,10:25]

# Group 2: Lowermiddle income
# train set: different income level of destination and origin: 1307 observations 
trn.LM.ts1 <- ex2_TS1 %>%
  filter (same_level_inc == "FALSE" & dest_income == "LM")
trn.LM.ts2 <- ex2_TS2 %>%
  filter (same_level_inc == "FALSE" & dest_income == "LM")
trn.LM.ts3 <- ex2_TS3 %>%
  filter (same_level_inc == "FALSE" & dest_income == "LM")

# test set: same income level of destination and origin: 313 observations 
tst.LM.ts1 <- ex2_TS1 %>%
  filter (same_level_inc == "TRUE" & dest_income == "LM")
tst.LM.ts2 <- ex2_TS2 %>%
  filter (same_level_inc == "TRUE" & dest_income == "LM")
tst.LM.ts3 <- ex2_TS3 %>%
  filter (same_level_inc == "TRUE" & dest_income == "LM")

# variables definition
# TS1 
x_trn_LM_ts1 <- trn.LM.ts1[,10:25]
x_tst_LM_ts1 <- tst.LM.ts1[,10:25]
# TS2 
x_trn_LM_ts2 <- trn.LM.ts2[,10:25]
x_tst_LM_ts2 <- tst.LM.ts2[,10:25]
# TS3 
x_trn_LM_ts3 <- trn.LM.ts3[,10:25]
x_tst_LM_ts3 <- tst.LM.ts3[,10:25]

# Group 3: Uppermiddle income
# train set: different income level of destination and origin: 2401 observations 
trn.UM.ts1 <- ex2_TS1 %>%
  filter (same_level_inc == "FALSE" & dest_income == "UM")
trn.UM.ts2 <- ex2_TS2 %>%
  filter (same_level_inc == "FALSE" & dest_income == "UM")
trn.UM.ts3 <- ex2_TS3 %>%
  filter (same_level_inc == "FALSE" & dest_income == "UM")

# test set: same income level of destination and origin: 858 observations 
tst.UM.ts1 <- ex2_TS1 %>%
  filter (same_level_inc == "TRUE" & dest_income == "UM")
tst.UM.ts2 <- ex2_TS2 %>%
  filter (same_level_inc == "TRUE" & dest_income == "UM")
tst.UM.ts3 <- ex2_TS3 %>%
  filter (same_level_inc == "TRUE" & dest_income == "UM")

# variables definition 
# TS1 
x_trn_UM_ts1 <- trn.UM.ts1[,10:25]
x_tst_UM_ts1 <- tst.UM.ts1[,10:25]
# TS2 
x_trn_UM_ts2 <- trn.UM.ts2[,10:25]
x_tst_UM_ts2 <- tst.UM.ts2[,10:25]
# TS3 
x_trn_UM_ts3 <- trn.UM.ts3[,10:25]
x_tst_UM_ts3 <- tst.UM.ts3[,10:25]

# Group 4: High income
# train set: different income level of destination and origin: 1793 observations 
trn.H.ts1 <- ex2_TS1 %>%
  filter (same_level_inc == "FALSE" & dest_income == "H")
trn.H.ts2 <- ex2_TS2 %>%
  filter (same_level_inc == "FALSE" & dest_income == "H")
trn.H.ts3 <- ex2_TS3 %>%
  filter (same_level_inc == "FALSE" & dest_income == "H")

# test set: same income level of destination and origin: 1296 observations 
tst.H.ts1 <- ex2_TS1 %>%
  filter (same_level_inc == "TRUE" & dest_income == "H")
tst.H.ts2 <- ex2_TS2 %>%
  filter (same_level_inc == "TRUE" & dest_income == "H")
tst.H.ts3 <- ex2_TS3 %>%
  filter (same_level_inc == "TRUE" & dest_income == "H")

# variables definition 
# TS1 
x_trn_H_ts1 <- trn.H.ts1[,10:25]
x_tst_H_ts1 <- tst.H.ts1[,10:25]
# TS2 
x_trn_H_ts2 <- trn.H.ts2[,10:25]
x_tst_H_ts2 <- tst.H.ts2[,10:25]
# TS3 
x_trn_H_ts3 <- trn.H.ts3[,10:25]
x_tst_H_ts3 <- tst.H.ts3[,10:25]

# Train and test model: Experiment 2 // Low income // TS 1 ---------------------
trn.Low.2.1.enet <- train( F.pc.14.15 ~ .,
                           x_trn_Low_ts1,
                           method = 'glmnet',
                           tuneGrid = data.frame(alpha = bestAlpha ,  
                                                 lambda = bestLambda), #set to be constant    
                           trControl = mod.ctrl)
# obtain training error [MSE] 
MSE.trn.Low.2.1.enet <- (trn.Low.2.1.enet$results$RMSE)^2

# predict and obtain testing error 
pred.Low.2.1.enet <- predict(trn.Low.2.1.enet, x_tst_Low_ts1)
MSE.tst.Low.2.1.enet <- mean((x_tst_Low_ts1$F.pc.14.15 - pred.Low.2.1.enet)^2)

# Train and test model: Experiment 2 // Low income // TS 2 ---------------------
trn.Low.2.2.enet <- train( F.pc.15.16 ~ .,
                           x_trn_Low_ts2,
                           method = 'glmnet',
                           tuneGrid = data.frame(alpha = bestAlpha ,  
                                                 lambda = bestLambda), #set to be constant    
                           trControl = mod.ctrl)

# obtain training error [MSE] 
MSE.trn.Low.2.2.enet <- (trn.Low.2.2.enet$results$RMSE)^2

# predict and obtain testing error 
pred.Low.2.2.enet <- predict(trn.Low.2.2.enet, x_tst_Low_ts2)
MSE.tst.Low.2.2.enet <- mean((x_tst_Low_ts2$F.pc.15.16 - pred.Low.2.2.enet)^2)

# Train and test model: Experiment 2 // Low income // TS 3 ---------------------
trn.Low.2.3.enet <- train( F.pc.16.17 ~ .,
                           x_trn_Low_ts3,
                           method = 'glmnet',
                           tuneGrid = data.frame(alpha = bestAlpha ,  
                                                 lambda = bestLambda), #set to be constant    
                           trControl = mod.ctrl)

# obtain training error [MSE] 
MSE.trn.Low.2.3.enet <- (trn.Low.2.3.enet$results$RMSE)^2

# predict and obtain testing error 
pred.Low.2.3.enet <- predict(trn.Low.2.3.enet, x_tst_Low_ts3)
MSE.tst.Low.2.3.enet <- mean((x_tst_Low_ts3$F.pc.16.17 - pred.Low.2.3.enet)^2)

# summarize the result of experiment 2 - low income condition ------------------
ex2.low.error.enet <- data.frame(c('Time.Window', 'train', 'test'),
                             c('1', MSE.trn.Low.2.1.enet, MSE.tst.Low.2.1.enet), 
                             c('2', MSE.trn.Low.2.2.enet, MSE.tst.Low.2.2.enet),
                             c('3', MSE.trn.Low.2.3.enet, MSE.tst.Low.2.3.enet))

ex2.low.pred.values <- data.frame(pred.Low.2.1.enet, pred.Low.2.2.enet, 
                                  pred.Low.2.3.enet)

# Train and test model: Experiment 2 // LM income // TS 1 ---------------------
trn.LM.2.1.enet <- train( F.pc.14.15 ~ .,
                          x_trn_LM_ts1,
                          method = 'glmnet',
                          tuneGrid = data.frame(alpha = bestAlpha ,  
                                                lambda = bestLambda), #set to be constant    
                          trControl = mod.ctrl)
# obtain training error [MSE] 
MSE.trn.LM.2.1.enet <- (trn.LM.2.1.enet$results$RMSE)^2

# predict and obtain testing error 
pred.LM.2.1.enet <- predict(trn.LM.2.1.enet, x_tst_LM_ts1)
MSE.tst.LM.2.1.enet <- mean((x_tst_LM_ts1$F.pc.14.15 - pred.LM.2.1.enet)^2)

# Train and test model: Experiment 2 // LM income // TS 2 ---------------------
trn.LM.2.2.enet <- train( F.pc.15.16 ~ .,
                          x_trn_LM_ts2,
                          method = 'glmnet',
                          tuneGrid = data.frame(alpha = bestAlpha ,  
                                                lambda = bestLambda), #set to be constant    
                          trControl = mod.ctrl)

# obtain training error [MSE] 
MSE.trn.LM.2.2.enet <- (trn.LM.2.2.enet$results$RMSE)^2

# predict and obtain testing error 
pred.LM.2.2.enet <- predict(trn.LM.2.2.enet, x_tst_LM_ts2)
MSE.tst.LM.2.2.enet <- mean((x_tst_LM_ts2$F.pc.15.16 - pred.LM.2.2.enet)^2)

# Train and test model: Experiment 2 // LM income // TS 3 ---------------------
trn.LM.2.3.enet <- train( F.pc.16.17 ~ .,
                          x_trn_LM_ts3,
                          method = 'glmnet',
                          tuneGrid = data.frame(alpha = bestAlpha ,  
                                                lambda = bestLambda), #set to be constant    
                          trControl = mod.ctrl)

# obtain training error [MSE] 
MSE.trn.LM.2.3.enet <- (trn.LM.2.3.enet$results$RMSE)^2

# predict and obtain testing error 
pred.LM.2.3.enet <- predict(trn.LM.2.3.enet, x_tst_LM_ts3)
MSE.tst.LM.2.3.enet <- mean((x_tst_LM_ts3$F.pc.16.17 - pred.LM.2.3.enet)^2)

# summarize the result of experiment 2 - LM income condition ------------------
ex2.LM.error.enet <- data.frame(c('Time.Window', 'train', 'test'),
                                c('1', MSE.trn.LM.2.1.enet, MSE.tst.LM.2.1.enet), 
                                c('2', MSE.trn.LM.2.2.enet, MSE.tst.LM.2.2.enet),
                                c('3', MSE.trn.LM.2.3.enet, MSE.tst.LM.2.3.enet))

ex2.LM.pred.values <- data.frame(pred.LM.2.1.enet, pred.LM.2.2.enet, 
                                 pred.LM.2.3.enet)

# Train and test model: Experiment 2 // UM income // TS 1 ---------------------
trn.UM.2.1.enet <- train( F.pc.14.15 ~ .,
                          x_trn_UM_ts1,
                          method = 'glmnet',
                          tuneGrid = data.frame(alpha = bestAlpha ,  
                                                lambda = bestLambda), #set to be constant    
                          trControl = mod.ctrl)
# obtain training error [MSE] 
MSE.trn.UM.2.1.enet <- (trn.UM.2.1.enet$results$RMSE)^2

# predict and obtain testing error 
pred.UM.2.1.enet <- predict(trn.UM.2.1.enet, x_tst_UM_ts1)
MSE.tst.UM.2.1.enet <- mean((x_tst_UM_ts1$F.pc.14.15 - pred.UM.2.1.enet)^2)

# Train and test model: Experiment 2 // UM income // TS 2 ---------------------
trn.UM.2.2.enet <- train( F.pc.15.16 ~ .,
                          x_trn_UM_ts2,
                          method = 'glmnet',
                          tuneGrid = data.frame(alpha = bestAlpha ,  
                                                lambda = bestLambda), #set to be constant    
                          trControl = mod.ctrl)

# obtain training error [MSE] 
MSE.trn.UM.2.2.enet <- (trn.UM.2.2.enet$results$RMSE)^2

# predict and obtain testing error 
pred.UM.2.2.enet <- predict(trn.UM.2.2.enet, x_tst_UM_ts2)
MSE.tst.UM.2.2.enet <- mean((x_tst_UM_ts2$F.pc.15.16 - pred.UM.2.2.enet)^2)

# Train and test model: Experiment 2 // UM income // TS 3 ---------------------
trn.UM.2.3.enet <- train( F.pc.16.17 ~ .,
                          x_trn_UM_ts3,
                          method = 'glmnet',
                          tuneGrid = data.frame(alpha = bestAlpha ,  
                                                lambda = bestLambda), #set to be constant    
                          trControl = mod.ctrl)

# obtain training error [MSE] 
MSE.trn.UM.2.3.enet <- (trn.UM.2.3.enet$results$RMSE)^2

# predict and obtain testing error 
pred.UM.2.3.enet <- predict(trn.UM.2.3.enet, x_tst_UM_ts3)
MSE.tst.UM.2.3.enet <- mean((x_tst_UM_ts3$F.pc.16.17 - pred.UM.2.3.enet)^2)

# summarize the result of experiment 2 - UM income condition ------------------
ex2.UM.error.enet <- data.frame(c('Time.Window', 'train', 'test'),
                                c('1', MSE.trn.UM.2.1.enet, MSE.tst.UM.2.1.enet), 
                                c('2', MSE.trn.UM.2.2.enet, MSE.tst.UM.2.2.enet),
                                c('3', MSE.trn.UM.2.3.enet, MSE.tst.UM.2.3.enet))

ex2.UM.pred.values <- data.frame(pred.UM.2.1.enet, pred.UM.2.2.enet, 
                                 pred.UM.2.3.enet)

# Train and test model: Experiment 2 // H income // TS 1 ---------------------
trn.H.2.1.enet <- train( F.pc.14.15 ~ .,
                         x_trn_H_ts1,
                         method = 'glmnet',
                         tuneGrid = data.frame(alpha = bestAlpha ,  
                                               lambda = bestLambda), #set to be constant    
                         trControl = mod.ctrl)
# obtain training error [MSE] 
MSE.trn.H.2.1.enet <- (trn.H.2.1.enet$results$RMSE)^2

# predict and obtain testing error 
pred.H.2.1.enet <- predict(trn.H.2.1.enet, x_tst_H_ts1)
MSE.tst.H.2.1.enet <- mean((x_tst_H_ts1$F.pc.14.15 - pred.H.2.1.enet)^2)

# Train and test model: Experiment 2 // H income // TS 2 ---------------------
trn.H.2.2.enet <- train( F.pc.15.16 ~ .,
                         x_trn_H_ts2,
                         method = 'glmnet',
                         tuneGrid = data.frame(alpha = bestAlpha ,  
                                               lambda = bestLambda), #set to be constant    
                         trControl = mod.ctrl)

# obtain training error [MSE] 
MSE.trn.H.2.2.enet <- (trn.H.2.2.enet$results$RMSE)^2

# predict and obtain testing error 
pred.H.2.2.enet <- predict(trn.H.2.2.enet, x_tst_H_ts2)
MSE.tst.H.2.2.enet <- mean((x_tst_H_ts2$F.pc.15.16 - pred.H.2.2.enet)^2)

# Train and test model: Experiment 2 // H income // TS 3 ---------------------
trn.H.2.3.enet <- train( F.pc.16.17 ~ .,
                         x_trn_H_ts3,
                         method = 'glmnet',
                         tuneGrid = data.frame(alpha = bestAlpha ,  
                                               lambda = bestLambda), #set to be constant    
                         trControl = mod.ctrl)

# obtain training error [MSE] 
MSE.trn.H.2.3.enet <- (trn.H.2.3.enet$results$RMSE)^2

# predict and obtain testing error 
pred.H.2.3.enet <- predict(trn.H.2.3.enet, x_tst_H_ts3)
MSE.tst.H.2.3.enet <- mean((x_tst_H_ts3$F.pc.16.17 - pred.H.2.3.enet)^2)

# summarize the result of experiment 2 - H income condition ------------------
ex2.H.error.enet <- data.frame(c('Time.Window', 'train', 'test'),
                               c('1', MSE.trn.H.2.1.enet, MSE.tst.H.2.1.enet), 
                               c('2', MSE.trn.H.2.2.enet, MSE.tst.H.2.2.enet),
                               c('3', MSE.trn.H.2.3.enet, MSE.tst.H.2.3.enet))

ex2.H.pred.values <- data.frame(pred.H.2.1.enet, pred.H.2.2.enet, 
                                pred.H.2.3.enet)

## Experiment 3: Host market conditions --------------------------------------
# determine x and y for experiment 3 -------------------------------------------
# partitioning 
# Group 1: Easy to access
# train set: different region of destination and origin: 2405 observations 
trn.Easy.ts1 <- ex2_TS1 %>%
  filter (same_region == "FALSE" & visa_access == "high")
trn.Easy.ts2 <- ex2_TS2 %>%
  filter (same_region == "FALSE" & visa_access == "high")
trn.Easy.ts3 <- ex2_TS3 %>%
  filter (same_region == "FALSE" & visa_access == "high")

# test set: same income level of destination and origin: 614 observations 
tst.Easy.ts1 <- ex2_TS1 %>%
  filter (same_region == "TRUE" & visa_access == "high")
tst.Easy.ts2 <- ex2_TS2 %>%
  filter (same_region == "TRUE" & visa_access == "high")
tst.Easy.ts3 <- ex2_TS3 %>%
  filter (same_region == "TRUE" & visa_access == "high")

# TS1 
x_trn_Easy_ts1 <- trn.Easy.ts1[,10:25]
x_tst_Easy_ts1 <- tst.Easy.ts1[,10:25]
# TS2 
x_trn_Easy_ts2 <- trn.Easy.ts2[,10:25]
x_tst_Easy_ts2 <- tst.Easy.ts2[,10:25]
# TS3 
x_trn_Easy_ts3 <- trn.Easy.ts3[,10:25]
x_tst_Easy_ts3 <- tst.Easy.ts3[,10:25]

# Group 2: Moderate 
# train set: different region of destination and origin: 1173 observations 
trn.moderate.ts1 <- ex2_TS1 %>%
  filter (same_region == "FALSE" & visa_access == "moderate")
trn.moderate.ts2 <- ex2_TS2 %>%
  filter (same_region == "FALSE" & visa_access == "moderate")
trn.moderate.ts3 <- ex2_TS3 %>%
  filter (same_region == "FALSE" & visa_access == "moderate")

# test set: same income level of destination and origin: 257 observations 
tst.moderate.ts1 <- ex2_TS1 %>%
  filter (same_region == "TRUE" & visa_access == "moderate")
tst.moderate.ts2 <- ex2_TS2 %>%
  filter (same_region == "TRUE" & visa_access == "moderate")
tst.moderate.ts3 <- ex2_TS3 %>%
  filter (same_region == "TRUE" & visa_access == "moderate")

# TS1 
x_trn_moderate_ts1 <- trn.moderate.ts1[,10:25]
x_tst_moderate_ts1 <- tst.moderate.ts1[,10:25]
# TS2 
x_trn_moderate_ts2 <- trn.moderate.ts2[,10:25]
x_tst_moderate_ts2 <- tst.moderate.ts2[,10:25]
# TS3 
x_trn_moderate_ts3 <- trn.moderate.ts3[,10:25]
x_tst_moderate_ts3 <- tst.moderate.ts3[,10:25]

# Group 3: Difficult
# train set: different region of destination and origin: 1759 observations 
trn.difficult.ts1 <- ex2_TS1 %>%
  filter (same_region == "FALSE" & visa_access == "low")
trn.difficult.ts2 <- ex2_TS2 %>%
  filter (same_region == "FALSE" & visa_access == "low")
trn.difficult.ts3 <- ex2_TS3 %>%
  filter (same_region == "FALSE" & visa_access == "low")

# test set: same income level of destination and origin: 437 observations 
tst.difficult.ts1 <- ex2_TS1 %>%
  filter (same_region == "TRUE" & visa_access == "low")
tst.difficult.ts2 <- ex2_TS2 %>%
  filter (same_region == "TRUE" & visa_access == "low")
tst.difficult.ts3 <- ex2_TS3 %>%
  filter (same_region == "TRUE" & visa_access == "low")

# TS1 
x_trn_difficult_ts1 <- trn.difficult.ts1[,10:25]
x_tst_difficult_ts1 <- tst.difficult.ts1[,10:25]
# TS2 
x_trn_difficult_ts2 <- trn.difficult.ts2[,10:25]
x_tst_difficult_ts2 <- tst.difficult.ts2[,10:25]
# TS3 
x_trn_difficult_ts3 <- trn.difficult.ts3[,10:25]
x_tst_difficult_ts3 <- tst.difficult.ts3[,10:25]

# Group 4: Very difficult 
# train set: different region of destination and origin: 2104 observations 
trn.Vdifficult.ts1 <- ex2_TS1 %>%
  filter (same_region == "FALSE" & visa_access == "very low")
trn.Vdifficult.ts2 <- ex2_TS2 %>%
  filter (same_region == "FALSE" & visa_access == "very low")
trn.Vdifficult.ts3 <- ex2_TS3 %>%
  filter (same_region == "FALSE" & visa_access == "very low")

# test set: same income level of destination and origin: 218 observations 
tst.Vdifficult.ts1 <- ex2_TS1 %>%
  filter (same_region == "TRUE" & visa_access == "very low")
tst.Vdifficult.ts2 <- ex2_TS2 %>%
  filter (same_region == "TRUE" & visa_access == "very low")
tst.Vdifficult.ts3 <- ex2_TS3 %>%
  filter (same_region == "TRUE" & visa_access == "very low")

# TS1 
x_trn_vdifficult_ts1 <- trn.Vdifficult.ts1[,10:25]
x_tst_vdifficult_ts1 <- tst.Vdifficult.ts1[,10:25]
# TS2 
x_trn_vdifficult_ts2 <- trn.Vdifficult.ts2[,10:25]
x_tst_vdifficult_ts2 <- tst.Vdifficult.ts2[,10:25]
# TS3 
x_trn_vdifficult_ts3 <- trn.Vdifficult.ts3[,10:25]
x_tst_vdifficult_ts3 <- tst.Vdifficult.ts3[,10:25]

# extract list of dyad 
country.easy <- data.frame(tst.Easy.ts1$Destination_code, tst.Easy.ts1$Origin_code)
country.moderate <- data.frame(tst.moderate.ts1$Destination_code, tst.moderate.ts1$Origin_code)
country.difficult <- data.frame(tst.difficult.ts1$Destination_code, tst.difficult.ts1$Origin_code)
country.vdifficult <- data.frame(tst.Vdifficult.ts1$Destination_code, tst.Vdifficult.ts1$Origin_code)

# Train and test model: Experiment 3 // easy access // TS 1 ---------------------
trn.easy.3.1.enet <- train( F.pc.14.15 ~ .,
                            x_trn_Easy_ts1,
                            method = 'glmnet',
                            tuneGrid = data.frame(alpha = bestAlpha ,  
                                                  lambda = bestLambda), #set to be constant    
                            trControl = mod.ctrl)
# obtain training error [MSE] 
MSE.trn.easy.3.1.enet <- (trn.easy.3.1.enet$results$RMSE)^2

# predict and obtain testing error 
pred.easy.3.1.enet <- predict(trn.easy.3.1.enet, x_tst_Easy_ts1)
MSE.tst.easy.3.1.enet <- mean((x_tst_Easy_ts1$F.pc.14.15 - pred.easy.3.1.enet)^2)

# Train and test model: Experiment 3 // easy access // TS 2 ---------------------
trn.easy.3.2.enet <- train( F.pc.15.16 ~ .,
                            x_trn_Easy_ts2,
                            method = 'glmnet',
                            tuneGrid = data.frame(alpha = bestAlpha ,  
                                                  lambda = bestLambda), #set to be constant    
                            trControl = mod.ctrl)

# obtain training error [MSE] 
MSE.trn.easy.3.2.enet <- (trn.easy.3.2.enet$results$RMSE)^2

# predict and obtain testing error 
pred.easy.3.2.enet <- predict(trn.easy.3.2.enet, x_tst_Easy_ts2)
MSE.tst.easy.3.2.enet <- mean((x_tst_Easy_ts2$F.pc.15.16 - pred.easy.3.2.enet)^2)

# Train and test model: Experiment 3 // easy access // TS 3 ---------------------
trn.easy.3.3.enet <- train( F.pc.16.17 ~ .,
                            x_trn_Easy_ts3,
                            method = 'glmnet',
                            tuneGrid = data.frame(alpha = bestAlpha ,  
                                                  lambda = bestLambda), #set to be constant    
                            trControl = mod.ctrl)

# obtain training error [MSE] 
MSE.trn.easy.3.3.enet <- (trn.easy.3.3.enet$results$RMSE)^2

# predict and obtain testing error 
pred.easy.3.3.enet <- predict(trn.easy.3.3.enet, x_tst_Easy_ts3)
MSE.tst.easy.3.3.enet <- mean((x_tst_Easy_ts3$F.pc.16.17 - pred.easy.3.3.enet)^2)

# summarize the result of experiment 3 - easy condition ------------------
ex3.easy.error.enet <- data.frame(c('Time.Window', 'train', 'test'),
                                  c('1', MSE.trn.easy.3.1.enet, MSE.tst.easy.3.1.enet), 
                                  c('2', MSE.trn.easy.3.2.enet, MSE.tst.easy.3.2.enet),
                                  c('3', MSE.trn.easy.3.3.enet, MSE.tst.easy.3.3.enet))

ex3.easy.pred.values <- data.frame(pred.easy.3.1.enet, pred.easy.3.2.enet, 
                                   pred.easy.3.3.enet)

# Train and test model: Experiment 3 // moderate access // TS 1 ---------------------
trn.moderate.3.1.enet <- train( F.pc.14.15 ~ .,
                                x_trn_moderate_ts1,
                                method = 'glmnet',
                                tuneGrid = data.frame(alpha = bestAlpha ,  
                                                      lambda = bestLambda), #set to be constant    
                                trControl = mod.ctrl)
# obtain training error [MSE] 
MSE.trn.moderate.3.1.enet <- (trn.moderate.3.1.enet$results$RMSE)^2

# predict and obtain testing error 
pred.moderate.3.1.enet <- predict(trn.moderate.3.1.enet, x_tst_moderate_ts1)
MSE.tst.moderate.3.1.enet <- mean((x_tst_moderate_ts1$F.pc.14.15 - pred.moderate.3.1.enet)^2)

# Train and test model: Experiment 3 // moderate access // TS 2 ---------------------
trn.moderate.3.2.enet <- train( F.pc.15.16 ~ .,
                                x_trn_moderate_ts2,
                                method = 'glmnet',
                                tuneGrid = data.frame(alpha = bestAlpha ,  
                                                      lambda = bestLambda), #set to be constant    
                                trControl = mod.ctrl)

# obtain training error [MSE] 
MSE.trn.moderate.3.2.enet <- (trn.moderate.3.2.enet$results$RMSE)^2

# predict and obtain testing error 
pred.moderate.3.2.enet <- predict(trn.moderate.3.2.enet, x_tst_moderate_ts2)
MSE.tst.moderate.3.2.enet <- mean((x_tst_moderate_ts2$F.pc.15.16 - pred.moderate.3.2.enet)^2)

# Train and test model: Experiment 3 // moderate access // TS 3 ---------------------
trn.moderate.3.3.enet <- train( F.pc.16.17 ~ .,
                                x_trn_moderate_ts3,
                                method = 'glmnet',
                                tuneGrid = data.frame(alpha = bestAlpha ,  
                                                      lambda = bestLambda), #set to be constant    
                                trControl = mod.ctrl)

# obtain training error [MSE] 
MSE.trn.moderate.3.3.enet <- (trn.moderate.3.3.enet$results$RMSE)^2

# predict and obtain testing error 
pred.moderate.3.3.enet <- predict(trn.moderate.3.3.enet, x_tst_moderate_ts3)
MSE.tst.moderate.3.3.enet <- mean((x_tst_moderate_ts3$F.pc.16.17 - pred.moderate.3.3.enet)^2)

# summarize the result of experiment 3 - moderate condition ------------------
ex3.moderate.error.enet <- data.frame(c('Time.Window', 'train', 'test'),
                                  c('1', MSE.trn.moderate.3.1.enet, MSE.tst.moderate.3.1.enet), 
                                  c('2', MSE.trn.moderate.3.2.enet, MSE.tst.moderate.3.2.enet),
                                  c('3', MSE.trn.moderate.3.3.enet, MSE.tst.moderate.3.3.enet))

ex3.moderate.pred.values <- data.frame(pred.moderate.3.1.enet, pred.moderate.3.2.enet, 
                                   pred.moderate.3.3.enet)

#Train and test model: Experiment 3 // difficult access // TS 1 ---------------------
trn.difficult.3.1.enet <- train( F.pc.14.15 ~ .,
                                   x_trn_difficult_ts1,
                                   method = 'glmnet',
                                   tuneGrid = data.frame(alpha = bestAlpha ,  
                                                         lambda = bestLambda), #set to be constant    
                                   trControl = mod.ctrl)
# obtain training error [MSE] 
MSE.trn.difficult.3.1.enet <- (trn.difficult.3.1.enet$results$RMSE)^2

# predict and obtain testing error 
pred.difficult.3.1.enet <- predict(trn.difficult.3.1.enet, x_tst_difficult_ts1)
MSE.tst.difficult.3.1.enet <- mean((x_tst_difficult_ts1$F.pc.14.15 - pred.difficult.3.1.enet)^2)

# Train and test model: Experiment 3 // difficult access // TS 2 ---------------------
trn.difficult.3.2.enet <- train( F.pc.15.16 ~ .,
                                 x_trn_difficult_ts2,
                                 method = 'glmnet',
                                 tuneGrid = data.frame(alpha = bestAlpha ,  
                                                       lambda = bestLambda), #set to be constant    
                                 trControl = mod.ctrl)

# obtain training error [MSE] 
MSE.trn.difficult.3.2.enet <- (trn.difficult.3.2.enet$results$RMSE)^2

# predict and obtain testing error 
pred.difficult.3.2.enet <- predict(trn.difficult.3.2.enet, x_tst_difficult_ts2)
MSE.tst.difficult.3.2.enet <- mean((x_tst_difficult_ts2$F.pc.15.16 - pred.difficult.3.2.enet)^2)

# Train and test model: Experiment 3 // difficult access // TS 3 ---------------------
trn.difficult.3.3.enet <- train( F.pc.16.17 ~ .,
                                 x_trn_difficult_ts3,
                                 method = 'glmnet',
                                 tuneGrid = data.frame(alpha = bestAlpha ,  
                                                       lambda = bestLambda), #set to be constant    
                                 trControl = mod.ctrl)

# obtain training error [MSE] 
MSE.trn.difficult.3.3.enet <- (trn.difficult.3.3.enet$results$RMSE)^2

# predict and obtain testing error 
pred.difficult.3.3.enet <- predict(trn.difficult.3.3.enet, x_tst_difficult_ts3)
MSE.tst.difficult.3.3.enet <- mean((x_tst_difficult_ts3$F.pc.16.17 - pred.difficult.3.3.enet)^2)

# summarize the result of experiment 3 - difficult condition ------------------
ex3.difficult.error.enet <- data.frame(c('Time.Window', 'train', 'test'),
                                      c('1', MSE.trn.difficult.3.1.enet, MSE.tst.difficult.3.1.enet), 
                                      c('2', MSE.trn.difficult.3.2.enet, MSE.tst.difficult.3.2.enet),
                                      c('3', MSE.trn.difficult.3.3.enet, MSE.tst.difficult.3.3.enet))

ex3.difficult.pred.values <- data.frame(pred.difficult.3.1.enet, pred.difficult.3.2.enet, 
                                       pred.difficult.3.3.enet)

# Train and test model: Experiment 3 // vdifficult access // TS 1 ---------------------
trn.vdifficult.3.1.enet <- train( F.pc.14.15 ~ .,
                                  x_trn_vdifficult_ts1,
                                  method = 'glmnet',
                                  tuneGrid = data.frame(alpha = bestAlpha ,  
                                                        lambda = bestLambda), #set to be constant    
                                  trControl = mod.ctrl)
# obtain training error [MSE] 
MSE.trn.vdifficult.3.1.enet <- (trn.vdifficult.3.1.enet$results$RMSE)^2

# predict and obtain testing error 
pred.vdifficult.3.1.enet <- predict(trn.vdifficult.3.1.enet, x_tst_vdifficult_ts1)
MSE.tst.vdifficult.3.1.enet <- mean((x_tst_vdifficult_ts1$F.pc.14.15 - pred.vdifficult.3.1.enet)^2)

# Train and test model: Experiment 3 // vdifficult access // TS 2 ---------------------
trn.vdifficult.3.2.enet <- train( F.pc.15.16 ~ .,
                                  x_trn_vdifficult_ts2,
                                  method = 'glmnet',
                                  tuneGrid = data.frame(alpha = bestAlpha ,  
                                                        lambda = bestLambda), #set to be constant    
                                  trControl = mod.ctrl)

# obtain training error [MSE] 
MSE.trn.vdifficult.3.2.enet <- (trn.vdifficult.3.2.enet$results$RMSE)^2

# predict and obtain testing error 
pred.vdifficult.3.2.enet <- predict(trn.vdifficult.3.2.enet, x_tst_vdifficult_ts2)
MSE.tst.vdifficult.3.2.enet <- mean((x_tst_vdifficult_ts2$F.pc.15.16 - pred.vdifficult.3.2.enet)^2)

# Train and test model: Experiment 3 // vdifficult access // TS 3 ---------------------
trn.vdifficult.3.3.enet <- train( F.pc.16.17 ~ .,
                                  x_trn_vdifficult_ts3,
                                  method = 'glmnet',
                                  tuneGrid = data.frame(alpha = bestAlpha ,  
                                                        lambda = bestLambda), #set to be constant    
                                  trControl = mod.ctrl)

# obtain training error [MSE] 
MSE.trn.vdifficult.3.3.enet <- (trn.vdifficult.3.3.enet$results$RMSE)^2

# predict and obtain testing error 
pred.vdifficult.3.3.enet <- predict(trn.vdifficult.3.3.enet, x_tst_vdifficult_ts3)
MSE.tst.vdifficult.3.3.enet <- mean((x_tst_vdifficult_ts3$F.pc.16.17 - pred.vdifficult.3.3.enet)^2)

# summarize the result of experiment 3 - vdifficult condition ------------------
ex3.vdifficult.error.enet <- data.frame(c('Time.Window', 'train', 'test'),
                                        c('1', MSE.trn.vdifficult.3.1.enet, MSE.tst.vdifficult.3.1.enet), 
                                        c('2', MSE.trn.vdifficult.3.2.enet, MSE.tst.vdifficult.3.2.enet),
                                        c('3', MSE.trn.vdifficult.3.3.enet, MSE.tst.vdifficult.3.3.enet))

ex3.vdifficult.pred.values <- data.frame(pred.vdifficult.3.1.enet, pred.vdifficult.3.2.enet, 
                                         pred.vdifficult.3.3.enet)
