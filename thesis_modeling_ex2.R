## modeling script experiment 2
## Natanop Pimonsathian 

## load packages ---------------------------------------------------------------
library(ggplot2) #plot data
library(glmnet) #glm model
library(tidyr) #tidyverse
library(caret)
library(dplyr)
require(randomForest) #rf
library(e1071) #SVR

set.seed(1)

## load dataset ----------------------------------------------------------------
data <- read.csv("input/experiment_data.csv", stringsAsFactors = FALSE)
  # 9066 lines at the begining 
data$F2017 <- as.numeric(data$F2017)

# divide into windows 1, 2, 3 
data_TS1 <- data %>%
            select(Destination_code, Origin_code, dest_income, org_income, 
                   same_level_inc, visa_access, sub_reg_dest, sub_reg_org,
                   same_region,
                   T2013, T2014, T2015, T.mean.TS1, T.med.TS1, T.max.TS1, T.min.TS1,
                   T.range.TS1, T.diff.TS1, T.sd.TS1, T.var.TS1, T.sl.TS1, 
                   T.pc.13.14, T.pc.14.15, F.pc.13.14, F.pc.14.15)

data_TS2 <- data %>% 
            select(Destination_code, Origin_code, dest_income, org_income, 
                   same_level_inc, visa_access, sub_reg_dest, sub_reg_org,
                   same_region,
                   T2014, T2015, T2016, T.mean.TS2, T.med.TS2, T.max.TS2, T.min.TS2,
                   T.range.TS2, T.diff.TS2, T.sd.TS2, T.var.TS2, T.sl.TS2, 
                   T.pc.14.15, T.pc.15.16, F.pc.14.15, F.pc.15.16) 

data_TS3 <- data %>%
            select(Destination_code, Origin_code, dest_income, org_income, 
                   same_level_inc, visa_access, sub_reg_dest, sub_reg_org,
                   same_region,
                   T2015, T2016, T2017, T.mean.TS3, T.med.TS3, T.max.TS3, T.min.TS3,
                   T.range.TS3, T.diff.TS3, T.sd.TS3, T.var.TS3, T.sl.TS3, 
                   T.pc.15.16, T.pc.16.17, F.pc.15.16, F.pc.16.17)

## Experiment 2: The predictive power on the different income level ------------

# partitioning 
# Group 1: Low income
  # train set: different income level of destination and origin: 917 observations 
  trn.Low.ts1 <- data_TS1 %>%
    filter (same_level_inc == "FALSE" & dest_income == "L")

  trn.Low.ts2 <- data_TS2 %>%
    filter (same_level_inc == "FALSE" & dest_income == "L")

  trn.Low.ts3 <- data_TS3 %>%
    filter (same_level_inc == "FALSE" & dest_income == "L")

  # test set: same income level of destination and origin: 211 observations 
  tst.Low.ts1 <- data_TS1 %>%
    filter (same_level_inc == "TRUE" & dest_income == "L")
  
  tst.Low.ts2 <- data_TS2 %>%
    filter (same_level_inc == "TRUE" & dest_income == "L")
  
  tst.Low.ts3 <- data_TS3 %>%
    filter (same_level_inc == "TRUE" & dest_income == "L")

# Group 2: Lowermiddle income
  # train set: different income level of destination and origin: 1307 observations 
  trn.LM.ts1 <- data_TS1 %>%
    filter (same_level_inc == "FALSE" & dest_income == "LM")
  
  trn.LM.ts2 <- data_TS2 %>%
    filter (same_level_inc == "FALSE" & dest_income == "LM")
  
  trn.LM.ts3 <- data_TS3 %>%
    filter (same_level_inc == "FALSE" & dest_income == "LM")
  
  # test set: same income level of destination and origin: 313observations 
  tst.LM.ts1 <- data_TS1 %>%
    filter (same_level_inc == "TRUE" & dest_income == "LM")
  
  tst.LM.ts2 <- data_TS2 %>%
    filter (same_level_inc == "TRUE" & dest_income == "LM")
  
  tst.LM.ts3 <- data_TS3 %>%
    filter (same_level_inc == "TRUE" & dest_income == "LM")
  
# Group 3: Uppermiddle income
  # train set: different income level of destination and origin: 2401 observations 
  trn.UM.ts1 <- data_TS1 %>%
    filter (same_level_inc == "FALSE" & dest_income == "UM")
  
  trn.UM.ts2 <- data_TS2 %>%
    filter (same_level_inc == "FALSE" & dest_income == "UM")
  
  trn.UM.ts3 <- data_TS3 %>%
    filter (same_level_inc == "FALSE" & dest_income == "UM")
  
  # test set: same income level of destination and origin: 858 observations 
  tst.UM.ts1 <- data_TS1 %>%
    filter (same_level_inc == "TRUE" & dest_income == "UM")
  
  tst.UM.ts2 <- data_TS2 %>%
    filter (same_level_inc == "TRUE" & dest_income == "UM")
  
  tst.UM.ts3 <- data_TS3 %>%
    filter (same_level_inc == "TRUE" & dest_income == "UM")

# Group 4: High income
  # train set: different income level of destination and origin: 1793 observations 
  trn.H.ts1 <- data_TS1 %>%
    filter (same_level_inc == "FALSE" & dest_income == "H")
  
  trn.H.ts2 <- data_TS2 %>%
    filter (same_level_inc == "FALSE" & dest_income == "H")
  
  trn.H.ts3 <- data_TS3 %>%
    filter (same_level_inc == "FALSE" & dest_income == "H")
  
  # test set: same income level of destination and origin: 1296 observations 
  tst.H.ts1 <- data_TS1 %>%
    filter (same_level_inc == "TRUE" & dest_income == "H")
  
  tst.H.ts2 <- data_TS2 %>%
    filter (same_level_inc == "TRUE" & dest_income == "H")
  
  tst.H.ts3 <- data_TS3 %>%
    filter (same_level_inc == "TRUE" & dest_income == "H")
  
## modelling: LOW income -------------------------------------------------------
# Baseline: Low ---------------------------------------------------------------- 
  # rate of change at year t is predicted by year t-1 
  
  # TS1 
  # train
  y.trn.Low.ts1 <- trn.Low.ts1$F.pc.13.14
  # calculate MSE
  mse.y.trn.Low.ts1 <- mean((trn.Low.ts1$F.pc.14.15 - y.trn.Low.ts1 )^2)
  
  # test 
  y.tst.Low.ts1 <- tst.Low.ts1$F.pc.13.14
  # calculate MSE
  mse.y.tst.Low.ts1 <- mean((tst.Low.ts1$F.pc.14.15 - y.tst.Low.ts1 )^2)
  
  # TS2 
  # train
  y.trn.Low.ts2 <- trn.Low.ts2$F.pc.14.15
  # calculate MSE
  mse.y.trn.Low.ts2 <- mean((trn.Low.ts2$F.pc.15.16 - y.trn.Low.ts2 )^2)
  
  # test 
  y.tst.Low.ts2 <- tst.Low.ts2$F.pc.14.15
  # calculate MSE
  mse.y.tst.Low.ts2 <- mean((tst.Low.ts2$F.pc.15.16 - y.tst.Low.ts2 )^2)
  
  # TS3 
  # train
  # train
  y.trn.Low.ts3 <- trn.Low.ts3$F.pc.15.16
  # calculate MSE
  mse.y.trn.Low.ts3 <- mean((trn.Low.ts3$F.pc.16.17 - y.trn.Low.ts3 )^2)
  
  # test 
  y.tst.Low.ts3 <- tst.Low.ts3$F.pc.15.16
  # calculate MSE
  mse.y.tst.Low.ts3 <- mean((tst.Low.ts3$F.pc.16.17 - y.tst.Low.ts3 )^2)
 
  
  base_MSE_trn_Low <- c(mse.y.trn.Low.ts1, mse.y.trn.Low.ts2, mse.y.trn.Low.ts3)
  base_MSE_tst_Low <- c(mse.y.tst.Low.ts1, mse.y.tst.Low.ts2, mse.y.tst.Low.ts3)
  
  e2_error <- data.frame(base_MSE_trn_Low, base_MSE_tst_Low)

  
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
  
## Elastic net Low -------------------------------------------------------------
  mod.ctrl <- trainControl(method = "repeatedcv", number = 10, verboseIter = T)
  
  # TS 1 
  # Train 
  trn.Low.ts1.Enet <- train( F.pc.14.15 ~ .,
                       x_trn_Low_ts1,
                       method = 'glmnet',
                       tuneGrid = expand.grid(alpha = 0.5,
                                              lambda = seq(0.00001, 10000, length = 1000)),
                       trControl = mod.ctrl)
  
  mse.trn.Low.ts1.Enet <- min((trn.Low.ts1.Enet$results$RMSE)^2)
  
  # test
  pred.Low.ts1.Enet <- predict(trn.Low.ts1.Enet, x_tst_Low_ts1)
  mse.tst.Low.ts1.Enet <- mean((x_tst_Low_ts1$F.pc.14.15 - pred.Low.ts1.Enet)^2)
  
  # TS 2 
  # Train 
  trn.Low.ts2.Enet <- train( F.pc.15.16 ~ .,
                             x_trn_Low_ts2,
                             method = 'glmnet',
                             tuneGrid = expand.grid(alpha = 0.5,
                                                    lambda = seq(0.00001, 10000, length = 1000)),
                             trControl = mod.ctrl)
  
  mse.trn.Low.ts2.Enet <- min((trn.Low.ts2.Enet$results$RMSE)^2)
  
  # test
  pred.Low.ts2.Enet <- predict(trn.Low.ts2.Enet, x_tst_Low_ts2)
  mse.tst.Low.ts2.Enet <- mean((x_tst_Low_ts2$F.pc.15.16 - pred.Low.ts2.Enet)^2)
  
  # TS 3 
  # Train 
  trn.Low.ts3.Enet <- train( F.pc.16.17 ~ .,
                             x_trn_Low_ts3,
                             method = 'glmnet',
                             tuneGrid = expand.grid(alpha = 0.5,
                                                    lambda = seq(0.00001, 10000, length = 1000)),
                             trControl = mod.ctrl)
  
  mse.trn.Low.ts3.Enet <- min((trn.Low.ts3.Enet$results$RMSE)^2)
  
  # test
  pred.Low.ts3.Enet <- predict(trn.Low.ts3.Enet, x_tst_Low_ts3)
  mse.tst.Low.ts3.Enet <- mean((x_tst_Low_ts3$F.pc.16.17 - pred.Low.ts3.Enet)^2)
  
  # summarize error 
  enet_MSE_trn_Low <- c(mse.trn.Low.ts1.Enet, mse.trn.Low.ts2.Enet, mse.trn.Low.ts3.Enet)
  enet_MSE_tst_Low <- c(mse.tst.Low.ts1.Enet, mse.tst.Low.ts2.Enet, mse.tst.Low.ts3.Enet)
  
  e2_error <- data.frame(e2_error, enet_MSE_trn_Low, enet_MSE_tst_Low )
  
## Random Forest: Low ----------------------------------------------------------
  # TS1
  # train
  trn.Low.ts1.RF = randomForest(F.pc.14.15 ~ . , data = x_trn_Low_ts1, ntree = 50, 
                            cv.fold = 10)
  mse.trn.Low.ts1.RF <- min(trn.Low.ts1.RF$mse)
  
  # predict
  pred.Low.ts1.RF <- predict(trn.Low.ts1.RF, x_tst_Low_ts1)
  mse.tst.Low.ts1.RF <- mean((x_tst_Low_ts1$F.pc.14.15 - pred.Low.ts1.RF)^2)
  
  # TS2
  # train
  trn.Low.ts2.RF = randomForest(F.pc.15.16 ~ . , data = x_trn_Low_ts2, ntree = 50, 
                                cv.fold = 10)
  mse.trn.Low.ts2.RF <- min(trn.Low.ts2.RF$mse)
  
  # predict
  pred.Low.ts2.RF <- predict(trn.Low.ts2.RF, x_tst_Low_ts2)
  mse.tst.Low.ts2.RF <- mean((x_tst_Low_ts2$F.pc.15.16 - pred.Low.ts2.RF)^2)
  
  # TS3
  # train
  trn.Low.ts3.RF = randomForest(F.pc.16.17 ~ . , data = x_trn_Low_ts3, ntree = 50, 
                                cv.fold = 10)
  mse.trn.Low.ts3.RF <- min(trn.Low.ts3.RF$mse)
  
  # predict
  pred.Low.ts3.RF <- predict(trn.Low.ts3.RF, x_tst_Low_ts3)
  mse.tst.Low.ts3.RF <- mean((x_tst_Low_ts3$F.pc.16.17 - pred.Low.ts3.RF)^2)
  
  # summarize error 
  RF_MSE_trn_Low <- c(mse.trn.Low.ts1.RF, mse.trn.Low.ts2.RF, mse.trn.Low.ts3.RF)
  RF_MSE_tst_Low <- c(mse.tst.Low.ts1.RF, mse.tst.Low.ts2.RF, mse.tst.Low.ts3.RF)
  
  e2_error <- data.frame(e2_error, RF_MSE_trn_Low, RF_MSE_tst_Low )
  
## SVM: Low --------------------------------------------------------------------
  # TS1 
  # train
  trn.Low.ts1.SVM <- svm(F.pc.14.15 ~ . , data = x_trn_Low_ts1, kernel = "radial", 
                     cost = 1, gamma = 0.1, epsilon = 0.1, cv.fold = 10)
  
  mse.trn.Low.ts1.SVM <- sqrt(mean(trn.Low.ts1.SVM $residuals^2))
  
  # test
  pred.trn.Low.ts1.SVM <- predict(trn.Low.ts1.SVM, x_tst_Low_ts1)
  mse.tst.Low.ts1.SVM <- mean((x_tst_Low_ts1$F.pc.14.15 - pred.trn.Low.ts1.SVM )^2)
  
  # TS2 
  # train
  trn.Low.ts2.SVM <- svm(F.pc.15.16 ~ . , data = x_trn_Low_ts2, kernel = "radial", 
                         cost = 1, gamma = 0.1, epsilon = 0.1, cv.fold = 10)
  
  mse.trn.Low.ts2.SVM <- sqrt(mean(trn.Low.ts2.SVM $residuals^2))
  
  # test
  pred.trn.Low.ts2.SVM <- predict(trn.Low.ts2.SVM, x_tst_Low_ts2)
  mse.tst.Low.ts2.SVM <- mean((x_tst_Low_ts2$F.pc.15.16 - pred.trn.Low.ts2.SVM )^2)
  
  # TS3 
  # train
  trn.Low.ts3.SVM <- svm(F.pc.16.17 ~ . , data = x_trn_Low_ts3, kernel = "radial", 
                         cost = 1, gamma = 0.1, epsilon = 0.1, cv.fold = 10)
  
  mse.trn.Low.ts3.SVM <- sqrt(mean(trn.Low.ts3.SVM $residuals^2))
  
  # test
  pred.trn.Low.ts3.SVM <- predict(trn.Low.ts3.SVM, x_tst_Low_ts3)
  mse.tst.Low.ts3.SVM <- mean((x_tst_Low_ts3$F.pc.16.17 - pred.trn.Low.ts3.SVM )^2)
  
  # summarize error 
  SVM_MSE_trn_Low <- c(mse.trn.Low.ts1.SVM, mse.trn.Low.ts2.SVM, mse.trn.Low.ts3.SVM)
  SVM_MSE_tst_Low <- c(mse.tst.Low.ts1.SVM, mse.tst.Low.ts2.SVM, mse.tst.Low.ts3.SVM)
  
  e2_error_low <- data.frame(e2_error, SVM_MSE_trn_Low, SVM_MSE_tst_Low )
  
  e2.pred.svr.low <- data.frame(pred.trn.Low.ts1.SVM, pred.trn.Low.ts2.SVM, pred.trn.Low.ts3.SVM)
  
## modelling: LM income -------------------------------------------------------
  # Baseline: LM ---------------------------------------------------------------- 
  # rate of change at year t is predicted by year t-1 
  
  # TS1 
  # train
  y.trn.LM.ts1 <- trn.LM.ts1$F.pc.13.14
  # calculate MSE
  mse.y.trn.LM.ts1 <- mean((trn.LM.ts1$F.pc.14.15 - y.trn.LM.ts1 )^2)
  
  # test 
  y.tst.LM.ts1 <- tst.LM.ts1$F.pc.13.14
  # calculate MSE
  mse.y.tst.LM.ts1 <- mean((tst.LM.ts1$F.pc.14.15 - y.tst.LM.ts1 )^2)
  
  # TS2 
  # train
  y.trn.LM.ts2 <- trn.LM.ts2$F.pc.14.15
  # calculate MSE
  mse.y.trn.LM.ts2 <- mean((trn.LM.ts2$F.pc.15.16 - y.trn.LM.ts2 )^2)
  
  # test 
  y.tst.LM.ts2 <- tst.LM.ts2$F.pc.14.15
  # calculate MSE
  mse.y.tst.LM.ts2 <- mean((tst.LM.ts2$F.pc.15.16 - y.tst.LM.ts2 )^2)
  
  # TS3 
  # train
  # train
  y.trn.LM.ts3 <- trn.LM.ts3$F.pc.15.16
  # calculate MSE
  mse.y.trn.LM.ts3 <- mean((trn.LM.ts3$F.pc.16.17 - y.trn.LM.ts3 )^2)
  
  # test 
  y.tst.LM.ts3 <- tst.LM.ts3$F.pc.15.16
  # calculate MSE
  mse.y.tst.LM.ts3 <- mean((tst.LM.ts3$F.pc.16.17 - y.tst.LM.ts3 )^2)
  
  
  base_MSE_trn_LM <- c(mse.y.trn.LM.ts1, mse.y.trn.LM.ts2, mse.y.trn.LM.ts3)
  base_MSE_tst_LM <- c(mse.y.tst.LM.ts1, mse.y.tst.LM.ts2, mse.y.tst.LM.ts3)
  
  e2_error <- data.frame(base_MSE_trn_LM, base_MSE_tst_LM)
  
  
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
  
  ## Elastic net LM -------------------------------------------------------------
  mod.ctrl <- trainControl(method = "cv", number = 10)
  
  is.na(x_trn_LM_ts1)
  
  # TS 1 
  # Train 
  trn.LM.ts1.Enet <- train( F.pc.14.15 ~ .,
                            x_trn_LM_ts1,
                            method = 'glmnet',
                            lambda = 10000,
                            trControl = mod.ctrl)
  
  mse.trn.LM.ts1.Enet <- min((trn.LM.ts1.Enet$results$RMSE)^2)
  
  plot(varImp(trn.LM.ts1.Enet, scale = F))
  
  # test
  pred.LM.ts1.Enet <- predict(trn.LM.ts1.Enet, x_tst_LM_ts1)
  mse.tst.LM.ts1.Enet <- mean((x_tst_LM_ts1$F.pc.14.15 - pred.LM.ts1.Enet)^2)
  
  # TS 2 
  # Train 
  trn.LM.ts2.Enet <- train( F.pc.15.16 ~ .,
                            x_trn_LM_ts2,
                            method = 'glmnet',
                            tuneGrid = expand.grid(alpha = 0.5,
                                                   lambda = seq(0.00001, 10000, length = 1000)),
                            trControl = mod.ctrl)
  
  mse.trn.LM.ts2.Enet <- min((trn.LM.ts2.Enet$results$RMSE)^2)
  
  # test
  pred.LM.ts2.Enet <- predict(trn.LM.ts2.Enet, x_tst_LM_ts2)
  mse.tst.LM.ts2.Enet <- mean((x_tst_LM_ts2$F.pc.15.16 - pred.LM.ts2.Enet)^2)
  
  # TS 3 
  # Train 
  trn.LM.ts3.Enet <- train( F.pc.16.17 ~ .,
                            x_trn_LM_ts3,
                            method = 'glmnet',
                            tuneGrid = expand.grid(alpha = 0.5,
                                                   lambda = seq(0.00001, 10000, length = 1000)),
                            trControl = mod.ctrl)
  
  mse.trn.LM.ts3.Enet <- min((trn.LM.ts3.Enet$results$RMSE)^2)
  
  # test
  pred.LM.ts3.Enet <- predict(trn.LM.ts3.Enet, x_tst_LM_ts3)
  mse.tst.LM.ts3.Enet <- mean((x_tst_LM_ts3$F.pc.16.17 - pred.LM.ts3.Enet)^2)
  
  # summarize error 
  enet_MSE_trn_LM <- c(mse.trn.LM.ts1.Enet, mse.trn.LM.ts2.Enet, mse.trn.LM.ts3.Enet)
  enet_MSE_tst_LM <- c(mse.tst.LM.ts1.Enet, mse.tst.LM.ts2.Enet, mse.tst.LM.ts3.Enet)
  
  e2_error <- data.frame(e2_error, enet_MSE_trn_LM, enet_MSE_tst_LM )
  
  ## Random Forest: LM ----------------------------------------------------------
  # TS1
  # train
  trn.LM.ts1.RF = randomForest(F.pc.14.15 ~ . , data = x_trn_LM_ts1, ntree = 50, 
                               cv.fold = 10)
  mse.trn.LM.ts1.RF <- min(trn.LM.ts1.RF$mse)
  
  # predict
  pred.LM.ts1.RF <- predict(trn.LM.ts1.RF, x_tst_LM_ts1)
  mse.tst.LM.ts1.RF <- mean((x_tst_LM_ts1$F.pc.14.15 - pred.LM.ts1.RF)^2)
  
  # TS2
  # train
  trn.LM.ts2.RF = randomForest(F.pc.15.16 ~ . , data = x_trn_LM_ts2, ntree = 50, 
                               cv.fold = 10)
  mse.trn.LM.ts2.RF <- min(trn.LM.ts2.RF$mse)
  
  # predict
  pred.LM.ts2.RF <- predict(trn.LM.ts2.RF, x_tst_LM_ts2)
  mse.tst.LM.ts2.RF <- mean((x_tst_LM_ts2$F.pc.15.16 - pred.LM.ts2.RF)^2)
  
  # TS3
  # train
  trn.LM.ts3.RF = randomForest(F.pc.16.17 ~ . , data = x_trn_LM_ts3, ntree = 50, 
                               cv.fold = 10)
  mse.trn.LM.ts3.RF <- min(trn.LM.ts3.RF$mse)
  
  # predict
  pred.LM.ts3.RF <- predict(trn.LM.ts3.RF, x_tst_LM_ts3)
  mse.tst.LM.ts3.RF <- mean((x_tst_LM_ts3$F.pc.16.17 - pred.LM.ts3.RF)^2)
  
  # summarize error 
  RF_MSE_trn_LM <- c(mse.trn.LM.ts1.RF, mse.trn.LM.ts2.RF, mse.trn.LM.ts3.RF)
  RF_MSE_tst_LM <- c(mse.tst.LM.ts1.RF, mse.tst.LM.ts2.RF, mse.tst.LM.ts3.RF)
  
  e2_error <- data.frame(e2_error, RF_MSE_trn_LM, RF_MSE_tst_LM )
  
  ## SVM: LM --------------------------------------------------------------------
  # TS1 
  # train
  trn.LM.ts1.SVM <- svm(F.pc.14.15 ~ . , data = x_trn_LM_ts1, kernel = "radial", 
                        cost = 1, gamma = 0.1, epsilon = 0.1, cv.fold = 10)
  
  mse.trn.LM.ts1.SVM <- sqrt(mean(trn.LM.ts1.SVM $residuals^2))
  
  # test
  pred.trn.LM.ts1.SVM <- predict(trn.LM.ts1.SVM, x_tst_LM_ts1)
  mse.tst.LM.ts1.SVM <- mean((x_tst_LM_ts1$F.pc.14.15 - pred.trn.LM.ts1.SVM )^2)
  
  # TS2 
  # train
  trn.LM.ts2.SVM <- svm(F.pc.15.16 ~ . , data = x_trn_LM_ts2, kernel = "radial", 
                        cost = 1, gamma = 0.1, epsilon = 0.1, cv.fold = 10)
  
  mse.trn.LM.ts2.SVM <- sqrt(mean(trn.LM.ts2.SVM $residuals^2))
  
  # test
  pred.trn.LM.ts2.SVM <- predict(trn.LM.ts2.SVM, x_tst_LM_ts2)
  mse.tst.LM.ts2.SVM <- mean((x_tst_LM_ts2$F.pc.15.16 - pred.trn.LM.ts2.SVM )^2)
  
  # TS3 
  # train
  trn.LM.ts3.SVM <- svm(F.pc.16.17 ~ . , data = x_trn_LM_ts3, kernel = "radial", 
                        cost = 1, gamma = 0.1, epsilon = 0.1, cv.fold = 10)
  
  mse.trn.LM.ts3.SVM <- sqrt(mean(trn.LM.ts3.SVM $residuals^2))
  
  # test
  pred.trn.LM.ts3.SVM <- predict(trn.LM.ts3.SVM, x_tst_LM_ts3)
  mse.tst.LM.ts3.SVM <- mean((x_tst_LM_ts3$F.pc.16.17 - pred.trn.LM.ts3.SVM )^2)
  
  # summarize error 
  SVM_MSE_trn_LM <- c(mse.trn.LM.ts1.SVM, mse.trn.LM.ts2.SVM, mse.trn.LM.ts3.SVM)
  SVM_MSE_tst_LM <- c(mse.tst.LM.ts1.SVM, mse.tst.LM.ts2.SVM, mse.tst.LM.ts3.SVM)
  
  e2_error_LM <- data.frame(e2_error, SVM_MSE_trn_LM, SVM_MSE_tst_LM )

## modelling: UM income -------------------------------------------------------
  # Baseline: UM ---------------------------------------------------------------- 
  # rate of change at year t is predicted by year t-1 
  
  # TS1 
  # train
  y.trn.UM.ts1 <- trn.UM.ts1$F.pc.13.14
  # calculate MSE
  mse.y.trn.UM.ts1 <- mean((trn.UM.ts1$F.pc.14.15 - y.trn.UM.ts1 )^2)
  
  # test 
  y.tst.UM.ts1 <- tst.UM.ts1$F.pc.13.14
  # calculate MSE
  mse.y.tst.UM.ts1 <- mean((tst.UM.ts1$F.pc.14.15 - y.tst.UM.ts1 )^2)
  
  # TS2 
  # train
  y.trn.UM.ts2 <- trn.UM.ts2$F.pc.14.15
  # calculate MSE
  mse.y.trn.UM.ts2 <- mean((trn.UM.ts2$F.pc.15.16 - y.trn.UM.ts2 )^2)
  
  # test 
  y.tst.UM.ts2 <- tst.UM.ts2$F.pc.14.15
  # calculate MSE
  mse.y.tst.UM.ts2 <- mean((tst.UM.ts2$F.pc.15.16 - y.tst.UM.ts2 )^2)
  
  # TS3 
  # train
  # train
  y.trn.UM.ts3 <- trn.UM.ts3$F.pc.15.16
  # calculate MSE
  mse.y.trn.UM.ts3 <- mean((trn.UM.ts3$F.pc.16.17 - y.trn.UM.ts3 )^2)
  
  # test 
  y.tst.UM.ts3 <- tst.UM.ts3$F.pc.15.16
  # calculate MSE
  mse.y.tst.UM.ts3 <- mean((tst.UM.ts3$F.pc.16.17 - y.tst.UM.ts3 )^2)
  
  
  base_MSE_trn_UM <- c(mse.y.trn.UM.ts1, mse.y.trn.UM.ts2, mse.y.trn.UM.ts3)
  base_MSE_tst_UM <- c(mse.y.tst.UM.ts1, mse.y.tst.UM.ts2, mse.y.tst.UM.ts3)
  
  e2_error <- data.frame(base_MSE_trn_UM, base_MSE_tst_UM)
  
  
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
  
  ## Elastic net UM -------------------------------------------------------------
  mod.ctrl <- trainControl(method = "repeatedcv", number = 10, verboseIter = T)
  
  # TS 1 
  # Train 
  trn.UM.ts1.Enet <- train( F.pc.14.15 ~ .,
                            x_trn_UM_ts1,
                            method = 'glmnet',
                            tuneGrid = expand.grid(alpha = 0.5,
                                                   lambda = seq(0.00001, 10000, length = 1000)),
                            trControl = mod.ctrl)
  
  mse.trn.UM.ts1.Enet <- min((trn.UM.ts1.Enet$results$RMSE)^2)
  
  # test
  pred.UM.ts1.Enet <- predict(trn.UM.ts1.Enet, x_tst_UM_ts1)
  mse.tst.UM.ts1.Enet <- mean((x_tst_UM_ts1$F.pc.14.15 - pred.UM.ts1.Enet)^2)
  
  # TS 2 
  # Train 
  trn.UM.ts2.Enet <- train( F.pc.15.16 ~ .,
                            x_trn_UM_ts2,
                            method = 'glmnet',
                            tuneGrid = expand.grid(alpha = 0.5,
                                                   lambda = seq(0.00001, 10000, length = 1000)),
                            trControl = mod.ctrl)
  
  mse.trn.UM.ts2.Enet <- min((trn.UM.ts2.Enet$results$RMSE)^2)
  
  # test
  pred.UM.ts2.Enet <- predict(trn.UM.ts2.Enet, x_tst_UM_ts2)
  mse.tst.UM.ts2.Enet <- mean((x_tst_UM_ts2$F.pc.15.16 - pred.UM.ts2.Enet)^2)
  
  # TS 3 
  # Train 
  trn.UM.ts3.Enet <- train( F.pc.16.17 ~ .,
                            x_trn_UM_ts3,
                            method = 'glmnet',
                            tuneGrid = expand.grid(alpha = 0.5,
                                                   lambda = seq(0.00001, 10000, length = 1000)),
                            trControl = mod.ctrl)
  
  mse.trn.UM.ts3.Enet <- min((trn.UM.ts3.Enet$results$RMSE)^2)
  
  # test
  pred.UM.ts3.Enet <- predict(trn.UM.ts3.Enet, x_tst_UM_ts3)
  mse.tst.UM.ts3.Enet <- mean((x_tst_UM_ts3$F.pc.16.17 - pred.UM.ts3.Enet)^2)
  
  # summarize error 
  enet_MSE_trn_UM <- c(mse.trn.UM.ts1.Enet, mse.trn.UM.ts2.Enet, mse.trn.UM.ts3.Enet)
  enet_MSE_tst_UM <- c(mse.tst.UM.ts1.Enet, mse.tst.UM.ts2.Enet, mse.tst.UM.ts3.Enet)
  
  e2_error <- data.frame(e2_error, enet_MSE_trn_UM, enet_MSE_tst_UM )
  
  ## Random Forest: UM ----------------------------------------------------------
  # TS1
  # train
  trn.UM.ts1.RF = randomForest(F.pc.14.15 ~ . , data = x_trn_UM_ts1, ntree = 50, 
                               cv.fold = 10)
  mse.trn.UM.ts1.RF <- min(trn.UM.ts1.RF$mse)
  
  # predict
  pred.UM.ts1.RF <- predict(trn.UM.ts1.RF, x_tst_UM_ts1)
  mse.tst.UM.ts1.RF <- mean((x_tst_UM_ts1$F.pc.14.15 - pred.UM.ts1.RF)^2)
  
  # TS2
  # train
  trn.UM.ts2.RF = randomForest(F.pc.15.16 ~ . , data = x_trn_UM_ts2, ntree = 50, 
                               cv.fold = 10)
  mse.trn.UM.ts2.RF <- min(trn.UM.ts2.RF$mse)
  
  # predict
  pred.UM.ts2.RF <- predict(trn.UM.ts2.RF, x_tst_UM_ts2)
  mse.tst.UM.ts2.RF <- mean((x_tst_UM_ts2$F.pc.15.16 - pred.UM.ts2.RF)^2)
  
  # TS3
  # train
  trn.UM.ts3.RF = randomForest(F.pc.16.17 ~ . , data = x_trn_UM_ts3, ntree = 50, 
                               cv.fold = 10)
  mse.trn.UM.ts3.RF <- min(trn.UM.ts3.RF$mse)
  
  # predict
  pred.UM.ts3.RF <- predict(trn.UM.ts3.RF, x_tst_UM_ts3)
  mse.tst.UM.ts3.RF <- mean((x_tst_UM_ts3$F.pc.16.17 - pred.UM.ts3.RF)^2)
  
  # summarize error 
  RF_MSE_trn_UM <- c(mse.trn.UM.ts1.RF, mse.trn.UM.ts2.RF, mse.trn.UM.ts3.RF)
  RF_MSE_tst_UM <- c(mse.tst.UM.ts1.RF, mse.tst.UM.ts2.RF, mse.tst.UM.ts3.RF)
  
  e2_error <- data.frame(e2_error, RF_MSE_trn_UM, RF_MSE_tst_UM )
  
  ## SVM: UM --------------------------------------------------------------------
  # TS1 
  # train
  trn.UM.ts1.SVM <- svm(F.pc.14.15 ~ . , data = x_trn_UM_ts1, kernel = "radial", 
                        cost = 1, gamma = 0.1, epsilon = 0.1, cv.fold = 10)
  
  mse.trn.UM.ts1.SVM <- sqrt(mean(trn.UM.ts1.SVM $residuals^2))
  
  # test
  pred.trn.UM.ts1.SVM <- predict(trn.UM.ts1.SVM, x_tst_UM_ts1)
  mse.tst.UM.ts1.SVM <- mean((x_tst_UM_ts1$F.pc.14.15 - pred.trn.UM.ts1.SVM )^2)
  
  # TS2 
  # train
  trn.UM.ts2.SVM <- svm(F.pc.15.16 ~ . , data = x_trn_UM_ts2, kernel = "radial", 
                        cost = 1, gamma = 0.1, epsilon = 0.1, cv.fold = 10)
  
  mse.trn.UM.ts2.SVM <- sqrt(mean(trn.UM.ts2.SVM $residuals^2))
  
  # test
  pred.trn.UM.ts2.SVM <- predict(trn.UM.ts2.SVM, x_tst_UM_ts2)
  
  mse.tst.UM.ts2.SVM <- mean((x_tst_UM_ts2$F.pc.15.16 - pred.trn.UM.ts2.SVM )^2)
  
  # TS3 
  # train
  trn.UM.ts3.SVM <- svm(F.pc.16.17 ~ . , data = x_trn_UM_ts3, kernel = "radial", 
                        cost = 1, gamma = 0.1, epsilon = 0.1, cv.fold = 10)
  
  mse.trn.UM.ts3.SVM <- sqrt(mean(trn.UM.ts3.SVM $residuals^2))
  
  # test
  pred.trn.UM.ts3.SVM <- predict(trn.UM.ts3.SVM, x_tst_UM_ts3)
  mse.tst.UM.ts3.SVM <- mean((x_tst_UM_ts3$F.pc.16.17 - pred.trn.UM.ts3.SVM )^2)
  
  # summarize error 
  SVM_MSE_trn_UM <- c(mse.trn.UM.ts1.SVM, mse.trn.UM.ts2.SVM, mse.trn.UM.ts3.SVM)
  SVM_MSE_tst_UM <- c(mse.tst.UM.ts1.SVM, mse.tst.UM.ts2.SVM, mse.tst.UM.ts3.SVM)
  
  e2_error_UM <- data.frame(e2_error, SVM_MSE_trn_UM, SVM_MSE_tst_UM )    
  
## modelling: H income -------------------------------------------------------
  # Baseline: H ---------------------------------------------------------------- 
  # rate of change at year t is predicted by year t-1 
  
  # TS1 
  # train
  y.trn.H.ts1 <- trn.H.ts1$F.pc.13.14
  # calculate MSE
  mse.y.trn.H.ts1 <- mean((trn.H.ts1$F.pc.14.15 - y.trn.H.ts1 )^2)
  
  # test 
  y.tst.H.ts1 <- tst.H.ts1$F.pc.13.14
  # calculate MSE
  mse.y.tst.H.ts1 <- mean((tst.H.ts1$F.pc.14.15 - y.tst.H.ts1 )^2)
  
  # TS2 
  # train
  y.trn.H.ts2 <- trn.H.ts2$F.pc.14.15
  # calculate MSE
  mse.y.trn.H.ts2 <- mean((trn.H.ts2$F.pc.15.16 - y.trn.H.ts2 )^2)
  
  # test 
  y.tst.H.ts2 <- tst.H.ts2$F.pc.14.15
  # calculate MSE
  mse.y.tst.H.ts2 <- mean((tst.H.ts2$F.pc.15.16 - y.tst.H.ts2 )^2)
  
  # TS3 
  # train
  # train
  y.trn.H.ts3 <- trn.H.ts3$F.pc.15.16
  # calculate MSE
  mse.y.trn.H.ts3 <- mean((trn.H.ts3$F.pc.16.17 - y.trn.H.ts3 )^2)
  
  # test 
  y.tst.H.ts3 <- tst.H.ts3$F.pc.15.16
  # calculate MSE
  
  mse.y.tst.H.ts3 <- mean((tst.H.ts3$F.pc.16.17 - y.tst.H.ts3 )^2)
  
  
  base_MSE_trn_H <- c(mse.y.trn.H.ts1, mse.y.trn.H.ts2, mse.y.trn.H.ts3)
  base_MSE_tst_H <- c(mse.y.tst.H.ts1, mse.y.tst.H.ts2, mse.y.tst.H.ts3)
  
  e2_error <- data.frame(base_MSE_trn_H, base_MSE_tst_H)
  
  
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
  
  ## Elastic net H -------------------------------------------------------------
  mod.ctrl <- trainControl(method = "repeatedcv", number = 10, verboseIter = T)
  
  # TS 1 
  # Train 
  trn.H.ts1.Enet <- train( F.pc.14.15 ~ .,
                           x_trn_H_ts1,
                           method = 'glmnet',
                           tuneGrid = expand.grid(alpha = 0.5,
                                                  lambda = seq(0.00001, 10000, length = 1000)),
                           trControl = mod.ctrl)
  
  mse.trn.H.ts1.Enet <- min((trn.H.ts1.Enet$results$RMSE)^2)
  
  # test
  pred.H.ts1.Enet <- predict(trn.H.ts1.Enet, x_tst_H_ts1)
  mse.tst.H.ts1.Enet <- mean((x_tst_H_ts1$F.pc.14.15 - pred.H.ts1.Enet)^2)
  
  # TS 2 
  # Train 
  trn.H.ts2.Enet <- train( F.pc.15.16 ~ .,
                           x_trn_H_ts2,
                           method = 'glmnet',
                           tuneGrid = expand.grid(alpha = 0.5,
                                                  lambda = seq(0.00001, 10000, length = 1000)),
                           trControl = mod.ctrl)
  
  mse.trn.H.ts2.Enet <- min((trn.H.ts2.Enet$results$RMSE)^2)
  
  # test
  pred.H.ts2.Enet <- predict(trn.H.ts2.Enet, x_tst_H_ts2)
  mse.tst.H.ts2.Enet <- mean((x_tst_H_ts2$F.pc.15.16 - pred.H.ts2.Enet)^2)
  
  # TS 3 
  # Train 
  trn.H.ts3.Enet <- train( F.pc.16.17 ~ .,
                           x_trn_H_ts3,
                           method = 'glmnet',
                           tuneGrid = expand.grid(alpha = 0.5,
                                                  lambda = seq(0.00001, 10000, length = 1000)),
                           trControl = mod.ctrl)
  
  mse.trn.H.ts3.Enet <- min((trn.H.ts3.Enet$results$RMSE)^2)
  
  # test
  pred.H.ts3.Enet <- predict(trn.H.ts3.Enet, x_tst_H_ts3)
  mse.tst.H.ts3.Enet <- mean((x_tst_H_ts3$F.pc.16.17 - pred.H.ts3.Enet)^2)
  
  # summarize error 
  enet_MSE_trn_H <- c(mse.trn.H.ts1.Enet, mse.trn.H.ts2.Enet, mse.trn.H.ts3.Enet)
  enet_MSE_tst_H <- c(mse.tst.H.ts1.Enet, mse.tst.H.ts2.Enet, mse.tst.H.ts3.Enet)
  
  e2_error <- data.frame(e2_error, enet_MSE_trn_H, enet_MSE_tst_H )
  
  ## Random Forest: H ----------------------------------------------------------
  # TS1
  # train
  trn.H.ts1.RF = randomForest(F.pc.14.15 ~ . , data = x_trn_H_ts1, ntree = 50, 
                              cv.fold = 10)
  mse.trn.H.ts1.RF <- min(trn.H.ts1.RF$mse)
  
  # predict
  pred.H.ts1.RF <- predict(trn.H.ts1.RF, x_tst_H_ts1)
  mse.tst.H.ts1.RF <- mean((x_tst_H_ts1$F.pc.14.15 - pred.H.ts1.RF)^2)
  
  # TS2
  # train
  trn.H.ts2.RF = randomForest(F.pc.15.16 ~ . , data = x_trn_H_ts2, ntree = 50, 
                              cv.fold = 10)
  mse.trn.H.ts2.RF <- min(trn.H.ts2.RF$mse)
  
  # predict
  pred.H.ts2.RF <- predict(trn.H.ts2.RF, x_tst_H_ts2)
  mse.tst.H.ts2.RF <- mean((x_tst_H_ts2$F.pc.15.16 - pred.H.ts2.RF)^2)
  
  # TS3
  # train
  trn.H.ts3.RF = randomForest(F.pc.16.17 ~ . , data = x_trn_H_ts3, ntree = 50, 
                              cv.fold = 10)
  mse.trn.H.ts3.RF <- min(trn.H.ts3.RF$mse)
  
  # predict
  pred.H.ts3.RF <- predict(trn.H.ts3.RF, x_tst_H_ts3)
  mse.tst.H.ts3.RF <- mean((x_tst_H_ts3$F.pc.16.17 - pred.H.ts3.RF)^2)
  
  # summarize error 
  RF_MSE_trn_H <- c(mse.trn.H.ts1.RF, mse.trn.H.ts2.RF, mse.trn.H.ts3.RF)
  RF_MSE_tst_H <- c(mse.tst.H.ts1.RF, mse.tst.H.ts2.RF, mse.tst.H.ts3.RF)
  
  e2_error <- data.frame(e2_error, RF_MSE_trn_H, RF_MSE_tst_H )
  
  ## SVM: H --------------------------------------------------------------------
  # TS1 
  # train
  trn.H.ts1.SVM <- svm(F.pc.14.15 ~ . , data = x_trn_H_ts1, kernel = "radial", 
                       cost = 1, gamma = 0.1, epsilon = 0.1, cv.fold = 10)
  
  mse.trn.H.ts1.SVM <- sqrt(mean(trn.H.ts1.SVM $residuals^2))
  
  # test
  pred.trn.H.ts1.SVM <- predict(trn.H.ts1.SVM, x_tst_H_ts1)
  mse.tst.H.ts1.SVM <- mean((x_tst_H_ts1$F.pc.14.15 - pred.trn.H.ts1.SVM )^2)
  
  # TS2 
  # train
  trn.H.ts2.SVM <- svm(F.pc.15.16 ~ . , data = x_trn_H_ts2, kernel = "radial", 
                       cost = 1, gamma = 0.1, epsilon = 0.1, cv.fold = 10)
  
  mse.trn.H.ts2.SVM <- sqrt(mean(trn.H.ts2.SVM $residuals^2))
  
  # test
  pred.trn.H.ts2.SVM <- predict(trn.H.ts2.SVM, x_tst_H_ts2)
  
  mse.tst.H.ts2.SVM <- mean((x_tst_H_ts2$F.pc.15.16 - pred.trn.H.ts2.SVM )^2)
  
  # TS3 
  # train
  trn.H.ts3.SVM <- svm(F.pc.16.17 ~ . , data = x_trn_H_ts3, kernel = "radial", 
                       cost = 1, gamma = 0.1, epsilon = 0.1, cv.fold = 10)
  
  mse.trn.H.ts3.SVM <- sqrt(mean(trn.H.ts3.SVM $residuals^2))
  
  # test
  pred.trn.H.ts3.SVM <- predict(trn.H.ts3.SVM, x_tst_H_ts3)
  mse.tst.H.ts3.SVM <- mean((x_tst_H_ts3$F.pc.16.17 - pred.trn.H.ts3.SVM )^2)
  
  # summarize error 
  SVM_MSE_trn_H <- c(mse.trn.H.ts1.SVM, mse.trn.H.ts2.SVM, mse.trn.H.ts3.SVM)
  SVM_MSE_tst_H <- c(mse.tst.H.ts1.SVM, mse.tst.H.ts2.SVM, mse.tst.H.ts3.SVM)
  
  e2_error_H <- data.frame(e2_error, SVM_MSE_trn_H, SVM_MSE_tst_H )
  
  pred.e2.high.svm <- data.frame(pred.trn.H.ts1.SVM, pred.trn.H.ts2.SVM,
                                 pred.trn.H.ts3.SVM)
  