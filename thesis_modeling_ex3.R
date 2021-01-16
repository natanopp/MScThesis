## modeling script experiment 3
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

## Experiment 3: The predictive power on the different acessibility ------------

# partitioning 
# Group 1: Easy to access
# train set: different region of destination and origin: 2405 observations 
trn.Easy.ts1 <- data_TS1 %>%
  filter (same_region == "FALSE" & visa_access == "high")

trn.Easy.ts2 <- data_TS2 %>%
  filter (same_region == "FALSE" & visa_access == "high")

trn.Easy.ts3 <- data_TS3 %>%
  filter (same_region == "FALSE" & visa_access == "high")

# test set: same income level of destination and origin: 614 observations 
tst.Easy.ts1 <- data_TS1 %>%
  filter (same_region == "TRUE" & visa_access == "high")

tst.Easy.ts2 <- data_TS2 %>%
  filter (same_region == "TRUE" & visa_access == "high")

tst.Easy.ts3 <- data_TS3 %>%
  filter (same_region == "TRUE" & visa_access == "high")

# Group 2: Moderate 
# train set: different region of destination and origin: 1173 observations 
trn.moderate.ts1 <- data_TS1 %>%
  filter (same_region == "FALSE" & visa_access == "moderate")

trn.moderate.ts2 <- data_TS2 %>%
  filter (same_region == "FALSE" & visa_access == "moderate")

trn.moderate.ts3 <- data_TS3 %>%
  filter (same_region == "FALSE" & visa_access == "moderate")

# test set: same income level of destination and origin: 257 observations 
tst.moderate.ts1 <- data_TS1 %>%
  filter (same_region == "TRUE" & visa_access == "moderate")

tst.moderate.ts2 <- data_TS2 %>%
  filter (same_region == "TRUE" & visa_access == "moderate")

tst.moderate.ts3 <- data_TS3 %>%
  filter (same_region == "TRUE" & visa_access == "moderate")

# Group 3: Diffcult
# train set: different region of destination and origin: 1759 observations 
trn.difficult.ts1 <- data_TS1 %>%
  filter (same_region == "FALSE" & visa_access == "low")

trn.difficult.ts2 <- data_TS2 %>%
  filter (same_region == "FALSE" & visa_access == "low")

trn.difficult.ts3 <- data_TS3 %>%
  filter (same_region == "FALSE" & visa_access == "low")

# test set: same income level of destination and origin: 437 observations 
tst.difficult.ts1 <- data_TS1 %>%
  filter (same_region == "TRUE" & visa_access == "low")

tst.difficult.ts2 <- data_TS2 %>%
  filter (same_region == "TRUE" & visa_access == "low")

tst.difficult.ts3 <- data_TS3 %>%
  filter (same_region == "TRUE" & visa_access == "low")

# Group 4: Very difficult 
# train set: different region of destination and origin: 2104 observations 
trn.Vdifficult.ts1 <- data_TS1 %>%
  filter (same_region == "FALSE" & visa_access == "very low")

trn.Vdifficult.ts2 <- data_TS2 %>%
  filter (same_region == "FALSE" & visa_access == "very low")

trn.Vdifficult.ts3 <- data_TS3 %>%
  filter (same_region == "FALSE" & visa_access == "very low")

# test set: same income level of destination and origin: 218 observations 
tst.Vdifficult.ts1 <- data_TS1 %>%
  filter (same_region == "TRUE" & visa_access == "very low")

tst.Vdifficult.ts2 <- data_TS2 %>%
  filter (same_region == "TRUE" & visa_access == "very low")

tst.Vdifficult.ts3 <- data_TS3 %>%
  filter (same_region == "TRUE" & visa_access == "very low")

## modeling: easy to access
## modeling: baseline ----------------------------------------------------------
# rate of change at year t is predicted by year t-1 

# TS1 
# train
y.trn.Easy.ts1 <- trn.Easy.ts1$F.pc.13.14
# calculate MSE
mse.y.trn.Easy.ts1 <- mean((trn.Easy.ts1$F.pc.14.15 - y.trn.Easy.ts1 )^2)

# test 
y.tst.Easy.ts1 <- tst.Easy.ts1$F.pc.13.14
# calculate MSE
mse.y.tst.Easy.ts1 <- mean((tst.Easy.ts1$F.pc.14.15 - y.tst.Easy.ts1 )^2)

# TS2 
# train
y.trn.Easy.ts2 <- trn.Easy.ts2$F.pc.14.15
# calculate MSE
mse.y.trn.Easy.ts2 <- mean((trn.Easy.ts2$F.pc.15.16 - y.trn.Easy.ts2 )^2)

# test 
y.tst.Easy.ts2 <- tst.Easy.ts2$F.pc.14.15
# calculate MSE
mse.y.tst.Easy.ts2 <- mean((tst.Easy.ts2$F.pc.15.16 - y.tst.Easy.ts2 )^2)

# TS3 
# train
y.trn.Easy.ts3 <- trn.Easy.ts3$F.pc.15.16
# calculate MSE
mse.y.trn.Easy.ts3 <- mean((trn.Easy.ts3$F.pc.16.17 - y.trn.Easy.ts3 )^2)

# test 
y.tst.Easy.ts3 <- tst.Easy.ts3$F.pc.15.16
# calculate MSE
mse.y.tst.Easy.ts3 <- mean((tst.Easy.ts3$F.pc.16.17 - y.tst.Easy.ts3 )^2)


base_MSE_trn_Easy <- c(mse.y.trn.Easy.ts1, mse.y.trn.Easy.ts2, mse.y.trn.Easy.ts3)
base_MSE_tst_Easy <- c(mse.y.tst.Easy.ts1, mse.y.tst.Easy.ts2, mse.y.tst.Easy.ts3)

e3_error <- data.frame(base_MSE_trn_Easy, base_MSE_tst_Easy)

# variables definition ---------------------------------------------------------
# TS1 
x_trn_Easy_ts1 <- trn.Easy.ts1[,10:25]
x_tst_Easy_ts1 <- tst.Easy.ts1[,10:25]
# TS2 
x_trn_Easy_ts2 <- trn.Easy.ts2[,10:25]
x_tst_Easy_ts2 <- tst.Easy.ts2[,10:25]
# TS3 
x_trn_Easy_ts3 <- trn.Easy.ts3[,10:25]
x_tst_Easy_ts3 <- tst.Easy.ts3[,10:25]

## Elastic net Easy -------------------------------------------------------------
mod.ctrl <- trainControl(method = "repeatedcv", number = 10, verboseIter = T)

# TS 1 
# Train 
trn.Easy.ts1.Enet <- train( F.pc.14.15 ~ .,
                            x_trn_Easy_ts1,
                            method = 'glmnet',
                            tuneGrid = expand.grid(alpha = 0.5,
                                                   lambda = seq(0.00001, 10000, length = 1000)),
                            trControl = mod.ctrl)

mse.trn.Easy.ts1.Enet <- min((trn.Easy.ts1.Enet$results$RMSE)^2)

# test
pred.Easy.ts1.Enet <- predict(trn.Easy.ts1.Enet, x_tst_Easy_ts1)
mse.tst.Easy.ts1.Enet <- mean((x_tst_Easy_ts1$F.pc.14.15 - pred.Easy.ts1.Enet)^2)

# TS 2 
# Train 
trn.Easy.ts2.Enet <- train( F.pc.15.16 ~ .,
                            x_trn_Easy_ts2,
                            method = 'glmnet',
                            tuneGrid = expand.grid(alpha = 0.5,
                                                   lambda = seq(0.00001, 10000, length = 1000)),
                            trControl = mod.ctrl)

mse.trn.Easy.ts2.Enet <- min((trn.Easy.ts2.Enet$results$RMSE)^2)

# test
pred.Easy.ts2.Enet <- predict(trn.Easy.ts2.Enet, x_tst_Easy_ts2)
mse.tst.Easy.ts2.Enet <- mean((x_tst_Easy_ts2$F.pc.15.16 - pred.Easy.ts2.Enet)^2)

# TS 3 
# Train 
trn.Easy.ts3.Enet <- train( F.pc.16.17 ~ .,
                            x_trn_Easy_ts3,
                            method = 'glmnet',
                            tuneGrid = expand.grid(alpha = 0.5,
                                                   lambda = seq(0.00001, 10000, length = 1000)),
                            trControl = mod.ctrl)

mse.trn.Easy.ts3.Enet <- min((trn.Easy.ts3.Enet$results$RMSE)^2)

# test
pred.Easy.ts3.Enet <- predict(trn.Easy.ts3.Enet, x_tst_Easy_ts3)
mse.tst.Easy.ts3.Enet <- mean((x_tst_Easy_ts3$F.pc.16.17 - pred.Easy.ts3.Enet)^2)

# summarize error 
enet_MSE_trn_Easy <- c(mse.trn.Easy.ts1.Enet, mse.trn.Easy.ts2.Enet, mse.trn.Easy.ts3.Enet)
enet_MSE_tst_Easy <- c(mse.tst.Easy.ts1.Enet, mse.tst.Easy.ts2.Enet, mse.tst.Easy.ts3.Enet)

e3_error <- data.frame(e3_error, enet_MSE_trn_Easy, enet_MSE_tst_Easy )

## Random Forest: Easy ----------------------------------------------------------
# TS1
# train
trn.Easy.ts1.RF = randomForest(F.pc.14.15 ~ . , data = x_trn_Easy_ts1, ntree = 50, 
                               cv.fold = 10)
mse.trn.Easy.ts1.RF <- min(trn.Easy.ts1.RF$mse)

# predict
pred.Easy.ts1.RF <- predict(trn.Easy.ts1.RF, x_tst_Easy_ts1)
mse.tst.Easy.ts1.RF <- mean((x_tst_Easy_ts1$F.pc.14.15 - pred.Easy.ts1.RF)^2)

# TS2
# train
trn.Easy.ts2.RF = randomForest(F.pc.15.16 ~ . , data = x_trn_Easy_ts2, ntree = 50, 
                               cv.fold = 10)
mse.trn.Easy.ts2.RF <- min(trn.Easy.ts2.RF$mse)

# predict
pred.Easy.ts2.RF <- predict(trn.Easy.ts2.RF, x_tst_Easy_ts2)
mse.tst.Easy.ts2.RF <- mean((x_tst_Easy_ts2$F.pc.15.16 - pred.Easy.ts2.RF)^2)

# TS3
# train
trn.Easy.ts3.RF = randomForest(F.pc.16.17 ~ . , data = x_trn_Easy_ts3, ntree = 50, 
                               cv.fold = 10)
mse.trn.Easy.ts3.RF <- min(trn.Easy.ts3.RF$mse)

# predict
pred.Easy.ts3.RF <- predict(trn.Easy.ts3.RF, x_tst_Easy_ts3)
mse.tst.Easy.ts3.RF <- mean((x_tst_Easy_ts3$F.pc.16.17 - pred.Easy.ts3.RF)^2)

# summarize error 
RF_MSE_trn_Easy <- c(mse.trn.Easy.ts1.RF, mse.trn.Easy.ts2.RF, mse.trn.Easy.ts3.RF)
RF_MSE_tst_Easy <- c(mse.tst.Easy.ts1.RF, mse.tst.Easy.ts2.RF, mse.tst.Easy.ts3.RF)

e3_error <- data.frame(e3_error, RF_MSE_trn_Easy, RF_MSE_tst_Easy )

## SVM: Easy --------------------------------------------------------------------
# TS1 
# train
trn.Easy.ts1.SVM <- svm(F.pc.14.15 ~ . , data = x_trn_Easy_ts1, kernel = "radial", 
                        cost = 1, gamma = 0.1, epsilon = 0.1, cv.fold = 10)

mse.trn.Easy.ts1.SVM <- sqrt(mean(trn.Easy.ts1.SVM $residuals^2))

# test
pred.trn.Easy.ts1.SVM <- predict(trn.Easy.ts1.SVM, x_tst_Easy_ts1)
mse.tst.Easy.ts1.SVM <- mean((x_tst_Easy_ts1$F.pc.14.15 - pred.trn.Easy.ts1.SVM )^2)

# TS2 
# train
trn.Easy.ts2.SVM <- svm(F.pc.15.16 ~ . , data = x_trn_Easy_ts2, kernel = "radial", 
                        cost = 1, gamma = 0.1, epsilon = 0.1, cv.fold = 10)

mse.trn.Easy.ts2.SVM <- sqrt(mean(trn.Easy.ts2.SVM $residuals^2))

# test
pred.trn.Easy.ts2.SVM <- predict(trn.Easy.ts2.SVM, x_tst_Easy_ts2)
mse.tst.Easy.ts2.SVM <- mean((x_tst_Easy_ts2$F.pc.15.16 - pred.trn.Easy.ts2.SVM )^2)

# TS3 
# train
trn.Easy.ts3.SVM <- svm(F.pc.16.17 ~ . , data = x_trn_Easy_ts3, kernel = "radial", 
                        cost = 1, gamma = 0.1, epsilon = 0.1, cv.fold = 10)

mse.trn.Easy.ts3.SVM <- sqrt(mean(trn.Easy.ts3.SVM $residuals^2))

# test
pred.trn.Easy.ts3.SVM <- predict(trn.Easy.ts3.SVM, x_tst_Easy_ts3)
mse.tst.Easy.ts3.SVM <- mean((x_tst_Easy_ts3$F.pc.16.17 - pred.trn.Easy.ts3.SVM )^2)

# summarize error 
SVM_MSE_trn_Easy <- c(mse.trn.Easy.ts1.SVM, mse.trn.Easy.ts2.SVM, mse.trn.Easy.ts3.SVM)
SVM_MSE_tst_Easy <- c(mse.tst.Easy.ts1.SVM, mse.tst.Easy.ts2.SVM, mse.tst.Easy.ts3.SVM)

e3_error_Easy <- data.frame(e3_error, SVM_MSE_trn_Easy, SVM_MSE_tst_Easy )

## moderate --------------------------------------------------------------------
## modeling: moderate to access
## modeling: baseline ----------------------------------------------------------
# rate of change at year t is predicted by year t-1 

# TS1 
# train
y.trn.moderate.ts1 <- trn.moderate.ts1$F.pc.13.14
# calculate MSE
mse.y.trn.moderate.ts1 <- mean((trn.moderate.ts1$F.pc.14.15 - y.trn.moderate.ts1 )^2)

# test 
y.tst.moderate.ts1 <- tst.moderate.ts1$F.pc.13.14
# calculate MSE
mse.y.tst.moderate.ts1 <- mean((tst.moderate.ts1$F.pc.14.15 - y.tst.moderate.ts1 )^2)

# TS2 
# train
y.trn.moderate.ts2 <- trn.moderate.ts2$F.pc.14.15
# calculate MSE
mse.y.trn.moderate.ts2 <- mean((trn.moderate.ts2$F.pc.15.16 - y.trn.moderate.ts2 )^2)

# test 
y.tst.moderate.ts2 <- tst.moderate.ts2$F.pc.14.15
# calculate MSE
mse.y.tst.moderate.ts2 <- mean((tst.moderate.ts2$F.pc.15.16 - y.tst.moderate.ts2 )^2)

# TS3 
# train
y.trn.moderate.ts3 <- trn.moderate.ts3$F.pc.15.16
# calculate MSE
mse.y.trn.moderate.ts3 <- mean((trn.moderate.ts3$F.pc.16.17 - y.trn.moderate.ts3 )^2)

# test 
y.tst.moderate.ts3 <- tst.moderate.ts3$F.pc.15.16
# calculate MSE
mse.y.tst.moderate.ts3 <- mean((tst.moderate.ts3$F.pc.16.17 - y.tst.moderate.ts3 )^2)


base_MSE_trn_moderate <- c(mse.y.trn.moderate.ts1, mse.y.trn.moderate.ts2, mse.y.trn.moderate.ts3)
base_MSE_tst_moderate <- c(mse.y.tst.moderate.ts1, mse.y.tst.moderate.ts2, mse.y.tst.moderate.ts3)

e3_error <- data.frame(base_MSE_trn_moderate, base_MSE_tst_moderate)

# variables definition ---------------------------------------------------------
# TS1 
x_trn_moderate_ts1 <- trn.moderate.ts1[,10:25]
x_tst_moderate_ts1 <- tst.moderate.ts1[,10:25]

# TS2 
x_trn_moderate_ts2 <- trn.moderate.ts2[,10:25]
x_tst_moderate_ts2 <- tst.moderate.ts2[,10:25]

# TS3 
x_trn_moderate_ts3 <- trn.moderate.ts3[,10:25]
x_tst_moderate_ts3 <- tst.moderate.ts3[,10:25]

## Elastic net moderate -------------------------------------------------------------
mod.ctrl <- trainControl(method = "repeatedcv", number = 10, verboseIter = T)

# TS 1 
# Train 
trn.moderate.ts1.Enet <- train( F.pc.14.15 ~ .,
                                x_trn_moderate_ts1,
                                method = 'glmnet',
                                tuneGrid = expand.grid(alpha = 0.5,
                                                       lambda = seq(0.00001, 10000, length = 1000)),
                                trControl = mod.ctrl)

mse.trn.moderate.ts1.Enet <- min((trn.moderate.ts1.Enet$results$RMSE)^2)

# test
pred.moderate.ts1.Enet <- predict(trn.moderate.ts1.Enet, x_tst_moderate_ts1)
mse.tst.moderate.ts1.Enet <- mean((x_tst_moderate_ts1$F.pc.14.15 - pred.moderate.ts1.Enet)^2)

# TS 2 
# Train 
trn.moderate.ts2.Enet <- train( F.pc.15.16 ~ .,
                                x_trn_moderate_ts2,
                                method = 'glmnet',
                                tuneGrid = expand.grid(alpha = 0.5,
                                                       lambda = seq(0.00001, 10000, length = 1000)),
                                trControl = mod.ctrl)

mse.trn.moderate.ts2.Enet <- min((trn.moderate.ts2.Enet$results$RMSE)^2)

# test
pred.moderate.ts2.Enet <- predict(trn.moderate.ts2.Enet, x_tst_moderate_ts2)
mse.tst.moderate.ts2.Enet <- mean((x_tst_moderate_ts2$F.pc.15.16 - pred.moderate.ts2.Enet)^2)

# TS 3 
# Train 
trn.moderate.ts3.Enet <- train( F.pc.16.17 ~ .,
                                x_trn_moderate_ts3,
                                method = 'glmnet',
                                tuneGrid = expand.grid(alpha = 0.5,
                                                       lambda = seq(0.00001, 10000, length = 1000)),
                                trControl = mod.ctrl)

mse.trn.moderate.ts3.Enet <- min((trn.moderate.ts3.Enet$results$RMSE)^2)

# test
pred.moderate.ts3.Enet <- predict(trn.moderate.ts3.Enet, x_tst_moderate_ts3)
mse.tst.moderate.ts3.Enet <- mean((x_tst_moderate_ts3$F.pc.16.17 - pred.moderate.ts3.Enet)^2)

# summarize error 
enet_MSE_trn_moderate <- c(mse.trn.moderate.ts1.Enet, mse.trn.moderate.ts2.Enet, mse.trn.moderate.ts3.Enet)
enet_MSE_tst_moderate <- c(mse.tst.moderate.ts1.Enet, mse.tst.moderate.ts2.Enet, mse.tst.moderate.ts3.Enet)

e3_error <- data.frame(e3_error, enet_MSE_trn_moderate, enet_MSE_tst_moderate )

## Random Forest: moderate ----------------------------------------------------------
# TS1
# train
trn.moderate.ts1.RF = randomForest(F.pc.14.15 ~ . , data = x_trn_moderate_ts1, ntree = 50, 
                                   cv.fold = 10)
mse.trn.moderate.ts1.RF <- min(trn.moderate.ts1.RF$mse)

# predict
pred.moderate.ts1.RF <- predict(trn.moderate.ts1.RF, x_tst_moderate_ts1)
mse.tst.moderate.ts1.RF <- mean((x_tst_moderate_ts1$F.pc.14.15 - pred.moderate.ts1.RF)^2)

# TS2
# train
trn.moderate.ts2.RF = randomForest(F.pc.15.16 ~ . , data = x_trn_moderate_ts2, ntree = 50, 
                                   cv.fold = 10)
mse.trn.moderate.ts2.RF <- min(trn.moderate.ts2.RF$mse)

# predict
pred.moderate.ts2.RF <- predict(trn.moderate.ts2.RF, x_tst_moderate_ts2)
mse.tst.moderate.ts2.RF <- mean((x_tst_moderate_ts2$F.pc.15.16 - pred.moderate.ts2.RF)^2)

# TS3
# train
trn.moderate.ts3.RF = randomForest(F.pc.16.17 ~ . , data = x_trn_moderate_ts3, ntree = 50, 
                                   cv.fold = 10)
mse.trn.moderate.ts3.RF <- min(trn.moderate.ts3.RF$mse)

# predict
pred.moderate.ts3.RF <- predict(trn.moderate.ts3.RF, x_tst_moderate_ts3)
mse.tst.moderate.ts3.RF <- mean((x_tst_moderate_ts3$F.pc.16.17 - pred.moderate.ts3.RF)^2)

# summarize error 
RF_MSE_trn_moderate <- c(mse.trn.moderate.ts1.RF, mse.trn.moderate.ts2.RF, mse.trn.moderate.ts3.RF)
RF_MSE_tst_moderate <- c(mse.tst.moderate.ts1.RF, mse.tst.moderate.ts2.RF, mse.tst.moderate.ts3.RF)

e3_error <- data.frame(e3_error, RF_MSE_trn_moderate, RF_MSE_tst_moderate )

## SVM: moderate --------------------------------------------------------------------
# TS1 
# train
trn.moderate.ts1.SVM <- svm(F.pc.14.15 ~ . , data = x_trn_moderate_ts1, kernel = "radial", 
                            cost = 1, gamma = 0.1, epsilon = 0.1, cv.fold = 10)

mse.trn.moderate.ts1.SVM <- sqrt(mean(trn.moderate.ts1.SVM $residuals^2))

# test
pred.trn.moderate.ts1.SVM <- predict(trn.moderate.ts1.SVM, x_tst_moderate_ts1)
mse.tst.moderate.ts1.SVM <- mean((x_tst_moderate_ts1$F.pc.14.15 - pred.trn.moderate.ts1.SVM )^2)

# TS2 
# train
trn.moderate.ts2.SVM <- svm(F.pc.15.16 ~ . , data = x_trn_moderate_ts2, kernel = "radial", 
                            cost = 1, gamma = 0.1, epsilon = 0.1, cv.fold = 10)

mse.trn.moderate.ts2.SVM <- sqrt(mean(trn.moderate.ts2.SVM $residuals^2))

# test
pred.trn.moderate.ts2.SVM <- predict(trn.moderate.ts2.SVM, x_tst_moderate_ts2)
mse.tst.moderate.ts2.SVM <- mean((x_tst_moderate_ts2$F.pc.15.16 - pred.trn.moderate.ts2.SVM )^2)

# TS3 
# train
trn.moderate.ts3.SVM <- svm(F.pc.16.17 ~ . , data = x_trn_moderate_ts3, kernel = "radial", 
                            cost = 1, gamma = 0.1, epsilon = 0.1, cv.fold = 10)

mse.trn.moderate.ts3.SVM <- sqrt(mean(trn.moderate.ts3.SVM $residuals^2))

# test
pred.trn.moderate.ts3.SVM <- predict(trn.moderate.ts3.SVM, x_tst_moderate_ts3)
mse.tst.moderate.ts3.SVM <- mean((x_tst_moderate_ts3$F.pc.16.17 - pred.trn.moderate.ts3.SVM )^2)

# summarize error 
SVM_MSE_trn_moderate <- c(mse.trn.moderate.ts1.SVM, mse.trn.moderate.ts2.SVM, mse.trn.moderate.ts3.SVM)
SVM_MSE_tst_moderate <- c(mse.tst.moderate.ts1.SVM, mse.tst.moderate.ts2.SVM, mse.tst.moderate.ts3.SVM)

e3_error_moderate <- data.frame(e3_error, SVM_MSE_trn_moderate, SVM_MSE_tst_moderate )

## difficult -------------------------------------------------------------------
## modeling: difficult to access
## modeling: baseline ----------------------------------------------------------
# rate of change at year t is predicted by year t-1 

# TS1 
# train
y.trn.difficult.ts1 <- trn.difficult.ts1$F.pc.13.14
# calculate MSE
mse.y.trn.difficult.ts1 <- mean((trn.difficult.ts1$F.pc.14.15 - y.trn.difficult.ts1 )^2)

# test 
y.tst.difficult.ts1 <- tst.difficult.ts1$F.pc.13.14
# calculate MSE
mse.y.tst.difficult.ts1 <- mean((tst.difficult.ts1$F.pc.14.15 - y.tst.difficult.ts1 )^2)

# TS2 
# train
y.trn.difficult.ts2 <- trn.difficult.ts2$F.pc.14.15
# calculate MSE
mse.y.trn.difficult.ts2 <- mean((trn.difficult.ts2$F.pc.15.16 - y.trn.difficult.ts2 )^2)

# test 
y.tst.difficult.ts2 <- tst.difficult.ts2$F.pc.14.15
# calculate MSE
mse.y.tst.difficult.ts2 <- mean((tst.difficult.ts2$F.pc.15.16 - y.tst.difficult.ts2 )^2)

# TS3 
# train
y.trn.difficult.ts3 <- trn.difficult.ts3$F.pc.15.16
# calculate MSE
mse.y.trn.difficult.ts3 <- mean((trn.difficult.ts3$F.pc.16.17 - y.trn.difficult.ts3 )^2)

# test 
y.tst.difficult.ts3 <- tst.difficult.ts3$F.pc.15.16
# calculate MSE
mse.y.tst.difficult.ts3 <- mean((tst.difficult.ts3$F.pc.16.17 - y.tst.difficult.ts3 )^2)


base_MSE_trn_difficult <- c(mse.y.trn.difficult.ts1, mse.y.trn.difficult.ts2, mse.y.trn.difficult.ts3)
base_MSE_tst_difficult <- c(mse.y.tst.difficult.ts1, mse.y.tst.difficult.ts2, mse.y.tst.difficult.ts3)

e3_error <- data.frame(base_MSE_trn_difficult, base_MSE_tst_difficult)

# variables definition ---------------------------------------------------------
# TS1 
x_trn_difficult_ts1 <- trn.difficult.ts1[,10:25]
x_tst_difficult_ts1 <- tst.difficult.ts1[,10:25]

# TS2 
x_trn_difficult_ts2 <- trn.difficult.ts2[,10:25]
x_tst_difficult_ts2 <- tst.difficult.ts2[,10:25]

# TS3 
x_trn_difficult_ts3 <- trn.difficult.ts3[,10:25]
x_tst_difficult_ts3 <- tst.difficult.ts3[,10:25]

## Elastic net difficult -------------------------------------------------------------
mod.ctrl <- trainControl(method = "repeatedcv", number = 10, verboseIter = T)

# TS 1 
# Train 
trn.difficult.ts1.Enet <- train( F.pc.14.15 ~ .,
                                 x_trn_difficult_ts1,
                                 method = 'glmnet',
                                 tuneGrid = expand.grid(alpha = 0.5,
                                                        lambda = seq(0.00001, 10000, length = 1000)),
                                 trControl = mod.ctrl)

mse.trn.difficult.ts1.Enet <- min((trn.difficult.ts1.Enet$results$RMSE)^2)

# test
pred.difficult.ts1.Enet <- predict(trn.difficult.ts1.Enet, x_tst_difficult_ts1)
mse.tst.difficult.ts1.Enet <- mean((x_tst_difficult_ts1$F.pc.14.15 - pred.difficult.ts1.Enet)^2)

# TS 2 
# Train 
trn.difficult.ts2.Enet <- train( F.pc.15.16 ~ .,
                                 x_trn_difficult_ts2,
                                 method = 'glmnet',
                                 tuneGrid = expand.grid(alpha = 0.5,
                                                        lambda = seq(0.00001, 10000, length = 1000)),
                                 trControl = mod.ctrl)

mse.trn.difficult.ts2.Enet <- min((trn.difficult.ts2.Enet$results$RMSE)^2)

# test
pred.difficult.ts2.Enet <- predict(trn.difficult.ts2.Enet, x_tst_difficult_ts2)
mse.tst.difficult.ts2.Enet <- mean((x_tst_difficult_ts2$F.pc.15.16 - pred.difficult.ts2.Enet)^2)

# TS 3 
# Train 
trn.difficult.ts3.Enet <- train( F.pc.16.17 ~ .,
                                 x_trn_difficult_ts3,
                                 method = 'glmnet',
                                 tuneGrid = expand.grid(alpha = 0.5,
                                                        lambda = seq(0.00001, 10000, length = 1000)),
                                 trControl = mod.ctrl)

mse.trn.difficult.ts3.Enet <- min((trn.difficult.ts3.Enet$results$RMSE)^2)

# test
pred.difficult.ts3.Enet <- predict(trn.difficult.ts3.Enet, x_tst_difficult_ts3)
mse.tst.difficult.ts3.Enet <- mean((x_tst_difficult_ts3$F.pc.16.17 - pred.difficult.ts3.Enet)^2)

# summarize error 
enet_MSE_trn_difficult <- c(mse.trn.difficult.ts1.Enet, mse.trn.difficult.ts2.Enet, mse.trn.difficult.ts3.Enet)
enet_MSE_tst_difficult <- c(mse.tst.difficult.ts1.Enet, mse.tst.difficult.ts2.Enet, mse.tst.difficult.ts3.Enet)

e3_error <- data.frame(e3_error, enet_MSE_trn_difficult, enet_MSE_tst_difficult )

## Random Forest: difficult ----------------------------------------------------------
# TS1
# train
trn.difficult.ts1.RF = randomForest(F.pc.14.15 ~ . , data = x_trn_difficult_ts1, ntree = 50, 
                                    cv.fold = 10)
mse.trn.difficult.ts1.RF <- min(trn.difficult.ts1.RF$mse)

# predict
pred.difficult.ts1.RF <- predict(trn.difficult.ts1.RF, x_tst_difficult_ts1)
mse.tst.difficult.ts1.RF <- mean((x_tst_difficult_ts1$F.pc.14.15 - pred.difficult.ts1.RF)^2)

# TS2
# train
trn.difficult.ts2.RF = randomForest(F.pc.15.16 ~ . , data = x_trn_difficult_ts2, ntree = 50, 
                                    cv.fold = 10)
mse.trn.difficult.ts2.RF <- min(trn.difficult.ts2.RF$mse)

# predict
pred.difficult.ts2.RF <- predict(trn.difficult.ts2.RF, x_tst_difficult_ts2)
mse.tst.difficult.ts2.RF <- mean((x_tst_difficult_ts2$F.pc.15.16 - pred.difficult.ts2.RF)^2)

# TS3
# train
trn.difficult.ts3.RF = randomForest(F.pc.16.17 ~ . , data = x_trn_difficult_ts3, ntree = 50, 
                                    cv.fold = 10)
mse.trn.difficult.ts3.RF <- min(trn.difficult.ts3.RF$mse)

# predict
pred.difficult.ts3.RF <- predict(trn.difficult.ts3.RF, x_tst_difficult_ts3)
mse.tst.difficult.ts3.RF <- mean((x_tst_difficult_ts3$F.pc.16.17 - pred.difficult.ts3.RF)^2)

# summarize error 
RF_MSE_trn_difficult <- c(mse.trn.difficult.ts1.RF, mse.trn.difficult.ts2.RF, mse.trn.difficult.ts3.RF)
RF_MSE_tst_difficult <- c(mse.tst.difficult.ts1.RF, mse.tst.difficult.ts2.RF, mse.tst.difficult.ts3.RF)

e3_error <- data.frame(e3_error, RF_MSE_trn_difficult, RF_MSE_tst_difficult )

pred.difficult.rf <- data.frame(pred.difficult.ts1.RF, pred.difficult.ts2.RF,
                                pred.difficult.ts3.RF)

## SVM: difficult --------------------------------------------------------------------
# TS1 
# train
trn.difficult.ts1.SVM <- svm(F.pc.14.15 ~ . , data = x_trn_difficult_ts1, kernel = "radial", 
                             cost = 1, gamma = 0.1, epsilon = 0.1, cv.fold = 10)

mse.trn.difficult.ts1.SVM <- sqrt(mean(trn.difficult.ts1.SVM $residuals^2))

# test
pred.trn.difficult.ts1.SVM <- predict(trn.difficult.ts1.SVM, x_tst_difficult_ts1)
mse.tst.difficult.ts1.SVM <- mean((x_tst_difficult_ts1$F.pc.14.15 - pred.trn.difficult.ts1.SVM )^2)

# TS2 
# train
trn.difficult.ts2.SVM <- svm(F.pc.15.16 ~ . , data = x_trn_difficult_ts2, kernel = "radial", 
                             cost = 1, gamma = 0.1, epsilon = 0.1, cv.fold = 10)

mse.trn.difficult.ts2.SVM <- sqrt(mean(trn.difficult.ts2.SVM $residuals^2))

# test
pred.trn.difficult.ts2.SVM <- predict(trn.difficult.ts2.SVM, x_tst_difficult_ts2)
mse.tst.difficult.ts2.SVM <- mean((x_tst_difficult_ts2$F.pc.15.16 - pred.trn.difficult.ts2.SVM )^2)

# TS3 
# train
trn.difficult.ts3.SVM <- svm(F.pc.16.17 ~ . , data = x_trn_difficult_ts3, kernel = "radial", 
                             cost = 1, gamma = 0.1, epsilon = 0.1, cv.fold = 10)

mse.trn.difficult.ts3.SVM <- sqrt(mean(trn.difficult.ts3.SVM $residuals^2))

# test
pred.trn.difficult.ts3.SVM <- predict(trn.difficult.ts3.SVM, x_tst_difficult_ts3)
mse.tst.difficult.ts3.SVM <- mean((x_tst_difficult_ts3$F.pc.16.17 - pred.trn.difficult.ts3.SVM )^2)

# summarize error 
SVM_MSE_trn_difficult <- c(mse.trn.difficult.ts1.SVM, mse.trn.difficult.ts2.SVM, mse.trn.difficult.ts3.SVM)
SVM_MSE_tst_difficult <- c(mse.tst.difficult.ts1.SVM, mse.tst.difficult.ts2.SVM, mse.tst.difficult.ts3.SVM)

e3_error_difficult <- data.frame(e3_error, SVM_MSE_trn_difficult, SVM_MSE_tst_difficult )

## Very difficult --------------------------------------------------------------
## modeling: Vdifficult to access
## modeling: baseline ----------------------------------------------------------
# rate of change at year t is predicted by year t-1 

# TS1 
# train
y.trn.Vdifficult.ts1 <- trn.Vdifficult.ts1$F.pc.13.14
# calculate MSE
mse.y.trn.Vdifficult.ts1 <- mean((trn.Vdifficult.ts1$F.pc.14.15 - y.trn.Vdifficult.ts1 )^2)

# test 
y.tst.Vdifficult.ts1 <- tst.Vdifficult.ts1$F.pc.13.14
# calculate MSE
mse.y.tst.Vdifficult.ts1 <- mean((tst.Vdifficult.ts1$F.pc.14.15 - y.tst.Vdifficult.ts1 )^2)

# TS2 
# train
y.trn.Vdifficult.ts2 <- trn.Vdifficult.ts2$F.pc.14.15
# calculate MSE
mse.y.trn.Vdifficult.ts2 <- mean((trn.Vdifficult.ts2$F.pc.15.16 - y.trn.Vdifficult.ts2 )^2)

# test 
y.tst.Vdifficult.ts2 <- tst.Vdifficult.ts2$F.pc.14.15
# calculate MSE
mse.y.tst.Vdifficult.ts2 <- mean((tst.Vdifficult.ts2$F.pc.15.16 - y.tst.Vdifficult.ts2 )^2)

# TS3 
# train
y.trn.Vdifficult.ts3 <- trn.Vdifficult.ts3$F.pc.15.16
# calculate MSE
mse.y.trn.Vdifficult.ts3 <- mean((trn.Vdifficult.ts3$F.pc.16.17 - y.trn.Vdifficult.ts3 )^2)

# test 
y.tst.Vdifficult.ts3 <- tst.Vdifficult.ts3$F.pc.15.16
# calculate MSE
mse.y.tst.Vdifficult.ts3 <- mean((tst.Vdifficult.ts3$F.pc.16.17 - y.tst.Vdifficult.ts3 )^2)


base_MSE_trn_Vdifficult <- c(mse.y.trn.Vdifficult.ts1, mse.y.trn.Vdifficult.ts2, mse.y.trn.Vdifficult.ts3)
base_MSE_tst_Vdifficult <- c(mse.y.tst.Vdifficult.ts1, mse.y.tst.Vdifficult.ts2, mse.y.tst.Vdifficult.ts3)

e3_error <- data.frame(base_MSE_trn_Vdifficult, base_MSE_tst_Vdifficult)

# variables definition ---------------------------------------------------------
# TS1 
x_trn_Vdifficult_ts1 <- trn.Vdifficult.ts1[,10:25]
x_tst_Vdifficult_ts1 <- tst.Vdifficult.ts1[,10:25]

# TS2 
x_trn_Vdifficult_ts2 <- trn.Vdifficult.ts2[,10:25]
x_tst_Vdifficult_ts2 <- tst.Vdifficult.ts2[,10:25]

# TS3 
x_trn_Vdifficult_ts3 <- trn.Vdifficult.ts3[,10:25]
x_tst_Vdifficult_ts3 <- tst.Vdifficult.ts3[,10:25]

## Elastic net Vdifficult -------------------------------------------------------------
mod.ctrl <- trainControl(method = "repeatedcv", number = 10, verboseIter = T)

# TS 1 
# Train 
trn.Vdifficult.ts1.Enet <- train( F.pc.14.15 ~ .,
                                  x_trn_Vdifficult_ts1,
                                  method = 'glmnet',
                                  tuneGrid = expand.grid(alpha = 0.5,
                                                         lambda = seq(0.00001, 10000, length = 1000)),
                                  trControl = mod.ctrl)

mse.trn.Vdifficult.ts1.Enet <- min((trn.Vdifficult.ts1.Enet$results$RMSE)^2)

# test
pred.Vdifficult.ts1.Enet <- predict(trn.Vdifficult.ts1.Enet, x_tst_Vdifficult_ts1)
mse.tst.Vdifficult.ts1.Enet <- mean((x_tst_Vdifficult_ts1$F.pc.14.15 - pred.Vdifficult.ts1.Enet)^2)

# TS 2 
# Train 
trn.Vdifficult.ts2.Enet <- train( F.pc.15.16 ~ .,
                                  x_trn_Vdifficult_ts2,
                                  method = 'glmnet',
                                  tuneGrid = expand.grid(alpha = 0.5,
                                                         lambda = seq(0.00001, 10000, length = 1000)),
                                  trControl = mod.ctrl)

mse.trn.Vdifficult.ts2.Enet <- min((trn.Vdifficult.ts2.Enet$results$RMSE)^2)

# test
pred.Vdifficult.ts2.Enet <- predict(trn.Vdifficult.ts2.Enet, x_tst_Vdifficult_ts2)
mse.tst.Vdifficult.ts2.Enet <- mean((x_tst_Vdifficult_ts2$F.pc.15.16 - pred.Vdifficult.ts2.Enet)^2)

# TS 3 
# Train 
trn.Vdifficult.ts3.Enet <- train( F.pc.16.17 ~ .,
                                  x_trn_Vdifficult_ts3,
                                  method = 'glmnet',
                                  tuneGrid = expand.grid(alpha = 0.5,
                                                         lambda = seq(0.00001, 10000, length = 1000)),
                                  trControl = mod.ctrl)

mse.trn.Vdifficult.ts3.Enet <- min((trn.Vdifficult.ts3.Enet$results$RMSE)^2)

# test
pred.Vdifficult.ts3.Enet <- predict(trn.Vdifficult.ts3.Enet, x_tst_Vdifficult_ts3)
mse.tst.Vdifficult.ts3.Enet <- mean((x_tst_Vdifficult_ts3$F.pc.16.17 - pred.Vdifficult.ts3.Enet)^2)

# summarize error 
enet_MSE_trn_Vdifficult <- c(mse.trn.Vdifficult.ts1.Enet, mse.trn.Vdifficult.ts2.Enet, mse.trn.Vdifficult.ts3.Enet)
enet_MSE_tst_Vdifficult <- c(mse.tst.Vdifficult.ts1.Enet, mse.tst.Vdifficult.ts2.Enet, mse.tst.Vdifficult.ts3.Enet)

e3_error <- data.frame(e3_error, enet_MSE_trn_Vdifficult, enet_MSE_tst_Vdifficult )

## Random Forest: Vdifficult ----------------------------------------------------------
# TS1
# train
trn.Vdifficult.ts1.RF = randomForest(F.pc.14.15 ~ . , data = x_trn_Vdifficult_ts1, ntree = 50, 
                                     cv.fold = 10)
mse.trn.Vdifficult.ts1.RF <- min(trn.Vdifficult.ts1.RF$mse)

# predict
pred.Vdifficult.ts1.RF <- predict(trn.Vdifficult.ts1.RF, x_tst_Vdifficult_ts1)
mse.tst.Vdifficult.ts1.RF <- mean((x_tst_Vdifficult_ts1$F.pc.14.15 - pred.Vdifficult.ts1.RF)^2)

# TS2
# train
trn.Vdifficult.ts2.RF = randomForest(F.pc.15.16 ~ . , data = x_trn_Vdifficult_ts2, ntree = 50, 
                                     cv.fold = 10)
mse.trn.Vdifficult.ts2.RF <- min(trn.Vdifficult.ts2.RF$mse)

# predict
pred.Vdifficult.ts2.RF <- predict(trn.Vdifficult.ts2.RF, x_tst_Vdifficult_ts2)
mse.tst.Vdifficult.ts2.RF <- mean((x_tst_Vdifficult_ts2$F.pc.15.16 - pred.Vdifficult.ts2.RF)^2)

# TS3
# train
trn.Vdifficult.ts3.RF = randomForest(F.pc.16.17 ~ . , data = x_trn_Vdifficult_ts3, ntree = 50, 
                                     cv.fold = 10)
mse.trn.Vdifficult.ts3.RF <- min(trn.Vdifficult.ts3.RF$mse)

# predict
pred.Vdifficult.ts3.RF <- predict(trn.Vdifficult.ts3.RF, x_tst_Vdifficult_ts3)
mse.tst.Vdifficult.ts3.RF <- mean((x_tst_Vdifficult_ts3$F.pc.16.17 - pred.Vdifficult.ts3.RF)^2)

# summarize error 
RF_MSE_trn_Vdifficult <- c(mse.trn.Vdifficult.ts1.RF, mse.trn.Vdifficult.ts2.RF, mse.trn.Vdifficult.ts3.RF)
RF_MSE_tst_Vdifficult <- c(mse.tst.Vdifficult.ts1.RF, mse.tst.Vdifficult.ts2.RF, mse.tst.Vdifficult.ts3.RF)

e3_error <- data.frame(e3_error, RF_MSE_trn_Vdifficult, RF_MSE_tst_Vdifficult )

## SVM: Vdifficult --------------------------------------------------------------------
# TS1 
# train
trn.Vdifficult.ts1.SVM <- svm(F.pc.14.15 ~ . , data = x_trn_Vdifficult_ts1, kernel = "radial", 
                              cost = 1, gamma = 0.1, epsilon = 0.1, cv.fold = 10)

mse.trn.Vdifficult.ts1.SVM <- sqrt(mean(trn.Vdifficult.ts1.SVM $residuals^2))

# test
pred.trn.Vdifficult.ts1.SVM <- predict(trn.Vdifficult.ts1.SVM, x_tst_Vdifficult_ts1)
mse.tst.Vdifficult.ts1.SVM <- mean((x_tst_Vdifficult_ts1$F.pc.14.15 - pred.trn.Vdifficult.ts1.SVM )^2)

# TS2 
# train
trn.Vdifficult.ts2.SVM <- svm(F.pc.15.16 ~ . , data = x_trn_Vdifficult_ts2, kernel = "radial", 
                              cost = 1, gamma = 0.1, epsilon = 0.1, cv.fold = 10)

mse.trn.Vdifficult.ts2.SVM <- sqrt(mean(trn.Vdifficult.ts2.SVM $residuals^2))

# test
pred.trn.Vdifficult.ts2.SVM <- predict(trn.Vdifficult.ts2.SVM, x_tst_Vdifficult_ts2)
mse.tst.Vdifficult.ts2.SVM <- mean((x_tst_Vdifficult_ts2$F.pc.15.16 - pred.trn.Vdifficult.ts2.SVM )^2)

# TS3 
# train
trn.Vdifficult.ts3.SVM <- svm(F.pc.16.17 ~ . , data = x_trn_Vdifficult_ts3, kernel = "radial", 
                              cost = 1, gamma = 0.1, epsilon = 0.1, cv.fold = 10)

mse.trn.Vdifficult.ts3.SVM <- sqrt(mean(trn.Vdifficult.ts3.SVM $residuals^2))

# test
pred.trn.Vdifficult.ts3.SVM <- predict(trn.Vdifficult.ts3.SVM, x_tst_Vdifficult_ts3)
mse.tst.Vdifficult.ts3.SVM <- mean((x_tst_Vdifficult_ts3$F.pc.16.17 - pred.trn.Vdifficult.ts3.SVM )^2)

# summarize error 
SVM_MSE_trn_Vdifficult <- c(mse.trn.Vdifficult.ts1.SVM, mse.trn.Vdifficult.ts2.SVM, mse.trn.Vdifficult.ts3.SVM)
SVM_MSE_tst_Vdifficult <- c(mse.tst.Vdifficult.ts1.SVM, mse.tst.Vdifficult.ts2.SVM, mse.tst.Vdifficult.ts3.SVM)

e3_error_Vdifficult <- data.frame(e3_error, SVM_MSE_trn_Vdifficult, SVM_MSE_tst_Vdifficult )

