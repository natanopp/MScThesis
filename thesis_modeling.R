## modeling script
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

## load datasets ---------------------------------------------------------------
tourist <- read.csv("input/tourist_dat.csv", stringsAsFactors = FALSE)
 # calculate relative change for each year 
 # function precentage change 
 calc_perchg <- function(data, scol, ecol) {
   TS_perchg <-c()
   for(i in 1:nrow(data)) {
     TS_perchg[i] <- (data[i, ecol] - data[i, scol])/data[i, scol]
   }
   return (TS_perchg)
 }

 # 13-14
 tourist$F.pc.13.14 <- calc_perchg(tourist, 7, 8)
 plot(tourist$F.pc.13.14)

 # 14-15
 tourist$F.pc.14.15 <- calc_perchg(tourist, 8, 9)
 plot(tourist$F.pc.14.15) 

 # 15-16
 tourist$F.pc.15.16 <- calc_perchg(tourist, 9, 10)
 plot(tourist$F.pc.15.16) 

 # 16-17 
 tourist$F.pc.16.17 <- calc_perchg(tourist, 10, 11)
 plot(tourist$F.pc.16.17)

 #take outliers off (more than 400%)
 tourist <- tourist %>%
   filter(F.pc.13.14 <= 4)
 
 tourist <- tourist %>%
   filter(F.pc.14.15 <= 4)

 tourist <- tourist %>%
   filter(F.pc.15.16 <= 4)
 
 tourist <- tourist %>%
   filter(F.pc.16.17 <= 4)

# gdelt dataset  
gdelt <- read.csv("input/GDELT_dat.csv", stringsAsFactors = FALSE)

  # 13-14
  gdelt$T.pc.13.14 <- calc_perchg(gdelt, 5, 6)
  plot(gdelt$T.pc.13.14)

  # 14-15
  gdelt$T.pc.14.15 <- calc_perchg(gdelt, 6, 7)

  # 15-16
  gdelt$T.pc.15.16 <- calc_perchg(gdelt, 7, 8)
 
  # 16-17 
  gdelt$T.pc.16.17 <- calc_perchg(gdelt, 8, 9)


income <- read.csv("input/income_level.csv", stringsAsFactors = FALSE)
income <- income[ ,1:7]
visa <- read.csv("input/passport-index-matrix-iso3.csv", stringsAsFactors = FALSE)

## join dataset ----------------------------------------------------------------
working_dat <- inner_join(tourist[ ,2:45] , gdelt[ ,c(-1, -3, -4)] , by = "dyad")
working_dat$F2017 <- as.numeric(working_dat$F2017)
  #9124 instances

write.csv(working_dat, file = "model.dat.csv")

## shotcut: load the completed data here --------------------------------------------
working_dat <- read.csv("input/experiment_data.csv", stringsAsFactors = FALSE)
#working_dat <- working_dat[,-1]
working_dat$F2017 <- as.numeric(working_dat$F2017)

## experiment 1: tone predict percentage of change in flow ---------------------
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

## model 0 baseline ------------------------------------------------------------
  # rate of change at year t is predicted by year t-1 
# TS1 
  # train
  y_trn1.1.m0 <- trn.1.1$F.pc.13.14
  # calculate MSE
  mse_trn1.1.m0 <- mean((trn.1.1$F.pc.14.15 - y_trn1.1.m0)^2)

  # test 
  y_tst1.1.m0 <- tst.1.1$F.pc.13.14
  # error per row
  se_row_tst1.1.m0 <- ((tst.1.1$F.pc.14.15 - y_tst1.1.m0)^2)
  # calculate MSE
  mse_tst1.1.m0 <- mean((tst.1.1$F.pc.14.15 - y_tst1.1.m0)^2)

# TS2 
  # train
  y_trn1.2.m0 <- trn.1.2$F.pc.14.15
  # calculate MSE
  mse_trn1.2.m0 <- mean((trn.1.2$F.pc.15.16 - y_trn1.2.m0)^2)
  
  # test 
  y_tst1.2.m0 <- tst.1.2$F.pc.14.15
  # error per row
  se_row_tst1.2.m0 <- ((tst.1.2$F.pc.15.16 - y_tst1.2.m0)^2)
  # calculate MSE
  mse_tst1.2.m0 <- mean((tst.1.2$F.pc.15.16 - y_tst1.2.m0)^2)
  
# TS3 
  # train
  y_trn1.3.m0 <- trn.1.3$F.pc.15.16
  # calculate MSE
  mse_trn1.3.m0 <- mean((trn.1.3$F.pc.16.17 - y_trn1.3.m0)^2)
  
  # test 
  y_tst1.3.m0 <- tst.1.3$F.pc.15.16
  # error per row
  se_row_tst1.3.m0 <- ((tst.1.3$F.pc.16.17 - y_tst1.3.m0)^2)
  # calculate MSE
  mse_tst1.3.m0 <- mean((tst.1.3$F.pc.16.17 - y_tst1.3.m0)^2)
  
  base_MSE_trn <- c(mse_trn1.1.m0, mse_trn1.2.m0, mse_trn1.3.m0)
  base_MSE_tst <- c(mse_tst1.1.m0, mse_tst1.2.m0, mse_tst1.3.m0)
  
  e1_error <- data.frame(base_MSE_trn, base_MSE_tst)
  
  # record predict value, actual value, error rate, destination, and origin to 
  # the test set 
  e1.tst.err <- data.frame(test_e1$Destination_code, test_e1$Origin_code,
                           se_row_tst1.1.m0, se_row_tst1.2.m0, se_row_tst1.3.m0)
  e1.tst.err$base.err <- (e1.tst.err[,3] + e1.tst.err[,4] + e1.tst.err[,5])/3
  
  e1.tst.dyad <- data.frame(test_e1$Destination_code, test_e1$Origin_code)
                             

  

## model 1 lasso ---------------------------------------------------------------
  # define train control
  mod.ctrl <- trainControl(method = "repeatedcv", number = 10, verboseIter = T)
  
  # TS1 - train
  trn.1.1.m1 <- train( F.pc.14.15 ~ .,
                       trn.1.1,
                       method = 'glmnet',
                       tuneGrid = expand.grid(alpha = 1,
                                  lambda = seq(0.00001, 1000, length = 1000)),
                       trControl = mod.ctrl)
  plot(trn.1.1.m1)
  plot(trn.1.1.m1$finalModel, xvar = "lambda", label = T)
  plot(varImp(trn.1.1.m1, scale = F))
  mse_trn1.1.m1 <- min((trn.1.1.m1$results$RMSE)^0.5)
  
  # TS1 - predict
  pred.1.1.m1 <- predict(trn.1.1.m1, tst.1.1)
  MSE.tst.1.1.m1 <- mean((tst.1.1$F.pc.14.15 - pred.1.1.m1)^2)
  
  # TS2 - train
  trn.1.2.m1 <- train( F.pc.15.16 ~ .,
                          trn.1.2,
                          method = 'glmnet',
                          tuneGrid = expand.grid(alpha = 1,
                                     lambda = seq(0.00001, 1000, length = 1000)),
                          trControl = mod.ctrl)
  mse_trn1.2.m1 <- min((trn.1.2.m1$results$RMSE)^0.5)
  
  # TS2 - predict
  pred.1.2.m1 <- predict(trn.1.2.m1, tst.1.2)
  MSE.tst.1.2.m1 <- mean((tst.1.2$F.pc.15.16 - pred.1.2.m1)^2)
  
  # TS3 - train
  trn.1.3.m1 <- train( F.pc.16.17 ~ .,
                       trn.1.3,
                       method = 'glmnet',
                       tuneGrid = expand.grid(alpha = 1,
                                  lambda = seq(0.00001, 1000, length = 1000)),
                       trControl = mod.ctrl)
  mse_trn1.3.m1 <- min((trn.1.3.m1$results$RMSE)^0.5)
  
  # TS3 - predict
  pred.1.3.m1 <- predict(trn.1.3.m1, tst.1.3)
  MSE.tst.1.3.m1 <- mean((tst.1.3$F.pc.16.17 - pred.1.3.m1)^2)

## model 2 ridge ---------------------------------------------------------------
  # define train control
  mod.ctrl <- trainControl(method = "repeatedcv", number = 10, verboseIter = T)
  
  # TS1 - train
  trn.1.1.m2 <- train( F.pc.14.15 ~ .,
                       trn.1.1,
                       method = 'glmnet',
                       tuneGrid = expand.grid(alpha = 0,
                                  lambda = seq(0.00001, 1000, length = 1000)),
                       trControl = mod.ctrl)
  plot(trn.1.1.m2)
  plot(trn.1.1.m2$finalModel, xvar = "lambda", label = T)
  plot(varImp(trn.1.1.m2, scale = F))
  mse_trn1.1.m2 <- min((trn.1.1.m2$results$RMSE)^0.5)
  
  # TS1 - predict
  pred.1.1.m2 <- predict(trn.1.1.m2, tst.1.1)
  MSE.tst.1.1.m2 <- mean((tst.1.1$F.pc.14.15 - pred.1.1.m2)^2)
  
  # TS2 - train
  trn.1.2.m2 <- train( F.pc.15.16 ~ .,
                       trn.1.2,
                       method = 'glmnet',
                       tuneGrid = expand.grid(alpha = 0,
                                  lambda = seq(0.00001, 1000, length = 1000)),
                       trControl = mod.ctrl)
  mse_trn1.2.m2 <- min((trn.1.2.m2$results$RMSE)^0.5)
  
  # TS2 - predict
  pred.1.2.m2 <- predict(trn.1.2.m1, tst.1.2)
  MSE.tst.1.2.m2 <- mean((tst.1.2$F.pc.15.16 - pred.1.2.m2)^2)
  
  # TS3 - train
  trn.1.3.m2 <- train( F.pc.16.17 ~ .,
                       trn.1.3,
                       method = 'glmnet',
                       tuneGrid = expand.grid(alpha = 0,
                                  lambda = seq(0.00001, 1000, length = 1000)),
                       trControl = mod.ctrl)
  mse_trn1.3.m2 <- min((trn.1.3.m2$results$RMSE)^0.5)
  
  # TS3 - predict
  pred.1.3.m2 <- predict(trn.1.3.m2, tst.1.3)
  MSE.tst.1.3.m2 <- mean((tst.1.3$F.pc.16.17 - pred.1.3.m2)^2)
  
    
## model 3 elasticnet ----------------------------------------------------------
  # define train control
  mod.ctrl <- trainControl(method = "cv", number = 10, verboseIter = T)
  
  # TS1 - train
  trn.1.1.m3 <- train( F.pc.14.15 ~ .,
                       trn.1.1,
                       method = 'glmnet',
                       tuneGrid = expand.grid(alpha = seq(0, 1, 0.05),
                                  lambda = seq(0.00001, 10000, length = 1000)),
                       trControl = mod.ctrl)
  plot(trn.1.1.m3)
  plot(trn.1.1.m3$finalModel, xvar = "lambda", label = T)
  plot(varImp(trn.1.1.m3, scale = F))
  mse_trn1.1.m3_a <- min((trn.1.1.m3$results$RMSE)^2)
  
  
  trn.LM.ts1.Enet <- train( F.pc.14.15 ~ .,
                            x_trn_LM_ts1,
                            method = 'glmnet',
                            lambda = 10000,
                            trControl = mod.ctrl)
  
  # New code train enet ts1 - tunelength of 20 
  trn.1.1.m3.new <- train( F.pc.14.15 ~ .,
                       trn.1.1,
                       method = 'glmnet',
                       #tuneGrid = expand.grid(alpha = seq(0, 1, 0.05),
                                              #lambda = seq(0.00001, 10000, length = 1000)),
                       tuneLength = 20,
                       trControl = mod.ctrl)
  
  plot(trn.1.1.m3.new)
  plot(trn.1.1.m3.new$finalModel, xvar = "lambda", label = T)
  plot(varImp(trn.1.1.m3.new, scale = F))
  
  # New code 2 train enet ts1
  trn.1.1.m3.new2 <- train( F.pc.14.15 ~ .,
                           trn.1.1,
                           method = 'glmnet',
                           tuneGrid = expand.grid(alpha = seq(0, 1, 0.05),
                           lambda = seq(0.00001, 10000, length = 1000)),
                           trControl = mod.ctrl)
  
  plot(trn.1.1.m3.new2)
  plot(trn.1.1.m3.new2$finalModel, xvar = "lambda", label = T)
  plot(varImp(trn.1.1.m3.new2, scale = F))
  
  
  # TS1 - predict
  pred.1.1.m3 <- predict(trn.1.1.m3, tst.1.1)
  se.row.tst1.1.m3 <- (tst.1.1$F.pc.14.15 - pred.1.1.m3)^2 
  mse.tst1.1.m3 <- mean((tst.1.1$F.pc.14.15 - pred.1.1.m3)^2)
  
  #new code predict enet ts1
  pred.1.1.m3.new <- predict(trn.1.1.m3.new, tst.1.1)
  se.row.tst1.1.m3.new <- (tst.1.1$F.pc.14.15 - pred.1.1.m3.new)^2 
  mse.tst1.1.m3.new <- mean((tst.1.1$F.pc.14.15 - pred.1.1.m3.new)^2)
  
  # TS2 - train
  trn.1.2.m3 <- train( F.pc.15.16 ~ .,
                       trn.1.2,
                       method = 'glmnet',
                       tuneGrid = expand.grid(alpha = 0.5,
                                  lambda = seq(0.00001, 10000, length = 1000)),
                       trControl = mod.ctrl)
  mse_trn1.2.m3 <- min((trn.1.2.m3$results$RMSE)^2)
  
  # TS2 - predict
  pred.1.2.m3 <- predict(trn.1.2.m3, tst.1.2)
  se.row.tst1.2.m3 <- (tst.1.2$F.pc.15.16 - pred.1.2.m3)^2 
  mse.tst1.2.m3 <- mean((tst.1.2$F.pc.15.16 - pred.1.2.m3)^2)
  
  # TS3 - train
  trn.1.3.m3 <- train( F.pc.16.17 ~ .,
                       trn.1.3,
                       method = 'glmnet',
                       tuneGrid = expand.grid(alpha = 0.5,
                                              lambda = seq(0.00001, 10000, length = 1000)),
                       trControl = mod.ctrl)
  mse_trn1.3.m3 <- min((trn.1.3.m3$results$RMSE)^2)
  
  # TS3 - predict
  pred.1.3.m3 <- predict(trn.1.3.m3, tst.1.3)
  se.row.tst1.3.m3 <- (tst.1.3$F.pc.16.17 - pred.1.3.m3)^2 
  mse.tst1.3.m3 <- mean((tst.1.3$F.pc.16.17 - pred.1.3.m3)^2)
  
  enet_MSE_trn <- c(mse_trn1.1.m3, mse_trn1.2.m3, mse_trn1.3.m3)
  enet_MSE_tst <- c(mse.tst1.1.m3, mse.tst1.2.m3, mse.tst1.3.m3)
  
  e1_error <- data.frame(e1_error, enet_MSE_trn, enet_MSE_tst)
  
  e1.tst.err <- data.frame(e1.tst.err,
                           se.row.tst1.1.m3, se.row.tst1.2.m3, se.row.tst1.3.m3)
  e1.tst.err$enet.err <- (e1.tst.err[,7] + e1.tst.err[,8] + e1.tst.err[,9])/3

  #collect the actual values 
  e1.enet.values <- data.frame(pred.1.1.m3, pred.1.2.m3, pred.1.3.m3)
  write.csv(e1.enet.values, file = "e1_enet.csv")
  
##model 4 Random forest  -------------------------------------------------------
  # TS1
  # train
  trn.1.1.m4 = randomForest(F.pc.14.15 ~ . , data = trn.1.1, ntree = 50, 
                            cv.fold = 10)
  mse_trn1.1.m4 <- min(trn.1.1.m4$mse)
  
  # predict
  pred.1.1.m4 <- predict(trn.1.1.m4, tst.1.1)
  mse.tst1.1.m4 <- mean((tst.1.1$F.pc.14.15 - pred.1.1.m4)^2)
  
  #importance
  varImpPlot(trn.1.1.m4)
  
  # TS2
  trn.1.2.m4 = randomForest(F.pc.15.16 ~ . , data = trn.1.2, ntree = 50, 
                            cv.fold = 10)
  mse_trn1.2.m4 <- min(trn.1.2.m4$mse)
  
  # predict
  pred.1.2.m4 <- predict(trn.1.2.m4, tst.1.2)
  mse.tst1.2.m4 <- mean((tst.1.2$F.pc.15.16 - pred.1.2.m4)^2)

  # TS3
  trn.1.3.m4 = randomForest(F.pc.16.17 ~ . , data = trn.1.3, ntree = 50, 
                            cv.fold = 10)
  mse_trn1.3.m4 <- min(trn.1.3.m4$mse)
  
  # predict
  pred.1.3.m4 <- predict(trn.1.3.m4, tst.1.3)
  mse.tst1.3.m4 <- mean((tst.1.3$F.pc.16.17 - pred.1.3.m4)^2)
  
  rf_MSE_trn <- c(mse_trn1.1.m4, mse_trn1.2.m4, mse_trn1.3.m4)
  rf_MSE_tst <- c(mse.tst1.1.m4, mse.tst1.2.m4, mse.tst1.3.m4)
  
  e1_error <- data.frame(e1_error, rf_MSE_trn, rf_MSE_tst)
  
# collect Decrease Node Purity
RF.IMP.EX1 <- cbind.data.frame(importance(trn.1.1.m4, type = 2),
                               importance(trn.1.2.m4, type = 2),
                               importance(trn.1.3.m4, type = 2)) 
RF.IMP.EX1$MeanPurity <- (RF.IMP.EX1[,1] + RF.IMP.EX1[,2] + 
                          RF.IMP.EX1[,1]) /3

row = c("St-2", "St-1", "St", "Smean", "Smed",
        "Smax", "Smin", "Sr", "Sdiff", "Ssd", "Svar",
        "Ssl", "Spc, t-2, t-1", "Spc, t-1 ,t",
        "Fpc, t-2, t-1")  

RF.IMP.EX1 = data.frame(row, RF.IMP.EX1)
  
##model 5 Support vector machine ----------------------------------------------- 
  # TS1
  #Tune the SVM model
  Opt.1.1.M4 =tune(svm, F.pc.14.15 ~ ., data = trn.1.1,
                   ranges=list(epsilon=seq(0,1,0.1), cost=1), cv.fold = 10)
  
  #Apply to the train set 
  # train
  train1.1.m5 <- svm(F.pc.14.15 ~ . , data = trn.1.1, kernel = "radial", 
                     cost = 1, gamma = 0.1, epsilon = 0.8, cv.fold = 10)
  
  mse.trn.1.1.m5 <- sqrt(mean(train1.1.m5$residuals^2))
  # test
  pred.1.1.m5 <- predict(train1.1.m5, tst.1.1)
  
  mse.tst1.1.m5 <- mean((tst.1.1$F.pc.14.15 - pred.1.1.m5)^2)
  
  #TS2
  # train
  train1.2.m5 <- svm(F.pc.15.16 ~ . , data = trn.1.2, kernel = "radial", 
                     cost = 1, gamma = 0.1, epsilon = 0.1, cv.fold = 10)
  mse.trn.1.2.m5 <- sqrt(mean(train1.2.m5$residuals^2))
  
  # test
  pred.1.2.m5 <- predict(train1.2.m5, tst.1.2)
  mse.tst1.2.m5 <- mean((tst.1.2$F.pc.15.16 - pred.1.2.m5)^2)
  
  #TS3
  # train
  train1.3.m5 <- svm(F.pc.16.17 ~ . , data = trn.1.3, kernel = "radial", 
                     cost = 1, gamma = 0.1, epsilon = 0.1, cv.fold = 10)
  mse.tst1.3.m5 <- sqrt(mean(train1.3.m5$residuals^2))
  
  # test
  pred.1.3.m5 <- predict(train1.3.m5, tst.1.3)
  mse.tst1.3.m5 <- mean((tst.1.3$F.pc.16.17 - pred.1.3.m5)^2)
  
  svr_MSE_trn <- c(mse.trn.1.1.m5, mse.trn.1.2.m5, mse.trn.1.2.m5)
  svr_MSE_tst <- c(mse.tst1.1.m5, mse.tst1.2.m5, mse.tst1.3.m5)
  
  e1_error <- data.frame(e1_error, svr_MSE_trn, svr_MSE_tst)

## get the actual value 
act_value_ex1 <- data.frame(mean(tst.1.1$F.pc.14.15), mean(tst.1.1$F.pc.13.14), mean(pred.1.1.m3),
                            mean(tst.1.2$F.pc.15.16), mean(tst.1.2$F.pc.14.15), mean(pred.1.2.m3),
                            mean(tst.1.3$F.pc.16.17), mean(tst.1.3$F.pc.15.16), mean(pred.1.3.m3))
  
    
## collected error by country --------------------------------------------------
  
err_by_dest <- data.frame(test_e1$Destination_code, test_e1$Origin_code, 
                               test_e1$F.pc.13.14, test_e1$F.pc.14.15, 
                               test_e1$F.pc.15.16, test_e1$F.pc.16.17,
                               pred.1.1.m3, pred.1.2.m3, pred.1.3.m3,
                               pred.1.1.m4, pred.1.2.m4, pred.1.3.m4,
                               pred.1.1.m5, pred.1.2.m5, pred.1.3.m5)  

# baseline  
err_by_dest$base_ts1_err <- err_by_dest$test_e1.F.pc.14.15 - err_by_dest$test_e1.F.pc.13.14     
err_by_dest$base_ts2_err <- err_by_dest$test_e1.F.pc.15.16 - err_by_dest$test_e1.F.pc.14.15
err_by_dest$base_ts3_err <- err_by_dest$test_e1.F.pc.16.17 - err_by_dest$test_e1.F.pc.15.16

err_by_dest$base_err <- ((err_by_dest$base_ts1_err + err_by_dest$base_ts2_err 
                         + err_by_dest$base_ts3_err)^2)/3

# elasticnet
err_by_dest$enet_ts1_err <- err_by_dest$test_e1.F.pc.14.15 - err_by_dest$pred.1.1.m3     
err_by_dest$enet_ts2_err <- err_by_dest$test_e1.F.pc.15.16 - err_by_dest$pred.1.2.m3
err_by_dest$enet_ts3_err <- err_by_dest$test_e1.F.pc.16.17 - err_by_dest$pred.1.3.m3

err_by_dest$enet_err <- ((err_by_dest$enet_ts1_err + err_by_dest$enet_ts2_err 
                          + err_by_dest$enet_ts3_err)^2)/3

# randomforest
err_by_dest$rf_ts1_err <- err_by_dest$test_e1.F.pc.14.15 - err_by_dest$pred.1.1.m4     
err_by_dest$rf_ts2_err <- err_by_dest$test_e1.F.pc.15.16 - err_by_dest$pred.1.2.m4
err_by_dest$rf_ts3_err <- err_by_dest$test_e1.F.pc.16.17 - err_by_dest$pred.1.3.m4

err_by_dest$rf_err <- ((err_by_dest$rf_ts1_err + err_by_dest$rf_ts2_err 
                          + err_by_dest$rf_ts3_err)^2)/3

# SVM
err_by_dest$SVR_ts1_err <- err_by_dest$test_e1.F.pc.14.15 - err_by_dest$pred.1.1.m5     
err_by_dest$SVR_ts2_err <- err_by_dest$test_e1.F.pc.15.16 - err_by_dest$pred.1.2.m5
err_by_dest$SVR_ts3_err <- err_by_dest$test_e1.F.pc.16.17 - err_by_dest$pred.1.3.m5

err_by_dest$SVM_err <- ((err_by_dest$SVR_ts1_err + err_by_dest$SVR_ts2_err 
                        + err_by_dest$SVR_ts3_err)^2)/3

# summary 
sum_err_by_dest <- err_by_dest %>% 
  group_by(test_e1.Destination_code) %>%
  summarise(Baseline = mean(base_err),
            ElasticNet = mean(enet_err),
            RDMForest = mean(rf_err),
            SVM = mean(SVM_err))%>%
  as.data.frame(sum_err_by_dest)

# plot
sum_err_plt <- ggplot(sum_err_by_dest, aes(test_e1.Destination_code, group =1)) +
  geom_line(aes(y = Baseline, linetype = "Baseline")) +
  geom_point(aes(y = Baseline, shape =  "Baseline")) +
  geom_line(aes(y = ElasticNet, linetype = "ElasticNet")) +
  geom_point(aes(y = ElasticNet, shape = "ElasticNet")) +
  geom_line(aes(y = RDMForest, linetype = "Random Forest")) +
  geom_point(aes(y = RDMForest, shape = "Random Forest")) +
  geom_line(aes(y = SVM, linetype = "SVM")) +
  geom_point(aes(y = SVM, shape = "SVM")) +
  labs(x = "Destination") + 
  labs(y = "MSE Score") 

# MSE score 
  score_summary <- data.frame(mean(base_MSE_trn), mean(enet_MSE_trn), 
                              mean(rf_MSE_trn), mean(svr_MSE_trn),
                              mean(base_MSE_tst), mean(enet_MSE_tst), 
                              mean(rf_MSE_tst), mean(svr_MSE_tst))
# Exporting error by country 
write.csv(err_by_dest, file = "error_exp1.csv")

e1_err_dest_enet
