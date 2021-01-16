# Libraries Needed
library(caret)
library(glmnet)
library(mlbench)
library(psych)

# Data
data("BostonHousing")
data <- BostonHousing

# Data Partition
set.seed(222)
ind <- sample(2, nrow(data), replace = T, prob = c(0.7, 0.3))
train <- data[ind==1,]
test <- data[ind==2,]

# Custom Control Parameters
custom <- trainControl(method = "---",
                       number = ---,
                       repeats = 5,
                       verboseIter = T)

# Linear Model
set.seed(1234)
lm <- train(---)

# Results
plot(lm$finalModel)

# Ridge Regression
set.seed(1234)
ridge <- train(---)

# Plot Results
plot(ridge)
plot(ridge$finalModel, xvar = "lambda", label = T)
plot(ridge$finalModel, xvar = 'dev', label=T)
plot(varImp(ridge, scale=T))

# Lasso Regression
set.seed(1234)
lasso <- train(---)

# Plot Results
plot(lasso)
plot(lasso$finalModel, xvar = 'lambda', label=T)

# Elastic Net Regression
set.seed(1234)
en <- train(---)

# Plot Results
plot(en)
plot(en$finalModel, xvar = 'lambda', label=T)
plot(en$finalModel, xvar = 'dev', label=T)
plot(varImp(en))

# Compare Models
model_list <- list(---)
res <- resamples(model_list)
summary(res)

# Best Model
en$bestTune
best <- en$finalModel
coef(best, s = en$bestTune$lambda)

# Save Final Model for Later Use
saveRDS(en, "final_model.rds")
fm <- readRDS("final_model.rds")
print(fm)

# Prediction
p1 <- predict(fm, train)
sqrt(mean((train$medv-p1)^2))

p2 <- predict(fm, test)
sqrt(mean((test$medv-p2)^2))
