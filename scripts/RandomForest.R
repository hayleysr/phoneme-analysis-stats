setwd("D:\\26 WAYNE\\3.2\\Data Science\\Project\\RawAcapellaData\\csv\\clean")
remove(list=ls())
set.seed(42)

# Load and preprocess data
library(dplyr)
data1 = read.csv("blankspace-taylorswift-clean.csv", header=T, stringsAsFactors=T)
data2 = read.csv("blindinglights-weeknd-clean.csv", header=T, stringsAsFactors=T)
data3 = read.csv("roar-katyperry-clean.csv", header=T, stringsAsFactors=T)
data = bind_rows(data1, data2, data3)

data$phoneme = gsub("\\t", "", data$phoneme)
data = data[data$phoneme != "",]
data = data[rowSums(is.na(data)) == 0, ]
data$phoneme = as.factor(data$phoneme)
# Set data bins to average of timestamps
data$bin = sapply(strsplit(gsub("\\[|\\(|\\]", "", data$bin), ","), function(x){
  mean(as.numeric(x))
})

# Convert frequencies to linear
data$frequency = log2(data$frequency)

# Output check
plot(data$frequency ~ data$bin, lty=1)

# Split into train and test
data_length = length(data$frequency)
train_ind = sample(nrow(data), floor(nrow(data) / 2))
d_train = data[train_ind,]
d_test = data[-train_ind,]

# Descriptive Statistics
max(data$frequency)
min(data$frequency)
max(data$frequency) - min(data$frequency) #range
levels(data$phoneme)

# Control: LM FIXME
lm_pred = lm(d_train$frequency~d_train$phoneme, data = d_train)
lm_pred_test = predict(lm_pred, newdata = d_test)

# Strategy 1: Random Forest with Cross Validation
library(caret)
library(randomForest)

# 10 fold cross validation
train_control = trainControl(
  method="cv",
  number=10
)

# mtry hyperparameter
rf_grid = expand.grid(.mtry = c(2, 3, 4))

rf_pred = train(
    frequency ~ phoneme + bin,
    data = d_train,
    method = "rf",
    trControl = train_control,
    tuneGrid = rf_grid,
    ntree = 1000  # Number of trees
)

rf_pred_test = predict(rf_pred, newdata = d_test)

print(rf_pred)
plot(rf_pred)

# Strategy 2: Gradient Boosting with Cross-Validation
library(xgboost)

# Prepare predictors and response
x = model.matrix(frequency ~ phoneme + bin, data = d_train)[, -1]  # Exclude intercept
y = d_train$frequency

# Cross-validation with xgboost
dtrain = xgb.DMatrix(data = x, label = y)

params = list(
  objective = "reg:squarederror",  # For regression
  eta = 0.1,                       # Learning rate
  max_depth = 6,                   # Tree depth
  subsample = 0.8,                 # Subsample ratio
  colsample_bytree = 0.8           # Column subsample
)

cv_results = xgb.cv(
  params = params,
  data = dtrain,
  nrounds = 1000,                  # Number of boosting rounds
  nfold = 10,                      # 10-fold CV
  early_stopping_rounds = 10,      # Stop if no improvement
  verbose = FALSE                  # Disable verbose output
)

# Train final model with optimal rounds
xg_pred = xgboost(
  params = params,
  data = dtrain,
  nrounds = cv_results$best_iteration
)

x_test = model.matrix(frequency ~ phoneme + bin, data = d_test)[, -1]
xg_pred_test = predict(xg_pred, newdata = x_test)

# View feature importance
importance = xgb.importance(colnames(x), model = xg_pred)
print(importance)


# Evaluations

evaluate_model = function(predictions, actual) {
  rmse = sqrt(mean((actual - predictions)^2))
  mae = mean(abs(actual - predictions))
  r2 = cor(actual, predictions)^2  # R2 approximation
  return(data.frame(RMSE = rmse, MAE = mae, R2 = r2))
}


# Example usage:
results = rbind(
  evaluate_model(rf_pred_test, d_test$frequency),
  evaluate_model(xg_pred_test, d_test$frequency),
  evaluate_model(lm_pred_test, d_test$frequency)
)
rownames(results) = c("RF", "XGB", "LM")
print(results)
