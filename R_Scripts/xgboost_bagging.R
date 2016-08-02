#Load relevant libraries
library(data.table)
library(Metrics)
library(foreach)
library(dplyr)
library(xgboost)
library(slackr)
library(Matrix)
slackr_setup(channel = "@kes367", username = "i_bimbobot", icon_emoji = "",
             incoming_webhook_url = "datasqawd.slack.com", api_token = "xoxb-51493476675-82bjBdzsaiejafpuSj4zoloj",
             echo = FALSE)


#Set working directory to the west nile folder.
setwd("~/GitHub-kszela24/west-nile")

#Load in competition training spreadsheet as data table.
train <- fread("./Data/train.csv", stringsAsFactors = F)
test <- fread("./Data/test.csv", stringsAsFactors = F)

#Binding test and train to process the data simultaneously and to avoid inaccuracies in factors
test.id <- test$Id
test$Id <- NULL

train.y <- train$WnvPresent
train$WnvPresent <- NULL
train$NumMosquitos <- NULL

dataset <- rbind(train, test)
dataset$ordering <- 1:length(dataset$AddressAccuracy)

#Converting species to factor so xgboost can use i
dataset$Species <- as.factor(dataset$Species)

#Separating into separate months, years, and days of month.
dataset$Date <- as.Date(dataset$Date)
dataset$Month <- as.numeric(format(dataset$Date, '%m'))
dataset$Day <- as.numeric(format(dataset$Date, '%d'))

#Removing potentially unnecessary variables.
dataset$Address <- NULL
dataset$Street <- NULL
dataset$Trap <- NULL
dataset$AddressNumberAndStreet <- NULL

#Don't need year other than for xgboost bagging, will remove once sets are separated again.

train <- dataset[ordering <= length(train$AddressAccuracy),]
test <- dataset[ordering > length(train$AddressAccuracy),]

test$Date <- NULL
test$ordering <- NULL
train$ordering <- NULL

#Creating year variable to split on for bagging, and putting back in the target variable
#for train.
train$Year <- as.numeric(as.factor(format(train$Date, '%Y')))
train$Date <- NULL
train$WnvPresent <- train.y

#From CV best parameters were eta = 0.1, depth = 4, rounds = 100, colsample = 0.7
learning_rate = 0.1
depths = 4
rounds = 100
colsamples_bytree = 0.7

#Preparing and creating the necessary data structures for the test set.
test$WnvPresent <- -1.
test.model <- sparse.model.matrix(WnvPresent ~ ., data = test)

for (y in 1:4) {
  #Separate the training set into different validation sets based on year.
  train.bagged = train[Year != y,]
  
  #Remove year variable since it was only used to separate the training and test.
  train.bagged$Year <- NULL
  
  #Save target variables for the validation sets
  train.bagged.y <- train.bagged$WnvPresent
  
  train.model <- sparse.model.matrix(WnvPresent ~ ., data = train.bagged)
  
  dtrain <- xgb.DMatrix(data = train.model, label = train.bagged.y)
  
  watchlist <- list(train=dtrain)
  
  #Set seed for reproducibility.
  set.seed(1234)
  
  #Set xgboost parameters.
  param <- list(  objective           = "binary:logistic",
                  booster             = "gbtree",
                  eval_metric         = "auc",
                  eta                 = learning_rate,
                  max_depth           = depths,
                  colsample_bytree    = colsamples_bytree
  )
  
  clf <- xgb.train(   params              = param, 
                      data                = dtrain, 
                      nrounds             = rounds,
                      verbose             = 1,
                      watchlist           = watchlist
  )
  
  #Predicting on the validation set.
  preds.bagged <- predict(clf, test.model)
  
  #Save predictions for bagging on test set.
  to_csv <- paste0('predictions_on_test_train_year_out', y, '.csv')
  predictions <- data.frame(Id = test.id, WnvPresent = preds.bagged)
  write.csv(predictions, to_csv, row.names = F)
}

from_csv <- paste0('predictions_on_test_train_year_out', 1, '.csv')
avg_preds <- fread(from_csv)

for (y in 2:4) {
  from_csv <- paste0('predictions_on_test_train_year_out', y, '.csv')
  new_preds <- fread(from_csv)
  avg_preds <- cbind(avg_preds, new_preds$WnvPresent)
}

avg_preds.id <- avg_preds$Id
avg_preds$Id <- NULL

avg <- rowMeans(avg_preds)

submission = data.frame(Id = avg_preds.id, WnvPresent = avg)
write.csv(submission, 'submission_first_bagging.csv', row.names = F)
#First submission received a score of Public: 0.71909	Private: 0.70283



