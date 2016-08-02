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

#Converting species to factor so xgboost can use it.
train$Species <- as.factor(train$Species)

#Separating into separate months, years, and days of month.
train$Date <- as.Date(train$Date)
train$Month <- as.numeric(format(train$Date, '%m'))
train$Day <- as.numeric(format(train$Date, '%d'))

#Turn years into numeric in order to separate for cross validation.
train$Year <- as.numeric(as.factor(format(train$Date, '%Y')))

#Removing all character features and date.
train$Date <- NULL
train$Address <- NULL
train$Street <- NULL
train$Trap <- NULL
train$AddressNumberAndStreet <- NULL

#Can't use NumMosquitos since it isn't present in the test set.  I could predict on the test
#set for these values, but it may not work out well.
train$NumMosquitos <- NULL

learning_rate = 0.1
depths = c(4, 5, 6)
rounds = c(50, 60, 70, 80, 90, 100, 110, 120, 130, 140)
colsamples_bytree = c(0.6, 0.7, 0.8, 0.9, 1)
logging = data.frame(learning_rate = 0, depth = 0, rounds = 0, colsample_bytree = 0, auc = 0)


for (d in 1:length(depths)) {
  for (r in 1:length(rounds)) {
    for (s in 1:length(colsamples_bytree)) {
      
      results_per_year = c(0, 0, 0, 0)
      
      for (y in 1:4) {
        #Separate the training set into different validation sets based on year.
        train.cv = train[Year != y,]
        test.cv = train[Year == y,]
        
        #Remove year variable since it was only used to separate the training and test.
        train.cv$Year <- NULL
        test.cv$Year <- NULL
        
        #Save target variables for the validation sets
        train.cv.y <- train.cv$WnvPresent
        
        test.cv.y <- test.cv$WnvPresent
        test.cv$WnvPresent <- NULL
        
        train.model <- sparse.model.matrix(WnvPresent ~ ., data = train.cv)
        
        dtrain <- xgb.DMatrix(data = train.model, label = train.cv.y)
        
        watchlist <- list(train=dtrain)
        
        #Preparing and creating the necessary data structures for the test set.
        test.cv$WnvPresent <- -1.
        test.model <- sparse.model.matrix(WnvPresent ~ ., data = test.cv)
        
        #Set seed for reproducibility.
        set.seed(1234)
        
        #Set xgboost parameters.
        param <- list(  objective           = "binary:logistic",
                        booster             = "gbtree",
                        eval_metric         = "auc",
                        eta                 = learning_rate,
                        max_depth           = depths[d],
                        colsample_bytree    = colsamples_bytree[s]
        )
        
        clf <- xgb.train(   params              = param, 
                            data                = dtrain, 
                            nrounds             = rounds[r],
                            verbose             = 1,
                            watchlist           = watchlist
        )
        
        #Predicting on the validation set.
        preds.cv <- predict(clf, test.model)
        
        #Find the auc score of this iteration from cross validation.
        result = auc(test.cv.y, preds.cv)
        results_per_year[y] = result
      }
      
      result = mean(results_per_year)
      
      message = paste0('For eta = ', learning_rate, ', nrounds = ', rounds[r], ', depth = ', depths[d], ', and with colsample_bytree = ', colsamples_bytree[s], ', the auc was: ', result)
      slackr(message)
      
      to_log_csv = data.frame(learning_rate = learning_rate, depth = depths[d], rounds = rounds[r], colsample_bytree = colsamples_bytree[s], auc = result)
      logging <- rbind(logging, to_log_csv)
      write.csv(logging, "./Logging/naive_xgb_log_depths4-6_rounds50-140_colsample06-1.csv", row.names = F)
    }
  }
}