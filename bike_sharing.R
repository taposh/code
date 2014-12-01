#--------------------------------------------------------
#  Conditional Inference Tree for kaggle-bike-sharing
#  Taposh Roy
#  @taposh_dr
#--------------------------------------------------------

setwd("/Users/taposhdr/workspace/decision_science/kaggle/bikeSharing/")
#read in train/test

train <- read.csv("train.csv")
test <- read.csv("test.csv")

str(train)

#factorize training set
train_factor <- train
train_factor$weather <- factor(train$weather)
train_factor$holiday <- factor(train$holiday)
train_factor$workingday <- factor(train$workingday)
train_factor$season <- factor(train$season)
train_factor$casual <- factor(train$casual)
train_factor$registered <- factor(train$registered)

#factorize test set
test_factor <- test
test_factor$weather <- factor(test$weather)
test_factor$holiday <- factor(test$holiday)
test_factor$workingday <- factor(test$workingday)
test_factor$season <- factor(test$season)
test_factor$casual <- factor(test$casual)
test_factor$registered <- factor(test$registered)

#create time column by stripping out timestamp
train_factor$time <- substring(train$datetime,12,20)
test_factor$time <- substring(test$datetime,12,20)
str(train_factor)

#factorize new timestamp column
train_factor$time <- factor(train_factor$time)
test_factor$time <- factor(test_factor$time)


# Now that we’ve handled the hour portion of the ‘datetime’ variable, 
#what about the day? If we repeated the same process, we’d end up with
#a new variable with a bunch of strings like ‘01/01/2013’ and ‘01/02/2013’. 
#Not too useful for our model to work with. It makes much more sense to 
#turn these values into their actual names, such as Monday or Tuesday. 
#This will give us a variable that we can factorize with just seven levels. 
#To do this, we can use a combination of R’s weekdays() and as.Date() functions.


#create day of week column
train_factor$day <- weekdays(as.Date(train_factor$datetime))
train_factor$day <- as.factor(train_factor$day)
test_factor$day <- weekdays(as.Date(test_factor$datetime))
test_factor$day <- as.factor(test_factor$day)

aggregate(train_factor[,"count"],list(train_factor$day),mean)

#create Sunday variable
train_factor$sunday[train_factor$day == "Sunday"] <- "1"
train_factor$sunday[train_factor$day != "1"] <- "0"

test_factor$sunday[test_factor$day == "Sunday"] <- "1"
test_factor$sunday[test_factor$day != "1"] <- "0"

#convert to factor
train_factor$sunday <- as.factor(train_factor$sunday)
test_factor$sunday <- as.factor(test_factor$sunday)

#convert time and create $hour as integer to evaluate
train_factor$hour<- as.numeric(substr(train_factor$time,1,2))
test_factor$hour<- as.numeric(substr(test_factor$time,1,2))

#create daypart column, default to 4 to make things easier for ourselves
train_factor$daypart <- "4"
test_factor$daypart <- "4"

#4AM - 9AM = 1
train_factor$daypart[(train_factor$hour < 10) & (train_factor$hour > 3)] <- 1
test_factor$daypart[(test_factor$hour < 10) & (test_factor$hour > 3)] <- 1


#10AM - 3PM = 2
train_factor$daypart[(train_factor$hour < 16) & (train_factor$hour > 9)] <- 2
test_factor$daypart[(test_factor$hour < 16) & (test_factor$hour > 9)] <- 2


#4PM - 9PM = 3
train_factor$daypart[(train_factor$hour < 22) & (train_factor$hour > 15)] <- 3
test_factor$daypart[(test_factor$hour < 22) & (test_factor$hour > 15)] <- 3

#convert daypart to factor
train_factor$daypart <- as.factor(train_factor$daypart)
test_factor$daypart <- as.factor(test_factor$daypart)

#convert hour back to factor
train_factor$hour <- as.factor(train_factor$hour)
test_factor$hour <- as.factor(test_factor$hour)

#install party package
#install.packages('party')
library('party')

#build our formula
formula <- count ~ season + holiday + workingday + weather + temp + atemp + humidity + hour + daypart + sunday + casual + registered

formula

#train_factor<-na.omit(train_factor)

#build our model
fit.ctree <- ctree(formula, data=train_factor)

#examine model for variable importance
fit.ctree

#run model against test data set
predict.ctree <- predict(fit.ctree, test_factor)

#build a dataframe with our results
submit.ctree <- data.frame(datetime = test$datetime, count=predict.ctree)

#write results to .csv for submission
write.csv(submit.ctree, file="submit_ctree_v1.csv",row.names=FALSE)