cat('Importing the data\n')

data = read.csv(file.choose(), header = T)
cat('Checking the data set')
head(data)
cat('Checking the structure if the data\n')

str(data)
library(dplyr)

head(data)
cat('Converting the variables to numeric')
data$COLLEGE = as.numeric(data$COLLEGE)
data$INCOME = as.numeric(data$INCOME)
data$OVERAGE = as.numeric(data$OVERAGE)
data$LEFTOVER = as.numeric(data$LEFTOVER)
data$HOUSE= as.numeric(data$HOUSE)
data$HANDSET_PRICE = as.numeric(data$HANDSET_PRICE)
data$OVER_15MINS_CALLS_PER_MONTH = as.numeric(data$OVER_15MINS_CALLS_PER_MONTH)
data$AVERAGE_CALL_DURATION = as.numeric(data$AVERAGE_CALL_DURATION)
data$REPORTED_SATISFACTION = as.numeric(data$REPORTED_SATISFACTION)
data$REPORTED_USAGE_LEVEL = as.numeric(data$REPORTED_USAGE_LEVEL)
data$CONSIDERING_CHANGE_OF_PLAN = as.numeric(data$CONSIDERING_CHANGE_OF_PLAN)
data$LEAVE = data$LEAVE
cat('PArtitioning data into training and testing')
set.seed(1234)
ind= sample(2, nrow(data), replace = T, prob = c(0.7,0.3))
trainData = data[ind==1,]
testData = data[ind==2,]
cat('Building a decision tree')
library(party)
myTree = ctree(LEAVE~., data= trainData, controls = ctree_control(mincriterion = 0.99, minsplit =500 ))
myTree
cat('Plotting the decision Tree')
plot(myTree)
cat('changing the restrictions')
myTree = ctree(LEAVE~., data= trainData, controls = ctree_control(mincriterion = 0.99, minsplit =5000 ))
#myTree
plot(myTree)
dim(data)
# prediction
predict(myTree, data= trainData, type = 'prob')

# obtaining the error 
err = table(predict(myTree), trainData$LEAVE)
err
1-sum(diag(err))/sum(err)

predData = predict(myTree, newdata = testData)
err1 = table(predData, testData$LEAVE)
err1
1-sum(diag(err1))/sum(err1)
#########################################################################################################

#*****************Using the selected variables for the model
###################################################################################################

# importing the library to be used to subset the data
library(dplyr)

# Converting the data into the required data type
ddt = select(data, LEAVE, HOUSE, OVERAGE)
ddt$LEAVE = as.factor(ddt$LEAVE)
ddt$HOUSE= as.numeric(ddt$HOUSE)
ddt$OVERAGE = as.numeric(ddt$OVERAGE)

# partitioning into training and testing
set.seed(1234)
ind= sample(2, nrow(ddt), replace = T, prob = c(0.7,0.3))
trainData = ddt[ind==1,]
testData = ddt[ind==2,]
# Building a decision tree
library(party)
myTree = ctree(LEAVE~., data= trainData, controls = ctree_control(mincriterion = 0.99, minsplit =4500 ))
myTree
plot(myTree)
########################
# prediction
predict(myTree, data= trainData, type = 'prob')

# obtaining the error 
err = table(predict(myTree), trainData$LEAVE)
err
1-sum(diag(err))/sum(err)

predData = predict(myTree, newdata = testData)
err1 = table(predData, testData$LEAVE)
err1
1-sum(diag(err1))/sum(err1)
##########
plot(myTree)



