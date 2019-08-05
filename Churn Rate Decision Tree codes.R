cat('Importing the data\n')

data = read.csv(file.choose(), header = T)
cat('Checking the data set')
head(data)
cat('Checking the structure if the data\n')

str(data)
library(dplyr)
df = select(data,LEAVE, OVER_15MINS_CALLS_PER_MONTH,HOUSE, INCOME,COLLEGE)
df$LEAVE = as.factor(df$LEAVE)
df$OVER_15MINS_CALLS_PER_MONTH = as.numeric(df$OVER_15MINS_CALLS_PER_MONTH)
df$INCOME=as.numeric(df$INCOME)
df$COLLEGE = as.numeric(df$COLLEGE)
str(df)
# splitting the data into tarining and testing
set.seed(1234)
ind= sample(2, nrow(df), replace = T, prob = c(0.7,0.3))
trainData = df[ind==1,]
testData = df[ind==2,]
 # Building a decision tree
library(party)
myTree = ctree(LEAVE~., data= trainData, controls = ctree_control(mincriterion = 0.99, minsplit =500 ))
myTree
print(myTree)
plot(myTree)
str(data)
# removing some variables
df = select(df,LEAVE, OVER_15MINS_CALLS_PER_MONTH,HOUSE, INCOME)
set.seed(1234)
ind= sample(2, nrow(df), replace = T, prob = c(0.7,0.3))
trainData = df[ind==1,]
testData = df[ind==2,]
# Building a decision tree
library(party)
myTree = ctree(LEAVE~., data= trainData, controls = ctree_control(mincriterion = 0.99, minsplit =5000 ))
myTree
plot(myTree)
