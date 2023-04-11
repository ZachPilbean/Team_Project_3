library(tidyverse)

#Load our dataset onto R Script
dataset=read.csv('credit_card.csv')
head(dataset)
glimpse(dataset)
length(dataset)
summary(dataset)

#We can see some different things about the data
#Most of the people fall between 25 and 35 years old
#Most people's debt falls between 2 and 5
#Most people have a prior default on their credit cards
#The majority does not get approved

#Check for any missing values
colSums(is.na(dataset))
#There are none so we are good to move on

#Experiment #1

#Splitting the data into two sets
library(caTools)
split=sample.split(dataset$Approved, SplitRatio= 0.8) 
training_set=subset(dataset, split=TRUE)
testing_set=subset(dataset, split=FALSE)
#Using a split ratio of .8


#Multiple linear regression training

names(dataset)
MLR=lm(formula=Approved~ ., 
       data=training_set)
summary(MLR)


# MSE 
sum=summary(MLR)
MSE=(mean(sum$residuals^2))
paste("Mean Square Error: ", MSE)
#The mean square error is .096


#R- square
summary(MLR)
#The R-square is .609

#testing set prediction
y_pred=predict(MLR, newdata=testing_set)
data=data.frame(testing_set$Approved, y_pred)
head(data)


#Experiment #2

#Splitting the data into two sets
library(caTools)
split=sample.split(dataset$Approved, SplitRatio= 0.85) 
training_set=subset(dataset, split=TRUE)
testing_set=subset(dataset, split=FALSE)
#Using a split ratio of .85

#Multiple linear regression training

names(dataset)
MLR=lm(formula=Approved~ ., 
       data=training_set)
summary(MLR)


# MSE
sum=summary(MLR)
MSE=(mean(sum$residuals^2))
paste("Mean Square Error: ", MSE)
#MSE is still .096

#R- square
summary(MLR)
#R-square is still .609

#testing set prediction
y_pred=predict(MLR, newdata=testing_set)
data=data.frame(testing_set$Approved, y_pred)
head(data)

#Changing the split ratio did not seem to change the MSE or R-Square very much

#Experiment #3

#Splitting the data into two sets
library(caTools)
split=sample.split(dataset$Approved, SplitRatio= 0.75) 
training_set=subset(dataset, split=TRUE)
testing_set=subset(dataset, split=FALSE)
#Using a split ratio of .75


#Multiple linear regression training

names(dataset)
MLR=lm(formula=Approved~ ., 
       data=training_set)
summary(MLR)


# MSE 
sum=summary(MLR)
MSE=(mean(sum$residuals^2))
paste("Mean Square Error: ", MSE)
#MSE is .096

#R- square
summary(MLR)
#R-squared is .609

#testing set prediction
y_pred=predict(MLR, newdata=testing_set)
data=data.frame(testing_set$Approved, y_pred)
head(data)

#Once again, changing the split ratio did not change the MSE or R Square very much


