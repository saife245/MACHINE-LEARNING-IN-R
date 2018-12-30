#multiple linear regression

setwd('F:\\Machine Learning A-Z\\Part 2 - Regression\\Section 5 - Multiple Linear Regression')

#importing the dataset
dataset = read.csv('50_Startups.csv')

#encoding categorical data
dataset$State = factor(dataset$State,
                       levels = c('New York', 'California','Florida'),
                       labels = c(1,2,3))

#spliting the dataset
library(caTools)
set.seed(123)
split = sample.split(dataset$Profit, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

#fitting the multiple linear regression to the training set
#regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State)
#or we can write
regressor = lm(formula = Profit ~ .,
               data = training_set)

#predicting the test set result
y_pred = predict(regressor, newdata = test_set)
