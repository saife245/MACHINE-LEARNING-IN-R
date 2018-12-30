#Simple linear regression

setwd('F:\\Machine Learning A-Z\\Part 2 - Regression\\Section 4 - Simple Linear Regression')
#importing the data
dataset = read.csv('Salary_Data.csv')

#spliting the dataset into training set and test set
library(caTools)
set.seed(123)
split = sample.split(dataset$Salary, SplitRatio = 2/3)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

#fitting the simple linear regression
regressor = lm(formula = Salary ~ YearsExperience,
               data = training_set)

#prdictin the test set results
y_pred = predict(regressor, newdata = test_set)

#visualising the training set results
library(ggplot2)
ggplot() +
  geom_point(aes(x = training_set$YearsExperience, y = training_set$Salary),
             color = 'red')+
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)),
            color = 'blue') + 
  ggtitle('Salary vs Experience (Training set)') + 
  xlab('Years of experiance') + 
  ylab('salary')

#visualising the test set results
library(ggplot2)
ggplot() +
  geom_point(aes(x = test_set$YearsExperience, y = test_set$Salary),
             color = 'red')+
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)),
            color = 'blue') + 
  ggtitle('Salary vs Experience (Test set)') + 
  xlab('Years of experiance') + 
  ylab('salary')
