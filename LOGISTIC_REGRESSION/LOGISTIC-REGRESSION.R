#Logistic Regression

setwd("F:\\Machine Learning A-Z\\Part 3 - Classification\\Section 14 - Logistic Regression")

#reading the dataset
dataset = read.csv('Social_Network_Ads.csv')
dataset = dataset[, 3:5]

#spliting the dataset into train and test
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
train_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

#Applying the feature scaling
train_set[, 1:2] = scale(train_set[, 1:2])
test_set[, 1:2]  = scale(test_set[, 1:2])

#fitting the logistic regression
classifier = glm(formula = Purchased ~ .,
                 family = binomial,
                 data = train_set)

#predicting the result in logistic regression
prob_pred = predict(classifier, type = 'response', newdata = test_set[-3])
Y_pred = ifelse(prob_pred > 0.5,
                1,
                0)

#making the confusion matrix
cm = table(test_set[,3], y_pred)
print(cm)

#visualising the result
library(ElemStatLearn)
set = train_set
X1 = seq(min(set[,1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[,2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c("Age", "EstimatedSalary")
prob_set = predict(classifier, type = 'response', newdata = grid_set)
Y_grid = ifelse(prob_set > 0.5,
                1,
                0)
plot(set[, 3],
     main = "Logistic Regression(Train set)",
     Xlab = 'Age', ylab = 'EstimatedSalary',
     Xlim = range(X1), ylim = range(X2))

contour(X1, X2, matrix(as.numeric(Y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(Y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))
