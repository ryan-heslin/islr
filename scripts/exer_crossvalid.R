# Cross-Validation

# Start by plotting predictor relationship
library(tidyverse)
library(dslabs)
data("mnist_27")
mnist_27$test%>% ggplot(aes(x_1, x_2, color = y)) +  geom_point()

#General idea: for each feature obsrvation(x1, x2), average k nearest points (neighborhood)
# This gives an estimate of outcome probability for each obsrevation.
# Larger ks yield smoother estiamts
#
#This caret function does a knn for all preficotrs
library(caret)
knn_fit <- knn3(y ~ ., data = mnist_27$train)

#With five negihbors:
knn_fit <- knn3(y ~ ., data = mnist_27$train, k = 5)

#predict yields preds for each class
#
y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(y_hat_knn, mnist_27$test$y)$overall["Accuracy"]
#> Accuracy 
#>    0.815
#
##This is much better than the linear model, but concelas a rpoblem.
##Training set accuracty is better than test, indicating overtrianing
y_hat_knn <- predict(knn_fit, mnist_27$train, type = "class")
confusionMatrix(y_hat_knn, mnist_27$train$y)$overall["Accuracy"]
#> Accuracy  
#
y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(y_hat_knn, mnist_27$test$y)$overall["Accuracy"]
#
#
# IF k = 1, accuracry is near perfect, but of course the model is useless, attuned to patterns that only exist in the training set
knn_fit_1 <- knn3(y ~ ., data = mnist_27$train, k = 1)
y_hat_knn_1 <- predict(knn_fit_1, mnist_27$train, type = "class")
confusionMatrix(y_hat_knn_1, mnist_27$train$y)$overall[["Accuracy"]]
#
#
#Large ks have the oppostie problem: over-smoothing
#
knn_fit_401 <- knn3(y ~ ., data = mnist_27$train, k = 401)
y_hat_knn_401 <- predict(knn_fit_401, mnist_27$test, type = "class")
confusionMatrix(y_hat_knn_401, mnist_27$test$y)$overall["Accuracy"]

#Generally, we want the k that minimizes MSE (if continuous outcome) or maxes accuracy (if continuous)
# This produces CMs for test and training sets and compares the results
# REminder: DO NOT pick k based on perfromance with test data
ks <- seq(3, 251, 2)

library(purrr)
accuracy <- map_df(ks, function(k){
  fit <- knn3(y ~ ., data = mnist_27$train, k = k)
  
  y_hat <- predict(fit, mnist_27$train, type = "class")
  cm_train <- confusionMatrix(y_hat, mnist_27$train$y)
  train_error <- cm_train$overall["Accuracy"]
  
  y_hat <- predict(fit, mnist_27$test, type = "class")
  cm_test <- confusionMatrix(y_hat, mnist_27$test$y)
  test_error <- cm_test$overall["Accuracy"]
  
  tibble(k = k, train = train_error, test = test_error)
})

# Test set is less accurate due to overfitting
# Jaggedness occurs b/c this is a random sample. That's why wer're
# trying to optimize EXPECTED MSE
accuracy %>% pivot_longer(cols = c(train, test), names_to = "set") %>% 
  ggplot(aes(k, value, col = set )) +
  geom_point() +
  geom_line()

# MSE has two formss: apparent error (in sample) and true error (population)
# Apparent error is a random variable
# FOr this reason, training on the same data we use to determine apparent error can cause overtraining
# 
# Cross validaiton solves this problem by approximatign true MSE by testing on many data combinations


##K-FOLDING
# 1. Divide data into test and training sets (no more than 20% for testing)
# 2. Decide algorithm parameters
# 3 Decide number K of folded samples to create, eacch with N/K observations
# 4 Split training set into K equally sized validation sets
#   Alternatrively, randomly sample with replacemnet K equally sized sets (bootstrapping)
# 5. Estimate MSE for each validation set for each paramter combination
# 6. average estimates to approximate true MSE
# 7. Select best model
# 8. Estimate for test set
# 9. Refit model on whole dataset
# 
# Bigger Ks give finer estaimtes but increase computation time greatly, so 5 or 10 are popular
# 
# INTRODCUTION TO CARET
# 
# Caret's train fun uses common sysntax for different model types:
# library(caret)
train_glm <- train(y ~ ., method = "glm", data = mnist_27$train)
train_knn <- train(y ~ ., method = "knn", data = mnist_27$train)

#Predict.train will predict outcomes for any model:
#
y_hat_glm <- predict(train_glm, mnist_27$test, type = "raw")
y_hat_knn <- predict(train_knn, mnist_27$test, type = "raw")

# You can plot the bootstrap accuracy of a knn model like so:
# 
#

train_knn <- train(y ~ ., method = "knn", data = mnist_27$train)
ggplot(train_knn, highlight = TRUE)

# the tuneGrid parameter of the kNN method takes a dataframe specifcying ks to try
# #By default, it does 25 bootstraps of 25% of observations
# 
# set.seed(2008)
train_knn <- train(y ~ ., method = "knn", 
                   data = mnist_27$train,
                   tuneGrid = data.frame(k = seq(9, 71, 2)))
ggplot(train_knn, highlight = TRUE)
train_knn$bestTune
train_knn$finalModel

#predict automatically uses the best model of the set
confusionMatrix(predict(train_knn, mnist_27$test, type = "raw"),
mnist_27$test$y)$overall["Accuracy"]

#the trainControl fun sets number and proportion of folds:
control <- trainControl(method = "cv", number = 10, p = .9)
train_knn_cv <- train(y ~ ., method = "knn", 
                      data = mnist_27$train,
                      tuneGrid = data.frame(k = seq(9, 71, 2)),
                      trControl = control)
ggplot(train_knn_cv, highlight = TRUE)

#kNN models can be improved by using loess somoothing to weight central points more closely
library(gam)
#gam models have 2 paratmeters: span and 
#
#Note the tuning grid needs a degree column, even if it's kept constant
grid <- expand.grid(span = seq(0.15, 0.65, len = 10), degree = 1)
train_loess <- train(y ~ ., 
                     method = "gamLoess", 
                     tuneGrid=grid,
                     data = mnist_27$train)
ggplot(train_loess, highlight = TRUE)