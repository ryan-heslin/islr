# MACHINE LEARNING IN PRACTICE
# 
#Mnist contains a matrix of predictor features and a vector of outcomes (digits) as integers


library(tidyverse)
library(dslabs)
mnist <- read_mnist()

dim(mnist$train$images)
#> [1] 60000   784
#
class(mnist$train$labels)
#> [1] "integer"
table(mnist$train$labels)


#Smaple test and training sets
set.seed(1990)
index <- sample(nrow(mnist$train$images), 10000)
x <- mnist$train$images[index,]
y <- factor(mnist$train$labels[index])

index <- sample(nrow(mnist$test$images), 1000)
x_test <- mnist$test$images[index,]
y_test <- factor(mnist$test$labels[index])

#PREPROCESSING
#It is often necessary to trainsform data before working
#
#First, remove predictors with litte varaition across observations
#library(matrixStats)
#sds <- colSds(x)
#gqplot(sds, bins = 256)
#
#Caret's nearZeroVAr fun reccomends features to remove
#
library(caret)
nzv <- nearZeroVar(x)
image(matrix(1:784 %in% nzv, 28, 28))

col_index <- setdiff(1:ncol(x), nzv)
length(col_index)

#Be sure to add colnames for features, since caret expects them
colnames(x) <- 1:ncol(mnist$train$images)
colnames(x_test) <- colnames(x)

# Because the dataset is huge, it's best to start on a small subset of data

n <- 1000
b <- 2
index <- sample(nrow(x), n)
control <- trainControl(method = "cv", number = b, p = .9)
train_knn <- train(x[index, col_index], y[index], 
                   method = "knn", 
                   tuneGrid = data.frame(k = c(3,5,7)),
                   trControl = control)

#Then fit to entire dataset
fit_knn <- knn3(x[, col_index], y,  k = 3)

#overall accuracy is very good, but 8s are hardeest to preidct
y_hat_knn <- predict(fit_knn, x_test[, col_index], type="class")
cm <- confusionMatrix(y_hat_knn, factor(y_test))
cm$overall["Accuracy"]

# The imp function computes feature importancea
# #Ensemble models can do even better by averaging multiple models
# using this code I don't understand:
# 
p_rf <- predict(fit_rf, x_test[,col_index], type = "prob")  
p_rf<- p_rf / rowSums(p_rf)
p_knn  <- predict(fit_knn, x_test[,col_index])
p <- (p_rf + p_knn)/2
y_pred <- factor(apply(p, 1, which.max)-1)
confusionMatrix(y_pred, y_test)$overall["Accuracy"]

cm$byClass[,1:2]
