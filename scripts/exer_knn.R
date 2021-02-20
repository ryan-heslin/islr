# Example of fitting machine learning models to data
# 
# 
library(titanic)
library(tidyverse)
library(caret)
source("C:/Users/heslinr1/Documents/Software/R/Projects/funs_generic.R")


#Data already split into test and trainsing sets for us
glimpse(titanic_train)

#Drop irrelvant vars and factorize
drops <- c("Name", "Ticket", "Fare", "Cabin")
transforms = c("Survived", "Pclass", "Sex", "Embarked")
titanic_train <- titanic_train %>% 
  select(-drops) %>% 
  mutate(across(transforms, as.factor))
  

# No unvarying variables
apply(titanic_train, 2, nearZeroVar)

titanic_EDA <- titanic_train %>% select(-PassengerId) %>%
  mutate(Survived = as.numeric(as.character(Survived)))

descriptives <- desc_plot(select(titanic_train, -PassengerId))
print(descriptives)

titanic_train %>% ggplot(aes(Sex, Age, col = Sex)) +
  geom_boxplot() +
  facet_wrap(. ~ Pclass)

# Age doesn't impact survival that much
# #Seems youth were more likely ot survive in first class
age_survive <- titanic_EDA %>% ggplot(aes(Age, fill =factor(Survived ))) +
  geom_density(alpha = .2)

age_survive +facet_wrap(. ~Sex)
age_survive+facet_wrap(.~Pclass)

# Larger families reduced survival odds
family_survive <- titanic_EDA %>% ggplot(aes(x = as.factor(Parch))) +
  geom_bar(aes(fill = as.factor(Survived)), position = "fill")

family_survive

summarized <- titanic_EDA %>% 
  group_summarize(f = mean, response = Survived)

print(summarized)

#Looks like survival was less likely for men and the lower classes, naturally
lapply(summarized, function(summary){

  p <- summary %>% ggplot(aes(x = .data[[colnames(summary)[1]]], y = .data[[colnames(summary)[2]]], fill = .data[[colnames(summary)[1]]])) +
    geom_col()
  p
})


titanic_EDA %>% group_by(Age) %>% 
  summarize(age = Age, survival = mean(Survived)) %>% 
  ggplot(aes(age, survival)) +
  geom_col()

# Why were Cherbourg embarkees more likely to survive?
# #No age variation
titanic_EDA %>% 
  ggplot(aes(Embarked, Age)) +
  geom_boxplot()

# Seems 1st-class was more likely to embark at Cherbourg
titanic_EDA %>% 
  ggplot(aes(Embarked)) +
  geom_bar() +
  facet_wrap(. ~ Pclass )

titanic_EDA %>% ggplot(aes(Pclass, fill = factor(Survived))) +
  geom_bar() +
  facet_wrap(. ~ Sex)

train1 <- titanic_train %>% select(Survived, Pclass, Sex) %>% 
  mutate(Sex = as.integer(recode(Sex, "male" = 0, "female" = 1)),
         Pclass = as.numeric(Pclass))

train2 <- train1 %>% mutate(Age  = titanic_train$Age)

fit1 <- train(Survived ~ ., method = "knn", tuneGrid = data.frame(k= c(3,5,7,9)), data = train1)

ggplot(fit1, highlight = T)


# TRy a few models. Random forest models produce many distinct decision trees using repeated bootstraps.
# They then predict y-hat by averageing(if continuous) or most common classification (if categorical).
# Randaomness is ensured by sampling with replacement and randomly selecting features to use.
# # The fittign fun picks the number of trees tha yield best accuracy.
# #Forests can also be optimized by numebr of data points per tree node 
library(randomForest)
mods <- train_models(list(list(form = Survived ~ ., data = train1, method = "knn", tuneGrid = data.frame(k= c(3,5,7,9))),
                     list(form = Survived ~ ., data = train1, method = "gamLoess", tuneGrid = data.frame(expand_grid(span = seq(1, 5, by = 1),
                                                                                                      degree= 1))),
                     list(form = Survived ~ ., data = train2[complete.cases(train2),], method = "knn", tuneGrid = data.frame(k = c(10,20,2))),
                     list(form = Survived ~ ., data = titanic_train[complete.cases(titanic_train),], method = "rf",
                          tuneGrid = data.frame(mtry = 1:5))))
summary <- data.frame(model = character(), tuning= character(), train_accuracy = numeric(), sensitivity= numeric(), specificity = numeric())

summary <- lapply(mods, function(mod){
  library(caret)
  pred <- predict(mod)
  ref <- mod$trainingData$.outcome
  train_cm <- confusionMatrix(pred, mod$trainingData$.outcome)
  #test_cm <- confusionMatrix(pred,  titanic_test$Survived)
  tune <- (mod$finalModel$tuneValue[1])

  summary <- bind_rows(summary, list(model = mod$method, tuning = paste0(colnames(tune[1]), "=", tune), train_accuracy = train_cm$overall[["Accuracy"]],
                                  sensitivity = sensitivity(ref, pred), specificity = specificity(ref, pred)))
  
})
summary <- bind_rows(summary)

# RF model appers suspiciously accurate. Overtraining?
print(summary)
print(summary[which.max(summary$train_accuracy),])

# 2 trees is by far the most accurate
plot(mods[[4]])

 
titanic_test <- titanic_test[complete.cases(titanic_test),] %>% select(-drops) %>% 
  mutate(across(transforms[which(transforms != "Survived")], factor)) 
pred <- predict(mods[[4]], test)
titanic_test <- titanic_test %>% mutate(Predicted = pred)

# For each model
# # Get sensitivity, sepcificty, accuracy
print("In summation, it seems class and gender were the main determinants of survival, not suprisingly.")


#TODO: EDA

# TODO Modeling #figure out approach to test set
# Decide model types to use
# Make list of predictor combos to use for each model
#   Set params fro each model
#   Make tuning grids and trControl for each model (set number of folds)
# For each model
#   Train model
#   do k-folds
#   Add accruacy, sensitivty, specificity from CM
# For each model
#   Make plots
#     pred vs observed
#     
#   
# Get preds and accuracy for each
# 
# TODO Cross-validate

