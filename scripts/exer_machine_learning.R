# From ch. 27 of Intro to Data Science
# 
# 1. General goal: predict outcome Y from features X
# 
# 1. 
# Approach differs for categorical or continousos outcomes
# 
# 2. 
# Categorical classification: Y is one of K classes (often indsexed)
# 
# 3. 
# Predicitons either right or wrong
# 4. 
# Decision rule generates classification
# 5. 
# Continuous (prediciton): Function generates predicition of y-hat
# 
# 1. 
# Error is y-hat - y
# 2. 
# Exact outcome not likely
# 2. 
# Train model on observed values, then apply to new ones'

# 
#Predict sex from height
library(tidyverse)
library(dslabs)
library(caret)
library(e1071)
data(heights)

y = heights$sex
x = heights$height

#First, partition data to create training set. This samples indices of half the data once.
#
set.seed(2007)
test_index <- createDataPartition(y, times = 1, p = .5, list = F)

test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]

#Overall accuracy simply measures correct predictions
#
#guessing does not work werll
y_hat <- factor(sample(c("Male", "Female"), length(test_index), replace = T))


mean(y_hat == test_set$sex)

#Male height is taller on average, so try:

y_hat = if_else(x > mean(heights$height[heights$sex == "Male"]) -
                  2 * sd(heights$height[heights$sex == "Male"]),
                "Male", "Female") %>% factor()

#This is a lot more accurate
mean(y == y_hat)

#We should experiment with different cutoff heights and pick the best

cutoff <- seq(61, 70)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
})
qplot(cutoff, accuracy)

# So 64 is the best cutoff
max(accuracy)
best = cutoff[which.max(accuracy)]

# Testing on test set confiems the result
y_hat <- ifelse(test_set$height > best, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)
mean(y_hat == test_set$sex)

# Why is the prediciton so accurate, since 64 inches is the Female mean?
# Construct a confusion matrix comparing predicitons for groups of x
# 
# This SHOULD show we're predicitng too many femalse as Males because 
# Males are much more prevalent in the dataset, but I'm using the wrong one
# 
# Biases in training data result in biased algorithms
table(predicted = y_hat, actual = test_set$sex)

test_set %>% 
  mutate(y_hat = y_hat) %>%
  group_by(sex) %>% 
  summarize(accuracy = mean(y_hat == sex))

# SENSITIVITY vs. SPECIFICITY
# #Sensinitivyt: predict positive as positive, or true positives/false enagtives
# Specificty: predict negative as negative. or true negative/false postiives
# Non-prevalent groups have little impact on overall sensitivity
#confusionMatrix fun computes these metrics for you, with first level as postive
cm <- confusionMatrix(data = y_hat, reference = test_set$sex)

#F-score condenses these metrics to 1 number
#DOuble the product of specificyt asn sensitivty over their sum
#If sensitiviety is more important than specificity or vice veras, we
##can add a beta weight to represent sensitivty's relative importance
##
##F_meas computes F1, wtih beta 1 by default
##
##

cutoff <- seq(61, 70)
F_1 <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
  F_meas(data = y_hat, reference = factor(train_set$sex))
})

#Now 66 is the best cutoff
qplot(cutoff, F_1)

#Sensitivity and specificty are much more balanced
y_hat <- ifelse(test_set$height > best, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
sensitivity(data = y_hat, reference = test_set$sex)

specificity(data = y_hat, reference = test_set$sex)

#
##By Baeys' theorem, highly sensitive tests generate many false positives if prevalence is low

#The Receiver Operating Characteristic (ROC) plot plots sensitivity vs false postivies
#
#This code produces one manually comparing guessing against the 66 cutoff
# pROC and plotROC are the packages to use
probs <- seq(0, 1, length.out = 10)
guessing <- map_df(probs, function(p){
  y_hat <- 
    sample(c("Male", "Female"), length(y_hat), replace = TRUE, prob=c(p, 1-p)) %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Guessing",
       FPR = 1 - specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})

#Precision-recall plots plot specificty against sensitivity and are useful
# if stratified by group
# Here are practice height stats
 library(lubridate)
data(reported_heights)
dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & 
           date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & 
                         between(minute(date_time), 15, 30),
                       "inclass", "online")) %>% select(sex, type)
x <- dat$type
y <- factor(dat$sex, c("Female", "Male"))

dat %>% group_by(type) %>% 
  summarize(male = mean(sex=="Male"), female = mean(sex == "Female"))

#This algorithm is highly specific but not sensitive becuase females aren't prevalent in the dataset.

test_index <- createDataPartition(y, times = 1, p = .5, list = F)
train_set = dat[test_index,]
test_set = dat[-test_index,]
tapply(train_set$type == "inclass", train_set$sex, mean)
y_hat = factor(if_else(train_set$type =="inclass", "Female", "Male"))
confusionMatrix(data = y_hat, reference = factor(test_set$s))


#CONDITIONAL EXPECTATIONS
#Often, correctly predicting eveyry outcome is impossible, because the same predictor values can be associatied with different outcomes in the data.
#Any function will of course always return the same y for the same x
#
#Howver, we can assume equal observations have a *consistent porbablity* of genreating different outcomes
##Refer to notes
##
##
##Plot conditional probability of being male

rounded <- round(heights$height)
probs <- sapply(rounded, function(x){
  sub <- subset(heights, round(height) == x )
  prob <- mean(sub$sex == "Male")
  prob
})
dat2 <- data.frame(rounded, probs)
dat2 %>%  ggplot(aes(x = rounded, y = probs)) +
  geom_point()

#Now with 10 quantiles for smoothness
quants <- quantile(heights$height, seq(0, 1, 0.1))

probs <- sapply(1:10, function(x){
  sub <- subset(heights, between(heights$height, quants[x], quants[x+1]) )
  prob <- mean(sub$sex == "Male")
  prob
})

dat3 <- data.frame(quant = quants[-1], prob = probs)
dat3 %>%  ggplot(aes(x = quant, y = prob)) +
  geom_point() +
  geom_line()


Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat4 <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

rounded <- round(dat4$x, 1)
mod <- lm(y~ x, data = dat4)
dat4 <-  dat4 %>% mutate(preds = mod$fitted)
probs <- sapply(unique(rounded), function(round){
  subbed <- subset(dat4, round(x, 1) == round)
  prob <- mean(between(subbed$preds, subbed$y-1, subbed$y + 1))
  prob
})
dat5 <- data.frame(round = unique(rounded), probs = probs)
dat5 %>%  ggplot(aes(x = round, y = probs)) +
  geom_point()
