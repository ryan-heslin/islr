library(ISLR)


#The GLM fun handles logistic regression, among others. Set family= "binomial"
glm.fits=glm(Direction ∼ Lag1+Lag2+Lag3+Lag4+Lag5+Volume ,data=Smarket ,family=binomial)
summary(glm.fits)

#Set predict type as response to get probabilities isntead of logit
predict(glm.fits, type="response") %>% 
  map_chr(~ifelse(.x < .5, "Down", "Up")) %>% 
  table()

train <- Smarket[Smarket$Year<2005,]
direction_test <- Smarket$Direction[!(Smarket$Direction %in% test)]

glm.fits=glm(Direction ∼ Lag1+Lag2+Lag3+Lag4+Lag5+Volume , data= train, family = binomial)
summary(glm.fits)

#COmpare test preds to actual
probs <- predict(glm.fits, Smarket[Smarket$Year>=2005,], type = "response") %>% 
  map_chr(~ifelse(.x < .5, "Down", "Up")) %>% 
  table(., Smarket$Direction[Smarket$Year>=2005])

#You can predict on new data, too lazy to type
predict(glm.fits, newdata = da)

library(MASS)
lda.fit=lda(Direction ∼ Lag1+Lag2,data=Smarket)
#Call to display priors, group means by class
lda.fit
plot(lda.fit)
#Calling predict gives predicted class, psoterior prob of each class,
#and linear disciminants 
pred <- predict(lda.fit)
#Summing probs like this gives classifications per class below a threshold
sum(pred$posterior[,1]>=.5)

#Note there are no coefs because QDA is nonlinear
qda_fit <- qda(Direction∼Lag1+Lag2, data=Smarket)
qda_fit

#KNN operates on ABSOLUTe data values. If one variable is much large,
#it will skew results. Scale variables first
#
std_caravan <- scale(Caravan[,-86]) #Sacale all but response

test_x <- std_caravan[1:1000,]
train_x <-std_caravan[1000:nrow(std_caravan),]
test_y <- Caravan$Purchase[1:1000]
train_y <- Caravan$Purchase[-(1:1000)]
library(tidymodels)
knn.pred=knn(train_x, test_x,train_y, k=1)

foreign::read.dta("data/framingham_data.dta") %>% class()

#Uses LDA formula to compute classification odds for X = 4
#Notes how the distributions are summed in the denominatior

sigma <- 6
mu1 <- 10
mu2 <- 0
pi1 <- .8
pi2 <- 1 - pi1
f1 <- dnorm(4,mu1,sigma)
f2 <- dnorm(4,mu2,sigma)
pi1*f1/(pi1*f1+pi2*f2)

library(ISLR)
Weekly

str(Weekly)
Weekly %>% pivot_longer(cols = starts_with(("Lag"))) %>% 
  ggplot(aes(Year, value, col =name)) +
  geom_line()
qplot(Year, Volume, data = Weekly)

Weekly %>% select(where(is.numeric)) %>% cor() %>% corrplot()
summary(Weekly)
table(Weekly$Direction)
naive=mean(Weekly$Direction=="Up")

fit1 <- glm(Direction ∼ Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = Weekly, family = binomial)
summary(fit1)
predict(fit2, type = "response") %>% 
  map_chr(~ifelse(.x < .5, "Down", "Up")) %>% 
  cbind.data.frame(., Weekly$Direction) %>% 
  table()
compute_CM <- function(fit){
  out <-  predict(fit, type = "response") %>% 
    map_chr(~ifelse(.x < .5, "Down", "Up")) %>% 
    cbind.data.frame(., Weekly$Direction) %>% 
    table()
  out
}
fit2 <- glm(Direction ~ Lag2, data = Weekly, family=binomial)
compute_CM(fit2)
