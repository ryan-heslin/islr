library(ISLR)

# #10 ----------------------------------------------------------------------


carseats <- Carseats
mod1 <- lm(data = carseats, Sales ~ Price + Urban + US)
broom::tidy(mod1)

#Sales fall slighlty for each increment of price and rise greatly in the US
# UrbanYes is not significant
# Sales = 13  -.0545(Price) -.0219I(Urban) +1.2I(US)
# Use t distribution for confints, moron!
mod2 <- lm(data = carseats, Sales ~ Price + US)
broom::tidy(mod2) %>% mutate(lower = estimate + qt(.975, nrow(carseats) -2)*std.error,
                             upper = estimate - qt(.975, nrow(carseats) -2)*std.error)
plot(mod2)
#We see many outliers, and a few high-levelrage points, though the data are normally distributed



# 11. Non-Intercept Regression --------------------------------------------

#There is an intercept with y
set.seed(1)
x <- rnorm(100)
y <- 2 * x +rnorm(100)

#Pretty high SE
mod1 <- lm(y~x+0)
mod2 <- lm(x~y+0)
broom::tidy(mod1)
broom::tidy(mod2)

#Exact same t-value, but B1 lower for second model, as is SE. This is because y has extra noise, since it's
#derived from two random variables (x and itself). Since the denominator of SE is the predictor's sum of squares
#
#The same t-statistic with diferent SE's means the difference in beta estiamtes (numerator in t-stat formula) is
## exactly concaelled by that of the standard errors (the denominator)
#The fit lines aren't exact inverses because each reposne value's x-deviation from the line of fit (used in reverse regression)
##is not the same as its y-deviation- the y-deviations come from a noisier distribution.



# More No-Intercept Regression --------------------------------------------

# Regression coefs are equal only if the sum of squares in x and y is exaclty equal, since the 
# denominator of the estimator formula is the only part that varies. This implies equal variances.
x <- rnorm(100, sd = 1)
y <- rnorm(100, sd = 1)

lm(y ~ x +0)
lm(x ~ y+0)

#Cases where sum of sqaures are eaqual: absolute value, permutations of same vector, identical vectors

# 13. Simualted Data ------------------------------------------------------

#Simulate feature vector and error
X <- rnorm(100 )
eps <- rnorm(100, mean =0, sd = 10)

#True function
Y <- -1 + X/2 +eps

#Plot looks roughly liner, with nois
qplot(X, Y)

#Estiamtes a little off, well within 2 SE's
mod1 <- lm(Y ~ X)
broom::tidy(mod1)
ggplot(data = NULL, aes(x = X, y = Y))+
  geom_point()+
  geom_smooth(method  = 'lm') +
  geom_abline(intercept = -1, slope = .5)

#Quadrataic fit has no improvement
mod2 <- lm(Y ~ I(X^2))
broom::tidy(mod2)

ys<- map(0:10/10, ~rnorm(100, mean = 0, sd = .x)) %>%
  map(~`<-`(.x, -1 + X/2 +.x))

#The bias-variance cutoff here seeis around .8 -linear model begins doing better
map(ys, ~list(linear = lm(.x ~ X), quad = lm(.x ~ I(X^2)))) %>% 
  map_depth(2, broom::tidy)
  


# Collinearity ------------------------------------------------------------

#Correlation .83
x1 <- runif(100)
x2 <- .5*x1+rnorm(100)/10
y <-  -2 +2*x1 +3*x2 + rnorm(100)/10 
cor(x1, x2)

#Coefs are about right - about -.5 when the true values are 2 and 3.
#Intercept has wrong sign
mod1 <- lm(y ~ x1 + x2)
broom::tidy(mod1)

#Separate models both overesitma coefs, and x2 only also overestiamtes the mean
# All are significant, however.
# This is not contradictory because the standard error of coefficients is defined only with respect
# to the corresponding predictor and the residual variance, ignoring the effects of other variables
mod2 <- lm(y ~ x1)
broom::tidy(mod2)

mod3 <- lm(y ~ x2)
broom::tidy(mod3)

x1=c(x1, 0.1)
x2=c(x2, 0.8)
y=c(y,6)


mod1 <- lm(y ~ x1 + x2)
broom::tidy(mod1)

#Separate models both overesitma coefs, and x2 only also overestiamtes the mean
# All are significant, however.
# This is not contradictory because the standard error of coefficients is defined only with respect
# to the corresponding predictor and the residual variance, ignoring the effects of other variables
mod2 <- lm(y ~ x1)
broom::tidy(mod2)

mod3 <- lm(y ~ x2)
broom::tidy(mod3)

#Adding one extreme outlier borks the coefs, as expected.
#The effect is worse for x2, since its corresponding value is more extreme relative ot the other observations
#

seed <- 1152 #seed
n <- 100     #nb of observations
a <- 5       #intercept
b <- 2.7     #slope


# 15 ----------------------------------------------------------------------
library(MASS)
Boston

terms <- Boston %>% select(-crim) %>% 
  colnames() %>% 
  map(sym)

mods <- map(terms, ~lm(crim ~ eval(.x), data = Boston)) %>% 
  set_names(nm = map_chr(terms, deparse))


#Nitrogen oxide has a coef of 30 in the single model but -10 in multiple. The others are much
#closer
mult <- Boston %>% lm(data =., crim ~.)
tibble(single = map_dbl(mods, ~coef(.x)[[2]]),
  mult = coef(mult)[-1], pred =
    names(mods)) %>% 
  filter(pred != "nox") %>% 
  ggplot(aes(x = single, y = mult)) +
  geom_point() +
  geom_text(aes(label = pred)) 
  

mods2 <- map(terms, ~lm(crim ~ eval(.x)^3, data = Boston)) %>% 
  set_names(nm = map_chr(terms, deparse))

#Nox has a high coeffieinct with a polynomial model, suggesting a nonlinear relationship.
#Age has a smaller but still substantial one. These were the preds with thhe higehest divergence between
#single and multple model coefficients. 
set.seed(seed)
epsilon <- rnorm(n, mean=0, sd=sqrt(0.25))
x <- sample(x=c(0, 1), size=n, replace=TRUE)
y <- a + b * x + epsilon
#-----------------------------------------------------------

#------using lm------
mod <- lm(y ~ x)
#--------------------

#------using the explicit formulas------
X <- cbind(1, x)
betaHat <- solve(t(X) %*% X) %*% t(X) %*% y
var_betaHat <- anova(mod)[[3]][2] * solve(t(X) %*% X)
#---------------------------------------

#------comparison------
#estimate
#
#------generate one data set with epsilon ~ N(0, 0.25)------
seed <- 1152 #seed
n <- 100     #nb of observations
a <- 5       #intercept
b <- 2.7     #slope

set.seed(seed)
epsilon <- rnorm(n, mean=0, sd=sqrt(0.25))
x <- sample(x=c(0, 1), size=n, replace=TRUE)
y <- a + b * x + epsilon
#-----------------------------------------------------------

#------using lm------
mod <- lm(y ~ x)
#--------------------

#------using the explicit formulas------
X <- cbind(1, x)
betaHat <- solve(t(X) %*% X) %*% t(X) %*% y
var_betaHat <- anova(mod)[[3]][2] * solve(t(X) %*% X)
#---------------------------------------
