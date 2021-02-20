library(tidyverse)
library(dslabs)
library(caret)
library(e1071)
data(heights)

#Smoothing attempts to detect trends hidden by noitse in data
## This is useful b/c codnitonal probailities are really trends
## 

#Here we try to estimate trend after election, predicitng amrgin by day

data("polls_2008")
qplot(day, margin, data = polls_2008)

#Linear modeling is a crude way to do this
polls_2008 %>% ggplot(aes(day, margin)) +
  geom_smooth(method = "lm") +
  geom_point()

#Bin smoothing tries to rsolve this by assuming f(x) doesn't vary over strata (bins)
#Intervals assume the EV of Y is cosntant for interval range + and - interval center
#
#We compute the average for the same binwidth centered on each x
#
#
## Some code doing this:
## 
span <- 7 
fit <- with(polls_2008, 
            ksmooth(day, margin, kernel = "box", bandwidth = span))
polls_2008 %>% mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(day, smooth), color="red")

#This method gives wiggly lines b/c 2 points change for each successive bin.
##Kernels fix this problem by weighting points closer to the center
###this code weights by normal density
###

span <- 7
fit <- with(polls_2008, 
            ksmooth(day, margin, kernel = "normal", bandwidth = span))

polls_2008 %>% mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(day, smooth), color="red")

#Local wieghted regression (loess) gives smoother esttiamtes by assumin bin values are LINEARLY RELATED, not cosntant
#This allows us to use larger bins, giving smoother results
##Loess fits a line for each bin; fits become estimate values
##
##The bigger the window, the smoother the curve
##
##Loess differs in several other ways:
##
##1. Binwidth is expressed as a proportin of total points
##2. Loess uses the Tukey tri-weight instead of the normal dist. This puts higher wieghts on more points
##3. Loess can fit robustly, detecting outliers in each iteraiton and downweighting them for the next
total_days <- diff(range(polls_2008$day))
span <- 21/total_days

fit <- loess(margin ~ day, degree=1, span = span, data=polls_2008)

polls_2008 %>% mutate(smooth = fit$fitted) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(day, smooth), color="red")

#Loess can also work with parabolas instead of linear functions. This allows larger windows.
#The function does this by default (degree = 2)
#
#Note the quadratic version is noisier
total_days <- diff(range(polls_2008$day))
span <- 28/total_days
fit_1 <- loess(margin ~ day, degree=1, span = span, data=polls_2008)

fit_2 <- loess(margin ~ day, span = span, data=polls_2008)


polls_2008 %>% mutate(smooth_1 = fit_1$fitted, smooth_2 = fit_2$fitted) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(day, smooth_1), color="red", lty = 2) +
  geom_line(aes(day, smooth_2), color="orange", lty = 1) 

#Smoothing is ueful in machine learning becuase multiple predictor values
#often have nonlinear reltionships in some regions
#
#
#library(tidyverse)
library(purrr)
library(pdftools)
library(dslabs)

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf",
                  package="dslabs")
dat <- map_df(str_split(pdf_text(fn), "\n"), function(s){
  s <- str_trim(s)
  header_index <- str_which(s, "2015")[1]
  tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
  month <- tmp[1]
  header <- tmp[-1]
  tail_index  <- str_which(s, "Total")
  n <- str_count(s, "\\d+")
  out <- c(1:header_index, which(n == 1), 
           which(n >= 28), tail_index:length(s))
  s[-out] %>%  str_remove_all("[^\\d\\s]") %>% str_trim() %>%
    str_split_fixed("\\s+", n = 6) %>% .[,1:5] %>% as_tibble() %>% 
    setNames(c("day", header)) %>%
    mutate(month = month, day = as.numeric(day)) %>%
    gather(year, deaths, -c(day, month)) %>%
    mutate(deaths = as.numeric(deaths))
}) %>%
  mutate(month = recode(month, 
                        "JAN" = 1, "FEB" = 2, "MAR" = 3, 
                        "APR" = 4, "MAY" = 5, "JUN" = 6, 
                        "JUL" = 7, "AGO" = 8, "SEP" = 9, 
                        "OCT" = 10, "NOV" = 11, "DEC" = 12)) %>%
  mutate(date = make_date(year, month, day)) %>%
  filter(date <= "2018-05-01")

fit <- loess(deaths ~ day, degree = 2, span = 60 / as.numeric(diff(range(dat$date))), data = dat)
print(length(fit$fitted))
dat %>% mutate(fit1 = fit$fitted) %>% 
  ggplot(aes(date, deaths)) +
  geom_line(aes(day, fit1))

data("mnist_27")
fit=mnist_27$train %>% 
  loess(as.numeric(as.character(y)) ~ x_2, span = 3, data = .)
mnist_27$train %>% 
  ggplot(aes(x_2, as.numeric(as.character(y)))) +
  geom_point() +
  geom_line(aes(x_2, fit$fitted))

mnist_27$train %>% 
  loess(y ~ x_2, span = 5, data = .) 