#From R for Data Science
#
library(tidyverse)
library(modelr)
options(na.action = na.warn)

library(nycflights13)
library(lubridate)

#General notes
#Use intuition to create computed varables
#Start with raw data to see trends
# Check residual trend after adding new vars 
# (use boxplots if categorical)
# Funs to use: data_grid, add_predicitons, add_residuals, cut

#Either include transformations in function or when building model
#Low-quality diamonds are more expsensvie because size confounds
ggplot(diamonds, aes(carat, price)) + 
  geom_hex(bins = 50)




# # Log transfrom
# diamonds2 <- diamonds %>% 
#   filter(carat <= 2.5) %>% 
#   mutate(lprice = log2(price), lcarat = log2(carat))
# mod_diamond <- lm(lprice ~ lcarat, data = diamonds2)
# ggplot(diamonds2, aes(lcarat, lresid)) + 
#   geom_hex(bins = 50)

# Plotting shows a strong linear trend. Use data_grid, add_predictions!
grid <- diamonds2 %>% 
  data_grid(carat = seq_range(carat, 20)) %>% 
  mutate(lcarat = log2(carat)) %>% 
  add_predictions(mod_diamond, "lprice") %>% 
  mutate(price = 2 ^ lprice) #Inverse transform

# ggplot(diamonds2, aes(carat, price)) + 
#   geom_hex(bins = 50) + 
#   geom_line(data = grid, colour = "red", size = 1)

# diamonds2 <- diamonds2 %>% 
#   add_residuals(mod_diamond, "lresid")
# 
# # Plotting residuals, we see the quality:price relationship we expect
# 
# ggplot(diamonds2, aes(cut, lresid)) + geom_boxplot()
# ggplot(diamonds2, aes(color, lresid)) + geom_boxplot()
# ggplot(diamonds2, aes(clarity, lresid)) + geom_boxplot()


# For plotting multivariate regressions, use .mdoel arg
mod_diamond2 <- lm(lprice ~ lcarat + color + cut + clarity, data = diamonds2)


# grid <- diamonds2 %>% 
#   data_grid(cut, .model = mod_diamond2) %>% 
#   add_predictions(mod_diamond2)

#grid %>% ggplot(aes(lcarat, pred)) + geom_hex(bins=50)

#> # A tibble: 5 x 5
#>   cut       lcarat color clarity  pred
#>   <ord>      <dbl> <chr> <chr>   <dbl>
#> 1 Fair      -0.515 G     VS2      11.2
#> 2 Good      -0.515 G     VS2      11.3
#> 3 Very Good -0.515 G     VS2      11.4
#> 4 Premium   -0.515 G     VS2      11.4
#> 5 Ideal     -0.515 G     VS2      11.4
#

#For NYCflights, start by graphing flights by date
daily <- flights %>% 
  mutate(date = make_date(year, month, day)) %>% 
  group_by(date) %>% 
  summarise(n = n())

ggplot(daily, aes(date, n)) + 
  geom_line()

#Checking for day-of-week shows much less travel on weekends
daily <- daily %>% 
  mutate(wday = wday(date, label = TRUE))
ggplot(daily, aes(wday, n)) + 
  geom_boxplot()

#Start with model using weekdate as x
#
#
mod <- lm(n ~ wday, data = daily)

grid <- daily %>% 
  data_grid(wday) %>% 
  add_predictions(model = mod, "n")

ggplot(daily, aes(wday, n)) + 
  geom_boxplot() +
  geom_point(data = grid, colour = "red", size = 4)

# Plot residuals. What are the spikes?
daily <- daily %>% 
add_residuals(mod)
daily %>% 
  ggplot(aes(date, resid)) + 
  geom_ref_line(h = 0) + 
  geom_line()

#Plottign weekdays indiviually shows we don't account for the summer increase or Saturdays
#
#
# A seasonal trend remains after smoothing
# 
daily %>% 
ggplot(aes(date, resid)) + 
  geom_ref_line(h = 0) + 
  geom_line(colour = "grey50") + 
  geom_smooth(se = FALSE, span = 0.20)
#> `geom_smooth()` using method = 'loess' and formula 'y ~ x'
#

term <- function(date) {
  cut(date, 
      breaks = ymd(20130101, 20130605, 20130825, 20140101),
      labels = c("spring", "summer", "fall") 
  )
}

# If we create a variable for school terms, the pattern becomes clear
daily <- daily %>% 
  mutate(term = term(date)) 

daily %>% 
  filter(wday == "Sat") %>% 
  ggplot(aes(date, n, colour = term)) +
  geom_point(alpha = 1/3) + 
  geom_line() +
  scale_x_date(NULL, date_breaks = "1 month", date_labels = "%b")

mod1 <- lm(n ~ wday, data = daily)
mod2 <- lm(n ~ wday * term, data = daily)

# Adding weekday to the model doesn't help much
daily %>% 
  gather_residuals(without_term = mod1, with_term = mod2) %>% 
  ggplot(aes(date, resid, colour = model)) +
  geom_line(alpha = 0.75)

# The data remain full of outliers. Use MASS's rlm fun, robust
# to outlier, to improve the model
mod3 <- MASS::rlm(n ~ wday * term, data = daily)

daily %>% 
  add_residuals(mod3, "resid") %>% 
  ggplot(aes(date, resid)) + 
  geom_hline(yintercept = 0, size = 2, colour = "white") + 
  geom_line()
