library(tidyverse)
library(rlist)
library(corrplot)
library(modelr)
library(ggloop)
library(lubridate)
library(zoo)
library(broom)

#!/usr/bin/env Rscript
args = commandArgs(trailingOnly = TRUE)
message(sprintf("Hello %s", args[1L]))

seatbelts_df <- as.data.frame(Seatbelts) 

seatbelts_df <- seatbelts_df %>% 
  mutate(month_year = yearmon(time(Seatbelts)), mon= month(month_year, label = T),
         yr = year(month_year), season = factor(ifelse(mon %in% c("Oct", "Nov", "Dec"), "autumn", "notautumn")),
         law = factor(law)) %>% 
  dplyr::select(month_year:yr, DriversKilled:season) %>% 
  pivot_longer(cols =  c(front, rear), names_to = "collis_type", values_to = "number")

print(summary(seatbelts_df))


# View seris
p_base <- seatbelts_df %>%  ggplot(aes(month_year)) +
  geom_vline(xintercept = as.yearmon("February 1983"), col = "red")
p_base

p_base + geom_line(aes(y = DriversKilled))
# no clear trend here
seatbelts_df %>% ggplot(aes(x = month_year, y = DriversKilled / PetrolPrice))+
  geom_line()


# Check for seasonal trend
# # Why is December deadliest? 
seatbelts_df %>% ggplot(aes(month_year, DriversKilled, col = mon)) +
  geom_line()

#Deaths per distance declines over period
##December deadliest despite fewer miles driven
seatbelts_df %>% ggplot(aes(month_year, DriversKilled/kms, col = mon)) +
  geom_line()

seatbelts_df %>% ggplot(aes(month_year, kms, col = mon)) +
  geom_line()

# Not much relationship between collision type and deaths
# 
p_base +geom_line(aes(x = month_year, y = number, col = collis_type))

seatbelts_df %>% ggplot(aes(collis_type, number)) +geom_boxplot()

#Late fall/early winter is deadliest 
seatbelts_df %>% ggplot(aes(mon, DriversKilled)) + geom_boxplot()

# Trend remains after correcting for miles driven
seatbelts_df %>% ggplot(aes(mon, DriversKilled/kms)) + geom_boxplot()

# Yearly trend shows a peak during the oil embargo - why?
seatbelts_df %>% ggplot(aes(factor(yr), DriversKilled)) + geom_boxplot()

#Law ahs major impact
seatbelts_df %>% ggplot(aes(factor(law), DriversKilled)) + geom_boxplot()


# Let's start with a simple model using petrol price

mod_gas <- lm(DriversKilled ~ PetrolPrice, data = seatbelts_df)

grid1 <- seatbelts_df %>% data_grid(PetrolPrice = seq_range(PetrolPrice, n = nrow(seatbelts_df))) %>%
  add_predictions(mod_gas)

View(grid1)

seatbelts_df  %>%
  ggplot(aes(PetrolPrice, DriversKilled))+
  geom_line() +
  geom_line(data = grid1, aes(x = PetrolPrice, y = pred))

seatbelts_df %>% add_residuals(mod_gas) %>% 
  ggplot(aes(PetrolPrice, DriversKilled)) +
  geom_point() +
  geom_point(aes(y= DriversKilled + resid, col = "red")) +
  geom_smooth()

seatbelts_df %>% add_predictions(mod_gas) %>% 
  ggplot(aes(month_year, pred)) +
  geom_line()+
  geom_line(aes(y = DriversKilled, col = "red")) 
seatbelts_df %>% add_residuals(mod_gas) %>% 
  ggplot(aes(month_year, resid)) +
  geom_point()

#Strognest relevant correlation appers to be neagvtive between gas price and deaths. Makes sense.
seatbelts_df %>% select(where(is.numeric)) %>% 
  make_corrplot()
# Test some basic models

mods <- list(
  "mod_gas" = lm(DriversKilled ~ PetrolPrice, data = seatbelts_df),
  "mod_season" = lm(DriversKilled ~ season, data = seatbelts_df),
  "mod_gas_season_interact" = lm(DriversKilled ~ season * PetrolPrice, data = seatbelts_df),
  "mod_gas_season" = lm(DriversKilled ~ season + PetrolPrice, data = seatbelts_df),
  "mod_gas_season_law" =lm(DriversKilled ~ season + PetrolPrice  + law, data = seatbelts_df),
  "mod_kms" = lm(DriversKilled ~ kms, data = seatbelts_df),
  "mod_law" = lm(DriversKilled ~ law,data = seatbelts_df),
  "mod_law_season" = lm(DriversKilled ~ law + season, data = seatbelts_df))

# Looks like the law, season, and gas price effects are most critical

print(lapply(mods, summary))

# Visualize models
glanced <- lapply(mods, glance)

# Based on Tommy's answer at https://stackoverflow.com/questions/9950144/access-lapply-index-names-inside-fun
# #COmbine glance summaries for list of models
glanced <- lapply(seq_along(glanced), function(i, n){
  n = names(glanced)
  mod <-  as.data.frame(glanced[i], col.names = colnames(glanced[i])) %>%
    mutate(model = n[i]) %>% 
    select(model, everything())
  mod
})
glanced <-   bind_rows(glanced) %>% 
  arrange(desc(adj.r.squared))


# Do plots visualizing each model
# # None are espeiclaly great, but season, gas price, and law seem most important
print(glanced)
visualize_lms(mods)

mod_quad <- lm(DriversKilled ~ log(PetrolPrice) + law + season, data = seatbelts_df)
glance(mod_quad)
visualize_lms(list(mod_quad))
# This is the best model I can come up with for now. Thanks for reading.
              