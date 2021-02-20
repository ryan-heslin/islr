library(tidyverse)
library(dslabs)
library(caret)
library(e1071)
data(heights)

# From these digits data, we will try to estimate conditional probability function
library(tidyverse)
library(dslabs)
data("mnist_27")
mnist_27$test%>% ggplot(aes(x_1, x_2, color = y)) +  geom_point()