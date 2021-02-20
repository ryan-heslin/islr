library(tidyverse)

glimpse(iris)

# Example of formula syntax with where
iris %>% group_by(Species) %>% summarize(avg = across(where(is.numeric), ~ mean(.x)))


# Named function lists can be passed to across. This gets cumsums for each species

iris %>% group_by(Species) %>% 
  summarize(across(is.numeric, list(cum_sum = ~ cumsum(.x), cum_prod = ~ cumprod(.x))))

#Use names arg to customize outputs 
iris %>% group_by(Species) %>% 
  summarize(across(is.numeric, list(cum_sum = ~ cumsum(.x), cum_prod = ~ cumprod(.x)),
                   .names = "{fn}_{col}"))

#Use cur_column to access the column currently being worked on,
#useful if transformations vary by column. Note across can also
# work with names()
#
rnd <- list(Sepal.Length = 6, Sepal.Width = 5)

iris %>% ungroup() %>%  mutate(across(all_of(names(rnd)), ~ .x + rnd[[cur_column()]]))

# across also cooperates with mutate and filter

iris %>% mutate(across(where(is.numeric), floor ))

# The sumamry funs can be left out fir group_by and some others

iris %>% group_by(Species) %>% distinct(across(contains("Sepal")))

