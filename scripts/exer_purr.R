#Purr exercises
#
#
library(gapminder)
library(tidyverse)
us

#pluck removes list elements by name or position as elements
#list1 <- list(
list1 <- list(
  numbers = 1:3,
  letters = c("a", "b", "c"),
  logicals = c(TRUE, FALSE)
)

pluck(list1, 1) # list1 %>% pluck(1)
pluck(list1, "numbers") # list1 %>% pluck("numbers")

#Last() and first() index vectors as you'd expect

#Map and its variants can also be passed a charcter string or integer to extract indexed elements from nested lists
#Note that if map takes extra args, only the first is vectorized.
map(gap_list, pluck, "lifeExp")

params <- list(
  "norm1" = list("mu" = 0, "sd" = 1),
  "norm2" = list("mu" = 1, "sd" = 1),
  "norm3" = list("mu" = 2, "scale" = 1)
)
map(params, "mu") # params %>% map("mu")

params <- list(
  "norm1" = list("mu" = 0, "sd" = 1),
  "norm2" = list("mu" = 1, "sd" = 1),
  "norm3" = list("mu" = 2, "scale" = 1)
)
map(params, 1) # params %>% map(1)

#This behavior also works for columns of dataframes stored in lists
#
#Map takes transformations in a custom syntax, using a preceding ~ and .x to stand for manipulated elements
#Note how this example combines it with a function call

params <- list(
  "norm1" = list("mu" = 0, "sd" = 1),
  "norm2" = list("mu" = 1, "sd" = 1),
  "norm3" = list("mu" = 2, "scale" = 1)
)

map(params, ~ rnorm(5, mean = pluck(.x, 1), sd = pluck(.x, 2)))

#More funs:
#1. Enframe converts named vectors into dataframes with name and value columns
named_vec <- c(uno = 1, dos = 2, tres = 3)
enframe(named_vec)

#In genral with map functions:
#1. extract element from vector
#2. Apply code to run on vector
#3, USe map to apply code
#
gap_dfs %>%
  map(~ lm(lifeExp ~ year + gdpPercap, data = .x))

##MPPING OVER MULTIPLE VECTORS
#Map2 takes 2 vectors as args. It give fun 1st element as irst arg and
#2nd as second arg.
#
##In other words, it vectorizes a second arg to its invoked fun
#This runs an anova for the 2 models for each country
map2(model1, model2, anova)

#As with map, map2can be given extra args
map2(model1, model2, anova, test = "Chisq")

#Note that map2 will recycle short args

#Map2 has same output variants as ma
#
##Map2 expression forumals refer to elements from vec1 as .x and vec2 as .y
###This compares coefficients across 2 models for each group
model1 %>%
  map2_dbl(model2, ~ pluck(coef(.y), "year") - pluck(coef(.x), "year")) %>%
  round(digits = 2)
#Enframe works with these results
#
#pmap maps over any number of vectors
#It takes a list of vectors, then a function to apply to each vector in it
##Entries in vector list can match by name if they have same names as args in fun
##
long_numbers <- list(pi, exp(1), sqrt(2))
digits <- list(2, 3, 4)
pmap(list(x = long_numbers, digits = digits), round)

# It is this ridiculously easy
mods = list(model1, model2, model3)
pmap(mods, anova)

#Constatn args come after list of vector args, as ever
pmap_dbl(list(xs, ws), weighted.mean, na.rm = TRUE)

#Named lists can match args to called fun (mean's trim arg here)
trims <- c(0, 0.1, 0.2, 0.5)
x <- rcauchy(1000)

pmap_dbl(list(trim = trims), mean, x = x)

#as ever, pmap has output variants
#
#Pmap can alos take expression syntax, but names elemetns ..1, ..2, etc.
#
##if given a dataframe, pmap will apply transform to each row
params = data.frame(n = c(3, 2,1),
                    mean = c(0, 2, 10),
                    sd = c(1, 2,100))

pmap(params, rnorm)

#invoke_map take a vector of functions followed by ones of args, running each fun with each arg
functions <- list(rnorm, rlnorm, rcauchy)
n <- c(1, 2, 3)

invoke_map(functions, n)  functions %>% invoke_map(n)

#Like other map funs, it can pass args, but will throw error if any fun in input can't take arg specified by name
##However, positional mataching of unnamed args ill not throw error, assuming each fun can take them
#
#To iterate over multiple vectors, give invoke_map a list of vectors as second arg, each containing args for correponsing fun in first invoke_map arg
args <- list(norm = c(3, mean = 0, sd = 1), 
lnorm = c(2, meanlog = 1, sdlog = 2),
cauchy = c(1, location = 10, scale = 100))

invoke_map(functions, args)

#Miscellaneous purrr funs:
#1. lmap: only funs that take lists
#imap: aplies fun to each element and index
#map_at, map_if: apply only to specific elements
#modify_ variatns: modify original data at given positions
#
# LIST COLUMNS
# General idea: store various realted data objects in table of lists
# SInce lists are really vectors, they can serve as dataframe 
# 
df <- data.frame(
  name = c("John", "Mary", "Mike"),
  age = c(20, 21, 22),
  height = c(69, 64, 72)
)
df$height2 <- list(
  c(feet = 5, inches = 9),
  c(feet = 5, inches = 4),
  c(feet = 6, inches = 0)
)
#Tibbles clarify which elements are lists
as_tibble(df)
#List columns, being lists, can store anything, and should be treated accrodingly
library(gapminder)
#nest creates list columns from non-grouping variables
# Alternately, nesting cols can be specified by vector passed to data arg
gapminder %>% group_by(country) %>%
  nest() %>% pluck("data")
nested_gapminder <- gapminder %>% group_by(country) %>%
  nest()
#Unnest reverses nesting. Note neeeded cols arg
nested_gapminder %>% unnest(cols = c(data))

#Nesting makes it easy to map models over lists
mods <- nested_gapminder %>%  
  pluck("data") %>%
  map(~ lm(year ~lifeExp, data = .))

#Map funs can be used on list cols
nested_gapminder %>% 
  mutate(models = map(data, ~lm(lifeExp ~ year, data = .x)),
         coef = map_dbl(models, ~coef(.x) %>% pluck("year")))

# MORE ON LIST COLUMNS: R FOR DATA SCIENCE
#Messy! How to simplify?
gapminder %>% 
  ggplot(aes(year, lifeExp, group = country)) +
  geom_line(alpha = 1/3)

gapminder2 <- gapminder %>% group_by(country) %>% 
  nest() %>%
  mutate(models = map(data, ~ lm(lifeExp ~ year, data = .)),
         residuals = map(models, residuals)) %>%
  unnest(c(data, residuals))


# This makes a broom summary for every model!
map(glance <- gapminder2 %>% 
  mutate(glance = map(models, broom::glance)) %>% 
  unnest(glance, .drop = TRUE))

#Nest can also nest speicifed columns of ungrouped dfs
gapminder %>% 
  nest(year:gdpPercap)

#Unnest will work with vectorized functions that ouptut lists, like str_split
df <- tribble(
  ~x1,
  "a,b,c", 
  "d,e,f,g"
) 

df %>% 
  mutate(x2 = stringr::str_split(x1, ",")) %>% 
  unnest()

#To get summaries with multiple values, you can wrap outputs in a list, then unnest. Note the need for 
# a probs column to make quantiels meaningful
probs <- c(0.01, 0.25, 0.5, 0.75, 0.99)
mtcars %>% 
  group_by(cyl) %>% 
  summarise(p = list(probs), q = list(quantile(mpg, probs))) %>% 
  unnest()

#Enframe very usefully creates a df with list elements as names and contents as values
x <- list(
  a = 1:5,
  b = 3:4, 
  c = 5:6
) 


df <- enframe(x)
df

#To simplify list columns, use funs that return single values
df %>% mutate(
  type = map_chr(x, typeof),
  length = map_int(x, length)
)

# Note you cannot simulataneously unnest list columns with differing numbers of elements

#MORE PURR APPLICATIONS
#
#PAssing map an extra arg directly is only evaulated once. Passign it to map through an anonymous fun will evaluate it 
#for each input vector element:
#

#runif called mulltiple times if sent through fun to map
x <- c(0, 0, 0, 0)
map_dbl(x, plus, runif(1))
#> [1] 0.0625 0.0625 0.0625 0.0625
map_dbl(x, ~ plus(.x, runif(1)))
#> [1] 0.903 0.132 0.629 0.945
#
map_dbl(mtcars, sd)
mtcars <- data(mtcars)
map(mtcars, pluck)
data("fruit")
data("airmiles")
df <- gapminder::gapminder_unfiltered
map_dbl(df[map_lgl(df, is.numeric)], sd)


trials <- map(1:100, ~ t.test(rpois(10, 10), rpois(7, 10)))
trials
map_dbl(trials, pluck, "p.value") %>% 
  qplot()

#This fails b/c the fun is given to the outer map
x <- list(
  list(1, c(3, 9)),
  list(c(3, 6), 7, c(4, 7, 6))
)
#map_depth works trhough all levels above depth level
triple <- function(x) x * 3
map_depth(x, 3, triple) %>% map(., triple)

bootstrap <- function(df) {
  df[sample(nrow(df), replace = TRUE), , drop = FALSE]
}

bootstraps <- map(1:10, ~ bootstrap(mtcars))


#MAP VARIANTS
# Modify alwasy returns same outpu as input
df <- data.frame(
  x = 1:3,
  y = 6:4
)
df %>% modify(~(.x *5))

#walk returns .x invivsibly, useful for savign or showing messages:
#
welcome <- function(x) {
cat("Welcome ", x, "!\n", sep = "")
}
names <- c("Hadley", "Jenny")

walk(names, welcome)

#Walk2, analogous to map2, very useful for saving to disk over paths
#
#
cyls <- split(mtcars, mtcars$cyl)
paths <- file.path(temp, paste0("cyl-", names(cyls), ".csv"))
walk2(cyls, paths, write.csv)

dir(temp)

#IMAP
#Imap applies a fun to each vector element AND its index
imap_chr(iris, ~ paste0("The first value of ", .y, " is ", .x[[1]]))

#If vector is unnamed, second arg (.y) serves as index, even id not mentioned
x <- map(1:6, ~ sample(1000, 10))
imap_chr(x, ~ paste0("The highest value of ", .y, " is ", max(.x)))


modify(iris, 1)
temp <- tempfile()
dir.create(temp)

mtcars <- datasets::mtcars
cyls <- split(mtcars, mtcars$cyl)
paths <- file.path(temp, paste0("cyl-", names(cyls), ".csv"))
walk2(cyls, paths, write.csv)

#Using iwalk, we can store paths in a named llist. Note the name-setting line replaces each name of cyls AT ONCE (since names is assignment fun)
cyls <- split(mtcars, mtcars$cyl)
names(cyls) <- file.path(temp, paste0("cyl-", names(cyls), ".csv"))
iwalk(cyls, ~ write.csv(.x, .y))

#This example sets transfromation funs in a list and names them for columns to transform,
#then calls each fun on the appropriate column
trans <- list(
  disp = function(x) x * 0.0163871,
  am = function(x) factor(x, labels = c("auto", "manual"))
)

nm <- names(trans)
mtcars[nm] <- map2(trans, mtcars[nm], function(f, var) f(var))

#Using only map, we can instead call the fun stored on the list directly on the variable
#The key is setting list and var names equal
#
mtcars[vars] <- map(vars, ~ trans[[.x]](mtcars[[.x]]))

#Reduce collapses vectors (or dataframes) to scalars by calling a fun on first value pair to collapse it to one
#value, doing the same on this new value and the next value (originally the third), and so on
#
##This gives us values that occur in every vector
l <- map(1:4, ~ sample(1:10, 15, replace = T))
str(l)
reduce(l, intersect)

#If using reduce, supply .init arg, which tells reudce how to handle 0 or 1-length arg
reduce(integer(), `+`, .init = 0)


#PREDICATE FUNCTIONALS
#
#Predicate funs return 1-length logical vectors. Predicate functionsla apply predicates to each vecor element
#
# 1. some: T if any element matches
# 2.every: T if all elements match
# 3. detect: value of first match
# 4. detect_index: location of first match
# 5. keep: keep all matching elements
# 6. discard: discard all matchig elements
# 
df <- data.frame(x = 1:3, y = c("a", "b", "c"))
detect(df, is.factor)
detect_index(df, is.factor)

#PREDICATE MAP VARIANTS
#Map and modify each have _if variatns that work with predicates
str(map_if(mtcars, ~(!is.factor(.x) &is.numeric(x)), mean))

#map_depth takes a depth level for nested vectors
x <- list(a = list(foo = 1:2, bar = 3:4), b = list(baz = 5:6))
str(x)
map_depth(x, 2, paste, collapse = "/")

arg_max <- function(f, inputs){
  inputs[which.max(map(inputs, f))]
}
arg_max(function(x) -x, z)

#BASE FUNCTIONALS
#Apply, as you know, reduces each column or row to a single val
#BEware: it "simplifeis" dfs to matrices if possible-NOT GOOD
#USEFUl:
#1. Eapply applies to named values in environemnt, or all
#Rapply: recursive apply
