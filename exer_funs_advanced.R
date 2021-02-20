#exer_funs_advanced
#Ch.11-12 of Advanced R



# Fun Factories: Basics ---------------------------------------------------

#Fun factories are funcitons that make other funs
##You've unkowingly writtern some!
###They exploit the way funs generate temporary execution environments

power1 <- function(exp) {
  function(x) {
    x ^ exp
  }
}

square <- power1(2)
cube <- power1(3)

#The manufactured fun is enclossed by the execution environemnt of the factory
#The manufactured fun consists of a call to the factory for a specifed value. Calling it on an argument
#evaluates for that argument. For instance, square(10) = 100

square
#> function(x) {
#>     x ^ exp
#>   }
#> <environment: 0x23eeb50>

cube
#> function(x) {
#>     x ^ exp
#>   }
#> <bytecode: 0x2a858c0>
#> <environment: 0x2466c28>
#
##Lazy eval introduces a bug here, because x is evaluated only when sqaure is run, not power1
## power1 finds 3 as the value of x, not 2
x <- 2
square <- power1(x)
x <- 3

#VERY IMPORTANT:

#Prevent this by using force to force evaluation in the factory fun
#
power2 <- function(exp) {
  force(exp)
  function(x) {
    x ^ exp
  }
}

x <- 2
square <- power2(x)
x <- 3
square(2)

#Maintianing Fun State
#Funs can rebind names in the enclosing environment using the super assignment operator. This
#allows funs to track how many times they are called, for instance
#Note how we can create separate counters for each manufactured fun. I is stored in in the manufacturing fun and
#modified from the inner fun
new_counter <- function() {
i <- 0

function() {
  i <<- i + 1
  i
}
}

counter_one <- new_counter()
counter_two <- new_counter()

counter_one()
counter_two()

#Caution: because they preserve environments, manufactured funs aren't automatically garbage collected
#
#Here, the second object is smaller because it removes x

f1 <- function(n) {
  x <- runif(n)
  m <- mean(x)
  function() m
}

g1 <- f1(1e6)
lobstr::obj_size(g1)
#> 8,013,104 B

f2 <- function(n) {
  x <- runif(n)
  m <- mean(x)
  rm(x)
  function() m
}

g2 <- f2(1e6)
lobstr::obj_size(g2)
#> 12,944 B
#
#Custom example: get selected  index
pick <- function(i){
  force(i)
  function(x) 
    x[[i]]
}
pick(3)(mtcars)

lapply(mtcars, pick(1:5))


# Application: Graphical Factories ----------------------------------------


#The scales package uses factories to generate scales. They work well beacuae ggplot labell args take functions:
#
#df <- data.frame(x = 1, y = y)
core <- ggplot(df, aes(x, y)) + 
  geom_point() + 
  scale_x_continuous(breaks = 1, labels = NULL) +
  labs(x = NULL, y = NULL)

core
core + scale_y_continuous(
  labels = comma_format()
)
core + scale_y_continuous(
  labels = number_format(scale = 1e-3, suffix = " K")
)
core + scale_y_continuous(
  labels = scientific_format()
)

#this example ensures allows us to equallys cale binwidths across factes of distirubtionws with different SDs
sd <- c(1, 5, 15)
n <- 100

df <- data.frame(x = rnorm(3 * n, sd = sd), sd = rep(sd, n))

binwidth_bins <- function(n) {
  force(n)
  
  function(x) {
    (max(x) - min(x)) / n
  }
}

ggplot(df, aes(x)) + 
  geom_histogram(binwidth = binwidth_bins(20)) + 
  facet_wrap(~ sd, scales = "free_x") + 
  labs(x = NULL)

#ANother example, implementign the box-cox transform
boxcox2 <- function(lambda) {
  if (lambda == 0) {
    function(x) log(x)
  } else {
    function(x) (x ^ lambda - 1) / lambda
  }
}

#By using functionals to iterate over arg lists, many funs may be generated at once:
#

names <- list(
  square = 2, 
  cube = 3, 
  root = 1/2, 
  cuberoot = 1/3, 
  reciprocal = -1
)
funs <- purrr::map(names, power1)
funs$root(64)


# Function Operators ------------------------------------------------------

#Fun operators take a fun or funs as input and return a fun as output. They differ from fun factories in taking
#function isntead of value inputs. 

#This example takes any fun and returns a fun printing first arg
chatty <- function(f) {
  force(f)
  
  function(x, ...) {
    res <- f(x, ...)
    cat("Processing ", x, "\n", sep = "")
    res
  }
}
f <- function(x) x ^ 2
s <- c(3, 2, 1)

purrr::map_dbl(s, chatty(f))

library(memoise)

#Existing Fun Operators

#Purrrr:sagfely, when calle don a fun, returns a wrapped version that prints error results.
#Very useful when debugging functionsal,s which don't normally pinpoiubt eror locariotn


x <- list(
  c(0.512, 0.165, 0.717),
  c(0.064, 0.781, 0.427),
  c(0.890, 0.785, 0.495),
  "oops"
)
safe_sum <- safely(sum)

map(x, safe_sum)

#Useful for isolating cases of failure in many models
fit_model <- function(df) {
  glm(y ~ x1 + x2 * x3, data = df)
}

models <- transpose(map(datasets, safely(fit_model)))
ok <- map_lgl(models$error, is.null)

# which data failed to converge?
datasets[!ok]

# which models were successful?
models[ok]


#Memoise caches previous fun inputs in memory, avoiding the need to recalculate them if the fun is 
#called again. This trades memory for speed. Especially useful for recursive funs
#
slow_function <- function(x) {
Sys.sleep(1)
x * 10 * runif(1)
}

system.time(print(slow_function(1)))
#> [1] 0.808
#>    user  system elapsed 
#>       0       0       1
#
fast_function <- memoise::memoise(slow_function)
system.time(print(fast_function(1)))
#> [1] 6.01
#>    user  system elapsed 
#>       0       0       1
#
#Custom Fun Operators
#
#this assds a delay between executions of a mapped fun
delay_by <- function(f, amount) {
force(f)
force(amount)

function(...) {
  Sys.sleep(amount)
  f(...)
}
}
urls <- c(
  "adv-r" = "https://adv-r.hadley.nz", 
  "r4ds" = "http://r4ds.had.co.nz/"
  # and many many more
)
path <- paste(tempdir(), names(urls), ".html")

walk2(urls, path, delay_by(download.file, 0.1), quiet = TRUE)