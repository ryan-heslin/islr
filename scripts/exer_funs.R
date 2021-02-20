#exer_funs
#ch. 6
library(tidyverse)
library(purrr)

#Funs all have args(formals), a body, and an environment
#Environment impliclty determine by where fun is defined

#All non-primitive funs are closures, which enclose thier execution environments
#Becasue funs are objects, they may be stored in lists
funs <- list(
half = function(x) x / 2,
double = function(x) x * 2
)

objs <- mget(ls("package:base", all = TRUE), inherits = TRUE)
funs <- Filter(is.function, objs)
#scan has most args
map(funs, formals) %>% imap_dfr(~(data.frame(fun =.y, args = length(.x)))) %>% arrange(desc(args))
tb %>% tally(value)

#Note that x %>% f(y) = f(x, y)
#
#

# Lexical Scoping ---------------------------------------------------------

#Main rules:
#1. Fun names mask those defined outside funs
#
x <- 10
y <- 20
g02 <- function() {
  x <- 1
  y <- 2
  c(x, y)
}
g02()
#> [1] 1 2
#
#R searches up environemnt chain for object names
x <- 1
g04 <- function() {
  y <- 2
  i <- function() {
    z <- 3
    c(x, y, z)
  }
  i()
}
g04()

#If a fun and object share a name, R ignores non-fun objects:
g09 <- function(x) x + 100
g10 <- function() {
  g09 <- 10
  g09(g09)
}
g10()
#> [1] 110
#
#3. Objects defined in funs do not persist once environment closes
g11 <- function() {
  if (!exists("a")) {
    a <- 1
  } else {
    a <- a + 1
  }
  a
}

g11()
g11

#Funs only look up objects in calling environement once run, which can cause subtle bugs:
g12 <- function() x + 1
x <- 15
g12()
#> [1] 16

x <- 20
g12()
#> [1] 21
#Cure this with the codetools::findGlobals fun to list all dependencies
codetools::findGlobals(g12)

#

# Lazy Evaluation ---------------------------------------------------------


#Args that aren't accessed don't get used
#
h01 <- function(x) {
10
}
h01(stop("This is an error!"))

# Args consist of promises, which consist of expressions, eval environments, and a resulting value
# Promises only get evaluated once in each specified environment. If a name is defined by one promise,
# additional promises defining it are masked.
double <- function(x) { 
  message("Calculating...")
  x * 2
}

h03 <- function(x) {
  c(x, x)
}

h03(double(20))
#> Calculating...
#> [1] 40 40
#
##Critically, promises cannot be accessed without evaluating and disoelling them. This motivates quosures.
##
##Default args evaluate INSIDE funs, user-supplied ones  OUTSIDE
h05 <- function(x = ls()) {
  a <- 1
  x
}

# ls() evaluated inside h05:
h05()
#> [1] "a" "x"

# ls() evaluated in global environment:
h05(ls())
#> [1] "h05"
#This returns 100 because the fun finds its dedault value of z
f2 <- function(x = z) {
  z <- 100
  x
}
f2()

# Here y=0 is never evaluated b/c it has already been defined
y <- 10
f1 <- function(x = {y <- 1; 2}, y = 0) {
  c(x, y)
}
f1()
y

#This awful fun works because the value of stop is masked before it can trhow an error
show_time <- function(x = stop("Error!")) {
  stop <- function(...) Sys.time()
  print(x)
}
show_time()
#> [1] "2020-07-09 12:32:38 UTC"

# ... ---------------------------------------------------------------------

#Use dots to pass on args to extra funs
i01 <- function(y, z) {
list(y = y, z = z)
}

i02 <- function(x, ...) {
  i01(...)
}

str(i02(x = 1, y = 2, z = 3))


# Exiting Funs ------------------------------------------------------------

#Using invisible() suppresses returns
j04 <- function() invisible(1)
j04()

#If funs make changes to global state, use exit handlers to prevent them from persiting if erroneous.
#Exit handlers run whether or not fun ends with error
#Always set add = TRUE to prevent overrides of previous
j06 <- function(x) {
  cat("Hello\n")
  on.exit(cat("Goodbye!\n"), add = TRUE)
  
  if (x) {
    return(10)
  } else {
    stop("Error")
  }
}

j06(TRUE)
#> Hello
#> Goodbye!
#> [1] 10

j06(FALSE)
#> Hello
#> Error in j06(FALSE): Error
#> Goodbye!
#This can be used to run code in fun environmenets:
ith_dir <- function(dir, code) {
  old <- setwd(dir)
  on.exit(setwd(old), add = TRUE)
  
  force(code)
}

getwd()
#> [1] "/home/travis/build/hadley/adv-r"
with_dir("~", getwd())
#> [1] "/home/travis"


# Function Forms ----------------------------------------------------------

#R has 4 function forms:
#1. Prefix: name before args. Standard form
#2. Infix: name between args (binary only)
x + y

#Replacement: replace values by assignment
#
names(df) <- c("a", "b", "c"). 

#4. Special ([[, if]]) : no consistent form
#
##All funs can be rewritten in infix form

x + y
`+`(x, y)

#In prefix form,a rgs are matched by name, prefix, and position, in that order
k01 <- function(abcdef, bcde1, bcde2) {
  list(a = abcdef, b1 = bcde1, b2 = bcde2)
}
str(k01(1, 2, 3))
#> List of 3
#>  $ a : num 1
#>  $ b1: num 2
#>  $ b2: num 3
str(k01(2, 3, abcdef = 1))
#> List of 3
#>  $ a : num 1
#>  $ b1: num 2
#>  $ b2: num 3

# Can abbreviate long argument names:
str(k01(2, 3, a = 1))
#> List of 3
#>  $ a : num 1
#>  $ b1: num 2
#>  $ b2: num 3

# But this doesn't work because abbreviation is ambiguous
str(k01(1, 3, b = 1))
#> Error in k01(1, 3, b = 1): argument 3 matches multiple formal arguments
#
#You can make infix funs by writing 2-arg funs and binding to a name delimited by %
`%+%` <- function(a, b) paste0(a, b)
"new " %+% "string"

#Custom replacement funs may be defined like so:
`second<-` <- function(x, value) {
  x[2] <- value
  x
}

`%+%` <- function(a,b)
  paste0(a,b)