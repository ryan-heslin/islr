#Expressions
#
#BASICS
#rlang::expr captures and stores unevaluated expressions
#Eval evaluates
#
e <- expr(v <- y+5)
y <- 6
eval(e)
v
#Note expr DOES NOT work on code passed to funs. Enexpr() does this
#
#Compex expressions may be modified like lists

# EXPRESSIONS ------------------------------------------------------------------

f <- expr(f(x = 1, y = 2))
f$z <- 3
f

f[[2]] <- NULL
#Every line is a nested structure of Abstract Syntax Trees of interrlated elements
library(lobstr)
lobstr::ast(f(a, "b"))

lobstr::ast(1 + 2 * 3)
#Recall call2, which constructs call objects
call2("+", 1, call2("*", 2, 3))

#The foring operator inserts the AST of the object it is applied to into an expression
xx <- expr(x + x)
yy <- expr(y + y)

expr(!!xx / !!yy)

#Using enexpr, user-supplied code (arguments) can be spliced in. "En" funs are for args.
cv <- function(var) {
  var <- enexpr(var)
  expr(sd(!!var) / mean(!!var))
}

#Code can be written in "infix" form as a series of nested calls
x <- 4
y <- x * 10
`<-`(y, `*`(x, 10))

#Expressions consist of scalcars, symbols, call objects,a nd pairlists
#They can never contain non-scalra vectors because these can only be created with calls

#Csonstants are either NULL or 1-legnth vectors like TRUE or "x"
#
#They quote themselves, since they are equal to the expressiont that represetns them
identical(expr("x"), "x")

#Symbols represent object names (including funs) and are also cllaed names. They are never vecroized;
#mutliples msut be stored in lists. Evaluated, symbols return obj values
#2 ways to capture, always from strings:
x <- 4
eval(rlang::expr(x))
symbol <- rlang::sym("x")
eval(symbol) #4

#as_string converts back to strings
rlang::as_string(symbol)
##CALL OBJECTS stand for function calls. They are lists of a fun symbol and args

x <- expr(read.table("important.csv", row.names = FALSE))

#Typeof returns "langauge" for some reason
typeof(x)

#Calls may be subset like lists:
x[[1]]
x[-2]
x$row.names
x[-1]

#Rlang::call_standardize converts all args to full names, easing matching. This can't work with dots.
rlang::call_standardise(x)

#Note the fun of a call that invokes another function may not be the initial one, if the ultimate fin exists in a different environemnt
lobstr::ast(foo(1)(2))
#
#Call2 constructs calls without evaluating
call2("mean", x = expr(x), na.rm = TRUE)


# Parsing -----------------------------------------------------------------
#Grammar refers tot he order in which R processes infix functions (+, -, etc.)
#Most R operators are lef-associative (evaluate first on left)
#
#rlang:: parse_expr and parse_exprs conver strings to expressions
test <- "g+8"
g <- 3
eval(rlang::parse_expr(test))

#expr_text deparases expression values to strings
class(deparse(g))


#Notet that parsing and deparsing loses whitespce and backticks
# EVALUATION --------------------------------------------------------------

#Eval evaluates an expression in a given environment (current by default)
eval(expr(x + y), rlang::env(x = 1, y = 10))



#Setting environment values works for funs as well as vars
# Note how funs can be defined in the environmenta, and an argument wrapped in enexpr
string_math <- function(x) {
  e <- env(
    caller_env(),
    `+` = function(x, y) paste0(x, y),
    `*` = function(x, y) strrep(x, y)
  )
  
  eval(enexpr(x), e)
}

#Building on this idea, eval_tidy evaluates expressions inside a supplied data mask
df <- data.frame(x = 1:5, y = sample(5))
rlang::eval_tidy(expr(x + y), df)

#Helper funs to demosntrate "walking" ASTs
#THis checks an expression's type, then does something
#specified by ...
expr_type <- function(x) {
  if (rlang::is_syntactic_literal(x)) {
    "constant"
  } else if (is.symbol(x)) {
    "symbol"
  } else if (is.call(x)) {
    "call"
  } else if (is.pairlist(x)) {
    "pairlist"
  } else {
    typeof(x)
  }
}

switch_expr <- function(x, ...) {
  switch(expr_type(x),
         ...,
         stop("Don't know how to handle type ", typeof(x), call. = FALSE)
  )
}

switch_expr(sym("x"), print("derp"))
#This recursively checks for the exlement type of each AST element
##Recall that seitch statements with undefined outputs "fall through" to next defined output
recurse_call <- function(x) {
  switch_expr(x,
              # Base cases
              symbol = ,
              constant = ,
              
              # Recursive cases
              call = ,
              pairlist =
  )
}
#This example checks if a value is TRUE (a constant) or FALSE (a ymbol with the same value)
logical_abbr_rec <- function(x) {
  switch_expr(x,
              constant = FALSE,
              symbol = rlang::as_string(x) %in% c("F", "T"),
              #Recursive cases
              call = ,
              pairlist = purrr::some(x, logical_abbr_rec)
  )
}

#An example for recurison to extract all calls within a function call. For each call, we extract the LHS and children,
#then recursiely call find_assign_rec to repeat on nested calls. In the base case, we rely on find_assgin rec to
#ID symbols and constants
find_assign_call <- function(x) {
  if (is_call(x)) {
    lhs <- expr_text(x)
    children <- as.list(x)[-1]
    #Base case
  } else {
    lhs <- character()
    children <- as.list(x)
  }
  
  c(lhs, flat_map_chr(children, find_assign_rec))
}

find_assign_rec <- function(x) {
  switch_expr(x,
              # Base cases
              constant = ,
              symbol = character(),
              
              # Recursive cases
              pairlist = flat_map_chr(x, find_assign_rec),
              call = find_assign_call(x)
  )
}
#Wrapper for recursion fun
logical_abbr <- function(x) {
  logical_abbr_rec(rlang::enexpr(x))
}

logical_abbr_rec(rlang::expr(TRUE))
logical_abbr_rec(rlang::expr(T))
logical_abbr(mean(45))
logical_abbr(mean(x, na.rm = T)) #True; this is a call

# The Missing Arg SYmbol --------------------------------------------------

#R uses an invisible empty symbol to represent missing args to fun calls. Create like so:
missing_arg()
typeof(missing_arg())

#Since they do not print, check for them with is.missing
is.missing(missing_arg())

#Importatnly, ... is always represetned by the empty symbol:
f <- expr(function(...) list(...))
args <- f[[2]]
is_missing(args[[1]])


#EVALUATION
#
#By default, eval works in current environment, but can be overridden

x <- 10
y <- 2
eval(expr(x + y), env(x = 1000))

#This evaluates, rather than quotes, the given arg
eval(print(x + 1), env(x = 1000))

#One use of this is to work with intermediate variables in a temporary environment
local2 <- function(expr) {
  env <- env(caller_env())
  eval(enexpr(expr), env)
}

foo <- local2({
  x <- 10
  y <- 200
  x + y
})
#> [1] 11
#Beware: Base R functions generated using eval don't support quasiquaotation. Use new_function instead.
x <- 10
y <- 20
f <- eval(expr(function(x, y) !!x + !!y))
f
f() #30


# Examples ----------------------------------------------------------------

#Observe how each eval "peels off" a layer of exprs()
eval(expr(eval(expr(eval(expr(2 + 2)))))) #4
eval(eval(expr(eval(expr(eval(expr(2 + 2))))))) #4
expr(eval(expr(eval(expr(eval(expr(2 + 2)))))))

get2 <- function(name, env =callenr_env()) {
  eval(sym(name), envir = env))
  
}
#This assigns a symbol to a value in the caller env.
#Reemmebr we have to unquote args to get their values in function
assign2 <- function(name, value, env =caller_env()) {
  eval(expr(!!sym(name) <-  !!value), env)
}
assign2("x", 9)



#Here, susbtitute replaces the envir arg in the substitute call with the function value.
#But the following eval call uses the calling environment, allowign access to objects defined in it





local3 <- function(expr, envir = new.env()) {
  call <- substitute(eval(quote(expr), envir))
  print(call)
  eval(call, envir = parent.frame())
}
local3(x +4)
local3({x <- 10
        x+ 6})


# Quosures ----------------------------------------------------------------

#QUosures can be created 3 ways
#
#1 Use enquo or enquos. Do this almost always
foo <- function(x) enquo(x)
foo(a + b)

#Use quo pr quos() to capture non-user supplied expressions
#New_uosure: seldom useful
new_quosure(expr(x + y), env(x = 1, y = 10))

#Quosures are vital for dots work because ... can capture args in multiple environemnets
##This pattern captures the same expression in different environemnets
f <- function(...) {
  x <- 1
  g(..., f = x)
}
g <- function(...) {
  enquos(...)
}

x <- 0
qs <- f(global = x)
qs

#Quosures technically are formuals and therefore call objects
#They can be captured by expressions, sometimes useful
#
q2 <- new_quosure(expr(x), env(x = 1))
q3 <- new_quosure(expr(x), env(x = 10))

x <- expr(!!q2 + !!q3)


# Data Masks --------------------------------------------------------------

#Data masks allow single expressions to reference both environment and dataframe variables.
##Eval_tidy will accept a qusoure to create a datamask
#Base with does this as well
q1 <- new_quosure(expr(x * y), env(x = 100))
df <- data.frame(y = 1:10)



eval_tidy(q1, df)

#Data masks can create ambiguit, since it is unclear whether names refer to DF variables or environemnt objects. 
##Pronouns solve this. .data$x refers to datamask, .env$x to environment
##
x <- 1
df <- data.frame(x = 2)
.env$x

#As an example, here is how select uses a named list to select ranges of variables
select2 <- function(data, ...) {
  dots <- enquos(...)
  
  vars <- as.list(set_names(seq_along(data), names(data)))
  cols <- unlist(map(dots, eval_tidy, vars))
  
  data[, cols, drop = FALSE]
}
select2(df, b:d)


# Practical Uses of Tidy Evaluation ---------------------------------------

#In this fun, the cond arg has to be quoted to prevent it from being evaluated outside the data mask.
##It is unquoted (!!) wahen it is passed to the data mask function.
subsample <- function(df, cond, n = nrow(df)) {
  cond <- enquo(cond)
  
  df <- subset2(df, !!cond)
  resample(df, n)
}

subsample(df, x == 1)
#>   x y
#> 3 1 3
#> 1 1 1
#> 2 1 2
#
#When handling ambiguity, remember the variable can exist EITHER in the mask or environment. The pronous cure this ambiguity.

threshold_x <- function(df, val) {
  subset2(df, .data$x >= .env$val) #Unquoting val would also work here
}

x <- 10
threshold_x(no_x, 2)
#> Error: Column `x` not found in `.data`
threshold_x(has_val, 2)



# Base Evaluation ---------------------------------------------------------

#Subset should never be used in programming because it may evaluate a dots argument in an unineted environment
##Here xval is evaluated in the calling environment of the wrapper fun, not global env as desired
f1 <- function(df, ...) {
  xval <- 3
  subset_base(df, ...)
}

my_df <- data.frame(x = 1:3, y = 3:1)
xval <- 1
f1(my_df, x == xval)
#>   x y
#> 3 3 1
#
#Match.call captures an entire function call from inside the fun
g <- function(x, y, z) {
  match.call()
}
g(1, 2, z = 3)
#> g(x = 1, y = 2, z = 3)
#
#
#NSE functionc can be wrapped if you want custom output. 3 steps are involved
#1. Capture unevaluated args and caller enviornment
#2. Create new expresasion
##Evaluate in caller environment
#An example
lm3 <- function(formula, data, env = caller_env()) {
  formula <- enexpr(formula)
  data <- enexpr(data)
  
  lm_call <- expr(lm(!!formula, data = !!data))
  expr_print(lm_call)
  eval(lm_call, env)
}

lm3(mpg ~ disp, mtcars)

lm_loop <- function(y_val, data, preds){
  
  preds <- as.call(enexpr(preds))

  data <- enexpr(data)
  y_val <- ensym(y_val)
  
  out <- list()
  
for( i in 1:length(preds)){
  pred <- preds[[i]] %>% enexpr(.)
  print(pred)
  form <- expr(!!y_val ~ !!pred)
  call <- expr(lm(!!form, data = !!data))
  print(call)
  out[[i]] <- eval(call, env = eval(data))
}
  out
}

lm_loop(mpg, mtcars, list(disp, I(1/disp), disp * cyl))

# QUOTATION ----------------------------------------------------------------
#Qusosures capture expressions WITH THEIR ENVIRONEMNT, preventing masking in child environemnts from
#throwing off results
#ALWAYS use enquo when working with data masks
#
f <- function(...) {
  x <- 1
  g(..., f = x)
}
g <- function(...) {
  enquos(...)
}

x <- 0
qs <- f(global = x)
qs

a <- 10000
with2 <- function(df, expr) {
  a <- 1000
  rlang::eval_tidy(enquo(expr), df)
}

with2(df, x + a)

#This exampple quotes strigns as symbols, allowing pasting without use of quotation marks
#But it would fail if passed variable names. !! solves the problem
cement <- function(...) {
  args <- ensyms(...)
  paste(purrr::map(args, as_string), collapse = " ")
}

I <- "I"
verb <- "am"
name <- "Ryan"

cement(I, verb, name)
cement(!!I, !!verb, !!name) #This forces cement to unquote and evaluate args, preventing error

#Funs either evaluate or quote args. As a rule, quoted args don't evaluate outside funs:
library(tidyverse)


mtcars2 <- subset(mtcars, cyl == 4) #Example: here mtcars is evaluated and cyl quoted


# Quoting -----------------------------------------------------------------

#Recall expr-enexpr distinctin.
#Enexprs caputres all axpressions in ...
f <- function(...) enexprs(...)
f(x = 1, y = 10 * z)

#Ensym and ensyms only accept variable names, not expressions
##Of base funs, note that ~ is a special quosure arg
##
##Ina ddition to quoting expressions, substitute can be given expressions. It will replace expression symbol values with those in current environment
f4 <- function(x) substitute(x * 2)
f4(a + b + c)

#Illustrate expr:enexpr difference
f1 <- function(x, y) {
  exprs(x = x, y = y)
}
f2 <- function(x, y) {
  #enexprs(x = x, y = y)
  #enexpr(y)# Can't convert expression to expression!
}

f1(a + b, c + d)
f2(a + b, c + d)
f2(a +b)

str(exprs(a+5))
str(exprs(a = 5)) #Note expression lists only have named entries if == used

y <- 7
substitute(g + 7) #Does nothing; g unbound
foo <- function(x){
  substitute(x * 7)
}
foo(5) #Substitue arg value in expression

foo2 <- function(x){
  x <- 7
  substitute(x * 7) #Substitue variable value
}
foo2(4)

# Unquoting ---------------------------------------------------------------

#Rlang funs are capapnle of unquoting, unlike base
#In general, you can either unquote symbols indiviaully using !!, or evalueate the entire expression
##!! unquotes single args
x <- expr(-1)
expr(f(!!x, y))

#Also works with symbols and constants
a <- sym("y")
b <- 1
expr(f(!!a, !!b))

#If RHS of !! is a call to a fun, it is evaulated. It also preserves operator precedence
#Note nesting of expr calls here
mean_rm <- function(var) {
  var <- ensym(var)
  expr(mean(!!var, na.rm = TRUE))
}
expr(!!mean_rm(x) + !!mean_rm(y))

#It may also be used to unquote functions themselves or calls, rather than their values.
##Note this requres enclosing function name in parens
f <- expr(foo)
expr((!!f)(x, y))

f <- expr(pkg::foo)
expr((!!f)(x, y))

#Unquoting does not work with $ notation, which does not accept expressions. Write in prefix form instead
x <- expr(x)
expr(`$`(df, !!x))

#!!!, as you know, can be spliced ito all Rlang funs
#
##NEVER use !! outside quoting funs. R will interpret it as double negation!
##
###Unquoting can create nonstandard ASTS, which have non-expression components
### This wrongly evaluates a dataframe as a list because the data.frame call is forced
x1 <- expr(class(!!data.frame(x = 10)))
x1
#> class(list(x = 10))
#
#expr_print solves this
expr_print(x1)


xy <- expr(x + y)
xz <- expr(x + z)
yz <- expr(y + z)
abc <- exprs(a, b, c)

expr(!!xy/!!yz)
expr(-(!!xz) ^ !!yz)
expr(!!xy + !!yz - !!xy)
expr(atan2(!!xy, !!yz))
expr(sum(!!xy, !!xy, !!yz))
expr(sum(!!abc[[1]], !!abc[[2]], !!abc[[3]]))
expr(mean(c(!!abc[[1]], !!abc[[2]], !!abc[[3]]), na.rm = TRUE))
expr(foo(a = !!xy, b = !!yz))

# Non-Quoting: The Base R Approach ----------------------------------------

#Base R quotes args in some places but not others
#
#The second part of $ does not quote, for instance
##Here var is taken literally
x <- list(var = 1, y = 2)
var <- "y"

x$var

#PAried quote and nonquoting args
#
x <- 1
rm(x)

y <- 2
vars <- c("y", "vars")
rm(list = vars)

#Base plotting funs also use NSE
#
#

# ... ---------------------------------------------------------------------

# The := operator assigns a value to an unquoted expression (normally forbidden). Very useful for assigning variable names in funs.
 tibble::tibble(!!var := val)

#Rlang's list2  captures ... in a list, with customizable args
#
set_attr <- function(.x, ...) {
  attr <- rlang::list2(...)
  attributes(.x) <- attr
  .x
}
#dots.list offers finer control over output, including handling empy args and homonyms
attrs <- list(x = 1, y = 2)
attr_name <- "z"

1:10 %>%
  set_attr(w = 0, !!!attrs, !!attr_name := 3) %>% 
  str()

#Note exec takes arg names indirectly as well
arg_name <- "na.rm"
arg_val <- TRUE
exec("mean", 1:10, !!arg_name := arg_val)

#Exec can be mappedover a list of funs!
x <- c(runif(10), NA)
funs <- c("mean", "median", "sd")

purrr::map_dbl(funs, exec, x, na.rm = TRUE)
#> [1] 0.444 0.482 0.298
#
#

# APplications ------------------------------------------------------------

#Unquoting enables parsing of captured expressions
z <- expr(foo(x, y))
lobstr::ast(!!z)

#This simple fun extracts elements of an lm formula and returns them as a character vector
linear <- function(var, val) {
  var <- ensym(var)
  coef_name <- map(seq_along(val[-1]), ~ expr((!!var)[[!!.x]]))
  
  summands <- map2(val[-1], coef_name, ~ expr((!!.x * !!.y)))
  summands <- c(val[[1]], summands)
  
  reduce(summands, ~ expr(!!.x + !!.y))
}

linear(x, c(10, 5, -4))

#Generate functions on the fly. Leaving vars unassigned in an exprs call sets them without defaults
new_function(
  exprs(x = , y = ), 
  expr({x + y})
)

# new_function examples ---------------------------------------------------


#Example: using new_function to implement Box-Cox. Recall a onse-died expression is filled in by a return value.
#Given:
bc <- function(lambda) {
  if (lambda == 0) {
    function(x) log(x)
  } else {
    function(x) (x ^ lambda - 1) / lambda
  }
}
bc_quote <- function(lambda){
  
  lambda <- enexpr(lambda)
  if (!!lambda == 0) {
    new_function(exprs(x = ), expr(log(x)))
  } else {
    new_function(exprs(x = ), expr((x ^ (!!lambda) - 1) / !!lambda))
  }
}
#One call yields an expression. Calling again for lambda computes the result.
bc_quote(2)
bc_quote(2)(2)

#Another example
compose <- function(f, g) {
  function(...) f(g(...))
}

#In this example, any args passed thorugh dots are set equal to a call of ffun of of fun g of their value.
#The first call generates a nested call that works on any value/
compose2 <- function(f, g) {
  
  f <- enexpr(f)
  g <- enexpr(g)
  
  new_function(exprs(... = ), expr((!!f)((!!g)(...))))
}

compose2(sin, cos)(pi)

new_function(pairlist2(x = 5 + 5), quote(x))
new_function(exprs(x = 5 + 5), quote(x))
