#SUbstitute
#Substitute defualts ot the current environment. Here, it captures the expression associate dwith the promise x.
#In general, substitute can capture the code used to generate a value. If that expression is a symbol,
#Using deparse allows extraction of names from within functions
f <- function(x) {
  substitute(x)
}

f(x^2 + 7)

g <- function(x) deparse(substitute(x), width.cutoff = 500)

g(a + b + c + d + e + f + g + h + i + j + k + l + m +
    n + o + p + q + r + s + t + u + v + w + x + y + z)

#Why don't these work?
##Prediction: the promsie passed to f is called x, so the originale xpression is lost
###Got it in one!
f <- function(x) substitute(x)
g <- function(x) deparse(f(x))

g(1:10)
g(x)
g(x + y ^ 2 / z + exp(a * sin(b)))

#Quote captures an expression exactly as is
quote(x + 6)

#Eval(quote(x)) = x in all cases
##If quotes are nested (i.e., quote calls are quoted),e ach eval peels off one layer
eval(quote(5 + 7))
eval(quote(quote(2 + 2)))

#Quoting needed here to prevent early evaluation
e <- new.env()
e$x <- 20
eval(quote(x), e)

#Good
eval_tidy(quote(mpg), mtcars)
#Bad
eval_tidy(mpg, mtcars)


eval(quote(eval(quote(eval(quote(2 + 2))))))
eval(eval(quote(eval(quote(eval(quote(2 + 2)))))))
#Here the whole expression is quoted,e ven though it comes to 4
quote(eval(quote(eval(quote(eval(quote(2 + 2)))))))
#Subset is implemented something like this:
##We have to add drop = FALSE to prevent simplifcation of 1-col df
subset2 <- function(x, condition) {
condition_call <- substitute(condition)
r <- eval(condition_call, x)
print(condition_call)
x[r,, drop = FALSE]
}

sample_df2 <- data.frame(x = 1:10)
subset2(sample_df2, x > 8)

#this is how select syntax works. Every colname is keyed to its position, then the
#passed expression of variable names is evaluated using this list
select2 <- function(df, vars) {
  vars <- substitute(vars)
  print(vars)
  var_pos <- setNames(as.list(seq_along(df)), names(df))
  print(var_pos)
  pos <- eval(vars, var_pos)
  print(pos)
  df[, pos, drop = FALSE]
}
select2(mtcars, -cyl)



# Scoping Issues ----------------------------------------------------------
sample_df <- data.frame(a = 1:5, b = 5:1, c = c(5, 3, 1, 4, 1))

y <- 4
x <- 4
condition <- 4
condition_call <- 4

subset2(sample_df, a ==4)
subset2(sample_df, a == y)

#Fails since x in dataframe masks subset2 variable
subset2(sample_df, a == x)
#Fails since neither name exists in df
subset2(sample_df, a == condition)
#Fails since 
subset2(sample_df, a == condition_call)

#To fix this, pass evalan encolsing environement, which does not naturally exist for dataframes.
#Function will thus look in this environemnt,then its parents.

subset2 <- function(x, condition) {
  condition_call <- substitute(condition)
  r <- eval(condition_call, x, parent.frame())
  x[r, ]
}

subset2(sample_df, a == x)
subset2(sample_df, a == condition)

#You can get the same effect by convertign a list to an environment with list2env
#With works by substiuting an expression in the dataframe enviornment, with the parent frame as enclosure
##Transform is a mutate variant that discards columns used in transforms. It does this by
#evaluating ... args in df environemnt, then matching to column names and replacing transformed columns 
attach(airquality)
transform(Ozone, logOzone = log(Ozone), ozone2 = Ozone) # marginally interesting ...
detach(airquality)

#Problems with Calling from Functions
scramble <- function(x) x[sample(nrow(x)), ]

subscramble <- function(x, condition) {
  scramble(subset2(x, condition))
}

#This fails because susbet2 is passed the expression condition pointing to an unevaluated expression. Subsituting condtion
#thus susbstitues that unquoted expression, which is evaluated prematurely before the eval call can proceed. This illustrates the problem with susbistute: it only substiitutes
#one level up the AST, failng here to capture the underlying expression and peel off enough intermediate expressions.
subscramble(sample_df, a >= 4)
#Even if we define a in the global environment, the code fials, because
#evaluating condition_call evaluates condition. This evaluates the condiiton prematurely in
##the global environemnt, not in the dataframe

#This could be fixed by evaluating a quoted expression
#This ensures condition isn't evaluated prematurely by susbituting the promise expression, freezing the expression until
#the time for evaluation. Otherwise, evaluating condition evaluates the expression *during* the eval call and before the correct environemnt
##can be specified
subset2_q <- function(x, condition) {
  r <- eval(condition, x, parent.frame())
  x[r, ]
}

subset2 <- function(x, condition) {
  subset2_q(x, substitute(condition))
}

subscramble <- function(x, condition) {
  condition <- substitute(condition)
  scramble(subset2_q(x, condition))
}

subscramble(sample_df, a>=3)


#This better version place parent.frame as a default value,a llowing the user to specify an enclosinge nvironment
subset2_q <- function(x, cond, env = parent.frame()) {
  r <- eval(cond, x, env)
  x[r, ]
}

#Substitute uses NSE, so it fails if passed an expression bound to a name
x <- quote(a + b)
substitute(x, list(a = 1, b = 2))

#This celver fux substiutes x into a call to *subsitute itself*. Then the captured substiute call is evaluated
#in the the environment passed in env a = 1 and b =2
substitute_q <- function(x, env) {
  call <- substitute(substitute(y, env), list(y = x))
  eval(call)
}

x <- quote(a + b)
substitute_q(x, list(a = 1, b = 2))

#Overall, NSE is not referentially transparent: functions do not work the same if names are replaced with values
#
a <- 1
b <- 2
if ((b <- a + 1) > (a <- b - 1)) {
  b <- b + 2
}

#Good base way to capure dots, subbing in expressions into alist call
dots <- function(...) {
  eval(substitute(alist(...)))
}
nl <- function(...) {
  dots <- dots_list(...)
  lapply(dots, eval, parent.frame())
}
foo <- function(x){
  nl(x)
}
foo(mtcars)

test_fun <- function(){
  
}
  
  