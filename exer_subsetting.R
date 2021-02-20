x <- c(2.1, 4.2, 3.3, 5.4)

x[order(x)]

#Empty ind returns original vector
x[]

#Name matching can be in any order and must be exact
y <- setNames(x, letters[1:4])
y[c("a", "b", "b", "c", "cd")]

#Caution: factor subsetting uses level integers, not levels themselves!
y[factor("a")]

#MATRIXES AND DFS

a <- matrix(1:9, nrow = 3)
colnames(a) <- c("A", "B", "C")

#Note how rows can be selected and columns rearranged
a[c(TRUE, FALSE, TRUE), c("B", "A")]

#Note subsets simplify by default. THis returns a vector:
class(a[1,])

# Matrixes can be susbet like vectors, the index strating at (1,1) and running down each column
vals <- outer(1:5, 1:5, FUN = "paste", sep = ",")
vals
vals

#Matrixes can also be subset by matrixes, with a column for each dimension
select <- matrix(ncol = 2, byrow = TRUE, c(
  1, 1,
  3, 1,
  2, 4
))

#For dfs, 1 subset indexes columns, 2 subsets some combo of rows and columns
df <- data.frame(x = 1:3, y = 3:1, z = letters[1:3])

df[df$x ==3, 1:2]
vals[select]

#Cols can be selected like lists:
df[c("x","y","z")]

#Or like matrices
df[3, c("x", "z")]

#Only matrix-style simplifes by default
#
#
##But not tibbles, which always return tibbles
df <- tibble::tibble(x = 1:3, y = 3:1, z = letters[1:3])
str(df["z"])

#Subsetting by a single value simplifeis by default unless drop = FALSE is set

str(a[1,])
str(a[1, , drop = FALSE])

#For saftey, write funs with drop = FALSE to prevent confusion.
x <- 1:5
x[NA]
x[NA_real_]

mtcars[1:20,]


#SELECTING SINGLE ELEMENTS
# [[]] retunrs single list items, [] the list structure enclosing the element
x <- list(1:3, "a", 4:6)
class(x[1])
class(x[[1]])
#For saftey, use [[]] only with single values
#
#$ acts as shorthand for single value extraction. It rougly stands for x[["y]], which is why
#it doesn't work with qunquoted variable names

var = "qsec"
mtcars$var
mtcars[[var]]

#Careful: $ partial matches by default
#
##Default operators return OOB errors when susbetting nonexistent elements; pluck always reurns NULL or a default OOB value
##
library(tidyverse)
x <- list(
  a = list(1, 2, 3),
  b = list(3, 4, 5)
)

purrr::pluck(x, "3424", 1)

mtcars[3, "cyl"]
mtcars$cyl[3]
mtcars[["cyl"]][3]
pluck(mtcars, "cyl", 3)
mtcars[matrix(c(3, 2), ncol =2, byrow = T)]

#SUBSETING ASSIGNMENT
# Susbets can be matached to assignement values, though be careful of recycling!
#x <- 1:5
x[c(1, 2)] <- c(101, 102)

#For lists, x[[i]] -< NULL removes components, x[i] <- list(NULL) adds literal NULL
#
x <- list(a = 1, b = 2)
x[["b"]] <- NULL

#The empty subset is useful with DFs becauase it allows you to cahnge the CONTENTS of the object, not tthe object istelf
#
mtcars[] <- lapply(mtcars, as.integer)
is.data.frame(mtcars)
mtcars

#SUEFL APPLICATIONS
#
#Character mataching can create lookup tables:
# Remember to use match function or named vectors
x <- c("m", "f", "u", "f", "f", "m", "m")
lookup <- c(m = "Male", f = "Female", u = NA)
lookup[x]

#Subsetting makes random sampling and bootstrapping easy
#Reorder rows
df <- data.frame(x = c(1, 2, 3, 1, 2), y = 5:1, z = letters[1:5])
df[sample(nrow(df)), ]

#Order easily reorders columns or rows
df[order(df$x),]

#Using rep enables us to expand rows collapsed with a count column
#
df <- data.frame(x = c(2, 4, 1), y = c(9, 11, 6), n = c(3, 5, 1))
rep(1:nrow(df), df$n)
df[rep(1:nrow(df), df$n), ]

#Columns can be dropped by setting Null
df$y  <- NULL

#When subsettign rows, reember to use vectorized Boolean operators (& and |)
#Which converts Boolean subsets to integer ones, often useful
