#exer_debugginh
##
###Traceback is useful for showign calls bottom to top
f <- function(a) g(a)
g <- function(b) h(b)
h <- function(c) i(c)
i <- function(d) {
  if (!is.numeric(d)) {
    stop("`d` must be numeric", call. = FALSE)
  }
  d + 10
}

#TRacebacks can fial if funs use lazy evaluation
##The trace obscures the fact the error stems from evaluating the arg of f
j <- function() k()
k <- function() stop("Oops!", call. = FALSE)
f(j())
traceback()

f(j())
rlang::last_trace()
#last_trace shows the call tree, making isolation eassier
#
#

#Calls to browser() can be inserted conditionally to aid debugging
#
g <- function(b) {
  if (b < 0) {
    browser()
  }
  h(b)
}
g(-6)

#Usefulk browser commands:
#n: next line
#s: step into fun
#f: finsih execution
#c: leave debugging, continue
#Enter: reperate previous

#setting recover allows you to select a frame in stack to debug in
options(error = recover)
f("x")

#If you can't run code inteeractively, you can use dump.frames to save frame stack and load the
#resulting RDA object for interactive debugging
# In batch R process ----
dump_and_quit <- function() {
  # Save debugging info to file last.dump.rda
  dump.frames(to.file = TRUE)
  # Quit R with error status
  q(status = 1)
}
options(error = dump_and_quit)

# In a later interactive session ----
load("last.dump.rda")
debugger()

#When deubbing in RMarkdown, write this in the setup, which transfers knitr's results from chunks to console
options(error = function() {
  sink()
  recover()
})