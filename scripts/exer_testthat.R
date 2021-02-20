#testthat 3e
#Presented 8/13/20
#
#EDITION CONCEPT
#"3rd edition" means a versiton of testthat you must actively opt into
#It breaks many old packages
#Manually convert old packages to 3e
#MAin changes:
#1. No longer have to establish file being tested with context()
#expect_that, expect_is replaced by more exact funs
#2. setup/teardown removed
#
#NEW FEATURES
#
#expect_equal nwo uses waldo package, giving more concise output with names
#ALso allows args to consider function, formula environments
#This is too verbose
library(testthat)

expect_equal(mtcars[-1], mtcars)

f1 <- factor(letters[1:3])
f2 <- ordered(letters[1:3], levels = letters[1:4])
expect_equal(f1, f2)
#COnfig/testthat/edition/3  
#expect_identical is like expect_equal, but accounts for float diffferences
#
#SNAPSHOT TESTS
#Unit tests usuallyr quire output specification in code, but special characters can make this annoying
#Snapshot tests record expected outputs in another file and automatically updates it
# This highlights when results change after code is modified, but you must take note of this
foo <- function() whocares

test_that("foo works,"){
  out <- foo(
    expect_type(out, "character")
    expect_snapshot_output(out) #Creates RMD file to preserve output in tests dir
  })
#One snapshot file per test file, with dashes separting tets expectations
  

#PARALLEL TESTING
#RUns tests on multiple processes - don't worry about this
#
#Miscellany:
#StopReporter now better clarifies test passed or failed, plus backtrace
#YOu must wrap expect_warning in expect_message to capture both
#Doesn't work with pipes right now
#
##expect_snapshot captures side effects of non-returning funs
##Yes, you can suppress emojis