source("R/command.R")

## This calls the getLineDisplayData with the given code in an 
## environment containing the data envVals, comparing it to the
## expected result expResult.
## Note that the code passed in is not executed here. THe result will be
## read from the data passed in as envVals.
testDispVal <- function(name,code,envVals,expResult) {
  envir <- rlang::new_environment(envVals,rlang::global_env())
  exprs <- rlang::parse_exprs(code)
  result <- getLineDisplayData(exprs,envir)
  
  testthat::test_that(name,testthat::expect_that(result,testthat::equals(expResult)))
}

envVals <- list(s="hello",lst=list(a=1,b=2),x=c(a=1,b=2,c=3))

testDispVal("norm assign","s <- 4",envVals,list(s=envVals$s))
testDispVal("supe assing","s <<- 4",envVals,list(s=envVals$s))
testDispVal("equals assign","s = 4",envVals,list(s=envVals$s))
testDispVal("norm assign, vec","x <- 4",envVals,list(x=envVals$x))
testDispVal("[ LHS","x[2] <- 4",envVals,list(`x[2]`=envVals$x[2]))
testDispVal("names LHS","names(x) <- 4",envVals,list(`names(x)`=names(envVals$x)))
testDispVal("$ LHS","lst$a <- 4",envVals,list(`lst$a`=envVals$lst$a))
testDispVal("not assignment","names(x)",envVals,NULL)