source("serial.R")

runSerTest <- function(name,testData) {
  testthat::test_that(name,testthat::expect_that(serializeValues(testData$data),testthat::equals(testData$json)))
}

##===========================
##simple serialization tests
## NOTES
## - digits setting not tested
## - list and vector length not tested (or implemented)
## - no classes/derived types tested - just the basic types (see name list)
##===========================
sst <- list()

##integer vector, 1 element
sst$IntVector1 <- {
  x <- 1L
  varList <- list(x=x)
  json <- '{"x":{"type":"vector","atom":"integer","data":[1]}}'
  list(data=varList,json=json)
}

## integer vector
##also tests names
sst$IntVector2 <- {
  x <- 1:5
  names(x) <- c("one", "two", "three", "four", "five")
  varList <- list(x=x)
  json <- '{"x":{"type":"vector","atom":"integer","data":[1,2,3,4,5],"attributes":{"names":["one","two","three","four","five"]}}}'
  list(data=varList,json=json)
}

sst$boolVector <- {
  x <- c(TRUE,FALSE)
  names(x) <- c("true","false")
  varList <- list(x=x)
  json <- "{\"x\":{\"type\":\"vector\",\"atom\":\"logical\",\"data\":[true,false],\"attributes\":{\"names\":[\"true\",\"false\"]}}}"
  list(data=varList,json=json)
}

sst$numericVector <- {
  set.seed(1)
  x <- rnorm(10)
  varList <- list(x=x)
  json <- "{\"x\":{\"type\":\"vector\",\"atom\":\"double\",\"data\":[-0.6265,0.1836,-0.8356,1.5953,0.3295,-0.8205,0.4874,0.7383,0.5758,-0.3054]}}"
  list(data=varList,json=json)
}

sst$charVector <- {
  x <- "hello"
  varList <- list(x=x)
  json <- "{\"x\":{\"type\":\"vector\",\"atom\":\"character\",\"data\":[\"hello\"]}}"
  list(data=varList,json=json)
}

sst$complexVector <- {
  x <- 4 + 5i
  varList <- list(x=x)
  json <- "{\"x\":{\"type\":\"vector\",\"atom\":\"complex\",\"data\":[\"4+5i\"]}}"
  list(data=varList,json=json)
}

sst$matrix <- {
  x <- matrix(as.numeric(1:8),nrow=2)
  varList <- list(x=x)
  json <- "{\"x\":{\"type\":\"matrix\",\"atom\":\"double\",\"data\":[[1,3,5,7],[2,4,6,8]],\"attributes\":{\"dim\":[2,4]}}}"
  list(data=varList,json=json)
}

sst$matrix2 <- {
  x <- matrix(1:8,nrow=2)
  dimnames(x) <- list(x=c("a","b"),y=c("x","y","z","q"))
  varList <- list(x=x)
}

sst$array <- {
  x <- array(as.numeric(1:24),dim=c(2,3,4))
  varList <- list(x=x)
  json <- "{\"x\":{\"type\":\"array\",\"atom\":\"double\",\"data\":[[[1,7,13,19],[3,9,15,21],[5,11,17,23]],[[2,8,14,20],[4,10,16,22],[6,12,18,24]]],\"attributes\":{\"dim\":[2,3,4]}}}"
  list(data=varList,json=json)
}

sst$dataframe <- {
  set.seed(1)
  x <- data.frame(a=1:5,b=runif(5),c=rep(TRUE,5))
  varList <- list(x=x)
  json <- "{\"x\":{\"type\":\"data.frame\",\"data\":[[1,2,3,4,5],[0.2655,0.3721,0.5729,0.9082,0.2017],[true,true,true,true,true]],\"attributes\":{\"names\":[\"a\",\"b\",\"c\"],\"class\":[\"data.frame\"],\"row.names\":[1,2,3,4,5]}}}"
  list(data=varList,json=json)
}

sst$list <- {
  set.seed(1)
  y <- data.frame(a=1:5,b=runif(5),c=rep(TRUE,5))
  x <- list(a=1,b=TRUE,c=1:5,d=y,e=list(a=list(a=list(a="xxx"))))
  varList <- list(x=x)
  json <- "{\"x\":{\"type\":\"list\",\"data\":{\"a\":{\"type\":\"vector\",\"atom\":\"double\",\"data\":[1]},\"b\":{\"type\":\"vector\",\"atom\":\"logical\",\"data\":[true]},\"c\":{\"type\":\"vector\",\"atom\":\"integer\",\"data\":[1,2,3,4,5]},\"d\":{\"type\":\"data.frame\",\"data\":[[1,2,3,4,5],[0.2655,0.3721,0.5729,0.9082,0.2017],[true,true,true,true,true]],\"attributes\":{\"names\":[\"a\",\"b\",\"c\"],\"class\":[\"data.frame\"],\"row.names\":[1,2,3,4,5]}},\"e\":{\"type\":\"list\",\"data\":{\"a\":{\"type\":\"list\",\"data\":{\"a\":{\"type\":\"list\",\"data\":{\"a\":{\"type\":\"vector\",\"atom\":\"character\",\"data\":[\"xxx\"]}},\"attributes\":{\"names\":[\"a\"]}}},\"attributes\":{\"names\":[\"a\"]}}},\"attributes\":{\"names\":[\"a\"]}}},\"attributes\":{\"names\":[\"a\",\"b\",\"c\",\"d\",\"e\"]}}}"
  list(data=varList,json=json)
}

sst$factor <- {
  f1 <- factor(c(1,5,2,34,1,5,2,3,4,2,3,4))
  f2 <- ordered(c(1,2,4,2,4,4))
  f3 <- factor(c("one","two","one","two","five"),levels=c("one", "two", "three", "four", "five"))
  varList <- list(f1=f1,f2=f2,f3=f3)
  json <- "{\"f1\":{\"type\":\"factor\",\"ordered\":false,\"atom\":\"integer\",\"data\":[\"1\",\"5\",\"2\",\"34\",\"1\",\"5\",\"2\",\"3\",\"4\",\"2\",\"3\",\"4\"],\"attributes\":{\"levels\":[\"1\",\"2\",\"3\",\"4\",\"5\",\"34\"],\"class\":[\"factor\"]}},\"f2\":{\"type\":\"factor\",\"ordered\":true,\"atom\":\"integer\",\"data\":[\"1\",\"2\",\"4\",\"2\",\"4\",\"4\"],\"attributes\":{\"levels\":[\"1\",\"2\",\"4\"],\"class\":[\"ordered\",\"factor\"]}},\"f3\":{\"type\":\"factor\",\"ordered\":false,\"atom\":\"integer\",\"data\":[\"one\",\"two\",\"one\",\"two\",\"five\"],\"attributes\":{\"levels\":[\"one\",\"two\",\"three\",\"four\",\"five\"],\"class\":[\"factor\"]}}}"
  list(data=varList,json=json)
}

sst$multi <- {
  set.seed(1)
  z <- 4
  y <- data.frame(a=1:5,b=runif(5),c=rep(TRUE,5))
  x <- list(a=1,b=TRUE,c=1:5,d=y,e=list(a=list(a=list(a="xxx"))))
  varList <- list(x=x,y=y,z=z)
  json <- "{\"x\":{\"type\":\"list\",\"data\":{\"a\":{\"type\":\"vector\",\"atom\":\"double\",\"data\":[1]},\"b\":{\"type\":\"vector\",\"atom\":\"logical\",\"data\":[true]},\"c\":{\"type\":\"vector\",\"atom\":\"integer\",\"data\":[1,2,3,4,5]},\"d\":{\"type\":\"data.frame\",\"data\":[[1,2,3,4,5],[0.2655,0.3721,0.5729,0.9082,0.2017],[true,true,true,true,true]],\"attributes\":{\"names\":[\"a\",\"b\",\"c\"],\"class\":[\"data.frame\"],\"row.names\":[1,2,3,4,5]}},\"e\":{\"type\":\"list\",\"data\":{\"a\":{\"type\":\"list\",\"data\":{\"a\":{\"type\":\"list\",\"data\":{\"a\":{\"type\":\"vector\",\"atom\":\"character\",\"data\":[\"xxx\"]}},\"attributes\":{\"names\":[\"a\"]}}},\"attributes\":{\"names\":[\"a\"]}}},\"attributes\":{\"names\":[\"a\"]}}},\"attributes\":{\"names\":[\"a\",\"b\",\"c\",\"d\",\"e\"]}},\"y\":{\"type\":\"data.frame\",\"data\":[[1,2,3,4,5],[0.2655,0.3721,0.5729,0.9082,0.2017],[true,true,true,true,true]],\"attributes\":{\"names\":[\"a\",\"b\",\"c\"],\"class\":[\"data.frame\"],\"row.names\":[1,2,3,4,5]}},\"z\":{\"type\":\"vector\",\"atom\":\"double\",\"data\":[4]}}"
  list(data=varList,json=json)
}

for(index in seq_along(sst)) {
  name <- names(sst)[index]
  testData <- sst[[index]]
  runSerTest(name,testData)
}


