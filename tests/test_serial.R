source("./R/serial.R")

##Serialization test Type A
## In this test, we pass an object and we call the preserialize function, which 
## returns an R object suitable for serialization with the a call to jsonlite::toJSON
## The testData passed in should be:
## - data - the object to be serialized
## - json - the json that results from calling jsonlite::toJSON
## - options - an options object to pass in to the serialization function. This is optional and defaults to the standard options.
runSerTestA <- function(name,testData) {
  preJson <- if(!is.null(testData$options)) {
    preserialize(testData$data,options=testData$options)
  }
  else {
    preserialize(testData$data)
  }
  json <- as.character(jsonlite::toJSON(preJson))
  refJson <- as.character(jsonlite::minify(testData$json))
  testthat::test_that(name,testthat::expect_that(json,testthat::equals(refJson)))
}

removeWhiteSpace <- function(s) {
  gsub("\\s+","",s)
}

##===========================
##simple serialization tests
## NOTES
## - digits setting not tested
## - list and vector length not tested (or implemented)
## - no classes/derived types tested - just the basic types (see name list)
##===========================
sst <- list()

Xsst <- list()

##integer vector, 1 element
sst$`Int Vector 1` <- {
  data <- 1L
  json <- '{
    "fmt":"vector",
    "type":"integer",
    "len":1,
    "data":[1]
  }'
  list(data=data,json=json)
}

## integer vector
##also tests names
sst$`Int Vector 2` <- {
  data <- 1:5
  names(data) <- c("one", "two", "three", "four", "five")
  json <- '{
    "fmt":"vector",
    "type":"integer",
    "len":5,
    "data":[1,2,3,4,5],
    "names":["one","two","three","four","five"]
  }'
  list(data=data,json=json)
}

sst$`Int Vector with options` <- {
  data <- 1:5
  names(data) <- c("one", "two", "three", "four", "five")
  json <- '{
    "fmt":"vector",
    "type":"integer",
    "len":5,
    "data":[1,2,3],
    "names":["one","two","three","four"]
  }'
  options <- createOptions(vec.len=3,names.len=4)
  list(data=data,json=json,options=options)
}

sst$`bool vector` <- {
  data <- c(TRUE,FALSE)
  names(data) <- c("true","false")
  json <- '{
    "fmt":"vector",
    "type":"logical",
    "len": 2,
    "data":[true,false],
    "names":["true","false"]
  }'
  list(data=data,json=json)
}

sst$`numeric vector` <- {
  set.seed(1)
  data <- rnorm(20)
  json <- '{
    "fmt":"vector",
    "type":"numeric",
    "len": 20,
    "data":[-0.6265,0.1836,-0.8356,1.5953,0.3295,-0.8205,0.4874,0.7383,0.5758,-0.3054]
  }'
  list(data=data,json=json)
}

sst$charVector <- {
  data <- "hello"
  json <- '{
    "fmt":"vector",
    "type":"character",
    "len": 1,
    "data":["hello"]
  }'
  list(data=data,json=json)
}

sst$complexVector <- {
  data <- 4 + 5i
  json <- '{
    "fmt":"vector",
    "type":"complex",
    "len": 1,
    "data":["4+5i"]
  }'
  list(data=data,json=json)
}

sst$matrix <- {
  data <- matrix(as.numeric(1:8),nrow=2)
  json <- '{
    "fmt":"matrix",
    "type":"numeric",
    "dim":[2,4],
    "data":[[1,3,5,7],[2,4,6,8]]
  }'
  list(data=data,json=json)
}

sst$`matrix options` <- {
  data <- matrix(as.numeric(1:8),nrow=2)
  json <- '{
    "fmt":"matrix",
    "type":"numeric",
    "dim":[2,4],
    "data":[[1,3,5],[2,4,6]]
  }'
  options <- createOptions(mat.len=3)
  list(data=data,json=json,options=options)
}

sst$matrix2 <- {
  data <- matrix(1:8,nrow=2)
  dimnames(data) <- list(x=c("a","b"),y=c("x","y","z","q"))
  json <- '{
    "fmt":"matrix",
    "type":"integer",
    "dim":[2,4],
    "data":[[1,3,5,7],[2,4,6,8]],
    "dimNames": [["a","b"],["x","y","z","q"]],
    "dimLabels": ["x","y"]
  }'
  list(data=data,json=json)
}

sst$`matrix2 opts` <- {
  data <- matrix(1:8,nrow=2)
  dimnames(data) <- list(x=c("a","b"),y=c("x","y","z","q"))
  json <- '{
    "fmt":"matrix",
    "type":"integer",
    "dim":[2,4],
    "data":[[1,3,5],[2,4,6]],
    "dimNames": [["a","b"],["x","y"]],
    "dimLabels": ["x","y"]
  }'
  options <- createOptions(mat.len=3,names.len=2)
  list(data=data,json=json,options=options)
}

sst$array <- {
  data <- array(as.numeric(1:24),dim=c(2,3,4))
  json <- '{
    "fmt":"array",
    "type":"numeric",
    "dim":[2,3,4]
  }'
  list(data=data,json=json)
}

sst$factor1 <- {
  data <- factor(c(1,5,2,34,1,5,2,3,4,2,3,4))
  json <- '{
    "fmt":"factor",
    "len": 12,
    "data":["1","5","2","34","1","5","2","3","4","2"],
    "levels":["1","2","3","4","5","34"]
  }'
  list(data=data,json=json)
}

sst$factor2 <- {
  data <- ordered(c(1,2,4,2,4,4))
  json <- '{
    "fmt":"factor",
    "class": "ordered",
    "len": 6,
    "data":["1","2","4","2","4","4"],
    "levels":["1","2","4"]
  }'
  list(data=data,json=json)
}

sst$factor3 <- {
  data <- factor(c("one","two","one","two","five"),levels=c("one", "two", "three", "four", "five"))
  json <- '{
    "fmt":"factor",
    "len": 5,
    "data":["one","two","one","two","five"],
    "levels":["one","two","three","four","five"]
  }'
  list(data=data,json=json)
}

sst$`factor3 options` <- {
  data <- factor(c("one","two","one","two","five"),levels=c("one", "two", "three", "four", "five"))
  json <- '{
    "fmt":"factor",
    "len": 5,
    "data":["one","two","one"],
    "lvlsLen": 5,
    "levels":["one","two","three"]
  }'
  options <- createOptions(vec.len=3)
  list(data=data,json=json,options=options)
}


sst$dataframe <- {
  set.seed(1)
  data <- data.frame(a=1:5,b=runif(5),c=rep(TRUE,5),d=factor(c("one", "two", "one", "two", "five")))
  json <- '{
    "fmt":"data.frame",
    "dim":[5,4],
    "rowNames":["1","2","3","4","5"],
    "colNames":["a","b","c","d"],
    "colTypes":["integer","numeric","logical","factor"],
    "data":[[1,2,3,4,5],[0.2655,0.3721,0.5729,0.9082,0.2017],[true,true,true,true,true],["one","two","one","two","five"]]
  }'
  list(data=data,json=json)
}
sst$`dataframe opt 1` <- {
  set.seed(1)
  data <- data.frame(a=1:5,b=runif(5),c=rep(TRUE,5),d=factor(c("one", "two", "one", "two", "five")))
  json <- '{
    "fmt":"data.frame",
    "dim":[5,4],
    "rowNames":["1","2"],
    "colNames":["a","b"],
    "colTypes":["integer","numeric","logical","factor"],
    "data":[[1,2,3],[0.2655,0.3721,0.5729],[true,true,true],["one","two","one"]]
  }'
  options <- createOptions(df.len=3,names.len=2)
  list(data=data,json=json,options=options)
}
sst$`dataframe opt 2` <- {
  set.seed(1)
  data <- data.frame(a=1:5,b=runif(5),c=rep(TRUE,5),d=factor(c("one", "two", "one", "two", "five")))
  json <- '{
    "fmt":"data.frame",
    "dim":[5,4],
    "rowNames":["1","2","3","4","5"],
    "colNames":["a","b","c","d"],
    "colTypes":["integer","numeric","logical"],
    "data":[[1,2,3],[0.2655,0.3721,0.5729],[true,true,true]]
  }'
  options <- createOptions(df.len=3,list.len=3)
  list(data=data,json=json,options=options)
}
sst$datatable <- {
  set.seed(1)
  data <- data.table::data.table(a=1:5,b=runif(5),c=rep(TRUE,5),d=factor(c("one", "two", "one", "two", "five")))
  json <- '{
    "fmt":"data.frame",
    "class": "data.table",
    "dim":[5,4],
    "rowNames":["1","2","3","4","5"],
    "colNames":["a","b","c","d"],
    "colTypes":["integer","numeric","logical","factor"],
    "data":[[1,2,3,4,5],[0.2655,0.3721,0.5729,0.9082,0.2017],[true,true,true,true,true],["one","two","one","two","five"]]
  }'
  list(data=data,json=json)
}

sst$list1 <- {
  set.seed(1)
  data <- list(x=1,y=2,z=3)
  json <- '{
    "fmt":"list",
    "len": 3,
    "names": ["x","y","z"],
    "data": [
      {
        "fmt": "vector",
        "type": "numeric",
        "len": 1,
        "data": [1]
      },
      {
        "fmt": "vector",
        "type": "numeric",
        "len": 1,
        "data": [2]
      },
      {
        "fmt": "vector",
        "type": "numeric",
        "len": 1,
        "data": [3]
      }
    ]
  }'
  list(data=data,json=json)
}

sst$list2 <- {
  set.seed(1)
  y <- data.frame(a=1:5,b=runif(5),c=rep(TRUE,5))
  x <- list(a=1,b=TRUE,c=1:5,d=y,e=list(a=list(a="xxx")))
  data <- list(x=x)
  json <- '{
    "fmt": "list",
    "len": 1,
    "names":["x"],
    "data": [
      {
        "fmt":"list",
        "len": 5,
        "names": ["a","b","c","d","e"],
        "data":[
          {
            "fmt":"vector",
            "type":"numeric",
            "len": 1,
            "data":[1]
          },
          {
            "fmt":"vector",
            "type":"logical",
            "len": 1,
            "data":[true]
          },
          {
            "fmt":"vector",
            "type":"integer",
            "len": 5,
            "data":[1,2,3,4,5]
          },
          {
            "fmt":"data.frame",
            "dim": [5,3],
            "rowNames":["1","2","3","4","5"],
            "colNames":["a","b","c"],
            "colTypes":["integer","numeric","logical"],
            "data":[[1,2,3,4,5],[0.2655,0.3721,0.5729,0.9082,0.2017],[true,true,true,true,true]]
          },
          {
            "fmt":"list",
            "len":1,
            "names": ["a"],
            "data":[
              {
                "fmt":"list",
                "len":1,
                "names": ["a"],
                "data":[
                  {
                    "fmt":"vector",
                    "type":"character",
                    "len": 1,
                    "data":["xxx"]
                  }
                ]
              }
            ]
          }
        ]
      }
    ]
  }'
  options <- createOptions(max.depth = 5)
  list(data=data,json=json,options=options)
}

sst$`list2 opt` <- {
  set.seed(1)
  y <- data.frame(a=1:5,b=runif(5),c=rep(TRUE,5))
  x <- list(a=1,b=TRUE,c=1:5,d=y,e=list(a=list(a="xxx")))
  data <- list(x=x)
  json <- '{
    "fmt": "list",
    "len": 1,
    "names":["x"],
    "data": [
      {
        "fmt":"list",
        "len": 5,
        "names": ["a","b","c","d","e"],
        "data":[
          {
            "fmt":"vector",
            "type":"numeric",
            "len": 1,
            "data":[1]
          },
          {
            "fmt":"vector",
            "type":"logical",
            "len": 1,
            "data":[true]
          },
          {
            "fmt":"vector",
            "type":"integer",
            "len": 5,
            "data":[1,2,3,4,5]
          },
          {
            "fmt":"data.frame",
            "dim": [5,3],
            "rowNames":["1","2","3","4","5"],
            "colNames":["a","b","c"],
            "colTypes":["integer","numeric","logical"],
            "data":[[1,2,3,4,5],[0.2655,0.3721,0.5729,0.9082,0.2017],[true,true,true,true,true]]
          },
          {
            "fmt":"list",
            "len":1,
            "names": ["a"],
            "data":[
              {
                "fmt":"list",
                "len":1,
                "names": ["a"],
                "depthExceeded": true
              }
            ]
          }
        ]
      }
    ]
  }'
  options <- createOptions(max.depth=3)
  list(data=data,json=json,options=options)
}

sst$`list w/unnamed` <- {
  set.seed(1)
  data <- list(a=1,2,3)
  json <- '{
    "fmt":"list",
    "len": 3,
    "names": ["a","",""],
    "data": [
      {
        "fmt": "vector",
        "type": "numeric",
        "len": 1,
        "data": [1]
      },
      {
        "fmt": "vector",
        "type": "numeric",
        "len": 1,
        "data": [2]
      },
      {
        "fmt": "vector",
        "type": "numeric",
        "len": 1,
        "data": [3]
      }
    ]
  }'
  list(data=data,json=json)
}


sst$fun <- {
  data <- function(a=5,b,c=list(z=5),...) 34
  json <- '{
    "fmt": "function",
    "params": ["a","b","c","..."],
    "paramList": "(a = 5, b, c = list(z = 5), ...)"
  }'
  list(data=data,json=json)
}


for(index in seq_along(sst)) {
  name <- names(sst)[index]
  testData <- sst[[index]]
  runSerTestA(name,testData)
}

