source("serial.R")

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
    "type":"integer",
    "len":5,
    "short": true,
    "data":[1,2,3],
    "names":["one","two","three"]
  }'
  options <- createOptions(vec.len=3)
  list(data=data,json=json,options=options)
}

sst$`bool vector` <- {
  data <- c(TRUE,FALSE)
  names(data) <- c("true","false")
  json <- '{
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
    "type":"double",
    "len": 20,
    "short": true,
    "data":[-0.6265,0.1836,-0.8356,1.5953,0.3295,-0.8205,0.4874,0.7383,0.5758,-0.3054]
  }'
  list(data=data,json=json)
}

sst$charVector <- {
  data <- "hello"
  json <- '{
    "type":"character",
    "len": 1,
    "data":["hello"]
  }'
  list(data=data,json=json)
}

sst$complexVector <- {
  data <- 4 + 5i
  json <- '{
    "type":"complex",
    "len": 1,
    "data":["4+5i"]
  }'
  list(data=data,json=json)
}

sst$matrix <- {
  data <- matrix(as.numeric(1:8),nrow=2)
  json <- '{
    "type":"matrix",
    "atom":"double",
    "dim":[2,4],
    "data":[[1,3,5,7],[2,4,6,8]]
  }'
  list(data=data,json=json)
}

sst$`matrix options` <- {
  data <- matrix(as.numeric(1:8),nrow=2)
  json <- '{
    "type":"matrix",
    "atom":"double",
    "dim":[2,4],
    "short": true,
    "data":[[1,3,5],[2,4,6]]
  }'
  options <- createOptions(mat.len=3)
  list(data=data,json=json,options=options)
}

sst$matrix2 <- {
  data <- matrix(1:8,nrow=2)
  dimnames(data) <- list(x=c("a","b"),y=c("x","y","z","q"))
  json <- '{
    "type":"matrix",
    "atom":"integer",
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
    "type":"matrix",
    "atom":"integer",
    "dim":[2,4],
    "short": true,
    "data":[[1,3,5],[2,4,6]],
    "dimNames": [["a","b"],["x","y","z"]],
    "dimLabels": ["x","y"]
  }'
  options <- createOptions(mat.len=3)
  list(data=data,json=json,options=options)
}

sst$array <- {
  data <- array(as.numeric(1:24),dim=c(2,3,4))
  json <- '{
    "type":"array",
    "atom":"double",
    "dim":[2,3,4]
  }'
  list(data=data,json=json)
}

sst$factor1 <- {
  data <- factor(c(1,5,2,34,1,5,2,3,4,2,3,4))
  json <- '{
    "type":"factor",
    "ordered":false,
    "len": 12,
    "short": true,
    "data":["1","5","2","34","1","5","2","3","4","2"],
    "levels":["1","2","3","4","5","34"]
  }'
  list(data=data,json=json)
}

sst$factor2 <- {
  data <- ordered(c(1,2,4,2,4,4))
  json <- '{
    "type":"factor",
    "ordered":true,
    "len": 6,
    "data":["1","2","4","2","4","4"],
    "levels":["1","2","4"]
  }'
  list(data=data,json=json)
}

sst$factor3 <- {
  data <- factor(c("one","two","one","two","five"),levels=c("one", "two", "three", "four", "five"))
  json <- '{
    "type":"factor",
    "ordered":false,
    "len": 5,
    "data":["one","two","one","two","five"],
    "levels":["one","two","three","four","five"]
  }'
  list(data=data,json=json)
}

sst$`factor3 options` <- {
  data <- factor(c("one","two","one","two","five"),levels=c("one", "two", "three", "four", "five"))
  json <- '{
    "type":"factor",
    "ordered":false,
    "len": 5,
    "short": true,
    "data":["one","two","one"],
    "levelsShort": true,
    "levels":["one","two","three"]
  }'
  options <- createOptions(vec.len=3)
  list(data=data,json=json,options=options)
}
sst$dataframe <- {
  set.seed(1)
  data <- data.frame(a=1:5,b=runif(5),c=rep(TRUE,5),d=factor(c("one", "two", "one", "two", "five")))
  json <- '{
    "type":"data.frame",
    "dim":[5,4],
    "rowNames":["1","2","3","4","5"],
    "colNames":["a","b","c","d"],
    "colTypes":["integer","double","logical","factor"],
    "data":[[1,2,3,4,5],[0.2655,0.3721,0.5729,0.9082,0.2017],[true,true,true,true,true],["one","two","one","two","five"]]
  }'
  list(data=data,json=json)
}
sst$`dataframe opt 1` <- {
  set.seed(1)
  data <- data.frame(a=1:5,b=runif(5),c=rep(TRUE,5),d=factor(c("one", "two", "one", "two", "five")))
  json <- '{
    "type":"data.frame",
    "dim":[5,4],
    "rowShort": true,
    "rowNames":["1","2","3"],
    "colNames":["a","b","c","d"],
    "colTypes":["integer","double","logical","factor"],
    "data":[[1,2,3],[0.2655,0.3721,0.5729],[true,true,true],["one","two","one"]]
  }'
  options <- createOptions(df.len=3)
  list(data=data,json=json,options=options)
}
sst$`dataframe opt 2` <- {
  set.seed(1)
  data <- data.frame(a=1:5,b=runif(5),c=rep(TRUE,5),d=factor(c("one", "two", "one", "two", "five")))
  json <- '{
    "type":"data.frame",
    "dim":[5,4],
    "rowShort": true,
    "colShort": true,
    "rowNames":["1","2","3"],
    "colNames":["a","b","c"],
    "colTypes":["integer","double","logical"],
    "data":[[1,2,3],[0.2655,0.3721,0.5729],[true,true,true]]
  }'
  options <- createOptions(df.len=3,list.len=3)
  list(data=data,json=json,options=options)
}
sst$datatable <- {
  set.seed(1)
  data <- data.table::data.table(a=1:5,b=runif(5),c=rep(TRUE,5),d=factor(c("one", "two", "one", "two", "five")))
  json <- '{
    "type":"data.frame",
    "dim":[5,4],
    "rowNames":["1","2","3","4","5"],
    "colNames":["a","b","c","d"],
    "colTypes":["integer","double","logical","factor"],
    "data":[[1,2,3,4,5],[0.2655,0.3721,0.5729,0.9082,0.2017],[true,true,true,true,true],["one","two","one","two","five"]],
    "class": "data.table"
  }'
  list(data=data,json=json)
}

sst$list1 <- {
  set.seed(1)
  data <- list(x=1,y=2,z=3)
  json <- '{
    "type":"list",
    "len": 3,
    "names": ["x","y","z"],
    "data": [
      {
        "type": "double",
        "len": 1,
        "data": [1]
      },
      {
        "type": "double",
        "len": 1,
        "data": [2]
      },
      {
        "type": "double",
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
  x <- list(a=1,b=TRUE,c=1:5,d=y,e=list(a=list(a=list(a="xxx"))))
  data <- list(x=x)
  json <- '{
    "type": "list",
    "len": 1,
    "names":["x"],
    "data": [
      {
        "type":"list",
        "len": 5,
        "names": ["a","b","c","d","e"],
        "data":[
          {
            "type":"double",
            "len": 1,
            "data":[1]
          },
          {
            "type":"logical",
            "len": 1,
            "data":[true]
          },
          {
            "type":"integer",
            "len": 5,
            "data":[1,2,3,4,5]
          },
          {
            "type":"data.frame",
            "dim": [5,3],
            "rowNames":["1","2","3","4","5"],
            "colNames":["a","b","c"],
            "colTypes":["integer","double","logical"],
            "data":[[1,2,3,4,5],[0.2655,0.3721,0.5729,0.9082,0.2017],[true,true,true,true,true]]
          },
          {
            "type":"list",
            "len":1,
            "names": ["a"],
            "data":[
              {
                "type":"list",
                "len":1,
                "names": ["a"],
                "data":[
                   {
                    "type":"list",
                    "len":1,
                    "names": ["a"],
                    "data":[
                      {
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
      }
    ]
  }'
  list(data=data,json=json)
}

sst$`list2 opt` <- {
  set.seed(1)
  y <- data.frame(a=1:5,b=runif(5),c=rep(TRUE,5))
  x <- list(a=1,b=TRUE,c=1:5,d=y,e=list(a=list(a=list(a="xxx"))))
  data <- list(x=x)
  json <- '{
    "type": "list",
    "len": 1,
    "names":["x"],
    "data": [
      {
        "type":"list",
        "len": 5,
        "names": ["a","b","c","d","e"],
        "data":[
          {
            "type":"double",
            "len": 1,
            "data":[1]
          },
          {
            "type":"logical",
            "len": 1,
            "data":[true]
          },
          {
            "type":"integer",
            "len": 5,
            "data":[1,2,3,4,5]
          },
          {
            "type":"data.frame",
            "dim": [5,3],
            "rowNames":["1","2","3","4","5"],
            "colNames":["a","b","c"],
            "colTypes":["integer","double","logical"],
            "data":[[1,2,3,4,5],[0.2655,0.3721,0.5729,0.9082,0.2017],[true,true,true,true,true]]
          },
          {
            "type":"list",
            "len":1,
            "names": ["a"],
            "data":[
              {
                "type":"list",
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
    "type":"list",
    "len": 3,
    "names": ["a","",""],
    "data": [
      {
        "type": "double",
        "len": 1,
        "data": [1]
      },
      {
        "type": "double",
        "len": 1,
        "data": [2]
      },
      {
        "type": "double",
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
    "type": "function",
    "params": ["a","b","c","..."],
    "signature": "function (a = 5, b, c = list(z = 5), ...) "
  }'
  list(data=data,json=json)
}


for(index in seq_along(sst)) {
  name <- names(sst)[index]
  testData <- sst[[index]]
  runSerTestA(name,testData)
}


