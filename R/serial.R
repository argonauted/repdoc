DEFAULT_OPTIONS <- list(vec.len=10,mat.len=5,list.len=10,df.len=5,names.len=100,digits=4,max.depth=4)

createOptions <- function(vec.len=NULL,mat.len=NULL,list.len=NULL,df.len=NULL,names.len=NULL,digits=NULL,max.depth=NULL) {
  options <- DEFAULT_OPTIONS
  if(!is.null(vec.len)) options$vec.len = vec.len
  if(!is.null(mat.len)) options$mat.len = mat.len
  if(!is.null(list.len)) options$list.len = list.len
  if(!is.null(names.len)) options$names.len = names.len
  if(!is.null(df.len)) options$df.len = df.len
  if(!is.null(digits)) options$digits = digits
  if(!is.null(max.depth)) options$max.depth = max.depth
  options
}


makeJson <- function(preJson,options=DEFAULT_OPTIONS) {
  jsonlite::toJSON(preJson,digits=options$digits,null="null",force=TRUE)
}


preserialize <- function(obj, depth=1, options=DEFAULT_OPTIONS) {
  if(is.null(obj)) {
    ## this means NULL is ignored (I'd like to keep it)
    NULL
  }
  else if(is.atomic(obj)) {
    preserializeAtomic(obj,options)
  }
  else if(is.function(obj)) {
    preserializeFunction(obj,options)
  }
  else if(is.recursive(obj)) {
    preserializeRecursive(obj,depth,options)
  }
  else {
    ## All else is ignored for now
    NULL
  }
}

preserializeAtomic <- function(obj,options=DEFAULT_OPTIONS) {
  objInfo <- list()
  if(is.vector(obj)) {
    preserializeVector(obj,options)
  }
  else if(is.matrix(obj)) {
    preserializeMatrix(obj,options)
  }
  else if(is.array(obj)) {
    preserializeArray(obj,options)
  }
  else if(is.factor(obj)) {
    preserializeFactor(obj,options)
  }
  else {
    preserializeUnknownAtomic(obj,options)
  }
}

preserializeVector <- function(obj,options=DEFAULT_OPTIONS) {
  objInfo <- list()
  objInfo$fmt <- jsonlite::unbox("vector")
  boxedType <- getAtomicType(obj)
  objInfo$type <- jsonlite::unbox(boxedType)
  dataObj <- head(obj,options$vec.len)
  if(!identical(boxedType,class(obj)[1])) {
    objInfo$class = jsonlite::unbox(class(obj)[1])
    dataObj <- as.vector(dataObj)
  }
  objInfo$len <- jsonlite::unbox(length(obj))
  objInfo$data <- dataObj
  if(!is.null(names(obj))) {
    objInfo$names <- head(names(obj),options$names.len)
  }
  objInfo
}

preserializeMatrix <- function(obj,options=DEFAULT_OPTIONS) {
  objInfo <- list()
  objInfo$fmt <- jsonlite::unbox("matrix")
  objInfo$type <- jsonlite::unbox(getAtomicType(obj))
  if( any(dim(obj) > options$mat.len) ) {
    dataObj <- getShortenedArray(obj,options$mat.len)
  }
  else {
    dataObj <- obj
  }
  if(!identical("matrix",class(obj)[1])) {
    objInfo$class = jsonlite::unbox(class(obj)[1])
    dataObj <- as.matrix(dataObj)
  }
  objInfo$dim <- dim(obj)
  dimNames <- dimnames(obj)
  dimnames(dataObj) <- NULL
  objInfo$data <- dataObj
  if(!is.null(dimNames)) {
    dimLabels <- names(dimNames)
    names(dimNames) <- NULL
    dimNames <- lapply(dimNames,head,n=options$names.len)
    objInfo$dimNames <- dimNames
    if(!is.null(dimLabels)) {
      objInfo$dimLabels <- dimLabels
    }
  }
  objInfo
}

preserializeArray <- function(obj,options=DEFAULT_OPTIONS) {
  objInfo <- list()
  objInfo$fmt <- jsonlite::unbox("array")
  objInfo$type <- jsonlite::unbox(getAtomicType(obj))
  if(!identical("array",class(obj)[1])) {
    objInfo$class = jsonlite::unbox(class(obj)[1])
  }
  objInfo$dim <- dim(obj)
  dimNames <- dimnames(obj)
  if(!is.null(dimNames)) {
    dimLabels <- names(dimNames)
    names(dimNames) = NULL
    dimNames <- lapply(dimNames,head,n=options$names.len)
    objInfo$dimNames <- dimNames
    if(!is.null(dimLabels)) {
      objInfo$dimLabels <- dimLabels
    }
  }
  objInfo
}

preserializeFactor <- function(obj,options=DEFAULT_OPTIONS) {
  objInfo <- list()
  objInfo$fmt <- jsonlite::unbox("factor")
  if(!identical("factor",class(obj)[1])) {
    objInfo$class = jsonlite::unbox(class(obj)[1])
  }
  objInfo$len <- jsonlite::unbox(length(obj))
  objInfo$data <- as.character(head(obj,options$vec.len))
  if(length(levels(obj)) > options$vec.len) {
    objInfo$lvlsLen <- jsonlite::unbox(length(levels(obj)))
    objInfo$levels <- as.character(head(levels(obj),options$vec.len))
  }
  else {
    objInfo$levels <- as.character(levels(obj))
  }
  objInfo
}

preserializeUnknownAtomic <- function(obj,options=DEFAULT_OPTIONS) {
  objInfo <- list()
  objInfo$fmt <- jsonlite::unbox("Atomic")
  objInfo$class <- jsonlite::unbox(class(obj)[1])
  objInfo
}

## This is what we call the type of the atomic object. It is the same as
## typeof except instead of using double we use numeric, only because
## we want this field to be the same as the class
getAtomicType <- function(atomicObj) {
  type <- typeof(atomicObj)
  if(identical(type,"double")) "numeric" else type
}

preserializeRecursive <- function(obj,depth=1,options=DEFAULT_OPTIONS) {
  if(is.data.frame(obj)) {
    preserializeDataFrame(obj,options)
  }
  else if(is.list(obj)) {
    preserializeList(obj,depth,options)
  }
  else {
    preserializeUnknownRecursive(obj,options)
  }
}

preserializeDataFrame <- function(obj,options=DEFAULT_OPTIONS) {
  objInfo <- list()
  objInfo$fmt <- jsonlite::unbox("data.frame")
  if(!identical(class(obj)[1],"data.frame")) {
    objInfo$class <- jsonlite::unbox(class(obj)[1])
  }
  objInfo$dim <- dim(obj)
  
  dataObj <- obj
  if(nrow(dataObj) > options$df.len) {
    dataObj <- head(dataObj,options$df.len)
  }
  dataObj <- as.list(dataObj)
  if(length(dataObj) > options$list.len) {
    dataObj <- head(dataObj,options$list.len)
  }
  names(dataObj) <- NULL
  colNames <- head(colnames(obj),options$names.len)
  rowNames <- head(rownames(obj),options$names.len)
  colTypes <- sapply(dataObj,getColType)
  objInfo$rowNames <- rowNames
  objInfo$colNames <- colNames
  objInfo$colTypes <- colTypes
  objInfo$data <- dataObj
  objInfo
}

getColType <- function(columnObj) {
  if(is.factor(columnObj)) {
    "factor"
  }
  else {
    getAtomicType(columnObj)
  }
}

preserializeList <- function(obj,depth=1,options=DEFAULT_OPTIONS) {
  objInfo <- list()
  objInfo$fmt <- jsonlite::unbox("list")
  if(!identical(class(obj)[1],"list")) {
    objInfo$class <- jsonlite::unbox(class(obj)[1])
  }
  objInfo$len <- jsonlite::unbox(length(obj))
  if(length(obj) > options$list.len) {
    dataObj <- head(obj,options$list.len)
  }
  else {
    dataObj <- obj
  }
  nms <- head(names(obj),n=options$names.len)
  objInfo$names <- nms
  if(depth <= options$max.depth) {
    data <- purrr::map(dataObj,preserialize,depth=depth+1,options)
    names(data) <- NULL
    objInfo$data <- data
  }
  else {
    objInfo$depthExceeded <- jsonlite::unbox(TRUE)
  }
  
  objInfo
}

preserializeUnknownRecursive <- function(obj,options=DEFAULT_OPTIONS) {
  objInfo <- list()
  objInfo$type <- jsonlite::unbox("Recursive")
  objInfo$class <- jsonlite::unbox(class(obj)[1])
  objInfo
}

preserializeFunction <- function(obj,options=DEFAULT_OPTIONS) {
  objInfo <- list()
  objInfo$fmt <- jsonlite::unbox("function")
  objInfo$params <- formalArgs(obj)
  objInfo$paramList <- jsonlite::unbox(getParamList(obj))
  objInfo
}

getParamList <- function(func) {
  val <- deparse(args(func))
  length(val) <- length(val) - 1
  val <- trimws(val,which="left")
  val <- paste(val,collapse="")
  val <- substring(val,nchar("function "))
  trimws(val)
}

##-----------------------------
## These functions returned truncated versions of our objects
##-----------------------------

getShortenedArray <- function(obj,vec.len) {
  ##create a list with the range of index values we want
  subsetRanges <- lapply(dim(obj),function(n) 1:min(n,vec.len))
  ##use do call to pass function args as a list, for the function "["
  do.call("[",c(list(obj),subsetRanges))
}
getShortenedDataFrame <- function(obj,vec.len,list.len) {
  nr <- min(nrow(obj),vec.len)
  nc <- min(ncol(obj),list.len)
  obj[1:nr,1:nc]
}
getShortenedList <- function(obj,list.len) {
  obj[1:min(length(obj),list.len)]
}
