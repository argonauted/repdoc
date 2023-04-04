DEFAULT_OPTIONS <- list(vec.len=10,mat.len=5,list.len=100,df.len=5,digits=4,max.depth=5)

createOptions <- function(vec.len=NULL,mat.len=NULL,list.len=NULL,df.len=NULL,digits=NULL,max.depth=NULL) {
  options <- DEFAULT_OPTIONS
  if(!is.null(vec.len)) options$vec.len = vec.len
  if(!is.null(mat.len)) options$mat.len = mat.len
  if(!is.null(list.len)) options$list.len = list.len
  if(!is.null(df.len)) options$df.len = df.len
  if(!is.null(digits)) options$digits = digits
  if(!is.null(max.depth)) options$max.depth = max.depth
  options
}

##value list includes a list of variable values, with the names given by the variable names
## functions (and some other things) are ignored
serializeValues <- function(valueList, vec.len=DEFAULT_VECTOR_LENGTH, list.len=DEFAULT_LIST_LENGTH, DEFAULT_DIGITS=4) {
  as.character(jsonlite::toJSON(purrr::reduce2(valueList,names(valueList),makeResult,.init=list(),vec.len=vec.len,list.len=list.len),digits=digits))
}

makeResult <- function(jsonList, val, nm, vec.len, list.len) {
  if(startsWith(nm,".")) {
    ## ignore names starting with a period
    return(NULL)
  }
  
  modVal <- serialize(val,vec.len=vec.len,list.len=list.len)
  if(!is.null(modVal)) {
    jsonList[[nm]] <- modVal
  }
  jsonList
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
  objInfo$type <- jsonlite::unbox(typeof(obj))
  objInfo$len <- jsonlite::unbox(length(obj))
  if(objInfo$len > options$vec.len) {
    objInfo$short <- jsonlite::unbox(TRUE)
  }
  objInfo$data <- head(obj,options$vec.len)
  if(!is.null(names(obj))) {
    objInfo$names <- head(names(obj),options$vec.len)
  }
  objInfo
}

preserializeMatrix <- function(obj,options=DEFAULT_OPTIONS) {
  objInfo <- list()
  objInfo$type <- jsonlite::unbox("matrix")
  objInfo$atom <- jsonlite::unbox(typeof(obj))
  objInfo$dim <- dim(obj)
  if( any(dim(obj) > options$mat.len) ) {
    objInfo$short <- jsonlite::unbox(TRUE)
    objInfo$data <- getShortenedArray(obj,options$mat.len)
  }
  else {
    objInfo$data <- obj
  }
  dimNames <- dimnames(objInfo$data)
  dimLabels <- names(dimNames)
  if(!is.null(dimNames)) {
    names(dimNames) = NULL ## we pass these dim labels below
    objInfo$dimNames <- dimNames
  }
  if(!is.null(dimLabels)) {
    objInfo$dimLabels <- dimLabels
  }
  objInfo
}

preserializeArray <- function(obj,options=DEFAULT_OPTIONS) {
  objInfo <- list()
  objInfo$type <- jsonlite::unbox("array")
  objInfo$atom <- jsonlite::unbox(typeof(obj))
  objInfo$dim <- dim(obj)
  dimNames <- dimnames(objInfo$data)
  dimLabels <- names(dimNames)
  if(!is.null(dimNames)) {
    names(dimNames) = NULL ## we pass these dim labels below
    objInfo$dimNames <- dimNames
  }
  if(!is.null(dimLabels)) {
    objInfo$dimLabels <- dimLabels
  }
  objInfo
  
}

preserializeFactor <- function(obj,options=DEFAULT_OPTIONS) {
  objInfo <- list()
  objInfo$type <- jsonlite::unbox("factor")
  objInfo$ordered <- jsonlite::unbox(is.ordered(obj))
  objInfo$len <- jsonlite::unbox(length(obj))
  
  if(length(obj) > options$vec.len) {
    objInfo$short <- jsonlite::unbox(TRUE)
  }
  objInfo$data <- head(obj,options$vec.len)
  
  if(length(levels(obj)) > options$vec.len) {
    objInfo$levelsShort <- jsonlite::unbox(TRUE)
  }
  objInfo$levels <- head(levels(obj),options$vec.len)
  
  objInfo
}

preserializeUnknownAtomic <- function(obj,options=DEFAULT_OPTIONS) {
  objInfo <- list()
  objInfo$type <- jsonlite::unbox("Atomic Type")
  objInfo
}

preserializeRecursive <- function(obj,depth=1,options=DEFAULT_OPTIONS) {
  if(is.data.frame(obj)) {
    preserializeDataFrame(obj,options)
  }
  else {
    preserializeList(obj,depth,options)
  }
}

preserializeDataFrame <- function(obj,options=DEFAULT_OPTIONS) {
  objInfo <- list()
  objInfo$type <- jsonlite::unbox("data.frame")
  objInfo$dim <- dim(obj)
  
  effObj <- obj
  if(nrow(effObj) > options$df.len) {
    objInfo$rowShort <- jsonlite::unbox(TRUE)
    effObj <- head(effObj,options$df.len)
  }
  rowNames <- rownames(effObj)
  effObj <- as.list(effObj)
  if(length(effObj) > options$list.len) {
    objInfo$colShort <- jsonlite::unbox(TRUE)
    effObj <- head(effObj,options$list.len)
  }
  colNames <- names(effObj)
  colTypes <- sapply(effObj,getColType)
  names(effObj) <- NULL
  objInfo$rowNames <- rowNames
  objInfo$colNames <- colNames
  objInfo$colTypes <- colTypes
  objInfo$data <- effObj
  if(class(obj)[1] != "data.frame") {
    objInfo$class <- jsonlite::unbox(class(obj)[1])
  }
  objInfo
}

getColType <- function(columnObj) {
  if(is.factor(columnObj)) {
    "factor"
  }
  else {
    typeof(columnObj)
  }
}

preserializeList <- function(obj,depth=1,options=DEFAULT_OPTIONS) {
  objInfo <- list()
  objInfo$type <- jsonlite::unbox("list")
  objInfo$len <- jsonlite::unbox(length(obj))
  if(length(obj) > options$list.len) {
    objInfo$short <- TRUE
    effObj <- head(obj,options$list.len)
  }
  else {
    effObj <- obj
  }
  nms <- names(effObj)
  objInfo$names <- nms
  if(depth <= options$max.depth) {
    data <- purrr::map(effObj,preserialize,depth=depth+1,options)
    names(data) <- NULL
    objInfo$data <- data
  }
  else {
    objInfo$depthExceeded <- jsonlite::unbox(TRUE)
  }
  if(class(obj)[1] != "list") {
    objInfo$class <- jsonlite::unbox(class(obj)[1])
  }
  objInfo
}

preserializeFunction <- function(obj,options=DEFAULT_OPTIONS) {
  objInfo <- list()
  objInfo$type <- jsonlite::unbox("function")
  objInfo$params <- formalArgs(obj)
  signature <- jsonlite::unbox(deparse(args(obj))[1])
  objInfo$signature <- signature 
  objInfo
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
