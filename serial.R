##value list includes a list of variable values, with the names given by the variable names
## functions (and some other things) are ignored
serializeValues <- function(valueList,vex.len=8,list.len=8) {
  jsonlite::toJSON(purrr::reduce2(valueList,names(valueList),makeResult,.init=list()))
}

makeResult <- function(jsonList,val,nm) {
  modVal <- serialize(val)
  if(!is.null(modVal)) {
    jsonList[[nm]] <- modVal
  }
  jsonList
}


serialize <- function(obj,vec.len=8,list.len=8) {
  if(is.null(obj)) {
    ## this means NULL is ignored (I'd like to keep it)
    return(NULL)
  }
  
  if(is.atomic(obj)) {
    serializeAtomic(obj)
  }
  else if(is.recursive(obj)) {
    serializeRecursive(obj)
  }
  else {
    ## All else is ignored for now
    return(NULL)
  }
}

serializeAtomic <- function(obj) {
  objInfo <- list()
  if(is.vector(obj)) {
    objInfo$type <- jsonlite::unbox("vector")
  }
  else if(is.matrix(obj)) {
    objInfo$type <- jsonlite::unbox("matrix")
  }
  else if(is.array(obj)) {
    objInfo$type <- jsonlite::unbox("array")
  }
  else if(is.factor(obj)) {
    objInfo$type <- jsonlite::unbox("factor")
    objInfo$ordered <- jsonlite::unbox(is.ordered(obj))
  }
  else {
    ## FIGURE OUT WHAT GOES HERE
    objInfo$type <- jsonlite::unbox("UNKNOWN ATOMIC TYPE")
  }
  objInfo$atom = jsonlite::unbox(typeof(obj))
  objInfo$data = obj
  
  objInfo <- addAttributes(objInfo,obj)
  objInfo
}

serializeRecursive <- function(obj) {
  objInfo <- list()
  if(is.data.frame(obj)) {
    objInfo$type <- jsonlite::unbox("data.frame")
    ## for data, this will give us json matrix. Our names are already in attributes
    standInObj <- as.list(obj)
    names(standInObj) <- NULL
    objInfo$data <- standInObj
  }
  else if(is.list(obj)) {
    objInfo$type <- jsonlite::unbox("list")
    objInfo$data <- serializeValues(obj)
  }
  else {
    ## figure out what to do here. For now ignore
    return(NULL)
  }
  
  objInfo <- addAttributes(objInfo,obj)
  objInfo
}

## This addes the addtribues to the object info for the object obj
addAttributes <- function(objInfo,obj) {
  attr <- attributes(obj)
  if(!is.null(attr)) {
    objInfo$attributes <- attr
    objInfo
  }
  else {
    objInfo
  }
}
