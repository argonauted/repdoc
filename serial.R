DEFAULT_VECTOR_LENGTH <- 8
DEFAULT_LIST_LENGTH <- 8
DEFAULT_DIGITS <- 4

SHORTENED_ATTRIBUTES <- c("names","dimnames","rownames")

## attributes
## If we shorten an object, we also shortedn some attributes (by sending the attributes from the shortened object)
## FROM SHORTENED OBJECT:
## - names
## - dimnames
## - rownames
## FROM FULL OBJECT:
## - dim
## - levels (note - we may want to restrict the length of this too, but taking from the shortened object will not do that.)
## - classes
## - (all others)


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


serialize <- function(obj, vec.len=DEFAULT_VECTOR_LENGTH, list.len=DEFAULT_LIST_LENGTH) {
  if(is.null(obj)) {
    ## this means NULL is ignored (I'd like to keep it)
    return(NULL)
  }
  
  if(is.atomic(obj)) {
    serializeAtomic(obj,vec.len)
  }
  else if(is.recursive(obj)) {
    serializeRecursive(obj,vec.len,list.len)
  }
  else {
    ## All else is ignored for now
    return(NULL)
  }
}

serializeAtomic <- function(obj,vec.len) {
  objInfo <- list()
  if(is.vector(obj)) {
    objInfo$type <- jsonlite::unbox("vector")
    ## shorten if applicable
    if(length(obj) > vec.len) {
      objInfo$length <- length(obj) #save orig length - not present in attributes!
      objInfo$shortened <- TRUE
      effObj <- getShortenedVector(obj,vec.len)
    }
    else {
      effObj <- obj
    }
  }
  else if(is.matrix(obj)) {
    objInfo$type <- jsonlite::unbox("matrix")
    ## shorten if applicable
    if( any(dim(obj) > vec.len) ) {
      objInfo$shortened <- TRUE
      effObj <- getShortenedArray(obj,vec.len)
    }
    else {
      effObj <- obj
    }
  }
  else if(is.array(obj)) {
    objInfo$type <- jsonlite::unbox("array")
    ## shorten if applicable
    if( any(dim(obj) > vec.len) ) {
      objInfo$shortened <- TRUE
      effObj <- getShortenedArray(obj,vec.len)
    }
    else {
      effObj <- obj
    }
  }
  else if(is.factor(obj)) {
    objInfo$type <- jsonlite::unbox("factor")
    objInfo$ordered <- jsonlite::unbox(is.ordered(obj))
    ## shorten if applicable
    if(length(obj) > vec.len) {
      objInfo$length <- length(obj) #save orig length - not present in attributes!
      objInfo$shortened <- TRUE
      effObj <- getShortenedVector(obj,vec.len)
    }
    else {
      effObj <- obj
    }
  }
  else {
    ## FIGURE OUT WHAT GOES HERE
    objInfo$type <- jsonlite::unbox("UNKNOWN ATOMIC TYPE")
  }
  objInfo$atom = jsonlite::unbox(typeof(obj))
  objInfo$data = effObj ##shortened if needed.
  
  objInfo <- addAttributes(objInfo,obj,effObj)
  objInfo
}

serializeRecursive <- function(obj,vec.len,list.len) {
  objInfo <- list()
  if(is.data.frame(obj)) {
    objInfo$type <- jsonlite::unbox("data.frame")
    if(ncol(obj) > list.len || nrow(obj) > vec.len) {
      objInfo$shortened <- TRUE
      objInfo$cols <- ncol(obj)
      objInfo$rows <- nrow(obj)
      effObj <- getShortenedDataFrame(obj,vec.len,list.len)
    }
    else {
      effObj <- obj
    }
    ## for data, this will give us json matrix. Our names are already in attributes
    standInObj <- as.list(effObj)
    names(standInObj) <- NULL
    objInfo$data <- standInObj
  }
  else if(is.list(obj)) {
    objInfo$type <- jsonlite::unbox("list")
    if(length(obj) > list.len) {
      objInfo$length <- length(obj) #save orig length - not present in attributes!
      objInfo$shortened <- TRUE
      effObj <- getShortenedList(obj,list.len)
    }
    else {
      effObj <- obj
    }
    objInfo$data <- purrr::map(effObj,serialize,vec.len=vec.len,list.len=list.len)
  }
  else {
    ## figure out what to do here. For now ignore
    return(NULL)
  }
  
  objInfo <- addAttributes(objInfo,obj,effObj)
  objInfo
}

## This adds the addtribues to the object info for the object obj
## We are passed both the original object and the shortened object
## We will take some attributes from the shortened object - that correspond
## to _supported_ shortened fields
## note - we are not shortening factor options!
addAttributes <- function(objInfo,obj,effObj) {
  attr <- attributes(obj)
   
  if(!is.null(attr)) {
    objInfo$attributes <- attr
    if(effObj != obj) {
      ## take some of the attributes from the shortened object!
      attrS <- attributes(effObj)
      namesFromShortened <- names(attrS)[names(attrS) %in% SHORTENED_ATTRIBUTES]
      objInfo$attributes[namesFromShortened] <- attrs[namesFromShortened]
    }
    objInfo
  }
  else {
    objInfo
  }
}

##-----------------------------
## These functions returned truncated versions of our objects
##-----------------------------

getShortenedVector <- function(obj,vec.len) {
  obj[1:min(length(obj),vec.len)]
}
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
