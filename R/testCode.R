analyzeCode <- function(codeText) {
  print(processExpr(rlang::parse_expr(codeText)))
}


processExpr <- function(astEntry) {
  astList = as.list(astEntry)
  
  if(rlang::is_syntactic_literal(astEntry)) {
    ##no action
    sprintf("(Literal,%s)",getEntryValue(astList))
  }
  else if(rlang::is_symbol(astEntry)) {
    sprintf("(Symbol,%s)",getEntryValue(astList))
  }
  else if(rlang::is_call(astEntry)) {
    callType <- getEntryValue(astList)
    sprintf("(Call,%s,%s)",callType,getCallArgs(callType,astList))
  }
  else if(rlang::is_pairlist(astEntry)) {
    stop("pairlist!")
  }
  else {
    stop("unkown!")
  }
}

getEntryValue <- function(astList) {
  as.character(astList[[1]])
}

getCallArgs <- function(callType,astList) {
  if(callType == "function") {
    paramList <- getParamList(astList[[2]])
    body <- processExpr(astList[[3]])
    paste(paramList,",",body,sep="")
  }
  else if(length(astList) > 1) {
    getArgList(tail(astList,-1))
  }
  else {
    "-"
  }
}

getArgList <- function(astList) {
  argList <- sapply(1:length(astList),function(i) {
      arg <- list(arg=processExpr(astList[[i]]))
      if(!is.null(names(astList))) {
        name <- names(astList)[i]
        if(name != "") arg$label <- name
      }
      arg
  })
    
  paste("(ArgList,",paste(argList,collapse=","),")",sep="")
}

getParamList <- function(astEntry) {
  astList = as.list(astEntry)
  
  paramList <- sapply(1:length(astList),function(i) {
    param <- list(param=names(astList)[[i]])
    if(class(astList[[i]]) != "name") param$default <- processExpr(astList[[i]])
    param
  })
  paste("(ParamList,",paste(paramList,collapse=","),")",sep="")
}


