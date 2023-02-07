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
    getCallText(astList)
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

getCallText <- function(astList) {
  callType <- getEntryValue(astList)
  if(callType == "function") {
    paramList <- getParamList(astList[[2]])
    body <- processExpr(astList[[3]])
    sprintf("FunctionDef(%s,%s)",paramList,body)
  }
  else if(length(astList) > 1) {
    sprintf("StdCall(%s,%s)",processExpr(astList[[1]]),getArgList(tail(astList,-1)))
  }
  else {
    sprintf("StdCall(%s)",processExpr(astList[[1]]))
  }
}

##Uggh!! need to fix these
getArgList <- function(astList) {
  argList <- sapply(1:length(astList),function(i) {
    argExprText <- processExpr(astList[[i]])
    if((!is.null(names(astList)))&&(names(astList)[i] != "")) {
      sprintf('ArgValue(Identifier,"=",%s)',argExprText)
    }
    else {
      sprintf('ArgValue(%s)',argExprText)
    }
  })
  sprintf("ArgList(%s)",paste(argList,collapse=","))
}

getParamList <- function(astEntry) {
  astList = as.list(astEntry)
  
  paramList <- sapply(1:length(astList),function(i) {
    ##we don't actually use the name, just the "Identifier" token
    if(class(astList[[i]]) != "name") {
      sprint("ParamValue(Identifier,"=",%s)",processExpr(astList[[i]]))
    }
    else {
      "ParamValue(Identifier)"
    }
  })
  sprintf("ParamList,(%s)",paste(paramList,collapse=","))
}


