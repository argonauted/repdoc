
## This funtion gets the extenal dependency symbols in some code text
getDependencies <- function(codeText,hiddenSymbols) {
  ## this is kludgey
  ## also I should protect against funny input. Maybe get an ast to see if it is a good command (above)
  closureText <- sprintf("function() {\n%s\n}",codeText)
  closureExpr <- rlang::parse_expr(closureText)
  closure <- eval(closureExpr)
  codetools::findGlobals(closure)
}

## This function loads the variable list from the current environment
getEnvironmentVars <- function(envir) {
  envirVars <- sapply(names(envir),function(name) envir[[name]])
  names(envirVars) = names(envir)

  ##remove hidden variables
  envirVars <- envirVars[names(varList) != "docState"]

  envirVars
}

insertAfter <- function(localDocState,entry,index) {
  newWrappedEntry <- list()
  newWrappedEntry[[entry$id]] <- entry
  if(index > 0) {
    localDocState$lines <- append(localDocState$lines,newWrappedEntry,after=index)
  }
  else {
    localDocState$lines <- append(newWrappedEntry,localDocState$lines)
  }
  localDocState
}
