
## This funtion gets the extenal dependency symbols in some code text
getDependencies <- function(codeText,hiddenSymbols) {
  ## this is kludgey
  ## also I should protect against funny input. Maybe get an ast to see if it is a good command (above)
  closureText <- sprintf("function() {\n%s\n}",codeText)
  closureExpr <- rlang::parse_expr(closureText)
  closure <- eval(closureExpr)
  deps <- codetools::findGlobals(closure)
  ## LHS functions don't work properly here (at least with <-, they do work with <<-)
  ## If there is a LHS function, add the local variables to the dependencies
  if(includesLHSFunc(deps)) {
    deps <- addLocalsToDeps(deps,codeText)
  } 
  deps
}

## This function checks if there are any LHS functions in the list of symbols
includesLHSFunc <- function(s) {
  any(s[grep(".<-$",s)] != "<<-")
}

getLocals <- function(codeText) {
  codetools::findLocals(rlang::parse_expr(codeText))
}

## This function gets any local variables in the codeText and adds them to the deps list
addLocalsToDeps <- function(deps,codeText) {
  lcls <- getLocals(codeText)
  union(lcls,deps)
}

## This function loads the variable list from the current environment
getEnvVars <- function(envir) {
  envirVars <- rlang::env_get_list(envir,rlang::env_names(envir))
  ##record everything excluding the session state variable
  envirVars <- envirVars[names(envirVars) != ".sessionStateEnv."]

  envirVars
}

insertAfter <- function(localDocState,entry,index) {
  newWrappedEntry <- list()
  newWrappedEntry[[entry$lineId]] <- entry
  if(index > 0) {
    localDocState$lines <- append(localDocState$lines,newWrappedEntry,after=index)
  }
  else {
    localDocState$lines <- append(newWrappedEntry,localDocState$lines)
  }
  localDocState
}

setEnvVars <- function(envir,envVars) {
  ## clear all names except session state variable
  names <- names(envir)
  names <- names[names != ".sessionStateEnv."]
  rlang::env_unbind(envir,names)
  ##set values from envVars
  rlang::env_bind(envir,!!!envVars)  ## BE CAREFUL OF NULL VALUES - I SHOULD PROBABLY DO THIS DIFFERENTLY
}

