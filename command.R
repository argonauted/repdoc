getAdd <- function(id,code,after) {
  list(type="add",id=id,code=code,after=after)
}

getSymbols <- function(codeText) {
  ## this is kludgey
  ## also I should protect against funny input. Maybe get an ast to see if it is a good command (above)
  closureText <- sprintf("function() {\n%s\n}",codeText)
  closureExpr <- rlang::parse_expr(closureText)
  closure <- eval(closureExpr)
  codetools::findGlobals(closure)
}

getVarList <- function(envir) {
  varList <- sapply(names(envir),function(name) envir[[name]])
  names(varList) = names(envir)
  
  ##remove hidden variables
  varList <- varList[names(varList) != "docState"]
  
  varList
}

executeCommand <- function(cmd) {
  localDocState <- docState
  envir <- rlang::caller_env()
  
  if(identical("add",cmd$type)) {
    if( (cmd$after < 0) || (cmd$after > length(localDocState$lines)) ) {
      stop("Specified previous line does not exist")
    }
    else if(cmd$after == 0) {
      startvarList <- localDocState$initVarList
    }
    else {
      startVarList <- localDocState$lines[[cmd$after]]$endVarList
    }
    
    ##we will use the start var list later, and maybe not here

    ##create entry
    entry <- list(id=cmd$id,code=cmd$code)
    entry$exprs <- rlang::parse_exprs(cmd$code)
    entry$symbols <- getSymbols(cmd$code)
    
    ##evaluate code
    tryCatch({
     ##this evaluates the exprs with autoprint, like the console does
      withAutoprint(exprs=entry$exprs,local=envir,evaluated=TRUE,echo=FALSE)
    },
    error=function(err) {
      message(err)
    }
    )
    
    ##save the final var list
    entry$varList <- getVarList(envir)
    
    ## add the entry to the doc state
    if(cmd$after > 0) {
      ## add afte specified line
      localDocState$lines <- append(localDocState$lines,list(id=entry),after=cmd$after)
    }
    else {
      ## add at start
      localDocState$lines <- append(list(id=entry),localDocState$lines)
    }
    
    ##save the state
    docState <<- localDocState
    
  }
  else {
    stop("Command type not known")
  }
}

docState <- list(
  initVarList= getVarList(environment()),
  lines=list(),
  current=NULL
)
