source("./R/utils.R")

## This function creates an "add" command
getAddCmd <- function(id,code,after) {
  list(type="add",id=id,code=code,after=after,eval=after+1)
}

getUpdateCmd <- function(id,code,doEval) {
  cmd <- list(type="update",id=id,code=code)
  if(doEval) {
    index = which(names(docState$lines) == id)
    cmd$eval = index
  }
  cmd
}

getDeleteCmd <- function(id,doEval) {
  cmd <- list(type="delete",id=id)
  if(doEval) {
    ## add eval index only if there is a line after this one
    index = which(names(docState$lines) == id)
    if(length(docState$lines) > index) {
      cmd$eval = index
    }
  }
  cmd
}

getEvaluateCmd <- function(id) {
  cmd <- list(type="evaluate")
  index = which(names(docState$lines) == id)
  cmd$eval = index
  cmd
}

## This function loads an initial doc state based on the current environment
initializeDocState <- function() {
  envir <- rlang::caller_env()
  docState <- list(
    initVarList= getVarList(envir),
    lines=list(),
    current=NULL,
    hidden=c("docstate")
  )
  rlang::env_bind(envir,docState=docState)
}

## This function executes the given command, updating the docState
executeCommand <- function(cmd) {

  ##validate the cmd
  if( !("type" %in% names(cmd)) ) {
    stop("Command type not specified")
  }
  if( !(cmd$type %in% names(commandList)) ) {
    stop(sprintf("Command not found: %s",cmd$type))
  }

  ## init
  localDocState <- docState
  envir <- rlang::caller_env()

  ##execute command
  cmdFunc <- commandList[[cmd$type]]
  localDocState <- cmdFunc(localDocState,cmd)

  ##evaluate requested lines
  if( "eval" %in% names(cmd)) {
    localDocState <- evaluate(localDocState,cmd$eval,envir)
  }

  ##save the state
  docState <<- localDocState
}

## This function evaluates the entry at the given index
evaluate <- function(localDocState,index,envir) {
  entry <- localDocState$lines[[index]]

  ##evaluate
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
  localDocState$lines[[index]] <- entry

  localDocState
}

##set up the command list
commandList <- list()

commandList$add <- function(localDocState,cmd) {
  if( cmd$id %in% names(localDocState$lines) ) {
    stop(sprintf("The id already exists: %s",cmd$id))
  }

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

  ##add to state and return
  insertAfter(localDocState,entry,cmd$after)
}

commandList$update <- function(localDocState,cmd) {
  if( !(cmd$id %in% names(localDocState$lines)) ) {
    stop(sprintf("The id is not found: %s",cmd$id))
  }

  entry <- localDocState$lines[[cmd$id]]

  ##we will use the start var list later, and maybe not here

  ##update entry
  entry$code <- cmd$code
  entry$exprs <- rlang::parse_exprs(cmd$code)
  entry$symbols <- getSymbols(cmd$code)

  ##add to state and return
  localDocState$lines[[entry$id]] <- entry

  localDocState
}

commandList$delete <- function(localDocState,cmd) {
  if( !(cmd$id %in% names(localDocState$lines)) ) {
    stop(sprintf("The id is not found: %s",cmd$id))
  }

  ## delete line
  localDocState$lines[[cmd$id]] = NULL

  localDocState
}

commandList$evaluate <- function(localDocState,cmd) {
  ##no action here - no entries are changed
  localDocState
}


