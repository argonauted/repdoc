source("./R/utils.R")

##Constants

INIT_ID <- ""
INIT_INDEX <- 1
INIT_VERSION <- "|1"

##======================
## Command Helpers
##======================

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

##======================
## Main Functions
##======================

## This function loads an initial doc state based on the current environment
initializeDocState <- function() {
  envir <- rlang::caller_env()
  docState <- list(
    initVarList= getEnvironmentVars(envir),
    lines=list(),
    ##hidden=c("docstate"),
    cmdIndex=INIT_INDEX
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

##======================
## Internal Functions
##======================

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
  entry$varList <- getEnvironmentVars(envir)
  localDocState$lines[[index]] <- entry

  localDocState
}

##This udpates th the doc state for any changes from the commands
evaluateDocState <- function(localDocState) {
  prevLine <- NULL
  currentCmdIndex <- localDocState$cmdIndex
  
  localDocState$lines <- lapply(localDocState$lines,function(line) {
    ##new working line
    modLine <- line

    if(!is.null(prevLine)) {
      prevLineId <- prevLine$id
      inIndex <- prevLine$outIndex
      inVarList <- prevLine$outVarList
      inVarVersions <- prevLine$outVarVersions
    }
    else {
      ##no previous line - use initial doc state
      prevLineId <- INIT_ID
      inIndex <- INIT_INDEX
      inVarList <- localDocState$initVarList
      inVarVersions <- INIT_VERSION
    }
    
    reval <- FALSE
    inputsChanged <- FALSE
    
    ##------------------------
    ## Process an input change
    ##------------------------
    if( !identical(prevLineId,modLine$prevLineId) || !identical(inIndex,modLine$inIndex) ) {
      modLine$prevLineId <- prevLineId
      modLine$inIndex <- inIndex
      modLine$inVarList <- inVarList
      modLine$inVarVersions <- inVarVersions
      inputsChanged = TRUE
    }
    
    ##------------------------
    ## check if we need to reevaluate
    ##------------------------

    if( identical(line$codeChangedIndex,currentCmdIndex) ) {
      reval <- TRUE
    }
    else if(inputsChanged) {
      ## check if we need to reevaluate because of input change
      if( any(inVarVersions[modLine$codeInputs] != modLine$inVarVersions[modeLine$codeInputs]) ) {
        ##code inputs were changed
        reval <- TRUE
      }
      else {
        ##no code inputs were changed
        ##carry over changed inputs that are not variables updated or deleted by lnie code
        carryoverNames <- setDiff(names(modeLine$inVarList),c(modLine$updated,modLine$deleted))
      
        modLine$outVarList[carryoverNames] <- inVarList[carryoverNames]
        modLine$outVarVersions[carryoverNames] <- inVarVersions[carryoverNames]
        modLine$outVarIndex <- currentCmdIndex
      }
    }

    ##------------------------
    ## reevaluate if needed (this will also recalculate all outputs
    ##------------------------
    if(reval) {
      modLine <- evalCode(modLine,currentCmdIndex)
    }
    
    prevLine <<- modLine
    modLine
  })
  
  localDocState
}

evalCode <- function(modLine,currentCmdIndex) {
  
  ##evaluate, printing outputs to the console
  tryCatch({
    ##this evaluates the exprs with autoprint, like the console does
    withAutoprint(exprs=entry$exprs,local=envir,evaluated=TRUE,echo=FALSE)
  },
  error=function(err) {
    message(err)
  }
  )
  
  ##save the final var list
  modLine <- updateLineOutputs(modLine,envir,currentCmdIndex)
  
  modLine
}

## This updates the line outputs for a newly evaluated entry
updateLineOutputs <- function(oldLine,envir,currentCmdIndex) {
  newVarList <- getEnvironmentVars(envir)
  
  ## MAKE SURE SOME OF THESE THING ARE PRESENT!!!
  ## see how to handle values on oldLine, in case some are missing
  
  kept <- intersect(names(oldLine$inVarList),names(newVarList))
  created <- setdiff(names(newVarList),names(oldLine$inVarList))
  deleted <- setdiff(names(oldLine$inVarList),names(newVarList))
  updated <- kept[sapply(kept,function(varName) !identical(oldLine$inVarList[[varName]],newVarList[[varName]]))]
  unchanged <- setdiff(names(kept),names(updated))
  
  ##set the versions for each variable value
  newVarVersions = rep(paste(oldLine$id,currentCmdIndex,sep="|"),length(newVarList))
  names(newVarVersions) = names(newVarList)
  newVarVersions[unchanged] = oldLine$inVarVersions[unchanged]
  
  ## set the new output values on the line
  newLine <- inLine
  newLine$created <- created
  newLine$updated <- updated
  newLine$deleted <- deleted ##do we need this?
  newLine$outIndex <- currentCmdIndex
  newLine$outVarList <- newVarList
  newLine$outVarVersions <- newVarVersions
  
  newLine
}

##set up the command list
commandList <- list()

commandList$add <- function(docState,cmd) {
  localDocState <- docState
  localDocState$cmdIndex <- docState$cmdIndex + 1
  
  if( cmd$id %in% names(localDocState$lines) ) {
    stop(sprintf("The id already exists: %s",cmd$id))
  }

  if( (cmd$after < 0) || (cmd$after > length(localDocState$lines)) ) {
    stop("Specified previous line does not exist")
  }

  ##create entry
  entry <- list(id=cmd$id)
  entry$code <- cmd$code
  entry$exprs <- rlang::parse_exprs(cmd$code)
  entry$codeInputs <- getDependencies(cmd$code)
  entry$codeChangeIndex <- localDocState$cmdIndex
  
  # set later:
  #entry$prevLineId
  #entry$inIndex
  #entry$inVarList
  #entry$inVarVersions
  #entry$outIndex
  #entry$outVarlist
  #entry$outVarVersion
  #entry$created
  #entry$updated
  #entry$deleted

  ##add to state and return
  localDocState <- insertAfter(localDocState,entry,cmd$after)
  
  localDocState
}

commandList$update <- function(docState,cmd) {
  if( !(cmd$id %in% names(localDocState$lines)) ) {
    stop(sprintf("The id is not found: %s",cmd$id))
  }
  
  localDocState <- docState
  entry <- localDocState$lines[[cmd$id]]

  ##update entry
  entry$code <- cmd$code
  entry$exprs <- rlang::parse_exprs(cmd$code)
  entry$codeInputs <- getDependencies(cmd$code)
  entry$codeChangeIndex <- localDocState$cmdIndex

  ##add to state and return
  localDocState$lines[[entry$id]] <- entry

  localDocState
}

commandList$delete <- function(localDocState,cmd) {
  if( !(cmd$id %in% names(localDocState$lines)) ) {
    stop(sprintf("The id is not found: %s",cmd$id))
  }

  ## delete entry
  localDocState$lines[[cmd$id]] = NULL

  localDocState
}

commandList$evaluate <- function(localDocState,cmd) {
  ##no action here - no entries are changed
  localDocState
}


