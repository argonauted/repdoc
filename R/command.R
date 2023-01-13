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
    initVarList= getEnvironmentVars(envir),
    lines=list(),
    current=NULL,
    hidden=c("docstate"),
    currentCommandIndex=1
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
  entry$varList <- getEnvironmentVars(envir)
  localDocState$lines[[index]] <- entry

  localDocState
}

##This udpates th the doc state for any changes from the commands
evaluateDocState <- function(localDocState) {
  prevLine <- NULL
  currentCmdIndex <- localDocState$currentCmdIndex
  
  localDocState$lines <- lapply(localDocState$lines,function(line) {
    ##new working line
    modLine <- line

    if(!is.null(prevLine)) {
      prevLineId <- prevLine$id
      inputvalidIndex <- prevLine$outputValidIndex
      inVarList <- prevLine$outVarList
      inVarVersions <- prevLine$outVarVersions
    }
    else {
      ##no previous line - use initial doc state
      prevLineId <- NULL
      inputValidIndex <- 1
      inVarList <- localDocState$initVarList
      inVarVersions <- "|1"
    }
    
    reval <- FALSE
    inputsChanged <- FALSE
    
    ##------------------------
    ## Process an input change
    ##------------------------
    if( !identical(prevLineId,modLine$prevLineId) || !identical(inputValidIndex,modLine$inputValidIndex) ) {
      modLine$prevLineId <- prevLineId
      modLine$inputvalidIndex <- inputValidIndex
      modLine$inVarList <- inVarList
      modLine$inVarVersions <- inVarVersions
      inputsChanged = TRUE
    }
    
    ##------------------------
    ## check if we need to reevaluate
    ##------------------------

    if( identical(line$codeChangedIndex,line$currentCmdIndex) ) {
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

##CHANGE ABOVE
## change codeInputs name!!
## outVarIds,Indicies => outVarVersions
## check I did carryover correctly factoring in deleted variables in code eval

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
updateLineOutputs <- function(inLine,envir,currentCmdIndex) {
  newVarList <- getEnvironmentVars(envir)
  
  ## MAKE SURE SOME OF THESE THING ARE PRESENT!!!
  ## see how to handle values on inLine, in case some are missing
  
  kept <- intersect(names(inLine$outVarList),names(newVarList))
  created <- setdiff(names(newVarList),names(inLine$outVarList))
  deleted <- setdiff(names(inLine$outVarList),names(newVarList))
  updated <- kept[sapply(kept,function(varName) !identical(inLine$outVarList[[varName]],newVarList[[varName]]))]
  unchanged <- setdiff(names(kept),names(updated))
  
  newVarVersions = rep(paste(inLine$id,currentCmdIndex,sep="|"),length(newVarList))
  names(newVarVersions) = names(newVarList)
  newVarVersions[unchanged] = inLine$outVarVersions[unchanged]
  
  ## set the new output values on the line
  outLine <- inLine
  outLine$created <- created
  outLine$updated <- updated
  outLine$deleted <- deleted ##do we need this?
  outLine$outValidIndex <- currentCmdIndex
  outLine$outVarList <- newVarList
  outLine$outVarVersions <- newVarVersions
  
  outLine
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
  entry$codeInputs <- getDependencies(cmd$code)

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
  entry$codeInputs <- getDependencies(cmd$code)

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


