source("./R/utils.R")

##Constants

INIT_DOC_SESSION_ID <- ""
INIT_LINE_ID <- ""
INIT_CMD_INDEX <- 0
INIT_VERSION <- "|0"

##======================
## Command Helpers
##======================

## This function creates an "add" command
addCmd <- function(docSessionId,lineId,code,after) {
  cmd <- list(type="add",lineId=lineId,code=code,after=after)
  executeCommand(docSessionId,cmd)
}

updateCmd <- function(docSessionId,lineId,code) {
  cmd <- list(type="update",lineId=lineId,code=code)
  executeCommand(docSessionId,cmd)
}

deleteCmd <- function(docSessionId,lineId) {
  cmd <- list(type="delete",lineId=lineId)
  executeCommand(docSessionId,cmd)
}


##======================
## Main Functions
##======================

## This function should be called to initialize a session. 
initializeSession <- function() {
  globalEnv <- rlang::global_env()
  
  ## add the session state environment
  rlang::env_bind(globalEnv,.sessionStateEnv.=rlang::child_env(globalEnv,docStates=list()))
  
  ##add the initial global environment state data
  initVarList<-getEnvVars(globalEnv)
  initVarVersions<-rep(INIT_VERSION,length(initVarList))
  names(initVarVersions) = names(initVarList)
  rlang::env_bind(.sessionStateEnv.,initVarList=initVarList,initVarVersions=initVarVersions)
  
  ##set the current global environment state
  setEnvVersion(INIT_DOC_SESSION_ID,INIT_LINE_ID,INIT_CMD_INDEX)
  
  NULL
}

## This function should be called to initialize a document session
## This doc session Id should be a unique string for this session.
initializeDocState <- function(docSessionId) {
  docState <- list(
    docSessionId=docSessionId,
    lines=list(),
    cmdIndex=INIT_CMD_INDEX
  )
  
  setDocState(docSessionId,docState)
  
  docState
}


## This function executes the given command in the given document session
executeCommand <- function(docSessionId,cmd) {

  ##validate the cmd
  if( !("type" %in% names(cmd)) ) {
    stop("Command type not specified")
  }
  if( !(cmd$type %in% names(commandList)) ) {
    stop(sprintf("Command not found: %s",cmd$type))
  }

  ## init
  docState <- getDocState(docSessionId)
  if(is.null(docState)) stop(sprintf("Document session not found: %s",docSessionId))
  envir <- rlang::global_env()
  
  docState$cmdIndex <- docState$cmdIndex + 1

  ##execute command
  cmdFunc <- commandList[[cmd$type]]
  docState <- cmdFunc(docState,cmd)

  ##update the doc state for the change to the code
  docState <- evaluateDocState(docState,envir)

  ##save the state
  setDocState(docSessionId,docState)
  
  NULL
}

##======================
## Internal Functions
##======================

##----------------------
## session data storage
##----------------------

## Stores the version for the global environment
setEnvVersion <- function(docSessionId,lineId,cmdIndex) {
  rlang::env_bind(.sessionStateEnv.,envVersion=list(docSessionId=docSessionId,
                                                    lineId=lineId,
                                                    cmdIndex=cmdIndex))
  NULL
}

## Sets the version of the global environment to NULL, symoblizing an unknown state
clearEnvVersion <- function() {
  rlang::env_bind(.sessionStateEnv.,envVersion=NULL)
  NULL
}

## Gets the global environment version
getEnvVersion <- function() {
  rlang::env_get(.sessionStateEnv.,"envVersion")
} 

## Sets the doc state for the given doc session ID
setDocState <- function(docSessionId,docState) {
  docStates <- rlang::env_get(.sessionStateEnv.,"docStates")
  docStates[[docSessionId]] = docState
  rlang::env_bind(.sessionStateEnv.,docStates=docStates)
}

## Gets the doc state for the given session ID
getDocState <- function(docSessionId) {
  docStates <- rlang::env_get(.sessionStateEnv.,"docStates")
  if(! docSessionId %in% names(docStates)) stop(sprintf("Application error! Invalid doc ID: %s",docSessionId))
  docStates[[docSessionId]]
}

## Gets the initial variable list for a document session state
getInitVarList <- function() {
  rlang::env_get(.sessionStateEnv.,"initVarList")
}

## Gets the initial variable versions for a document session state
getInitVarVersions <- function() {
  rlang::env_get(.sessionStateEnv.,"initVarVersions")
}


##-------------------
## doc updates
##-------------------

##This udpates th the doc state for any changes from the commands
evaluateDocState <- function(docState,envir) {
  prevLine <- NULL
  currentCmdIndex <- docState$cmdIndex
  
  docState$lines <- lapply(docState$lines,function(line) {
    ##new working line
    modLine <- line

    if(!is.null(prevLine)) {
      prevLineId <- prevLine$lineId
      inIndex <- prevLine$outIndex
      inVarList <- prevLine$outVarList
      inVarVersions <- prevLine$outVarVersions
    }
    else {
      ##no previous line - use initial doc state
      prevLineId <- INIT_LINE_ID
      inIndex <- INIT_CMD_INDEX
      inVarList <- getInitVarList()
      inVarVersions <- getInitVarVersions()
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
      
      ##clean this up - I do something similar for versions in other places
      ##also. later we should get better versions for what I call here "system" variables
      codeInputVersions <- rep("system",length(modLine$codeInputs))
      names(codeInputVersions) <- modLine$codeInputs
      envirCodeInputs <- intersect(modLine$codeInputs,names(modLine$inVarList))
      codeInputVersions[envirCodeInputs] <- modLine$inVarVersions[envirCodeInputs]
      modLine$codeInputVersions <- codeInputVersions
      
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
      
      ## old inputs: codeInputversions = versions associated with code Inputs
      ## FOR ALL CODE INPUTS. Use a "system" value if taken from the system
      ## (later we should work that out!)
      
      if( any(modLine$codeInputVersions != line$codeInputVersions) ) {
        ##code inputs were changed
        reval <- TRUE
      }
      else {
        ##no code inputs were changed
        ##carry over changed inputs that are not variables updated or deleted by lnie code
        carryoverNames <- setdiff(names(modLine$inVarList),c(modLine$updated,modLine$deleted))
      
        modLine$outVarList[carryoverNames] <- inVarList[carryoverNames]
        modLine$outVarVersions[carryoverNames] <- inVarVersions[carryoverNames]
        modLine$outIndex <- currentCmdIndex
      }
    }

    ##------------------------
    ## reevaluate if needed (this will also recalculate all outputs
    ##------------------------
    if(reval) {
      modLine <- evalCode(docState$docSessionId,modLine,currentCmdIndex,envir)
    }
    
    prevLine <<- modLine
    modLine
  })
  
  docState
}

evalCode <- function(docSessionId,modLine,currentCmdIndex,envir) {
  
  ## update environment if it is not in the right state
  envVersion <- getEnvVersion()
  if( is.null(envVersion) || 
      (modLine$prevLineId != envVersion$lineId) ||
      (modLine$inIndex != envVersion$cmdIndex) ) {
    ## we need to update the variables in the environment
    setEnvVars(envir,modLine$inVarList)
  }
  
  ##clear the value of the env state
  clearEnvVersion()
  
  ##evaluate, printing outputs to the console
  tryCatch({
    ##this evaluates the exprs with autoprint, like the console does
    withAutoprint(exprs=modLine$exprs,local=envir,evaluated=TRUE,echo=FALSE)
  },
  error=function(err) {
    message(err)
  }
  )
  
  ##set the env state to the version given by this line id and this cmd index
  setEnvVersion(docSessionId,modLine$lineId,currentCmdIndex)
  
  ##save the final var list
  modLine <- updateLineOutputs(modLine,envir,currentCmdIndex)
  
  modLine
}

## This updates the line outputs for a newly evaluated entry
updateLineOutputs <- function(oldLine,envir,currentCmdIndex) {
  newVarList <- getEnvVars(envir)
  
  ## MAKE SURE SOME OF THESE THING ARE PRESENT!!!
  ## see how to handle values on oldLine, in case some are missing
  
  kept <- intersect(names(oldLine$inVarList),names(newVarList))
  created <- setdiff(names(newVarList),names(oldLine$inVarList))
  deleted <- setdiff(names(oldLine$inVarList),names(newVarList))
  updated <- kept[sapply(kept,function(varName) !identical(oldLine$inVarList[[varName]],newVarList[[varName]]))]
  unchanged <- setdiff(kept,updated)
  
  ##set the versions for each variable value
  newVarVersions = rep(paste(oldLine$lineId,currentCmdIndex,sep="|"),length(newVarList))
  names(newVarVersions) = names(newVarList)
  newVarVersions[unchanged] = oldLine$inVarVersions[unchanged]
  
  ## set the new output values on the line
  newLine <- oldLine
  newLine$created <- created
  newLine$updated <- updated
  newLine$deleted <- deleted ##do we need this?
  newLine$outIndex <- currentCmdIndex
  newLine$outVarList <- newVarList
  newLine$outVarVersions <- newVarVersions
  
  newLine
}

##----------------------------
## doc command implementations
##----------------------------

##set up the command list
commandList <- list()

## Executes an add command
commandList$add <- function(docState,cmd) {
  
  if( cmd$lineId %in% names(docState$lines) ) {
    stop(sprintf("The line ID already exists: %s",cmd$lindId))
  }

  if( (cmd$after < 0) || (cmd$after > length(docState$lines)) ) {
    stop("Specified previous line does not exist")
  }

  ##create entry
  entry <- list(lineId=cmd$lineId)
  entry$code <- cmd$code
  entry$exprs <- rlang::parse_exprs(cmd$code)
  entry$codeInputs <- getDependencies(cmd$code)
  entry$codeChangedIndex <- docState$cmdIndex

  ##add to state and return
  docState <- insertAfter(docState,entry,cmd$after)
  
  docState
}

commandList$update <- function(docState,cmd) {
  if( !(cmd$lineId %in% names(docState$lines)) ) {
    stop(sprintf("Line ID not found: %s",cmd$lineId))
  }
  
  entry <- docState$lines[[cmd$lineId]]

  ##update entry
  entry$code <- cmd$code
  entry$exprs <- rlang::parse_exprs(cmd$code)
  entry$codeInputs <- getDependencies(cmd$code)
  entry$codeChangedIndex <- docState$cmdIndex

  ##add to state and return
  docState$lines[[entry$lineId]] <- entry

  docState
}

commandList$delete <- function(docState,cmd) {
  if( !(cmd$lineId %in% names(docState$lines)) ) {
    stop(sprintf("Line ID not found: %s",cmd$lindId))
  }

  ## delete entry
  docState$lines[[cmd$lineId]] = NULL

  docState
}

