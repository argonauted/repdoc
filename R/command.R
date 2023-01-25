source("./R/utils.R")

##Constants

INIT_DOC_SESSION_ID <- ""
INIT_LINE_ID <- ""
INIT_CMD_INDEX <- 0
INIT_VERSION <- "|0"

RANDOM_SEED_NAME <- ".Random.seed"

SYSTEM_REF_VERSION <- "system"

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

## This function executes a multi command. It takes a list of
## properly formatted child commands.
multiCmd <- function(docSessionId,cmds) {
  cmd <- list(type="multi", cmds=cmds)
  executeCommand(docSessionId,cmd)
}

## This function evaluates the next line in the session, if applicable
evaluate <- function(docSessionId) {
  ## init
  docState <- getDocState(docSessionId)
  if(is.null(docState)) stop(sprintf("Document session not found: %s",docSessionId))
  envir <- rlang::global_env()
  
  ##update the doc state for the change to the code
  docState <- evaluateDocState(docState,envir)
  
  ##save the state
  setDocState(docSessionId,docState)
  
  ##TEMP RETURN VALUE - IF EVALUATION COMPLETE=========
  docState$firstDirtyIndex > length(docState$lines)
  ##===================================================
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
  
  TRUE
}

## This function should be called to initialize a document session
## This doc session Id should be a unique string for this session.
initializeDocState <- function(docSessionId) {
  docState <- list(
    docSessionId=docSessionId,
    firstDirtyIndex=1,
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
  
  ##TEMP RETURN VALUE - IF EVALUATION COMPLETE=========
  docState$firstDirtyIndex > length(docState$lines)
  ##===================================================
}

##======================
## Internal Functions
##======================

##----------------------
## session data storage
##----------------------

## Stores the version for the global environment
setEnvVersion <- function(docSessionId,lineId,cmdIndex) {
  envVersion  <- list(docSessionId=docSessionId,lineId=lineId,cmdIndex=cmdIndex)
  rlang::env_bind(.sessionStateEnv.,envVersion=envVersion)
  envVersion
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
  docState
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
  
  ##working variables
  currentCmdIndex <- docState$cmdIndex
  firstReval <- TRUE
  evaluationInterrupted <- FALSE
  
  ##loop to evaluate un-evaluated lines, up until we get to the second
  ##break from loop on the second to be evaluated, without evaluating
  while((docState$firstDirtyIndex <= length(docState$lines))&&(!evaluationInterrupted)) {
    
    ##new working line
    prevLine <- if (docState$firstDirtyIndex > 1) docState$lines[[docState$firstDirtyIndex - 1]] else NULL
    modLine <- docState$lines[[docState$firstDirtyIndex]]

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
    nonCodeInputsChanged <- FALSE
    
    ##------------------------
    ## Process an input change
    ##------------------------
    if( !identical(prevLineId,modLine$prevLineId) || !identical(inIndex,modLine$inIndex) ) {
      modLine$prevLineId <- prevLineId
      modLine$inIndex <- inIndex
      modLine$inVarList <- inVarList
      modLine$inVarVersions <- inVarVersions
      
      ##clean this up - I do something similar for versions in other places
      ##also. later we should get better versions for what I call here SYSTEM_REF_VERSION variables
      codeInputVersions <- rep(SYSTEM_REF_VERSION,length(modLine$codeInputs))
      names(codeInputVersions) <- modLine$codeInputs
      envirCodeInputs <- intersect(modLine$codeInputs,names(modLine$inVarList))
      codeInputVersions[envirCodeInputs] <- modLine$inVarVersions[envirCodeInputs]
      
      ##update code input versions if code is newer than the inputs or any inputs change
      if( (modLine$codeChangedIndex > modLine$codeInputIndex) || any(codeInputVersions != modLine$codeInputVersions) ) {
        modLine$codeInputVersions = codeInputVersions
        modLine$codeInputIndex = currentCmdIndex
      }
      else {
        nonCodeInputsChanged = TRUE
      }
    }
    
    ##------------------------
    ## check if we need to reevaluate
    ##------------------------

    if( !identical(modLine$codeChangedIndex,modLine$codeEvalIndex) ) {
      ##not yet evaluated for new code
      reval <- TRUE
    }
    else if( !identical(modLine$codeInputIndex,modLine$inputEvalIndex) ) {
      ##not yet evaluated for new inputs
      reval <- TRUE
    }
    else if(nonCodeInputsChanged) {
      ## no code inputs were changed
      ## carry over any non-code inputs to the outputs that are not code outputs
      ## (This means we need to get the code outputs right!)
      outputs <- c(modLine$created,modLine$updated)
      
      outVarList <- modLine$inVarList
      outVarList[outputs] <- modLine$outVarList[outputs]
      
      outVarVersions <- modLine$inVarVersions
      outVarVersions[outputs] <- modLine$outVarVersions[outputs]
      
      ##we expect deletes to be inputs, but we will remove them in case they are not captured in dependencies
      if(length(modLine$deleted) > 0) {
        keptNameFlags <- !(names(outVarList) %in% modLine$deleted)
        outVarList <- outVarList[keptNameFlags]
        outVarVersions <- outVarList[keptNameFlags]
      }
    
      modLine$outVarList <- outVarList
      modLine$outVarVersions <- outVarVersions
      modLine$outIndex <- currentCmdIndex
    }

    ##------------------------
    ## reevaluate if needed (this will also recalculate all outputs)
    ##------------------------
    if(reval) {
      if(firstReval) {
        modLine <- evalCode(docState$docSessionId,modLine,currentCmdIndex,envir)
        firstReval <- FALSE
      }
      else {
        ##do not evaluate, and stop checking lines
        ##we can only evaluate once per request (for now)
        evaluationInterrupted <- TRUE
      }
    }
    
    ##update state for new line
    docState$lines[[docState$firstDirtyIndex]] <- modLine
    if(!evaluationInterrupted) {
      docState$firstDirtyIndex <- docState$firstDirtyIndex + 1
    }
  }
  
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
  
  ##for now, this is how I communicate with client
  print(sprintf("Evaluate session %s, line %s",docSessionId, modLine$lineId))
  
  ##evaluate, printing outputs to the console
  tryCatch({
    ##this evaluates the exprs with autoprint, like the console does
    withAutoprint(exprs=modLine$exprs,local=envir,evaluated=TRUE,echo=FALSE)
  },
  error=function(err) {
    message(err)
  }
  )
  
  ##update the eval index
  modLine$codeEvalIndex <- modLine$codeChangedIndex
  modLine$inputEvalIndex <- modLine$codeInputIndex
  
  print("Evaluate complete")
  
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
  
  ##========================================================================
  ## WORKAROUND FOR RANDOM SEED DEPENDENCY
  ## As of now we dont' get the random see as a dependency. But if it is used,
  ## it will also be udpated. So we will use this as a trigger to add it to code inputs.
  ## (for the sake of set.seed, this is not an input, but that means just an extra evaluation of that code)
  ## !!!:
  ## This is messy and future error prone 
  ## This should be cleaned up.
  if( !(RANDOM_SEED_NAME %in% newLine$codeInputs) ) {
    if(RANDOM_SEED_NAME %in% created) {
      ##move from created to updated, since we will set this to be a code input
      created = created[created != RANDOM_SEED_NAME]
      updated = c(updated,RANDOM_SEED_NAME)
      
      ##add code input with version coming from system
      newLine$codeInputs <- c(newLine$codeInputs,RANDOM_SEED_NAME)
      newLine$codeInputVersions[[RANDOM_SEED_NAME]] = SYSTEM_REF_VERSION
    }
    else if(RANDOM_SEED_NAME %in% updated) {
      ##add code input with version coming from system in var versions
      newLine$codeInputs <- c(newLine$codeInputs,RANDOM_SEED_NAME)
      newLine$codeInputVersions[[RANDOM_SEED_NAME]] = newLine$inVarVersions[RANDOM_SEED_NAME]
    }
  }
  ##========================================================================
  
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
  
  ##====================================================
  ##ADD UDPATE TO UNCHECKED LINES FOR CODE CHANGE!!!
  ##===================================================

  ##create entry
  entry <- list(lineId=cmd$lineId)
  entry$code <- cmd$code
  entry$exprs <- rlang::parse_exprs(cmd$code)
  entry$codeInputs <- getDependencies(cmd$code)
  entry$codeChangedIndex <- docState$cmdIndex
  
  entry$codeInputVersions <- character()
  entry$codeInputIndex <- 0

  ##add to state
  docState <- insertAfter(docState,entry,cmd$after)
  
  ##update the firstDirtyIndex if needed
  currentLine <- which(names(docState$lines) == cmd$lineId)
  if(currentLine < docState$firstDirtyIndex) {
      docState$firstDirtyIndex <- currentLine
  }
  
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
  
  ##update the firstDirtyIndex if needed
  currentLine <- which(names(docState$lines) == cmd$lineId)
  if(currentLine < docState$firstDirtyIndex) {
    docState$firstDirtyIndex <- currentLine
  }

  docState
}

commandList$delete <- function(docState,cmd) {
  if( !(cmd$lineId %in% names(docState$lines)) ) {
    stop(sprintf("Line ID not found: %s",cmd$lindId))
  }
  
  ##update the firstDirtyIndex if needed
  currentLine <- which(names(docState$lines) == cmd$lineId)
  if(currentLine < docState$firstDirtyIndex) {
    docState$firstDirtyIndex == docState$firstDirtyIndex - 1
  }

  ## delete entry
  docState$lines[[cmd$lineId]] = NULL

  docState
}

commandList$multi <- function(docState,cmd) {
  
  ## apply the child command code changes sequentially to the doc state
  for(childCmd in cmd$cmds) {
    ##validate the cmd
    if( !("type" %in% names(childCmd)) ) {
      stop("Child Command type not specified")
    }
    if( !(childCmd$type %in% names(commandList)) ) {
      stop(sprintf("Child Command not found: %s",childCmd$type))
    }
    
    ##execute command
    cmdFunc <- commandList[[childCmd$type]]
    docState <- cmdFunc(docState,childCmd)
  }
  
  docState
}

