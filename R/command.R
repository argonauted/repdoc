source("./R/utils.R")
source("./R/serial.R")

##Constants

INIT_DOC_SESSION_ID <- ""
INIT_LINE_ID <- ""
INIT_CMD_INDEX <- 0
INIT_VERSION <- "|0"

RANDOM_SEED_NAME <- ".Random.seed"

SYSTEM_REF_VERSION <- "system"

MESSAGE_HEADER <- "|$($|"
MESSAGE_FOOTER <- "|$)$|"

##======================
## Command Helpers
##======================

## This function creates an "add" command
addCmd <- function(docSessionId,lineId,code,after,cmdIndex=NULL) {
  cmd <- list(type="add",lineId=lineId,code=code,after=after)
  if(!is.null(cmdIndex)) {
    cmd$cmdIndex = cmdIndex
  }
  ##Temp change for breakpoint/browser investigation
  ##For this, we must load environment for the entry point (or get the caller env for the first function)
  executeCommand(docSessionId,cmd,envir=rlang::caller_env())
}

updateCmd <- function(docSessionId,lineId,code,cmdIndex=NULL) {
  cmd <- list(type="update",lineId=lineId,code=code)
  if(!is.null(cmdIndex)) {
    cmd$cmdIndex = cmdIndex
  }
  ##Temp change for breakpoint/browser investigation
  ##For this, we must load environment for the entry point (or get the caller env for the first function)
  executeCommand(docSessionId,cmd,envir=rlang::caller_env())
}

deleteCmd <- function(docSessionId,lineId,cmdIndex=NULL) {
  cmd <- list(type="delete",lineId=lineId)
  if(!is.null(cmdIndex)) {
    cmd$cmdIndex = cmdIndex
  }
  ##Temp change for breakpoint/browser investigation
  ##For this, we must load environment for the entry point (or get the caller env for the first function)
  executeCommand(docSessionId,cmd,envir=rlang::caller_env())
}

## This function executes a multi command. It takes a list of
## properly formatted child commands.
multiCmd <- function(docSessionId,cmds,cmdIndex=NULL) {
  cmd <- list(type="multi", cmds=cmds)
  if(!is.null(cmdIndex)) {
    cmd$cmdIndex = cmdIndex
  }
  ##Temp change for breakpoint/browser investigation
  ##For this, we must load environment for the entry point (or get the caller env for the first function)
  executeCommand(docSessionId,cmd,envir=rlang::caller_env())
}

## This function evaluates the next line in the session, if applicable
evaluate <- function(docSessionId) {
  ## init
  docState <- getDocState(docSessionId)
  if(is.null(docState)) stop(sprintf("Document session not found: %s",docSessionId))
  
  ##Temp change for breakpoint/browser investigation
  ##For this, we must load environment for the entry point (or get the caller env for the first function)
  ##envir <- rlang::global_env()
  envir <- rlang::caller_env()
  
  ##update the doc state for the change to the code
  docState <- evaluateDocState(docState,envir)
  
  ##we need to ensure these both happen or fail###########
  ##send cmd complete msg
  sendCompletionStatus(docState)
  ##save the state
  setDocState(docSessionId,docState)
  ########################################################
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
    varTable = list(),
    cmdIndex=INIT_CMD_INDEX
  )
  
  ## DO I NEED  MESSAGE BEFORE THIS? =========
  initVarList <- getInitVarList()
  initVarVersions <- getInitVarVersions()
  initEnvVarNames <- createVersionedNames(initVarVersions)
  docState <- updateVarTable(docState,INIT_LINE_ID,initEnvVarNames,character(),initVarList)
  ##===============================
  
  setDocState(docSessionId,docState)
  sendCompletionStatus(docState)
  
  invisible(docState)
}


## This function executes the given command in the given document session
executeCommand <- function(docSessionId,cmd,envir=NULL) {
  ##Temp change for breakpoint/browser investigation
  ##envir <- rlang::global_env()
  if(is.null(envir)) envir <- rlang::caller_env()

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
  
  if("cmdIndex" %in% names(cmd)) {
    if(cmd$cmdIndex <= docState$cmdIndex) {
      stop(sprintf("Invalid command index sumbitted! previous = %s, sumitted = %s",docState$cmdIndex, cmd$cmdIndex))
    }
    
    docState$cmdIndex <- cmd$cmdIndex
  }
  else {
    docState$cmdIndex <- docState$cmdIndex + 1
  }

  ##execute command
  cmdFunc <- commandList[[cmd$type]]
  docState <- cmdFunc(docState,cmd)

  ##update the doc state for the change to the code
  docState <- evaluateDocState(docState,envir)

  ##we need to ensure these both happen or fail###########
  ##send cmd complete msg
  sendCompletionStatus(docState)
  ##save the state
  setDocState(docSessionId,docState)
  ########################################################
}

## This Function changes the current data in the global context
## For lineId, the Id of the previous line should be set, to get the 
## initial state for a given line.
setActiveLine <- function(docSessionId,lineId) {
  docState <- getDocState(docSessionId) ## this executes a stop, not returns null! FIX ERROR HANDLING
  if(is.null(docState)) return(FALSE)

  if(is.null(lineId)) {
    desiredVarList <- getInitVarList()
    desiredCmdIndex <- INIT_CMD_INDEX
  }
  else {
    lineState <- docState$lines[[lineId]]
    if(!is.null(lineState)) {
      desiredVarList <- lineState$outVarList
      desiredCmdIndex <- lineState$outIndex
    }
    else {
      return(FALSE)
    }
  }
  
  envVersion <- getEnvVersion()
  if( is.null(envVersion) || 
      (docSessionId != envVersion$docSessionId) ||
      (lineId != envVersion$lineId) ||
      (desiredCmdIndex != envVersion$cmdIndex) ) {
    clearEnvVersion()
    ###################################
    ## temp logc for breakpoint/browser dev
    envir <- rlang::caller_env()
    setEnvVars(envir,desiredVarList)
    ##setEnvVars(rlang::global_env(),desiredVarList)
    ###############################################
    setEnvVersion(docSessionId,lineId,desiredCmdIndex)
  }
  sendActiveLineStatus(docSessionId,lineId)
  TRUE
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
  invisible(docState)
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
    oldLine <- docState$lines[[docState$firstDirtyIndex]]
    modLine <- oldLine

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
        
        ## find the line output
        lineDisplayData <- getLineDisplayData(modLine,envir)
        if(!is.null(lineDisplayData)) {
          modLine$displayData <- lineDisplayData
          sendLineDisplayMessage(docState$docSessionId,modLine)
        }
      }
      else {
        ##do not evaluate, and stop checking lines
        ##we can only evaluate once per request (for now)
        evaluationInterrupted <- TRUE
      }
    }
    
    ## cell and doc environment variables processing
    if(modLine$outIndex == currentCmdIndex) {
      modLine$envVarNames <- createVersionedNames(modLine$outVarVersions)
      
      ##cell env var message
      sendCellEnvMessage(docState$docSessionId,modLine)
      
      docState <- updateVarTable(docState,modLine$lineId,modLine$envVarNames,oldLine$envVarNames,modLine$outVarList)
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
      (docSessionId != envVersion$docSessionId) ||
      (modLine$prevLineId != envVersion$lineId) ||
      (modLine$inIndex != envVersion$cmdIndex) ) {
    ## we need to update the variables in the environment
    setEnvVars(envir,modLine$inVarList)
  }
  
  ##clear the value of the env state
  clearEnvVersion()
  
  ##signal start of eval
  sendEvalMessage(docSessionId,modLine$lineId,currentCmdIndex)
  
  ##evaluate, printing outputs to the console
  if(modLine$parseValid) {
    tryCatch({
      ##this evaluates the exprs with autoprint, like the console does
      withAutoprint(exprs=modLine$exprs,local=envir,evaluated=TRUE,echo=FALSE)
    },
    error=function(err) {
      sendConsoleMessage("stderr",err$message,docSessionId)
    },
    warning=function(wrn) {
      sendConsoleMessage("stdwrn",wrn$message,docSessionId)
    },
    message=function(m) {
      sendConsoleMessage("stdmsg",m$message,docSessionId)
    }
    )
  }
  else {
    sendConsoleMessage("stderr",modLine$parseMsg,docSessionId)
  }
  
  ##update the eval index
  modLine$codeEvalIndex <- modLine$codeChangedIndex
  modLine$inputEvalIndex <- modLine$codeInputIndex
  
  ##set the env state to the version given by this line id and this cmd index
  setEnvVersion(docSessionId,modLine$lineId,currentCmdIndex)
  
  ##save the final var list
  modLine <- updateLineOutputs(modLine,envir,currentCmdIndex)
}

## This updates the line outputs for a newly evaluated entry
updateLineOutputs <- function(oldLine,envir,currentCmdIndex) {
  newVarList <- getEnvVars(envir)
  
  ## MAKE SURE SOME OF THESE THING ARE PRESENT!!!
  ## see how to handle values on oldLine, in case some are missing
  
  kept <- intersect(names(oldLine$inVarList),names(newVarList))
  created <- setdiff(names(newVarList),names(oldLine$inVarList))
  deleted <- setdiff(names(oldLine$inVarList),names(newVarList))
  if(length(kept) > 0) {
    updated <- kept[sapply(kept,function(varName) !identical(oldLine$inVarList[[varName]],newVarList[[varName]]))]
  } else {
    updated <- character(0)
  }
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

createVersionedNames <- function(varVersions) {
  versionedNames <- paste(names(varVersions),varVersions,sep="|")
  names(versionedNames) <- names(varVersions)
  versionedNames
}

updateVarTable <- function(docState,lineId,newEnvVarNames,oldEnvVarNames,newVarList) {
  cellAddNames <- setdiff(newEnvVarNames,oldEnvVarNames)
  cellDropNames <- setdiff(oldEnvVarNames,newEnvVarNames)
  
  modVarList <- newVarList
  names(modVarList) <- newEnvVarNames  ##change the names on the var list so we can look up the added values
  cellAddValues <- modVarList[cellAddNames]
  
  stateData <- list(varTable=docState$varTable,adds=character(),drops=character())
  stateData <- purrr::reduce2(cellAddNames,cellAddValues,processCellAdds,lineId=lineId,.init=stateData)
  stateData <- purrr::reduce(cellDropNames,processCellDrops,lineId=lineId,.init=stateData)
  
  docState$varTable <- stateData$varTable
  
  ##send doc environment message - add as list of values, drops as vector of names
  addList <- modVarList[stateData$adds]
  names(addList) <- stateData$adds
  sendDocEnvMessage(docState$docSessionId,addList,stateData$drops)
  
  invisible(docState)
}

processCellAdds <- function(stateData,cellAddName,value,lineId) {
  varTable <- stateData$varTable
  adds <- stateData$adds
  if(hasName(varTable,cellAddName)) {
    cellList <- varTable[[cellAddName]]$lines
    if(! (lineId %in% cellList) ) {  #it shouldn't be there
      varTable[[cellAddName]]$lines <- c(cellList,lineId)
    }
  }
  else {
    varTable[[cellAddName]] <- list(lines=lineId,value=value)
    adds <- c(adds,cellAddName)
  }
  stateData$varTable <- varTable
  stateData$adds <- adds
  invisible(stateData)
}

processCellDrops <- function(stateData,cellDropName,lineId) {
  varTable <- stateData$varTable
  drops <- stateData$drops
  if(hasName(varTable,cellDropName)) { #it should be here
    cellList <- varTable[[cellDropName]]$lines
    newCellList <- cellList[cellList != lineId]
    varTable[[cellDropName]]$lines <- newCellList
    if(length(newCellList) == 0) {
      drops <- c(drops,cellDropName)
    }
    
  }
  stateData$varTable <- varTable
  stateData$drops <- drops
  invisible(stateData)
}

ASSIGN_SYMBOLS <- c("<-","<<-","=")
isAssignment <- function(expr) {
  if(rlang::is_call(expr)) {
    callee <- as.character(expr[[1]])
    callee %in% ASSIGN_SYMBOLS
  }
  else FALSE
}


## This function gets the display data for a given expression list.
## It returns NULL if it finds no data, and a list with the names being the display name
## and the value being the display value.
getLineDisplayData <- function(lineState,envir) {
  exprCount <- length(lineState$exprs)
  if(exprCount == 0) return(NULL)
  
  ##only read from the last expression in the line/cell
  lineExpr <- lineState$exprs[[exprCount]]
  
  if(isAssignment(lineExpr)) {
    targetExpr <- lineExpr[[2]]
    tryCatch({
      displayData <- list()
      if(class(targetExpr) == "name") {
        displayData$name <- as.character(targetExpr)
      }
      else if(rlang::is_call(targetExpr)) {
        displayData$label <- deparse(targetExpr)[[1]]
        displayData$value <- eval(targetExpr,envir=envir)
      }
      displayData
    },
    error=function(err) {
      ##no action 
      NULL
    })
  }
  else {
    NULL
  }
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
  entry <- processCode(entry,cmd$code)
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
  
  invisible(docState)
}

commandList$update <- function(docState,cmd) {
  if( !(cmd$lineId %in% names(docState$lines)) ) {
    stop(sprintf("Line ID not found: %s",cmd$lineId))
  }
  
  entry <- docState$lines[[cmd$lineId]]

  ##update entry
  entry <- processCode(entry,cmd$code)
  entry$codeChangedIndex <- docState$cmdIndex

  ##add to state and return
  docState$lines[[entry$lineId]] <- entry
  
  ##update the firstDirtyIndex if needed
  currentLine <- which(names(docState$lines) == cmd$lineId)
  if(currentLine < docState$firstDirtyIndex) {
    docState$firstDirtyIndex <- currentLine
  }

  invisible(docState)
}

commandList$delete <- function(docState,cmd) {
  if( !(cmd$lineId %in% names(docState$lines)) ) {
    stop(sprintf("Line ID not found: %s",cmd$lindId))
  }
  
  ##update the firstDirtyIndex if needed
  currentLine <- which(names(docState$lines) == cmd$lineId)
  if(currentLine < docState$firstDirtyIndex) {
    docState$firstDirtyIndex <- currentLine
  }

  ## delete entry
  docState$lines[[cmd$lineId]] = NULL

  invisible(docState)
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
  
  invisible(docState)
}

processCode <- function(entry,code) {
  entry$code <- code
  
  tryCatch({
      entry$exprs <- rlang::parse_exprs(code)
      entry$parseMsg <- NULL
      entry$parseValid <- TRUE
    },
    error=function(err) {
      entry$exprs <<- NULL
      entry$parseMsg <<- err$message
      entry$parseValid <<- FALSE
    }
    ##TBD - add other handlers, for warnings or messages
  )
  
  entry$codeInputs <- if(entry$parseValid) getDependencies(code) else character()
  
  entry
}

##---------------------------
## utils
##---------------------------

sendConsoleMessage <- function(msgType,msg,docSessionId) {
  sendMessage("console",docSessionId,
              list(msgType=jsonlite::unbox(msgType),
                   msg=jsonlite::unbox(msg)))
}


sendEvalMessage <- function(docSessionId,lineId,cmdIndex) {
  sendMessage("evalStart",docSessionId,list(
    lineId=jsonlite::unbox(lineId),
    cmdIndex=jsonlite::unbox(cmdIndex)
  ))
}

sendLineDisplayMessage <- function(docSessionId,lineState) {
  if(!is.null(lineState$displayData)) {
    displayData <- lineState$displayData
    data <- list(lineId=jsonlite::unbox(lineState$lineId))
    entry <- list()
    if(!is.null(displayData$name)) {
      entry$name <- jsonlite::unbox(displayData$name)
    }
    else if(!is.null(displayData$value)) {
      entry$label <- jsonlite::unbox(displayData$label)
      entry$value <- preserialize(displayData$value)
    }
    data$valList <- list(entry) ##unamed list to make json array. For now there is just one entry. We may allow more later
    sendMessage(type="lineDisplay",docSessionId,data)
  }
}

sendCellEnvMessage <- function(docSessionId,lineState) {
  ##send data as a JSON object { (var name):(versioned name) ), with the versioned name unboxed 
  varList <- lapply(as.list(lineState$envVarNames),jsonlite::unbox)
  data <- list(lineId=jsonlite::unbox(lineState$lineId),varList=varList)
  sendMessage(type="cellEnv",docSessionId,data)
}

sendDocEnvMessage <- function(docSessionId,addList,dropNames) {
  data <- list()
  if(length(addList) > 0) {
    data$adds <- lapply(addList,preserialize)
  }
  if(length(dropNames) > 0) {
    data$drops <- dropNames
  }
  if(length(data) > 0) {
    sendMessage(type="docEnv",docSessionId,data)
  }
}

## This sends the status of the document after completion of the evaluation
## It includes:
## - evalComplete - if false, further evaluation is necessary
## - nextIndex - included if evaluation necessary. The line index that next needs to be evaluated
sendCompletionStatus <- function(docState) {
  evalComplete <- docState$firstDirtyIndex > length(docState$lines)
  data <- list(
    evalComplete=jsonlite::unbox(evalComplete),
    cmdIndex=jsonlite::unbox(docState$cmdIndex)
  )
  if(!evalComplete) {
    nextLineId <- docState$lines[[docState$firstDirtyIndex]]$lineId
    data$nextLineIndex <- jsonlite::unbox(docState$firstDirtyIndex)
    data$nextLineId <- jsonlite::unbox(nextLineId)
  }
  sendMessage(type="docStatus",docState$docSessionId,data)
}

sendActiveLineStatus <- function(docSessionId,lineId) {
  data <- list(
    docSessionId=jsonlite::unbox(docSessionId),
    lineId=jsonlite::unbox(lineId)
  )
  sendMessage(type="ActiveLineStatus",docSessionId,data)
}

## This function sends a data message to the client, encoding it
## in the console output
## "data" is typically a list object. It is converted using jsonlite:toJSON.
## This translates simple values/single element vectors as JSON arrays. To avoid
## this, the value should be set as jsonlite::unbox(value)
sendMessage <- function(type,docSessionId,data) {
  body <- list(type=jsonlite::unbox(type),
               session=jsonlite::unbox(docSessionId),
               data=data)
  print(paste(MESSAGE_HEADER,makeJson(body),MESSAGE_FOOTER,sep=""))
}

