

## This finds the input and output variables for a code fragment
## Parameters:
## - code - code text
## - isGlobal - this should be true if the code is run the global level and false if in a child environment
##
## return: list with the following entries
## - stack - list of frames with one entry (isGlobal == TRUE) or two entries (isGlobal == FALSE). 
##           The higher index is the local frame, and if applicable, the other represents all parent frames
## - isGlobal - the input variable isGlobal
##
## each frame is a list with the entry for each variable being a logical vector with names:
## - assigned - bool    # TRUE means this variable was set in the code
## - isfunc - bool      # TRUE means it was assigned as a function
## - refascall - bool   # TRUE means it is referenced in a call
## - refasnon - bool    # TRUE means it is referenced in a non-call
##
## NOTES
## - References are only labeled if they are done before an assignment
## - All referenced variables are recorded in the local frame, unless it was referenced
## from a LHS function super assignment from the local frame and the local frame was not globals.
## Referenced variables may still be in a parent frame.
## - Assignments are in the parent frame only if the are from a super assign at the local 
## level and the local frame is not globals.
##
## INITIALLY I WILL DO isGlobal == TRUE!

## DOH! This still isn't right!  I haven't worked it out yet

FIELD_NAMES <- c("assigned","isfunc","refascall","refasnorm")
ASSIGNED <- 1
ISFUNC <- 2
REFASCALL <- 3
REFASNORM <- 4


analyze_code <- function(code,isGlobal) {
  if(!isGlobal) {
    stop("Only global code is supported now!")
  }
  varInfo <- getInitialVarInfo(isGlobal)
  exprs <- rlang::parse_exprs(code)
  traverse_exprs(varInfo,exprs)
}

## This traverses each of a list of expressions
traverse_exprs <- function(varInfo,exprs) {
  purrr::reduce(exprs,traverse_expr,.init=varInfo)
}

traverse_expr <- function(varInfo,expr) {
  if(rlang::is_symbol(expr)) {
    processSymbol(varInfo,expr)
  }
  else if(rlang::is_call(expr)) {
    processCall(varInfo,expr)
  }
  else if(rlang::is_pairlist(expr)) {
    stop("pairlist not supported outside function definition!")
  }
  else {
    ##no action - syntactic literal or other? 
    varInfo
  }
}

getInitialVarInfo <- function(isGlobal) {
  varInfo <- list(stack = list(getEmptyFrame()),isGlobal=isGlobal)
  if(!isGlobal) {
    varInfo <- pushFrame(varInfo)
  }
  varInfo
}

getEmptyFrame <- function() {
  list()
}

createVarEntry <- function(name,assigned,isfunc,refascall,refasnorm) {
  entry <- c(assigned,isfunc,refascall,refasnorm)
  names(entry) <- FIELD_NAMES
  entry
}

## Adds an empty frame to the varInfo frame stack
pushFrame <- function(varInfo) {
  varInfo$stack[[length(varInfo$stack)+1]] <- getEmptyFrame()
  varInfo
}

## remove the top frame from the varInfo frame stack
popFrame <- function(varInfo) {
  varInfo$stack <- varInfo$stack[1:(length(varInfo$stack)-1)]
  varInfo
}

getRefIndex <- function(varInfo,name,refAsCall,isSuper) {
  startIndex <- length(varInfo$stack)
  if(isSuper && startIndex > 1) startIndex <- startIndex - 1
  
  for(index in startIndex:1) {
    frame <- varInfo$stack[[index]]
    entry <- frame[[name]]
    ##=================================================================================================
    ## DOH! How should we respond to a reference to a function when we don't know if it is a function
    ## For now I am assuming it is a function only when we define it as a simple function definition
    ## THINK ABOUT OTHER OPTIONS - maybe multiple returns?
    ##=================================================================================================
    if(!is.null(entry) && ( !refAsCall || (refAsCall && frameEntryDefiniteFunction(entry))) ) {
      ## this index has the variable
      return(index)
    }
  }
  ## top index
  1
  
  
  ## REVIEW THIS - 
  ## get frame index:
  ## - go through frames (start at top if isSuper = false, otherwise start at next or global)
  ## - look for the variable name
  ## - get that index
  
  ## we will only mark as referenced if it has not been assigned
  ## - not present - mark as referenced by type (call or norm)
  ## - present, not assigned - update type field (call or norm)
  ## - present, assigned - mark as referenced if the assigned type does not "match" the referenced type
  ##   - ref is call, assign isfunc TRUE: no new reference
  ##   - ref is call, assign isfunc FALSE: yes new reference ## for now we might not even store this value in assignments
  ##   - ref is call, assign isfunc NA: ?? yes new reference ## conservative - maybe unnecessary reference
  ##   - ref is norm: no new reference
}

## this returns true if the frame entry counts as a function
frameEntryMaybeFunction <- function(entry) {
  entry[ISFUNC] != FALSE
}

frameEntryDefiniteFunction <- function(entry) {
  entry[ISFUNC] == TRUE
}


addToReferences <- function(varInfo,name,isCall,isSuper=FALSE) {
  frameIndex <- getRefIndex(varInfo,name,refAsCall=isCall,isSuper)
  
  ## add or update entry 
  entry <- varInfo$stack[[frameIndex]][[name]]
  if(is.null(entry)) {
    entry = createVarEntry(name,assigned=FALSE,isfunc=isCall,refascall=isCall,refasnorm=!isCall)
  }
  else if(!entry[ASSIGNED]) {
    ## mark as referenced only if it is not been assigned yet
    if(isCall) {
      if(!entry[REFASCALL]) entry[REFASCALL] = TRUE
    }
    else {
      if(!entry[REFASNORM]) entry[REFASNORM] = TRUE
    }
  }
  varInfo$stack[[frameIndex]][[name]] <- entry
  varInfo
}


addToAssignments <- function(varInfo,name,isFunction,isSuper) {
  ## get the proper frame to insert the assignment
  if(!isSuper) {
    frameIndex <- length(varInfo$stack)
  }
  else {
    frameIndex <- getRefIndex(varInfo,name,refAsCall=FALSE,isSuper=TRUE)
  }
  
  ## add or update entry 
  entry <- varInfo$stack[[frameIndex]][[name]]
  if(is.null(entry)) {
    entry = createVarEntry(name,assigned=TRUE,isfunc=isFunction,refascall=FALSE,refasnorm=FALSE)
  }
  else {
    ## mark as assigned whether we already referenced it or not.
    if(!entry[ASSIGNED]) entry[ASSIGNED] <- TRUE ## ok if already TRUE
    if(entry[ISFUNC] != isFunction) entry[ISFUNC] <- isFunction ## we want last assignment
  }
  varInfo$stack[[frameIndex]][[name]] <- entry
  varInfo
}

processSymbol <- function(varInfo,expr) {
  ##normal reference from code
  name <- as.character(expr)
  varInfo <- addToReferences(varInfo,name,isCall=FALSE)
  varInfo
}

processCall <- function(varInfo,expr) {
  if(rlang::is_symbol(expr[[1]])) {
    funcName <- as.character(expr[[1]])
    varInfo <- addToReferences(varInfo,funcName,isCall=TRUE)
  }
  else {
    ##DOH! we won't be recording this as a function, but currently that's ok
    varInfo <- traverse_expr(varInfo,expr[[1]])
  }
  
  if(rlang::is_call(expr,"function")) {
    processFunctionDef(varInfo,expr)
  }
  else if(rlang::is_call(expr,c("<-","=","<<-"))) {
    ## this includes all 5 assigns. Parser converts right assign to left assign
    processAssign(varInfo,expr)
  }
  else if(rlang::is_call(expr,c("$","@"))) {
    ##third argument is a symbol but not a variable
    traverse_expr(varInfo,expr[[2]])
  }
  else if(rlang::is_call(expr,c("::",":::"))) {
    ## change this!!! for now I'll just save the whole thing as a varible name
    varName <- paste(expr[[2]],expr[[1]],expr[[3]],sep="")
    varInfo <- addToReferences(varInfo,varName,isCall=FALSE) #DOH! I don't know if this is a call. I don't think it hurts us now
  }
  else {
    traverse_exprs(varInfo,as.list(expr)[2:length(expr)])
  }
  
}

processAssign <- function(varInfo,expr) {
  ## traverse the RHS first
  rhsExpr <- expr[[3]]
  varInfo <- traverse_expr(varInfo,rhsExpr)
  
  isSuper <- rlang::is_call(expr,"<<-")
  
  ## check if we are assigning a function
  ## true means yes, false means maybe
  isFunction <- rlang::is_call(rhsExpr,"function")
  
  if(rlang::is_call(expr[[2]])) {
    varInfo <- processFuncAssign(varInfo,expr[[2]],isFunction,isSuper)
  }
  else if(rlang::is_symbol(expr[[2]])) {
    assignee <- as.character(expr[[2]])
    varInfo <- addToAssignments(varInfo,assignee,isFunction,isSuper)
  }
  else {
    stop(sprintf("Unknown case: assignee is not a call or symbol: %s",as.character(expr[[2]])))
  }
  
  varInfo
}

processFuncAssign <- function(varInfo,lhsExpr,isSuper,isFunction) {
  if(rlang::is_symbol(lhsExpr[[1]])) {
    funcName <- sprintf("%s<-",as.character(lhsExpr[[1]]))
    varInfo <- addToReferences(varInfo,funcName,isCall=TRUE)
  }
  else {
    stop("expression for LHS function, as opposed to a string for the function name, not supported")
  }
  
  if(rlang::is_symbol(lhsExpr[[2]])) {
    ## this argument is what we are modifying
    varName <- as.character(lhsExpr[[2]])
    
    ## we will ignore the value of "isFunction" passed, since this being a LHS
    ## function complicates things. (This means it is a 'maybe')
    
    ## make assignments and reference
    varInfo <- addToReferences(varInfo,varName,isCall=FALSE,isSuper)
    varInfo <- addToAssignments(varInfo,varName,isFunction=FALSE,isSuper) ##change  this to NA?
  }
  else {
    ## we handle expressions on LHS recursively
    varInfo <- processFuncAssign(varInfo, lhsExpr[[2]],isSuper,isFunction)
  }
  
  ## any other args are regular references
  ## DOH! THere are problem other special cases besides "$<-". What about "@<-"?
  if( (length(lhsExpr) > 2) && (funcName != "$<-") ) {
    varInfo <- traverse_exprs(varInfo,as.list(lhsExpr)[3:length(lhsExpr)])
  }
  
  varInfo
}

processFunctionDef <- function(varInfo,expr) {
  ## add a new frame
  varInfo <- pushFrame(varInfo)
  
  ## get param list and defaults - process in order
  paramList <- expr[[2]]
  for(index in seq_along(paramList)) {
    varInfo <- processFuncParam(varInfo,names(paramList)[[index]],paramList[[index]])
  }
  
  ## process body
  varInfo <- traverse_expr(varInfo,expr[[3]])
  
  popFrame(varInfo)
}

processFuncParam <- function(varInfo,name,defaultExpr) {
  varInfo <- addToAssignments(varInfo,name,isFunction=FALSE,isSuper=FALSE)
  if(rlang::is_expression(defaultExpr)) {
    varInfo <- traverse_expr(varInfo,defaultExpr)
  }
  varInfo
}

