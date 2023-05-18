

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
## each frame is a data.frame with the columns:
## - name - char        # the variable name (also this is the row name)
## - isfunc - bool      # TRUE means it is a function. FALSE means unknown (for now)
## - assigned - bool    # TRUE means this variable was set in the code
## - refd - bool        # TRUE means this variable was references from the code
##
## NOTES
## - All referenced variables are recorded in the local frame, unless it was referenced
## from a LHS function super assignment from the local frame and the local frame was not globals.
## Referenced variables may still be in a parent frame.
## - Assignments are in the parent frame only if the are from a super assign at the local 
## level and the local frame is not globals.
##
## INITIALLY I WILL DO isGlobal == TRUE!

## DOH! This still isn't right!  I haven't worked it out yet


analyze_code <- function(code,isGlobal) {
  if(!isGlobal) {
    stop("Only global code is supported now!")
  }
  varInfo <- getInitialVarInfo()
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
    print(sprintf("skipped ast entry: %s",as.character(expr)))
    varInfo
  }
}

getInitialVarInfo(isGlobal) {
  varInfo <- list(stack = list(getEmptyFrame()),isGlobal=isGlobal)
  if(!isGlobal) {
    varInfo <- pushFrame(varInfo)
  }
  varInfo
}

getEmptyFrame <- function() {
  data.frame(name=character(),isfunc=logical(),assigned=logical(),refd=logical(),assuper=logical())
}

## Adds an empty frame to the varInfo frame stack
pushFrame <- function(varInfo) {
  varInfo$stack <- c(varInfo$stack,getEmptyFrame())
  varInfo
}

## remove the top frame from the varInfo frame stack
popFrame <- function(varInfo) {
  varInfo <- varInfo[1:(length(varInfo)-1)]
  varInfo
}


## super needed only when accessing from parent environment rather than current
## environment, such as for a LHS function assignment.
addToReferences <- function(varInfo,varName,isCall,superOnly=FALSE) {
  ## - name = varName
  ## - isFunction = isCall
  ## - assigned = unchanged
  ## - referenced = TRUE
  ## - assignAsSuper = superOnly # we need this 
}

addToAssignments <- function(varInfo,varName,isFunction,isSuper) {
  ## - name = varName
  ## - isFunction = isFunction
  ## - assigned = TRUE
  ## - referenced = unchanged
  
  ## isSuper == FALSE
  ## - add an assignment to the local environment if not already there
  ##
  ## isSuper == TRUE
  ## - add an assignment to a parent environment (up to global) if not already there
}

processSymbol <- function(varInfo,expr) {
  varInfo <- addToReferences(varInfo,lhsVarName,isCall=FALSE)
  varInfo$used <- c(varInfo$used,as.character(expr))
  varInfo
}

processCall <- function(varInfo,expr) {
  if(rlang::is_call(expr,"function")) {
    stop("Function definition not implemented!")
  }
  else if(rlang::is_call(expr,c("<-","=","<<-"))) {
    ## this includes all 5 assigns. Parser converts right assign to left assign
    processAssign(varInfo,expr)
  }
  else if(rlang::is_call(expr,c("$","@"))) {
    stop("$, @ call not implemented!")
  }
  else if(rlang::is_call(expr,c("::",":::"))) {
    stop("::, ::: call not implemented!")
  }
  else {
    traverse_exprs(varInfo,as.list(expr))
  }
  
}

processAssign <- function(varInfo,expr) {
  isSuper <- isCall(expr,"<<-")
  rhsExpr <- lhsExpr[[3]]
  
  ## check if we are assigning a function
  ## true means yes, false means maybe
  isFunction <- rlang::is_call(rhsExpr,"function")

  if(rlang::is_call(expr[[2]])) {
    varInfo <- processFuncAssign(varInfo,expr[[2]],isFunction,isSuper)
  }
  else if(rlang::is_symbol(expr[[2]])) {
    assignee <- as.character(expr[[2]])
    addToAssignments(varInfo,assignee,isFunction,isSuper)
  }
  else {
    stop(sprintf("Unknown case: assignee is not a call or symbol: %s",as.character(expr[[2]])))
  }
  
  ## traverse the RHS
  varInfo <- traverse_exprs(varInfo,as.list(rhsExpr))

  varInfo
}

processFuncAssign <- function(varInfo,lhsExpr,isSuper,isFunction) {
  if(rlang::is_symbol(lhsExpr[[1]])) {
    funcName <- sprintf("`%s<-`",as.character(lhsExpr[[1]]))
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
    varInfo <- addToReferences(varInfo,varName,isCall=FALSE,superOnly=isSuper)
    ## NOTE - 'FALSE' for isSuper. In either case, it creates a local copy of variable
    varInfo <- addToAssignments(varInfo,varName,isFunction=FALSE,isSuper=FALSE)
  }
  else {
    ## we handle expressions on LHS recursively
    varInfo <- processFuncAssign(varInfo, lhsExpr[[2]])
  }
  
  ## any other args are regular references
  if(length(lhsExpr) > 2) {
    varInfo <- traverse_exprs(varInfo,as.list(lhsExpr)[3:length(lhsExpr)],inGlvEnv)
  }
  
  varInfo
}

