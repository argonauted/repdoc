source("./R/codeInfo.R")

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
## - refasnorm - bool    # TRUE means it is referenced in a non-call
## - isvirt = bool      # TRUE means this is code text for an expression to invoke a function. The deps are kept elsewhere, but this is included to identify the function
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

## codeInfo
## - env - list of varInfo
## - func - list of funcInfo
## 
## 
## varInfo
## - assign
##  - name
##  - isfunc
##  - issuper
## - ref
##  - name
##  - iscall
##  - issuper
##  - isvirtual
##
## funcInfo
## - aname - key
## - env - index into codeInfo$varInfo
## - parenv - index into codeInfo$varInfo

FIELD_NAMES <- c("assigned","isfunc","refascall","refasnorm","isvirt")
ASSIGNED <- 1
ISFUNC <- 2
REFASCALL <- 3
REFASNORM <- 4
ISVIRT <- 5 


analyze_code <- function(code,isGlobal=TRUE) {
  if(!isGlobal) {
    ## my initial codeInfo below assume isGLobal = TRUE
    stop("Only global code is supported now!")
  }
  
  codeInfo <- CodeInfo(root_is_global=isGlobal)
  env <- 1 # index of environment in codeInfo
  exprs <- rlang::parse_exprs(code)
  codeInfo <- traverse_exprs(codeInfo,exprs,env)
  return(codeInfo)
}

## This traverses each of a list of expressions
traverse_exprs <- function(codeInfo,exprs,env) {
  codeInfo <- purrr::reduce(exprs,traverse_expr,.init=codeInfo,env=env)
  return(codeInfo)
}

traverse_expr <- function(codeInfo,expr,env) {
  if(rlang::is_symbol(expr)) {
    codeInfo <- processSymbol(codeInfo,expr,env)
  }
  else if(rlang::is_call(expr)) {
    codeInfo <- processCall(codeInfo,expr,env)
  }
  else if(rlang::is_pairlist(expr)) {
    stop("pairlist not supported outside function definition!")
  }

  ##no action - syntactic literal or other? 

  return(codeInfo)
}


processSymbol <- function(codeInfo,expr,env) {
  ##normal reference from code
  name <- as.character(expr)
  codeInfo <- addReference(codeInfo, env, name, is_call=FALSE, is_super=FALSE, is_virtual=FALSE)
  return(codeInfo)
}

processCall <- function(codeInfo,expr,env) {
  if(rlang::is_symbol(expr[[1]])) {
    funcName <- as.character(expr[[1]])
    codeInfo <- addReference(codeInfo, env, funcName, is_call=TRUE, is_super=FALSE, is_virtual=FALSE)
  }
  else {
    ##! create an extra reference entry that is the expression for the function called
    funcExprString <- deparse(expr[[1]])
    codeInfo <- addReference(codeInfo, env, funcExprString, is_call=TRUE, is_super=FALSE, is_virtual=TRUE)
    ## save the references to the contents of this expr
    codeInfo <- traverse_expr(codeInfo,expr[[1]],env)
  }
  
  if(rlang::is_call(expr,"function")) {
    ## this is a function definition outside an expression
    ## I guess it could be an immediate execute function?
    codeInfo <- processFunctionDef(codeInfo,expr,env)
  }
  else if(rlang::is_call(expr,c("<-","=","<<-"))) {
    ## this includes all 5 assigns. Parser converts right assign to left assign
    codeInfo <- processAssign(codeInfo,expr,env)
  }
  else if(rlang::is_call(expr,c("$","@"))) {
    ##third argument is a symbol but not a variable
    codeInfo <- traverse_expr(codeInfo,expr[[2]],env)
  }
  else if(rlang::is_call(expr,c("::",":::"))) {
    ##! change this!!! for now I'll just save the whole thing as a variable name
    varName <- deparse(expr)
    codeInfo <- addToReferences(codeInfo,env,varName,is_call=TRUE, is_super=FALSE, is_virtual=FALSE) ##! THIS MAY OR MAYNOT BE A CALL!!!
  }
  else {
    codeInfo <- traverse_exprs(codeInfo,as.list(expr)[2:length(expr)],env)
  }
  
  return(codeInfo)
}

processAssign <- function(codeInfo,expr,env) {
  rhsExpr <- expr[[3]]
  lhsExpr <- expr[[2]]
  isSuper <- rlang::is_call(expr,"<<-")
  
  ## check if we are assigning a function
  ## true means yes, false means maybe
  isFunction <- rlang::is_call(rhsExpr,"function")
  
  if(isFunction) {
    codeInfo <- addReference(codeInfo, env, "function", is_call=TRUE, is_super=FALSE, is_virtual=FALSE)
    funcName <- deparse(lhsExpr)
    codeInfo <- processFunctionDef(codeInfo,funcName,rhsExpr,env)
  }
  else {
    ## traverse the RHS first
    codeInfo <- traverse_expr(codeInfo,rhsExpr,env)
  }
  
  if(rlang::is_call(lhsExpr)) {
    if(isFunction) {
      ## For starters I don't support this because I am keeping track of all
      ## functions and for know I need the symbol.
      ## Change this later!!!
      stop("Assigning a function to an expression not currently supported")
    }
    codeInfo <- processLHSFuncAssign(codeInfo,expr[[2]],isFunction,isSuper,env)
  }
  else if(rlang::is_symbol(lhsExpr)) {
    assignee <- as.character(lhsExpr)
    codeInfo <- addAssignment(codeInfo,env,assignee,is_function=isFunction, is_super = isSuper, is_virtual = FALSE)
  }
  else {
    stop(sprintf("Unknown case: assignee is not a call or symbol: %s",as.character(expr[[2]])))
  }
  
  return(codeInfo)
}

processLHSFuncAssign <- function(codeInfo,lhsExpr,isFunction,isSuper,env) {
  if(rlang::is_symbol(lhsExpr[[1]])) {
    ##lhs function reference
    funcName <- sprintf("%s<-",as.character(lhsExpr[[1]]))
    codeInfo <- addReference(codeInfo, env, funcName, is_call=TRUE, is_super = FALSE, is_virtual = FALSE)
  }
  else {
    ## we handle expressions on LHS recursively
    codeInfo <- processLHSFuncAssign(codeInfo, env, lhsExpr[[1]], isFunction, isSuper)
  }
  
  if(length(lhsExpr) > 1) {
    if(rlang::is_symbol(lhsExpr[[2]])) {
      ## this argument is what we are modifying
      varName <- as.character(lhsExpr[[2]])
      
      ## we will ignore the value of "isFunction" passed, since this being a LHS
      ## function complicates things. (This means it is a 'maybe')
      
      ## make assignments and reference
      codeInfo <- addReference(codeInfo, env, varName, is_call=FALSE, is_super = isSuper, is_virtual = FALSE)
      codeInfo <- addAssignment(codeInfo, env, varName, is_function=FALSE, is_super = isSuper, is_virtual = FALSE) ##change  this to NA?
    }
    else {
      ## we handle expressions on LHS recursively
      codeInfo <- processLHSFuncAssign(codeInfo, env, lhsExpr[[2]], isFunction, isSuper)
    }
  
    ## any other args are regular references
    ## DOH! THere are problem other special cases besides "$<-". What about "@<-"?
    if( (length(lhsExpr) > 2) && (funcName != "$<-") ) {
      codeInfo <- traverse_exprs(codeInfo,as.list(lhsExpr)[3:length(lhsExpr)],env)
    }
  }
  
  return(codeInfo)
}

processFunctionDef <- function(codeInfo,name,expr,env) {
  ##create the var info for this function
  result <- pushNewEnv(codeInfo)
  funcEnv <- result$env_index
  codeInfo <- result$code_info
  
  codeInfo <- addFunction(codeInfo,name,env=funcEnv,parenv=env)
  
  ## get param list and defaults - process in order
  paramList <- expr[[2]]
  for(index in seq_along(paramList)) {
    codeInfo <- processFuncParam(codeInfo,names(paramList)[[index]],paramList[[index]],funcEnv)
  }
  
  ## process body
  codeInfo <- traverse_expr(codeInfo,expr[[3]],funcEnv)
  
  return(codeInfo)
}

processFuncParam <- function(codeInfo,name,defaultExpr,env) {
  codeInfo <- addAssignment(codeInfo, env, name, is_function=FALSE, is_super=FALSE, is_virtual = FALSE)
  if(rlang::is_expression(defaultExpr)) {
    codeInfo <- traverse_expr(codeInfo,defaultExpr,env)
  }
  return(codeInfo)
}
