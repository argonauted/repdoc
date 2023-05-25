# VarInfo constructor
VarInfo <- function(assign = data.frame(name = character(), 
                                        norm = logical(),
                                        nfunc = logical(),
                                        super = logical(),
                                        sfunc = logical()), 
                    ref = data.frame(name = character(), 
                                     norm = logical(),
                                     ncall = logical(), 
                                     super = logical(), ## used only for replacement (LHS) function with super operator 
                                     virtual = logical())) {
  
  var_info <- list(assign = assign, ref = ref)
  class(var_info) <- "VarInfo"
  return(var_info)
}

# CodeInfo constructor
CodeInfo <- function(root_is_global = TRUE,env = list(VarInfo()), func = data.frame(aname = character(), env = integer(), parenv = integer())) {
  
  code_info <- list(root_is_global = root_is_global, env = env, func = func)
  class(code_info) <- "CodeInfo"
  return(code_info)
}

addAssignment <- function(code_info, env, name, is_function, is_super = FALSE, is_virtual = FALSE) {
  
  if(code_info$root_is_global && env == 1) {
    ## super assign acts like normal assign at global level
    is_super = FALSE
  }

  assign_df <- code_info$env[[env]]$assign
  
  if(name %in% assign_df$name) {
    row_index <- which(assign_df$name == name)
    if(is_super) {
      assign_df[row_index, c("super", "sfunc", "virtual")] <- c(TRUE, is_function, is_virtual)
    } else {
      assign_df[row_index, c("norm", "nfunc", "virtual")] <- c(TRUE, is_function, is_virtual)
    }
  } else {
    new_row <- data.frame(name = name, 
                          norm = !is_super, 
                          nfunc = is_function && !is_super, 
                          super = is_super, 
                          sfunc = is_function && is_super,
                          virtual = is_virtual)
    assign_df <- rbind(assign_df, new_row)
  }
  
  code_info$env[[env]]$assign <- assign_df
  return(code_info)
}

addReference <- function(code_info, env, name, is_call, is_super = FALSE, is_virtual = FALSE) {
  
  if(code_info$root_is_global && env == 1) {
    ## super assign acts like normal assign at global level
    is_super = FALSE
  }

  ref_df <- code_info$env[[env]]$ref
  assign_df <- code_info$env[[env]]$assign
  
  should_update <- TRUE
  
  if (name %in% assign_df$name) {
    assign_row <- assign_df[assign_df$name == name, ]
    if ((is_call && !is_super && ((assign_row$norm && assign_row$nfunc) || (assign_row$super && assign_row$sfunc))) ||
        (!is_call && !is_super) ||
        (is_super && assign_row$super)) {
      should_update <- FALSE
    }
  }
  
  if (should_update) {
    if (name %in% ref_df$name) {
      row_index <- which(ref_df$name == name)
      if (is_super) {
        ref_df[row_index, c("super", "virtual")] <- c(TRUE, is_virtual)
      } else {
        ref_df[row_index, c("norm", "ncall", "virtual")] <- c(TRUE, is_call, is_virtual)
      }
    } else {
      new_row <- data.frame(name = name, 
                            norm = !is_super, 
                            ncall = is_call && !is_super, 
                            super = is_super, 
                            virtual = is_virtual)
      ref_df <- rbind(ref_df, new_row)
    }
  }
  
  code_info$env[[env]]$ref <- ref_df
  return(code_info)
}

addFunction <- function(code_info, name, env, parenv) {

  new_row <- data.frame(aname = name, env = env, parenv = parenv)
  code_info$func <- rbind(code_info$func, new_row)
  
  return(code_info)
}

## This methods pushes a new environment onth the environment list
pushNewEnv <- function(code_info) {
  env_index <- length(code_info$env) + 1
  code_info$env[[env_index]] <- VarInfo()
  list(env_index=env_index,code_info=code_info)
}


