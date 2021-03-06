g3_init_guess <- function(params, pattern, 
                          value = 0, lower = NA, upper = NA, 
                          optimise = 1){
 
  ## Are the parameters (1) bounded internally (2) time- or age-varying
  is_param_bounded <- any(grepl(paste0(pattern, '.lower'), params$switch))
  is_param_varying <- any(grepl(paste0(pattern, '\\.[0-9]'), params$switch))
  
  ## Create new pattern for parameters varying with eg. time
  v_pattern <- ifelse(is_param_varying, 
                      paste0(pattern, '\\.[0-9]'),
                      pattern)
  
  ## Make sure parameter values are within bounds
  if (value <= lower && optimise == 1){ 
    warning(paste("The 'value' provided for", pattern,
                  "is <= the 'lower' bound and is therefore adjusted to fall within the bounds"))
    value <- max(lower, value) + 0.01*(upper - lower)
  }
  else{
    if (value >= upper && optimise == 1){ 
      warning(paste("The 'value' provided for", pattern,
                    "is >= the 'upper' bound and is therefore adjusted to fall within the bounds"))
      value <- min(upper, value) - 0.01*(upper - lower)
    }  
  }
  
  if (is_param_bounded){
    
    ## If a parameter is bounded, NA's will crash the model
    if (is.na(lower)) lower <- -9999
    if (is.na(upper)) upper <- 9999
    
    ## If not optimising ensure lower and upper values = value
    if (optimise == 0){
      lower <- upper <- value
    }
    else value <- (lower + upper)/2
    
    ## Convert initial value to bounded equivalent
    value <- value_from_bounds(value, lower, upper)
    
    ## Fill in parameter template
    params[grepl(v_pattern, params$switch), 'value'] <- value
    params[grepl(paste0(pattern, '.lower'), params$switch), 'value'] <- lower
    params[grepl(paste0(pattern, '.upper'), params$switch), 'value'] <- upper
    
    ## Optimisation - ensure it is turned off for upper and lower parameters
    params[grepl(v_pattern, params$switch), 'optimise'] <- as.logical(optimise)
    params[grepl(paste0(pattern, '.lower'), params$switch), 'optimise'] <- FALSE
    params[grepl(paste0(pattern, '.upper'), params$switch), 'optimise'] <- FALSE
    
  }
  else{
    ## Fill in the horizontal template
    params[grepl(v_pattern, params$switch), 'value'] <- value
    params[grepl(v_pattern, params$switch), 'lower'] <- lower
    params[grepl(v_pattern, params$switch), 'upper'] <- upper
    params[grepl(v_pattern, params$switch), 'optimise'] <- as.logical(optimise)
  }
  return(params)
}

convert_tmb_param <- function(params){
  
  ## Identify the bounded switches
  bounded_params <- params$switch[(grepl('\\.lower$', params$switch))]
  
  if (length(bounded_params) > 0){
    
    bounded_params <- gsub('\\.lower$', '', bounded_params)
    
    for (i in bounded_params){
      
      ## Is it a varying parameter?
      value_index <- grepl(paste0(i, '\\.[0-9]{1,4}$'), params$switch)
     
      if (!any(value_index)){
        value_index <- grepl(paste0(i, '$'), params$switch)
      } 
      
      ## Fill in values
      params$value[value_index] <-
        as.list(eval_bounded(
          unlist(params$value[value_index]),
          params$value[grepl(paste0(i, '.lower$'), params$switch)][[1]],
          params$value[grepl(paste0(i, '.upper$'), params$switch)][[1]]
        ))
    }
  }
  return(params)
}

value_from_bounds <- function(x, lower, upper){
  if (x == lower && x == upper) return(x)
  else  return(log((upper - lower)/(x - lower) - 1))
}

eval_bounded <- function(x, lower, upper){
  return(lower + (upper - lower)/(1 + exp(x)))
}





