g3_init_guess <- function(params, pattern, 
                          value = 0, lower = NA, upper = NA, 
                          optimise = 1){
 
  ## Are the parameters bounded internally
  is_param_bounded <- any(grepl(paste0(pattern, '.lower'), params$switch))
  
  if (is_param_bounded){

    ## Check whether value is on bound & adjust if so to avoid Inf, -Inf
    if (value == lower && optimise == 1){ 
      value <- value + 0.1*(upper - lower)
    }
    if (value == upper && optimise == 1){ 
      value <- value - 0.1*(upper - lower)
    }
    
    ## Convert initial value to bounded equivalent
    value <- value_from_bounds(value, lower, upper)
    
    ## If not optimising ensure lower and upper values = value
    if (optimise == 0){
      lower <- upper <- value
    }
    else value <- 0 ## Temporary - using to test against existing models
    
    params[grepl(pattern, params$switch), 'value'] <- value
    params[grepl(paste0(pattern, '.lower'), params$switch), 'value'] <- lower
    params[grepl(paste0(pattern, '.upper'), params$switch), 'value'] <- upper
    
    ## Optimisation - ensure it is turned off for upper and lower parameters
    params[grepl(pattern, params$switch), 'optimise'] <- as.logical(optimise)
    params[grepl(paste0(pattern, '.lower'), params$switch), 'optimise'] <- FALSE
    params[grepl(paste0(pattern, '.upper'), params$switch), 'optimise'] <- FALSE
    
  }
  else{
    ## Fill in the horizontal template
    params[grepl(pattern, params$switch), 'value'] <- value
    params[grepl(pattern, params$switch), 'lower'] <- lower
    params[grepl(pattern, params$switch), 'upper'] <- upper
    params[grepl(pattern, params$switch), 'optimise'] <- as.logical(optimise)
  }
  return(params)
}

## Need to fix for time/age varying parameters
convert_tmb_param <- function(params){
  
  ## Remove year/age suffix and get switch names
  tmp <- gsub('\\.[0-9]$', '', params$switch)
  switch_ind <- which(!grepl('upper|lower', tmp))
  
  for (i in switch_ind){
    
    switch <- params[i, 'switch']
    
    ## Check whether switch is bounded
    if (paste0(tmp[[i]], '.lower') %in% params$switch){
      
      v0 <- params$value[grepl(paste0(params[i, 'switch'], '$'), params$switch)][[1]]
      vup <- params$value[grepl(paste0(tmp[[i]], '.upper'), params$switch)][[1]] 
      vdn <- params$value[grepl(paste0(tmp[[i]], '.lower'), params$switch)][[1]] 
      
      params$value[grepl(paste0(params[i, 'switch'], '$'), params$switch)][[1]] <- 
        eval_bounded(v0, vdn, vup)
      
    }
    else next
    
  }
  return(params)
}

value_from_bounds <- function(x, lower, upper){
  return(log((upper - lower)/(x - lower) - 1))
}

eval_bounded <- function(x, lower, upper){
  return(lower + (upper - lower)/(1 + exp(x)))
}


