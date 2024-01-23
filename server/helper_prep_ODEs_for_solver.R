diffeq_to_text <- function(list_of_diffeqs, list_of_vars){
  output <- "" #initialize output
  for (i in seq(length(list_of_diffeqs))) {
    current_eqn <- paste0("d", list_of_vars[i], " <- ", list_of_diffeqs[i])
    output <- paste0(output, current_eqn, "\n ")
  }
  return(output)
}

CustomEqnsToText <- function(customEqnRV) {
  # Reads custom eqn RV and pulls additional equations
  # Input:
  #   @ customEqnRV - Reactive variable with all custom eqn information
  # Output: 
  #   @output - String, additional eqns separated by newline
  
  
  # Extract info from input RV
  additional.eqns <- unname(sapply(customEqnRV,
                                   get,
                                   x = "Equation"))
  
  # Initialize
  output <- ""
  for (i in seq(length(additional.eqns)))
  {
    output <- paste0(output, additional.eqns[i], "\n ")
  }
  return(output)
}



output_var_for_ode_solver <- function(list_of_vars){
  output <- paste0("d", list_of_vars, collapse = ", ")
  output <- paste0("c(", output, ")")
  
  return(output)
}

output_param_for_ode_solver <- function(paramList){
  # Convert parmeter list to named vector for differential equation execution
  # Input 
  #   @paramList - list of params (RV rv.PARAMETERS$parameters)
  #     needs to have sublists of Name and Base.Value
  # Output 
  #   @out - named vector of parameter values
  nPar <- length(paramList)
  param.values <- vector()
  param.names  <- vector()
  for (i in seq_along(paramList)) {
    param.values[i] <- paramList[[i]]$BaseValue
    param.names[i]  <- paramList[[i]]$Name
  }
  
  out <-  as.numeric(param.values)
  names(out) <- param.names
  return(out)
}

output_ICs_for_ode_solver <- function(IC_Data_Structure){
  # Create named vector of model value with their names from RV species struct
  
  # Find length and initialize vectors
  nVar <- length(IC_Data_Structure)
  var.names <- vector(mode = "character", nVar)
  var.vals  <- rep(0, nVar)
  
  for (i in seq_along(IC_Data_Structure)) {
    var.names[i] <- IC_Data_Structure[[i]]$Name
    var.vals[i]  <- as.numeric(IC_Data_Structure[[i]]$BaseValue)
  }

  names(var.vals) <- var.names
  return(var.vals)
}
