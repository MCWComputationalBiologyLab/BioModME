BuildParameterStatement <- function(parameters, parameterValues){
  
  out <- "parameters <- c("
  
  for (i in seq(length(parameters))) {
    out <- paste0(out, parameters[i])
    if (i != length(parameters)) {
      out <- paste0(out, " = ", parameterValues[i], ", \n")
    }else{
      out <- paste0(out, " = ", parameterValues[i], ")\n")
    } #end else
  } #end for
  return(out)
}

BuildStateStatement <- function(ICvars, ICvals){
  
  out <- "state <- c("
  
  for (i in seq(length(ICvars))) {
    out <- paste0(out, ICvars[i])
    if (i != length(ICvars)) {
      out <- paste0(out, " = ", ICvals[i], ", \n")
    }else{
      out <- paste0(out, " = ", ICvals[i], ")\n")
    } #end else
  } #end for
  return(out)
}


CreateRModel <- function(variables, parameters, parameterValues, ICsValues,
                         additionalEqns, diffEQs, timeScaleBool, timeScaleValue,
                         solverMethod, timeStart, timeEnd, timeStep){
  
  out.script <- "library(deSolve)\n\n"
  
  start.function <- "myModel <- function(t, state, parameters) {\n "
  start.function <- paste0(start.function,
                           " with(as.list(c(state, parameters)), {\n")                         
  
  parameters <- BuildParameterStatement(parameters, parameterValues)
  print(parameters)
  state <- BuildStateStatement(variables, ICsValues)
  print(state)
  times <- paste0("times <- seq(", timeStart, ", ", timeEnd, ", by = ",
                  timeStep, ")\n")
  
  #convert differential equations to proper format for R
  diff.eqns <- diffeq_to_text(diffEQs, variables)
  d.of.var <- output_var_for_ode_solver(variables)
  #add check in case no additional equations are in model
  add.eqns <- ifelse(length(additionalEqns) == 0,
                     NA,
                     rateEqns_to_text(additionalEqns))
  #multiplier for time scale needs to be added
  
  ode.line <- paste0("\nout <- ode(y = state, times = times, func = myModel, ",
                     "parms = parameters)\n")
  
  #build output script from components above
  out.script <- paste0(out.script, start.function)
  if (!is.na(add.eqns)) 
    out.script <- paste0(out.script, add.eqns)
  out.script <- paste0(out.script, diff.eqns)
  out.script <- paste0(out.script, "list(", d.of.var, ")\n  })\n}\n\n")
  out.script <- paste0(out.script, parameters)
  out.script <- paste0(out.script, state)
  out.script <- paste0(out.script, times)
  out.script <- paste0(out.script, ode.line)
  
  return(out.script)

  
}