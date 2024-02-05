# This script contains all the control functions to build our model in julia

# Build State Variable ---------------------------------------------------------

#' Generate state function string
#'
#' This function generates a state function string based on the provided species and initial conditions.
#'
#' @param species Vector of species names.
#' @param ICs Vector of initial conditions corresponding to species.
#'
#' @return A string representing the state function.
#'
#' @examples
#' \dontrun{
#' state_string <- jl_state_variables(c("state1", "state2"), c(0.1, 6))
#' cat(state_string)
#' }
jl_state_variables <- function(species, ICs) {
  
  # Create the function string
  state.string <- "function state()\n\tComponentVector{Float64}(\n"
  
  for (i in seq_along(species)) {
    if (i != length(species)) {
      line <- paste("\t\t", species[i], " = ", ICs[i], ",", sep = "")
      
    } else {
      line <- paste("\t\t", species[i], " = ", ICs[i], sep = "")
    }
    state.string <- paste(state.string, line, "\n", sep = "")
  }
  
  state.string <- paste(state.string, "\t)\nend", sep = "")
  
  return(state.string)
}

# Build Param Variable ---------------------------------------------------------
#' Generate params function string
#'
#' This function generates a params function string based on the provided parameter names
#' and default values.
#'
#' @param param_names Vector of parameter names.
#' @param defaults_values Vector of default values corresponding to parameter names.
#'
#' @return A string representing the params function.
#'
#' @examples
#' \dontrun{
#' param_string <- generate_params_function(c("param1", "param2"), c(1, 5))
#' cat(param_string)
#' }
jl_parameter_variables <- function(param_names, defaults_values) {
  param_string <- 'function params(; kwargs...)\n\tdefaults = (\n'
  
  for (i in seq_along(param_names)) {
    line <- paste("\t\t", param_names[i], " = ", defaults_values[i], ",", sep = "")
    param_string <- paste(param_string, line, "\n", sep = "")
  }
  
  param_string <- paste(param_string, '\t)\n\tuser_params = merge(defaults, kwargs)\n\treturn ComponentVector{Float64}(user_params)\nend', sep = "")
  
  return(param_string)
}


# Build Diffeq Section----------------------------------------------------------
#' Generate modelDiffEq! function string
#'
#' This function generates a modelDiffEq! function string based on the provided
#' species, parameters, differential equations, and rules.
#'
#' @param species Vector of species names.
#' @param parameters Vector of parameter names.
#' @param diffeqs Vector of differential equations.
#' @param rules Vector of rules.
#'
#' @return A string representing the modelDiffEq! function.
#'
#' @examples
#' \dontrun{
#' diff_eq_string <- 
#'    generate_diff_eq_function(c("state1", "state2"),
#'                              c("parm1", "parm2"),
#'                              c("s1 = blah blah", "s2 = blah blah"),
#'                              c("rule1 = parm1*state1 + 5"))
#' cat(diff_eq_string)
#' }
jl_diff_equations <- function(species, 
                              parameters, 
                              diffeqs, 
                              rules,
                              line_width = 60) {
  
  # Create the function string
  diff_eq_string <- "function modelDiffEq!(d, u, p, t)\n\n"
  
  # Unpack State Variables
  unpack_state_line <- paste0(paste(species, collapse = ', '), ' = u')
  unpack_state_lines <- strwrap(unpack_state_line, width = line_width, simplify = TRUE)
  unpack_species <- paste("\t", unpack_state_lines, "\n", collapse = "\n")
  diff_eq_string <- 
    paste(
      diff_eq_string, 
      "# Unpack State Variables\n", 
      unpack_species, 
      "\n\n",
      sep = ""
    )
  
  # Unpack Parameter Variables
  param.line <- paste0(paste(parameters, collapse = ', '), ' = p')
  unpack_parameter_lines <- strwrap(param.line, width = line_width, simplify = TRUE)
  unpack_parameter_lines <- paste('\t', unpack_parameter_lines, collapse = '\n')
  
  diff_eq_string <- 
    paste(
      diff_eq_string, 
      "# Unpack Parameter Variables\n", 
      unpack_parameter_lines, 
      "\n\n",
      sep = ""
    )
  
  # NonConstant Values (Rules)
  if (length(rules) > 0) {
    unpack_rules <- paste("\t", paste(rules, collapse = "\n"), "\n", sep = "")
    diff_eq_string <- 
      paste(
        diff_eq_string, 
        "# NonConstant Values (Rules)\n", 
        unpack_rules, 
        "\n",
        sep = ""
      )
  }
  
  # Differential Equations
  if (length(diffeqs) > 0) {
    unpack_diffeqs <- 
      paste(
        "\t", 
        paste0("d.", species, " = ", diffeqs, collapse = "\n\t"), 
        "\n", 
        sep = ""
      )
    diff_eq_string <-
      paste(
        diff_eq_string, 
        "# Differential Equations\n", 
        unpack_diffeqs, 
        "\n", 
        sep = ""
      )
  }
  
  diff_eq_string <- paste(diff_eq_string, "\n\treturn d\nend", sep = "")
  
  return(diff_eq_string)
}

# Combine JL sections ----------------------------------------------------------
# Inputs:
#   @species - vector of species names in model
#   @parameters - vector of names of parameters in model
#   @diffEquations - vector of text differential equations
#   @parameterValues - vector of param values corresponding to parameters
#   @additionalEqns - vector of additional equations user created
#   @ICs - vector of species initial condition values
#   @timeScaleBool - boolean if time scale exists
#   @timeScaleValue - numeric value of scale value
#   @timeStart - time model starts
#   @timeEnd  - time model ends
#   @timeStep - step between start and end times
jl_generate_script <- function(species, 
                               parameters, 
                               diffEquations, 
                               parameterValues, 
                               rules, 
                               ICs, 
                               timeStart,
                               timeEnd) {
  
  ConvertVarForJulia <- function(varToConvert) {
    #sub all "." and "_"
    converted.var <- gsub("\\.", "_", varToConvert)
    
    return(converted.var)
  }
  
  species  <- sapply(species, ConvertVarForJulia, USE.NAMES = FALSE)
  parameters <- sapply(parameters, ConvertVarForJulia, USE.NAMES = FALSE)
  equations  <- sapply(diffEquations, ConvertVarForJulia, USE.NAMES = FALSE)
  # Need to apply var converter to rules
  
  # Code Header Info 
  h.open <- paste0("Code generated by BioModME")
  h.date <- paste0("Created on ", Sys.time())
  # h.units <- paste0("Unit output will go here.")
  # h.eqns <- paste0("Reaction equations will go here")
  # h.io   <- paste0("Model IO will go here")
  
  header <- paste0("# ", h.open, "\n",
                   "# ", h.date, "\n",
                   # "% ", h.units, "\n",
                   # "% ", h.eqns, "\n",
                   # "% ", h.io, "\n",
                   "# ", "-------------------------------")
  
  # Build State
  state.vars <- jl_state_variables(species, ICs)
  
  # Build Parameters
  parameter.vars <- jl_parameter_variables(parameters, parameterValues)
  
  # Build Differential Equations
  diff.eq <- 
    jl_diff_equations(species, parameters, diffEquations, rules, line_width = 60)
  
  # Create Driver
  driver <- c(
    "\n\n",
    "u0 = state()\n",
    "p = params()\n",
    "# We can pass and change parameter values above like so:\n",
    "#p = params(DOSE_ml = 5, DOSE_zf = 300)\n",
    paste0("tspan = (", timeStart, timeEnd, ")\n"),
    "prob = ODEProblem(modelDiffEq!, u0, tspan, p)\n",
    "sol = solve(prob)\n",
    "plot(sol, idxs = (0, 1))\n",
    
    # Add ability to plot by index -----------------------------------------------
    "\n",
    "# Index values of state variables to plot \n",
    paste("#", strwrap(paste0(seq_along(species), "-", species, collapse = ", "), 
                       width = line_width, simplify = TRUE), collapse = "\n"),
    "\n",
    "u0_names = state_names()\n",
    "selected_indices = [2, 3]  # Adjust as needed\n",
    "\n# Extracting values and names for selected variables\n",
    "selected_variables = [sol[i, :] for i in selected_indices]\n",
    "variable_names = u0_names[selected_indices]\n",
    "\n# Plotting selected variables against time\n",
    "plot()\n",
    "for (variable, name) in zip(selected_variables, variable_names)\n",
    "\tplot!(sol.t, variable, label=name, linewidth=2)\n",
    "end\n\n",
    "# Show the legend\n",
    "plot!(legend=true)"
  )
  
  out <- paste0(header, "\n\n", 
                state.vars, "\n\n", 
                parameter.vars,"\n\n", 
                diff.eq, "\n\n", 
                paste0(driver, collapse = ""))
  return(out)
}
