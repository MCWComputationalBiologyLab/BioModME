is_valid_expression <- function(expr_string, variables) {
  # Checks to see if an expression is a valid/legal expression
  # Runs a try catch loop  to see if expression evaulates.
  # Best way to do it? Probably not. O well.  
  # To solve we need to assign values to all the variables and see if it solves
  # This does seem to work for inf expressions such as 1/log(x)
  
  # Input:
  #   @expr_string - string expression to be checked for validity
  #   @variables - variables used in the model 
  #
  # Output:
  #   @ Boolean, TRUE if valid, FALSE if not valid
  
  # if expresssion is empty string, it is not valid
  if (expr_string == "") {return(FALSE)}
  
  # Assign value of 1 to all variables in expression
  for (i in seq_along(variables)) {
    eval(parse(text = paste0(variables[i], " = 1")))
  }
  
  # Wrap the expression in a tryCatch block to catch any errors
  result <- tryCatch(eval(parse(text = expr_string)), error = function(e) e)
  
  # Check if the result is an error or not
  return(!inherits(result, "error"))
}