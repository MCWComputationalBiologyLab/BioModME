PrintVar <- function(variable,
                     sameLine = TRUE) {
  # Prints a variable and tells what that variable is
  var.name <- deparse(substitute(variable))
  
  if (sameLine) {
    cat(var.name, "-", variable, "\n")
  }
  else {
    cat(var.name)
    cat('\n')
    cat(variable)
    cat("\n")
  }
}