Var2MathJ <- function(var = NULL){
  # Converts 
  # Args:
  #   var: variable to change to mathjax format converting subscripts properly
  #
  # Returns:
  #   var in latex readable form
  #
  # Ex: var = my_var -> var = my_{var} 
  
  
  latex.var = ""
  
  if (!is.null(var)) {
    split.var = strsplit(var, "")[[1]]
    has.underscore = FALSE
    
    for (i in seq(length(split.var))) {
      if (split.var[i] == "_" & !has.underscore) {
        has.underscore = TRUE
        latex.var = paste0(latex.var, split.var[i], "{")
      } else if (split.var[i] == "_" & has.underscore) {
        # Already inside subscript braces; escape additional underscores so
        # KaTeX renders them literally instead of raising a double-subscript
        # error (e.g. mu_max_x_2 -> mu_{max\_x\_2}).
        latex.var = paste0(latex.var, "\\_")
      } else {
        latex.var = paste0(latex.var, split.var[i])
      }
    }
    if (has.underscore) {
      latex.var = paste0(latex.var, "}")
    }
    
  }
  
  return(latex.var)
}