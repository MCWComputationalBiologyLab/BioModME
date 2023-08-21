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
      }else{
        latex.var = paste0(latex.var, split.var[i])
      }
    }
    if (has.underscore) {
      latex.var = paste0(latex.var, "}")
    }
    
  }
  
  return(latex.var)
}