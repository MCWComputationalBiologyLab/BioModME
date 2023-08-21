prettyDiffEquations <- function(strExpr, 
                                dfKey,
                                convertToMJ = FALSE) {
  # Converts a string expression of a differential equation to its pretty form
  # Input: 
  #   @strExpr - string of diffeq ("V_max*S/(K_m + S)")
  #   @dfKey - df: cols: term, type
  #     $term - contains all components in the string (V_max, S, K_m)
  #     $type - identifies component (param, species, param)
  #   @convertToMJ - bool, if true, variables are converted to mathjax format
  
  # Example: 
  # input: "V_max*S/(K_m + S)"
  # output: "V_max[S]/(K_m + [S])
  
  # Catch conversion if its 0, null, na
  if (strExpr == "0" || strExpr == 0 || !isTruthy(strExpr)) {return(strExpr)}
  
  # Split String and Process
  strExpr <- remove_braces(strExpr)
  strExpr <- gsub("\\left(", "", strExpr, fixed = TRUE)
  strExpr <- gsub("\\right)", "", strExpr, fixed = TRUE)
  vec     <- SplitEquationString(strExpr)
  
  # Placeholder for indexes that need to be removed later
  to_remove <- c()
  
  # Iterate over the terms in vec
  for (i in 1:length(vec)) {
    
    # Reconvert params to mathjax form if necessary
    if (vec[i] %in% dfKey$term[dfKey$type == 'param'] && convertToMJ) {
      vec[i] <- Var2MathJ(vec[i])
    }
    
    # Check if the term is in df and is of type 'species'
    if (vec[i] %in% dfKey$term[dfKey$type == 'species']) {
      if(convertToMJ) {
        vec[i] <- paste0("[", Var2MathJ(vec[i]), "]")
      } else {
        vec[i] <- paste0("[", vec[i], "]")
      }
      # Check if it is next to a '*', if so store to remove
      if (i > 1 && vec[i - 1] == "*") {
        to_remove <- c(to_remove, i - 1)
      } else if (i < length(vec) && vec[i + 1] == "*") {
        to_remove <- c(to_remove, i + 1)
      }
    }
  }
  
  # Remove the multiplication terms next to the converted values
  vec <- vec[-to_remove]
  
  # Collapse the modified vector into a single string
  result <- paste(vec, collapse = "")
  
  return(result)
}