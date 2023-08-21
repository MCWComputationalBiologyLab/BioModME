string2mathml <- function(stringExpression) {
  
  
  # Remove leading "+'
  stringExpression <- sub("^\\+", "", stringExpression)
  
  # Convert string to expression
  e <- parse(text = stringExpression)[[1]]
  
  # Convert to mathml vector terms
  mathml.terms <- expToMathML(e)
  
  # Collapse to String
  out <- paste0(mathml.terms, collapse = "")
  
  return(out)
}
