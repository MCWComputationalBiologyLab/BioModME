SplitEntry <- function(inString, delimiter = ", ") {
  # Short cut function to split reactive variable entries used in this model
  # Inputs:
  #   @inString: string to split (ex. "var1, var2, var3")
  #   @delimiter: value to split string on
  #
  # Outputs:
  #   @out - vector, each term of split (c("var1", "var2", "var3"))
  
  out <- NA
  
  if (isTruthy(inString)) {
    out <- strsplit(inString, delimiter)[[1]]
  }
  
  return(out)
}