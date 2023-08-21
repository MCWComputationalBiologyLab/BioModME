collapseVector <- function(vector, 
                           delimiter = ", ",
                           convertBlank = FALSE) {
  # Collapses vector to string by delimeter
  # Skips if anything in vector is NA
  # Inputs:
  #   @vector - vector to collapse
  #   @delimiter - value to collapse by
  #   @convertBlank - Convert terms of whitespace to NA ("  " -> NA, " " -> NA)- 
  out <- NA
  
  if (!(anyNA(vector))) {
    out <- paste0(vector, collapse = delimiter)
  }
  
  if (convertBlank &!(anyNA(vector))) {
    if (RemoveWS(out) == "") {out <- NA}
  }
  
  return(out)
}