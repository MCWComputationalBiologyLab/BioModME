RemoveNA <- function(vector2RemoveFrom) {
  # Remove NA from vector without na.omit attributes adding
  out <- vector2RemoveFrom[!is.na(vector2RemoveFrom)]
  
  return(out)
}