RemoveFromVector <- function(value, vector, firstOnly = FALSE) {
  # Removes value from vector. Will remove all occurances if firstOnly = F
  # Inputs
  #   @value - value(s) to remove from vector
  #   @vector - vector to remove values from by name
  #   @firstOnly - Bool, if true will only remove first instance of value
  # Output
  #   @Out - vector with removed value(s) by specifications
  # Remove Value from vector
  # Example:
  #   vec <- c(1,2, 2, 4, 6, 2)
  #   vec2 <- RemoveFromVector(2, vec)
  #   >>> vec2 = c(1,4,6)
  
  # Check to see if we should run
  run.removal <- FALSE
  for (val in value) {
    if (val %in% vector) {
      run.removal <- TRUE
    }
  }
  
  if (run.removal) {
    all.idxs.to.remove <- c()
    
    if (firstOnly) {
      for (i in seq_along(value)) {
        all.idxs.to.remove <- c(all.idxs.to.remove, match(value, vector))
      }
      out <- vector[-sort(all.idxs.to.remove)]
    } else {
      for (i in seq_along(value)) {
        all.idxs.to.remove <- c(all.idxs.to.remove, which(vector %in% value))
      }
      out <- vector[-sort(all.idxs.to.remove)]
    }
  } else {
    out <- vector
  }
  
  return(out)
}