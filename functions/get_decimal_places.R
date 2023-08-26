# Function: get_decimal_places
#
# Purpose:
#   Determines the number of decimal places in a given number.
#
# Inputs:
#   step (numeric): The number whose decimal places need to be determined.
#
# Outputs:
#   Returns an integer representing the number of decimal places.
#
get_decimal_places <- function(step) {
  # Convert the number to a string and split on the decimal
  parts <- unlist(strsplit(as.character(step), split = "\\.")) 
  
  # If there's a portion after the decimal, return its length
  if (length(parts) == 2) {
    return(nchar(parts[2]))
  } else {
    return(0)
  }
}

# Example usage:
# 
# step_size <- 0.001
# decimal_places <- get_decimal_places(step_size)
# print(decimal_places)  # This should print 3
