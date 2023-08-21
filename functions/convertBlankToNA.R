convertBlankToNA <- function(valueToChange) {
  # Check function to convert "" to NA
  if (valueToChange == "") {
    out <- NA
  } else {
    out <- valueToChange
  }
  return(out)
}