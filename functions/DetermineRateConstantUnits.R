DetermineRateConstantUnits <- function(coefs, 
                                       baseMassUnit, 
                                       baseVolumeUnit, 
                                       baseTimeUnit,
                                       selectedMassUnit,
                                       selectedVolumeUnit,
                                       selectedTimeUnit,
                                       addOrder = NA) {
  # Input: 
  #   coefs: string of coefficients for rate law separated by space
  #   baseMassUnit - backend mass unit 
  #   baseVolumeUnit - backend volume unit
  #   baseTimeUnt - backend time uniy
  #   selectedMassUnit - user selected mass unit
  #   selectedVolumeUnit - user selected volume unit
  #   selectedTimeUnit - user selected time unit
  #   addOrder - (num) if not NA, add specified number to order 
  # Output:
  #   unit - string of unit seen by user
  #   unit description - unit description (num <multiply> etc)
  #   unitbase -string describing base unit
  
  # Split and sum coefficients
  num.coefs <- as.numeric(strsplit(coefs, ", ")[[1]])
  sum.coefs <- sum(num.coefs)
  
  # Add order if specified
  if (!is.na(addOrder)) {
    sum.coefs <- sum.coefs + addOrder
  }
  
  # First Order
  if (sum.coefs == 1) {
    # print("First Order")
    # First order should be (1/time)
    u   <- paste0("1/", selectedTimeUnit)
    u.b <- paste0("1/", baseTimeUnit)
    u.d <- "num <div> time"
  } else if (sum.coefs == 2) {
    # Second order - volume/(concentration*time)
    # order relates to the exponents
    u   <- paste0(selectedVolumeUnit, 
                  "/(", selectedMassUnit, "*", selectedTimeUnit, ")")
    u.b <- paste0(baseVolumeUnit, 
                  "/(", baseMassUnit, "*", baseTimeUnit, ")")
    u.d <- paste0("volume", 
                  " <div> ",
                  "<group> conc (", baseMassUnit, ")",
                  "<multiply> time <endgroup>")
  } else if (sum.coefs > 2) {
    # n-th order greater than two: volume^(n-1)/(concentration^(n-1)*time)
    # order relates to the exponents
    coef = sum.coefs - 1
    u   <- paste0(
      selectedVolumeUnit, "^", coef, 
      "/(", selectedMassUnit, "^", coef, "*", selectedTimeUnit, ")")
    u.b <- paste0(baseVolumeUnit, "^", coef, 
                  "/(", baseMassUnit, "^", coef, "*", baseTimeUnit, ")")
    # u.d <- paste0("conc (",
    #               baseMassUnit,
    #               ") <power>(", 
    #               coef, ") <div> ",
    #               "<group> volume ",
    #               "<power>(", coef, ") ",
    #               "<multiply> time <endgroup>")
    
    u.d <- paste0("volume",
                  " <power>(", 
                  coef, ") <div> ",
                  "<group>",
                  "conc (", baseMassUnit, ")",
                  " <power>(", coef, ") ",
                  "<multiply> time <endgroup>")
  }
  
  out <- list("unit" = u,
              "unit.description" = u.d,
              "unit.base" = u.b)
  
  return(out)
}