
SimpleDiffusion <- function(species1, species2, PS) {
  # if species1 is on 
  
  str.out <- paste0(PS, "*(", species1, "-", species2, ")")
  
  ps <- paste0(PS, "*([", species1, "]-[", species2, "])")
  
  latex <- paste0(Var2Latex(PS),
                  "*(",
                  Var2Latex(species1),
                  "-",
                  Var2Latex(species2),
                  ")")
  
  mj <- paste0(Var2MathJ(PS),
               "*(",
               Var2MathJ(species1),
               "-",
               Var2MathJ(species2),
               ")")
  
  ml <- katex_mathml(latex)
  
  out.list <- list("string" = str.out,
                   "pretty.string" = ps,
                   "latex" = latex,
                   "mj" = mj,
                   "mathml" = ml)
  
  return(out.list)
}

FacilitatedDiffusion <- function(species, Vmax, Km) {
  
  str.out <- paste0(Vmax, "*", species, "/(", Km, "+", species, ")")
  
  ps <- paste0(Vmax, "[", species, "]", "/(", Km, "+[", species, "])")
  
  # Latex
  latex <- paste0("\\frac{",
                  Var2Latex(Vmax), "*", Var2Latex(species), 
                  "}{",
                  Var2Latex(Km), 
                  "+",
                  Var2Latex(species),
                  "}")
  
  # MathJax
  mj <- paste0("\\frac{",
               Var2MathJ(Vmax), "*", Var2MathJ(species), 
               "}{",
               Var2MathJ(Km), 
               "+",
               Var2MathJ(species),
               "}")
  
  # Mathml
  ml <- katex_mathml(latex)
  
  # 
  out.list <- list("string" = str.out,
                   "pretty.string" = ps,
                   "latex" = latex,
                   "mj" = mj,
                   "mathml" = ml)
  
  return(out.list)
}

Clearance <- function(species, rateConstant, compartmentVol) {
  
  str.out <- paste0(rateConstant, "*", species, "*", compartmentVol)
  
  ps <- paste0(rateConstant, "*[", species, "]*", compartmentVol)
  
  latex <- paste0(Var2Latex(rateConstant),
                  "*",
                  Var2Latex(species),
                  "*",
                  Var2Latex(compartmentVol))
  
  mj <- paste0(Var2MathJ(rateConstant),
               "*",
               Var2MathJ(species),
               "*",
               Var2MathJ(compartmentVol))
  
  ml <- katex_mathml(latex)
  
  out.list <- list("string" = str.out,
                   "pretty.string" = ps,
                   "latex" = latex,
                   "mj" = mj,
                   "mathml" = ml)
  
  return(out.list)
}

Flow <- function(species, 
                 rateConstant,
                 buildWPlus = FALSE,
                 buildWMinus = FALSE) {
  
  if (!buildWPlus && !buildWMinus) {
    str.out <- paste0(rateConstant, "*", species)
    
    ps <- paste0(rateConstant, "[", species, "]")
    
    latex <- paste0(Var2Latex(rateConstant),
                    "*",
                    Var2Latex(species))
    
    mj <- paste0(Var2MathJ(rateConstant),
                 "*",
                 Var2MathJ(species))
    
    ml <- katex_mathml(latex)
    
  } else if (buildWPlus) {
    str.out <- paste0("+", "(", rateConstant, "*", species, ")")
    
    ps <- paste0("+", "(", rateConstant, "[", species, "]", ")")
    
    latex <- paste0("+",
                    "(",
                    Var2Latex(rateConstant),
                    "*",
                    Var2Latex(species),
                    ")")
    
    mj <- paste0("+",
                 "\\left(",
                 Var2MathJ(rateConstant),
                 "*",
                 Var2MathJ(species),
                 "\\right)")
    
    ml <- katex_mathml(latex)
    
  } else if (buildWMinus) {
    str.out <- paste0("-", "(", rateConstant, "*", species, ")")
    
    ps <- paste0("-", "(", rateConstant, "[", species, "]", ")")
    
    latex <- paste0("-",
                    "(",
                    Var2Latex(rateConstant),
                    "*",
                    Var2Latex(species),
                    ")")
    
    mj <- paste0("-",
                 "\\left(",
                 Var2MathJ(rateConstant),
                 "*",
                 Var2MathJ(species),
                 "\\right)")
    
    ml <- katex_mathml(latex)
  }

  
  out.list <- list("string" = str.out,
                   "pretty.string" = ps,
                   "latex" = latex,
                   "mj" = mj,
                   "mathml" = ml)
  
  return(out.list)
}


FlowBetween <- function(speciesOut,
                        speciesIn,
                        compartmentOut,
                        compartmentIn, 
                        flowRates) {
  # @speciesOut - species leaving beginning flow
  # @speciesIn - string of species inputed from species out ("Sa Sb Sc etc")
  # @compartmentOut - compartment flow leaving from 
  # @compartmentIn - string of compartments flow going to ("C1 C2 C3")
  # @flowrate - flowrate variables in order of flowOut, flowIn1, flowIn2, etc
  
  # Output:
  # We will create a rate law here for each species in the flow and parse it 
  # different from the others. The first rate will be for the species out
  # and the rest of the rates are for the speciesIn.  The rate laws will be
  # a collapsed string using ", " delimiter. This process will be repeated for 
  # latex, mathjax, and mathml formats
  # Out <- "-(F1*A), +(F1.2*B), +(F1.3*C)
  
  # flows <- strsplit(flowRates, ", ")[[1]]
  print(speciesOut)
  print(speciesIn)
  print(compartmentIn)
  print(compartmentOut)
  print(flowRates)
  
  # Split Terms
  # speciesOut     <- strsplit(speciesOut, ", ")[[1]]
  # speciesIn      <- strsplit(speciesIn, ", ")[[1]]
  # compartment.Out <- strsplit(compartmentOut, ", ")[[1]]
  # compartment.In  <- strsplit(compartmentIn, ", ")[[1]]
  # flowRates      <- strsplit(flowRates, ", ")[[1]]
  
  # If no split, flowRates needs to be duped
  if (length(flowRates) == 1) {flowRates <- c(flowRates, flowRates)}
  
  # Initialize vectors
  rate.str <- vector("character", length = length(flowRates))
  rate.ps  <- vector("character", length = length(flowRates))
  rate.lat <- vector("character", length = length(flowRates))
  rate.mj  <- vector("character", length = length(flowRates))
  rate.ml  <- vector("character", length = length(flowRates))
  
  # Find rate law for species out
  rate.out <- Flow(speciesOut, 
                   flowRates[1],
                   buildWMinus = TRUE)
  print(rate.out)
  rate.str[1] <- rate.out$string
  rate.ps[1]  <- rate.out$pretty.string
  rate.lat[1] <- rate.out$latex
  rate.mj[1]  <- rate.out$mj
  rate.ml[1]  <- rate.out$mathml
  
  # Find rate laws for species in
  for (i in seq_along(speciesIn)) {
    rate.in <- Flow(speciesOut, 
                    flowRates[i+1],
                    buildWPlus = TRUE)
    print(rate.in)
    rate.str[i+1] <- rate.in$string
    rate.ps[i+1]  <- rate.in$pretty.string
    rate.lat[i+1] <- rate.in$latex
    rate.mj[i+1]  <- rate.in$mj
    rate.ml[i+1]  <- rate.in$mathml
  }
  
  # Collapse outputs
  str.out <- paste0(rate.str, collapse = ", ")
  ps      <- paste0(rate.ps,  collapse = ", ")
  latex   <- paste0(rate.lat, collapse = ", ")
  mj      <- paste0(rate.mj,  collapse = ", ")
  ml      <- paste0(rate.ml,  collapse = ", ")
  
  out.list <- list("string"        = str.out,
                   "pretty.string" = ps,
                   "latex"         = latex,
                   "mj"            = mj,
                   "mathml"        = ml)
  print("out list")
  print(out.list)
  return(out.list)
}
