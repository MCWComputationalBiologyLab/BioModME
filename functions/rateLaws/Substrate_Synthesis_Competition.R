Substrate_Synthesis_Competition <- function(rateConstant,
                                             substrate,
                                             species,
                                             competitor,
                                             alpha,
                                             Kc,
                                             speciesDependent,
                                             volumeVar) {
  
  # Build competition term: (1 - (X + alpha*Y) / Kc) or (1 - X / Kc) if no competitor
  if (!is.null(competitor) && !is.na(competitor) && competitor != "") {
    competition.term <- paste0("(1-(", species, "+", alpha, "*", competitor, ")/", Kc, ")")
    competition.term.latex <- paste0("\\left(1-\\frac{", Var2Latex(species), "+", Var2Latex(alpha), "\\cdot ", Var2Latex(competitor), "}{", Var2Latex(Kc), "}\\right)")
    competition.term.mj <- paste0("\\left(1-\\frac{", Var2MathJ(species), "+", Var2MathJ(alpha), "*", Var2MathJ(competitor), "}{", Var2MathJ(Kc), "}\\right)")
  } else {
    # If no competitor, just use species X: (1 - X / Kc)
    competition.term <- paste0("(1-", species, "/", Kc, ")")
    competition.term.latex <- paste0("\\left(1-\\frac{", Var2Latex(species), "}{", Var2Latex(Kc), "}\\right)")
    competition.term.mj <- paste0("\\left(1-\\frac{", Var2MathJ(species), "}{", Var2MathJ(Kc), "}\\right)")
  }
  
  # Build rate law based on species-dependent option
  if (speciesDependent) {
    # Option 1: k * S * X * (1 - (X + alpha*Y) / Kc)
    rate.law <- paste0(rateConstant, "*", substrate, "*", species, "*", competition.term)
    pretty.string <- paste0(rateConstant, "[", substrate, "][", species, "]", competition.term)
    latex.rate.law <- paste0(Var2Latex(rateConstant), "\\cdot ", Var2Latex(substrate), 
                             "\\cdot ", Var2Latex(species), "\\cdot ", competition.term.latex)
    mj.rate.law <- paste0(Var2MathJ(rateConstant), "*", Var2MathJ(substrate), "*", Var2MathJ(species),
                          "*", competition.term.mj)
  } else {
    # Option 2: k * S * (1 - (X + alpha*Y) / Kc)
    rate.law <- paste0(rateConstant, "*", substrate, "*", competition.term)
    pretty.string <- paste0(rateConstant, "[", substrate, "]", competition.term)
    latex.rate.law <- paste0(Var2Latex(rateConstant), "\\cdot ", Var2Latex(substrate), 
                             "\\cdot ", competition.term.latex)
    mj.rate.law <- paste0(Var2MathJ(rateConstant), "*", Var2MathJ(substrate),
                          "*", competition.term.mj)
  }
  
  # Add volume to terms
  rate.law <- paste0(volumeVar, "*(", rate.law, ")")
  
  # Mathjax
  mj.rate.law <- paste0(Var2Latex(volumeVar), "*(", mj.rate.law, ")")
  
  latex.rate.law <- paste0(Var2Latex(volumeVar), "*(", latex.rate.law, ")")
  
  # Mathml
  ml <- katex_mathml(latex.rate.law)
  
  # Contentml
  content.ml <- 
    paste0("<math xmlns=\"http://www.w3.org/1998/Math/MathML\">",
           string2mathml(rate.law),
           "</math>")
  
  out.list <- list("string" = rate.law,
                   "pretty.string" = pretty.string,
                   "latex" = latex.rate.law,
                   "mj" = mj.rate.law,
                   "mathml" = ml,
                   "content.ml" = content.ml)
  
  return(out.list)
}

