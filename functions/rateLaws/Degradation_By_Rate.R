Degradation_By_Rate <- function(rateConstant, 
                                concentrationDependent, 
                                degradatedVariable,
                                volumeVar) {
  
  if (concentrationDependent) {
    
    rate.law <- paste0(rateConstant, "*", degradatedVariable)
    
    ps <- paste0(rateConstant, "*", "[", degradatedVariable, "]")
    
    latex.rate.law <- paste0(Var2Latex(rateConstant), "*", 
                             (degradatedVariable))
    
    mj <- paste0(Var2MathJ(rateConstant), "*", Var2MathJ(degradatedVariable))
    
    # Add volume to terms
    rate.law <- paste0(volumeVar, "*(", rate.law, ")")
    
    # Mathjax
    mj <- paste0(Var2Latex(volumeVar), "*(", latex.rate.law, ")")
    
    latex.rate.law <- paste0(Var2Latex(volumeVar), "*(", latex.rate.law, ")")
    
    # Mathml
    ml <- katex_mathml(latex.rate.law)
  } else {
    
    rate.law <- rateConstant
    
    ps <- rateConstant
    
    latex.rate.law <- Var2Latex(rateConstant)
    
    mj <- Var2MathJ(rateConstant)
    
    # Add volume to terms
    rate.law <- paste0(volumeVar, "*(", rate.law, ")")
    
    # Mathjax
    mj <- paste0(Var2Latex(volumeVar), "*(", latex.rate.law, ")")
    
    latex.rate.law <- paste0(Var2Latex(volumeVar), "*(", latex.rate.law, ")")
    
    # Mathml
    ml <- katex_mathml(latex.rate.law)
  }
  
  # Contentml
  content.ml <- 
    paste0("<math xmlns=\"http://www.w3.org/1998/Math/MathML\">",
           string2mathml(rate.law),
           "</math>")
  
  out.list <- list("string" = rate.law,
                   "pretty.string" = ps,
                   "latex" = latex.rate.law,
                   "mj" = mj,
                   "mathml" = ml,
                   "content.ml" = content.ml)
  return(out.list)
}