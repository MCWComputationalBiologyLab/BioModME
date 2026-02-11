r_catalytic_activation_irrev <- function(V,
                                         substrate,
                                         activator,
                                         Kms,
                                         Ka,
                                         volumeVar) {
  # Reaction law for building a catalytic activation irreversible reaction. 
  
  
  rate.law <- 
    paste0(
      V, "*", substrate, "*", activator, "/((",
      Kms, "+", substrate, ")*(", Ka, "+", activator, "))" 
    )
  
  ps <- 
    paste0(
      V, "[", substrate, "][", activator, "]/((",
      Kms, "+[", substrate, "])*(", Ka, "+[", activator, "]))" 
    )

  lV         <- Var2Latex(V)
  lsubstrate <- Var2Latex(substrate)
  lactivator <- Var2Latex(activator)
  lKms       <- Var2Latex(Kms)
  lKa        <- Var2Latex(Ka)
  
  latex.rate.law <- 
    paste0(
      "\\frac{", lV, "*", lsubstrate, "*", lactivator, "}{(",
      lKms, "+", lsubstrate, ")*(", lKa, "+", lactivator, ")}" 
    )

  mV         <- Var2MathJ(V)
  msubstrate <- Var2MathJ(substrate)
  mactivator <- Var2MathJ(activator)
  mKms       <- Var2MathJ(Kms)
  mKa        <- Var2MathJ(Ka)
  
  mj <- 
    paste0(
      paste0(
        "\\frac{", mV, "*", msubstrate, "*", mactivator, "}{(",
        mKms, "+", msubstrate, ")*(", mKa, "+", mactivator, ")}" 
      )
    )

  # Add volume to terms
  rate.law <- paste0(volumeVar, "*(", rate.law, ")")
  ps <- paste0(volumeVar, "*(", ps, ")")
  latex.rate.law <- paste0(Var2Latex(volumeVar), "*(", latex.rate.law, ")")
  mj <- paste0(Var2Latex(volumeVar), "*(", mj, ")")


  # Mathml
  ml <- katex_mathml(latex.rate.law)

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
                   "content.ml" = content.ml
                   )
  return(out.list)
}
