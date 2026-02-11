Henri_Michaelis_Menten_Vmax <- function(substrate, Km, Vmax, volumeVar) {
  
  latex = NA
  mj = NA
  ml = NA
  
  # Functional String
  
  rate.law <- paste0(Vmax, "*", substrate, "/", "(", Km, "+", substrate, ")")
  
  # Pretty String
  ps <- paste0(Vmax, "[", substrate, "]", 
               "/", 
               "(", Km, " + ", "[", substrate, "]", ")")
  # Latex
  latex.rate.law <- paste0("\\frac{",
                           Var2Latex(Vmax), "*", Var2Latex(substrate), 
                           "}{",
                           Var2Latex(Km), 
                           "+",
                           Var2Latex(substrate),
                           "}")
  # MathJax
  mj <- paste0("\\frac{",
               Var2MathJ(Vmax), "*", Var2MathJ(substrate), 
               "}{",
               Var2MathJ(Km), 
               "+",
               Var2MathJ(substrate),
               "}")
  
  # Add volume to terms
  rate.law <- paste0(volumeVar, "*(", rate.law, ")")
  
  # Mathjax
  mj <- paste0(Var2Latex(volumeVar), "*(", latex.rate.law, ")")
  
  latex.rate.law <- paste0(Var2Latex(volumeVar), "*(", latex.rate.law, ")")
  
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
                   "content.ml" = content.ml)
  
  return(out.list)
}