Regulated_Law_Of_Mass_Action <- function(r.stoich, 
                                         reactants,
                                         p.stoich,
                                         products,
                                         reversible,
                                         kf,
                                         kr,
                                         volumeVar,
                                         Use.Forward.Mod,
                                         Forward.Mods,
                                         Forward.Pars,
                                         Use.Reverse.Mod,
                                         Reverse.Mods,
                                         Reverse.Pars) {
  
  # assign initial values to these
  kf.latex <- NULL
  kr.latex <- NULL
  
  # This function will (for now) calculate a new kf and kr based on the 
  # regulator values and then pass that info to the Law_of_Mass_Action fxn. 
  if (Use.Forward.Mod) {
    kf       <- regulatorToRate(Forward.Mods, Forward.Pars)
    kf.latex <- regulatorToRateLatex(Forward.Mods, Forward.Pars)
  }
  if (Use.Reverse.Mod) {
    kr       <- regulatorToRate(Reverse.Mods, Reverse.Pars)
    kr.latex <- regulatorToRateLatex(Reverse.Mods, Reverse.Pars)
  }
  
  results <- Law_Of_Mass_Action(r.stoich, 
                                reactants,
                                p.stoich,
                                products,
                                reversible,
                                kf,
                                kr,
                                volumeVar,
                                kf.latex = kf.latex,
                                kr.latex = kr.latex)
  # Contentml
  content.ml <- 
    paste0("<math xmlns=\"http://www.w3.org/1998/Math/MathML\">",
           string2mathml(results$string),
           "</math>")
  
  out.list <- list("string" = results$string,
                   "pretty.string" = results$pretty.string,
                   "latex" = results$latex,
                   "mj" = results$mj,
                   "mathml" = results$mathml,
                   "content.ml" = content.ml)
}
