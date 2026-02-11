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

Henri_Michaelis_Menten_no_Vmax <- function(substrate, 
                                           Km, 
                                           kcat, 
                                           enzyme,
                                           volumeVar) {
  
  ps = NA
  latex = NA
  mj = NA
  ml = NA
  
  # Functional String
  
  rate.law <- paste0(kcat, "*", enzyme, "*", substrate, "/",
                    "(", Km, "+", substrate, ")")
  
  # Pretty String
  ps <- paste0(kcat, "[", enzyme, "]", "[", substrate, "]", 
               "/", 
               "(", Km, " + ", "[", substrate, "]", ")")
  # Latex
  latex.rate.law <- paste0("\\frac{",
                  Var2Latex(kcat), "*", Var2Latex(enzyme),
                  "*", Var2Latex(substrate), 
                  "}{",
                  Var2Latex(Km), 
                  "+",
                  Var2Latex(substrate),
                  "}")
  # MathJax
  mj <- paste0("\\frac{",
               Var2MathJ(kcat), "*", Var2MathJ(enzyme),
               "*", Var2MathJ(substrate),
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

Synthesis_By_Rate <- function(rateConstant, volumeVar) {
  
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

Synthesis_By_Factor <- function(rateConstant, factor, volumeVar) {
  
  rate.law <- paste0(rateConstant, "*", factor)
  
  ps <- paste0(rateConstant, "*", "[", factor, "]")
  
  latex.rate.law <- paste0(Var2Latex(rateConstant), "*", Var2Latex(factor))
  
  mj <- paste0(Var2MathJ(rateConstant), "*", Var2MathJ(factor))
  
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

Degradation_By_Enzyme_Vmax <- function(degradatedVariable, 
                                       Km,
                                       Vmax,
                                       volumeVar) {
  
  # Functional String
  rate.law <- paste0(Vmax, "*", degradatedVariable, "/",
                    "(", Km, "+", degradatedVariable, ")")
  
  # Pretty String
  ps <- paste0(Vmax, "[", degradatedVariable, "]", 
               "/", 
               "(", Km, " + ", "[", degradatedVariable, "]", ")")
  # Latex
  latex.rate.law <- paste0("\\frac{",
                  Var2Latex(Vmax), "*", Var2Latex(degradatedVariable), 
                  "}{",
                  Var2Latex(Km), 
                  "+",
                  Var2Latex(degradatedVariable),
                  "}")
  # MathJax
  mj <- paste0("\\frac{",
               Var2MathJ(Vmax), "*", Var2MathJ(degradatedVariable), 
               "}{",
               Var2MathJ(Km), 
               "+",
               Var2MathJ(degradatedVariable),
               "}")
  
  
  # Add volume to terms
  rate.law <- paste0(volumeVar, "*(", rate.law, ")")
  
  # Mathjax
  mj <- paste0(Var2Latex(volumeVar), "*(", latex.rate.law, ")")
  
  latex.rate.law <- paste0(Var2Latex(volumeVar), "*(", latex.rate.law, ")")
  
  # Mathml
  ml <- katex_mathml(latex.rate.law)
  
  # 
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

Degradation_By_Enzyme_no_Vmax <- function(degradatedVariable, 
                                          Km, 
                                          kcat, 
                                          enzyme,
                                          volumeVar) {
  # Functional String
  rate.law <- paste0(kcat, "*", enzyme, "*", degradatedVariable, "/",
                    "(", Km, "+", degradatedVariable, ")")
  
  # Pretty String
  ps <- paste0(kcat, "[", enzyme, "]", "[", degradatedVariable, "]", 
               "/", 
               "(", Km, " + ", "[", degradatedVariable, "]", ")")
  # Latex
  latex.rate.law <- paste0("\\frac{",
                  Var2Latex(kcat), "*", Var2Latex(enzyme),
                  "*", Var2Latex(degradatedVariable), 
                  "}{",
                  Var2Latex(Km), 
                  "+",
                  Var2Latex(degradatedVariable),
                  "}")
  # MathJax
  mj <- paste0("\\frac{",
                  Var2MathJ(kcat), "*", Var2MathJ(enzyme),
                  "*", Var2MathJ(degradatedVariable), 
                  "}{",
                  Var2MathJ(Km), 
                  "+",
                  Var2MathJ(degradatedVariable),
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

Law_Of_Mass_Action <- function(r.stoich, 
                               reactants,
                               p.stoich,
                               products,
                               reversible,
                               kf,
                               kr,
                               volumeVar,
                               kf.latex = NULL,
                               kr.latex = NULL) {
  
  # Generated equations for law of mass action for given inputs.
  # Where law of mass action is:
  # for equation aA + bB (kr)<-->(kf) cC + dD then the derviation is as follows:
  # -(1/a)*A^a = -(1/b)*B^b = (1/c)*C^c = (1/d)*D^d = kf*A^a*B^b - kr*C^c*D^d
  
  # Example equation A + 2B ->(kf1) C used here
  # Inputs:
  #   @r.stoich - Coefficients of variables on RHS of equation in vector form c(1)
  #   @reactants - Var Name on right hand side in vector form: c(C)
  #   @p.stoich - Coefficients of var on LHS of equation in vector form: c(1,2)
  #   @products - Variable names of left hand side eqns in vector form: c(A, B)
  #   @reversible - Describes if reaction is forward (forward_only) or
  #             both (both_directions): "both_directions"
  #   @kf - numerical value of forward rate constant: kf1
  #   @kr - numerical value of reverse rate constant: kr1
  #   @volumeVar - volume variable reaction is occuring in
  #   @kf.latex - latex expression for kf, used for regulator expressions
  #   @kr.latex - latex expression for kr, used for reulator expressions
  
  # Outputs:
  # String of law of mass action result.  For example for A:
  # Case1: A -> B, one var on each side
  
  
  ps    <- NA
  latex <- NA
  mj    <- NA
  ml    <- NA
  
  #split to vectors
  r.stoich    <- strsplit(r.stoich, ", ")[[1]]
  p.stoich    <- strsplit(p.stoich, ", ")[[1]]
  reactants   <- strsplit(reactants, ", ")[[1]]
  products    <- strsplit(products, ", ")[[1]]
  n.reactants <- length(reactants)
  n.products  <- length(products)
  
  
  # Build Reactant Part Of Rate Law
  builder <- ""
  for (i in seq(n.reactants)) {
    if (i == 1) {
      if (r.stoich[i] == "1") {
        builder <- reactants[i]
        lat.builder <- Var2Latex(reactants[i])
      } else {
        builder <- paste0(reactants[i], "^", r.stoich[i])
        lat.builder <- paste0(Var2Latex(reactants[i]), 
                              "^{", 
                              r.stoich[i],
                              "}")
      }
    } else {
      if (r.stoich[i] == "1") {
        builder <- paste0(builder, "*", reactants[i])
        lat.builder <- paste0(lat.builder, "*", Var2Latex(reactants[i]))
      } else {
        builder <- paste0(builder, 
                          "*", 
                          reactants[i], 
                          "^", 
                          r.stoich[i])
        
        lat.builder <- paste0(lat.builder,
                              "*",
                              Var2Latex(reactants[i]),
                              "^{",
                              r.stoich[i],
                              "}")
      }
    }
  }
  # Add rate constant
  rate.from.reactant <- paste0(kf, "*", builder)
  rate.law <- rate.from.reactant  # Add rate constant
  
  # Latex
  if (is.null(kf.latex)) {
    lat.rate.from.reactant <- paste0(Var2Latex(kf), "*", lat.builder)
  } else {
    lat.rate.from.reactant <- paste0(kf.latex, "*", lat.builder)
  }
  latex.rate.law <- lat.rate.from.reactant
  
  if (reversible == "both_directions") {
    # Build Product Part Of Rate Law
    builder <- ""
    for (i in seq(n.products)) {
      if (i == 1) {
        if (p.stoich[i] == "1") {
          builder <- products[i]
          lat.builder <- Var2Latex(products[i])
        } else {
          builder <- paste0(products[i], "^", p.stoich[i])
          lat.builder <- paste0(Var2Latex(products[i]), 
                                "^{", 
                                p.stoich[i],
                                "}")
        }
      } else {
        if (p.stoich[i] == "1") {
          builder <- paste0(builder, "*", products[i])
          lat.builder <- paste0(builder, "*", Var2Latex(products[i]))
        } else {
          builder <- paste0(builder, "*",products[i], "^", p.stoich[i])
          lat.builder <- paste0(lat.builder, 
                                "*",
                                Var2Latex(products[i]), 
                                "^{", 
                                p.stoich[i],
                                "}")
        }
      }
    }
    # Add rate constant
    rate.from.product <- paste0(kr, "*", builder)
    rate.law <- paste0(rate.law, "-", rate.from.product)

    # Latex
    if (is.null(kr.latex)) {
      lat.rate.from.product <- paste0(Var2Latex(kr), "*", lat.builder)
      latex.rate.law <- paste0(latex.rate.law, "-", lat.rate.from.product)
    } else {
      lat.rate.from.product <- paste0(kr.latex, "*", lat.builder)
      latex.rate.law <- paste0(latex.rate.law, "-", lat.rate.from.product)
    }
  }
  
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
