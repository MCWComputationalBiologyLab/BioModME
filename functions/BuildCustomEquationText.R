BuildCustomEquationText <- function(reactants, 
                                    products,
                                    modifiers,
                                    parameters,
                                    reversible = FALSE,
                                    prodDegSymbol = FALSE) {
  # Used for building custom equations.  Will build the reaction strings
  # Inputs:
  # @reactants - vector, reactants in reaction
  # @products - vector, products in reaction
  # @modifiers - vector, modifiers in reaction
  # @parameters - vector, parameters in reaction
  # @reversible - bool, true if reversible, false if irrerevsible
  # @prodDegSymbol - bool, if true, product side will have latex/mj deg symbol
  
  # Outputs
  # @text - string, string version of eqn
  # @latex - string, latex version of eqn
  # @mathjax - string, mathjax version of eqn
  
  # BUILD STRING REACTION_______________________________________________________
  
  # Build reaction strings
  if (isTruthy(reactants)) {
    for (i in seq_along(reactants)) {
      if (i == 1) {
        reactant.side <- reactants[i]
      } else {
        reactant.side <- paste0(reactant.side, " + ", reactants[i])
      }
    }
  } else {
    reactant.side <- ""
  }
  
  # Build Product Strings
  if (isTruthy(products)) {
    for (i in seq_along(products)) {
      if (i == 1) {
        product.side <- products[i]
      } else {
        product.side <- paste0(product.side, " + ", products[i])
      }
    }
  } else {
    product.side  <- ""
  }
  
  # Build Modifier Strings
  if (isTruthy(modifiers)) {
    mods <- paste0(modifiers, collapse = ", ")
    mods <- paste0("[", mods, "]")
  } else {
    mods <- ""
  }
  
  # Build arrow type
  if (reversible) {
    direction <- "<->"
  } else {
    direction <- "->"
  }
  
  # Build Parameter Versions
  text.pars <- paste0(parameters, collapse = ", ")
  text.pars <- paste0("(", text.pars, ")")
  
  # Build Arrow
  text.arrow <- paste0(mods, direction, text.pars)
  
  # Build Final Reaction
  text.reaction <- paste0(reactant.side,
                          text.arrow,
                          product.side)
  
  
  # BUILD LATEX REACTION________________________________________________________
  # Build reaction strings
  if (isTruthy(reactants)) {
    for (i in seq_along(reactants)) {
      if (i == 1) {
        reactant.side <- Var2Latex(reactants[i])
      } else {
        reactant.side <- paste0(reactant.side, " + ", Var2Latex(reactants[i]))
      }
    }
  } else {
    reactant.side <- ""
  }
  
  # Build Product Strings
  if (prodDegSymbol) {
    product.side <- "\\bigotimes"
  } else if (isTruthy(products)) {
    for (i in seq_along(products)) {
      if (i == 1) {
        product.side <- Var2Latex(products[i])
      } else {
        product.side <- paste0(product.side, " + ", Var2Latex(products[i]))
      }
    }
  } else {
    product.side  <- ""
  }
  
  # Build Modifier Strings
  if (isTruthy(modifiers)) {
    for (i in seq_along(modifiers)) {
      if (i == 1) {
        mods <- Var2Latex(modifiers[i])
      } else {
        mods <- paste0(mods, ", ", Var2Latex(modifiers[i]))
      }
    }
  } else {
    mods <- ""
  }
  
  # Build arrow type
  if (reversible) {
    direction <- "\\xrightleftharpoons"
  } else {
    direction <- "\\xrightarrow"
  }
  
  # Build Parameter Versions
  if (isTruthy(parameters)) {
    for (i in seq_along(parameters)) {
      if (i == 1) {
        pars <- Var2Latex(parameters[i])
      } else {
        pars <- paste0(pars, ", ", Var2Latex(parameters[i]))
      }
    }
  } else {
    pars <- ""
  }
  
  # Build Arrow
  arrow <- paste0(direction,
                  "[", pars, "]",
                  "{", mods, "}")
  
  # Build Final Reaction
  latex.reaction <- paste0(reactant.side,
                           arrow,
                           product.side)
  
  # BUILD MATHJAX REACTION______________________________________________________
  # Build reaction strings
  if (length(reactants) > 0 && isTruthy(reactants)) {
    for (i in seq_along(reactants)) {
      if (i == 1) {
        reactant.side <- Var2MathJ(reactants[i])
      } else {
        if (isTruthy(reactants[i])) {
          reactant.side <- paste0(reactant.side, " + ", Var2MathJ(reactants[i]))
        }
        
      }
    }
  } else {
    reactant.side <- ""
  }
  
  # Build Product Strings
  if (prodDegSymbol) {
    product.side <- "\\bigotimes"
  } else if (length(products) > 0 && isTruthy(products)) {
    for (i in seq_along(products)) {
      if (i == 1) {
        product.side <- Var2MathJ(products[i])
      } else {
        if (isTruthy(products[i])) {
          product.side <- paste0(product.side, " + ", Var2MathJ(products[i]))
        }
      }
    }
  } else {
    product.side  <- ""
  }
  
  # Build Modifier Strings
  if (isTruthy(modifiers)) {
    for (i in seq_along(modifiers)) {
      if (i == 1) {
        mods <- Var2MathJ(modifiers[i])
      } else {
        if (isTruthy(modifiers[i])) {
          mods <- paste0(mods, ", ", Var2MathJ(modifiers[i]))
        }
      }
    }
  } else {
    mods <- ""
  }
  
  # Build arrow type
  if (reversible) {
    direction <- "<->"
  } else {
    direction <- "->"
  }
  
  # Build Parameter Versions
  if (isTruthy(parameters)) {
    for (i in seq_along(parameters)) {
      if (i == 1) {
        pars <- Var2MathJ(parameters[i])
      } else {
        if (isTruthy(parameters[i])) {
          pars <- paste0(pars, ", ", Var2MathJ(parameters[i]))
        }
      }
    }
  } else {
    pars <- ""
  }
  
  
  # Build Arrow
  arrow <- paste0("\\ce{",
                  direction,
                  "[{", pars, "}]",
                  "[{", mods, "}]",
                  "}")
  
  # Build Final Reaction
  mj.reaction <- paste0(reactant.side,
                        arrow,
                        product.side)
  
  out <- list("text" = text.reaction,
              "latex" = latex.reaction,
              "mathjax" = mj.reaction)
}