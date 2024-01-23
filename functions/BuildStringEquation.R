BuildStringEquation     <- function(reactants, 
                                    products,
                                    modifiers,
                                    parameters,
                                    stoich.reactants,
                                    stoich.products,
                                    reversible = FALSE,
                                    prodDegSymbol = FALSE) {
  # Used for building equations imported from SBML.  This is similar to the 
  # equation used to build custom equations, however, this ignored stoichemetry
  # in building the reactions. 
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
        if (stoich.reactants[i] == 1) {
          reactant.side <- reactants[i]
        } else {
          reactant.side <- paste0(stoich.reactants[i], "*", reactants[i])
        }
      } else {
        if (stoich.reactants[i] == 1) {
          reactant.side <- paste0(reactant.side, " + ", reactants[i])
        } else {
          new.to.add <- paste0(stoich.reactants[i], "*", reactants[i])
          reactant.side <- paste0(reactant.side, " + ", new.to.add)
        }
      }
    }
  } else {
    reactant.side <- ""
  }
  
  # Build Product Strings
  if (isTruthy(products)) {
    for (i in seq_along(products)) {
      
      if (i == 1) {
        if (stoich.products[i] == 1) {
          product.side <- products[i]
        } else {
          product.side <- paste0(stoich.products[i], "*", products[i])
        }
      } else {
        if (stoich.products[i] == 1) {
          product.side <- paste0(product.side, " + ", products[i])
        } else {
          new.to.add <- paste0(stoich.products[i], "*", products[i])
          product.side <- paste0(product.side, " + ", new.to.add)
        }
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
  # Build Reaction Strings in LaTeX
  if (isTruthy(reactants)) {
    for (i in seq_along(reactants)) {
      if (i == 1) {
        if (stoich.reactants[i] == 1) {
          reactant.side <- Var2Latex(reactants[i])
        } else {
          reactant.side <- 
            paste0(stoich.reactants[i], "*", Var2Latex(reactants[i]))
        }
      } else {
        if (stoich.reactants[i] == 1) {
          reactant.side <- paste0(reactant.side, " + ", Var2Latex(reactants[i]))
        } else {
          new.to.add <- 
            paste0(stoich.reactants[i], "*", Var2Latex(reactants[i]))
          reactant.side <- paste0(reactant.side, " + ", new.to.add)
        }
      }
    }
  } else {
    reactant.side <- ""
  }
  
  # Build Product Strings in LaTeX
  if (prodDegSymbol) {
    product.side <- "\\bigotimes"
  } else if (isTruthy(products)) {
    for (i in seq_along(products)) {
      if (i == 1) {
        product.side <- Var2Latex(products[i])
      } else {
        if (stoich.products[i] == 1) {
          product.side <- paste0(product.side, " + ", Var2Latex(products[i]))
        } else {
          new.to.add <- paste0(stoich.products[i], "*", Var2Latex(products[i]))
          product.side <- paste0(product.side, " + ", new.to.add)
        }
      }
    }
  } else {
    product.side <- ""
  }
  
  # Build Product Strings
  if (prodDegSymbol) {
    product.side <- "\\bigotimes"
  } else if (isTruthy(products)) {
    for (i in seq_along(products)) {
      if (i == 1) {
        if (stoich.products[i] == 1) {
          product.side <- Var2Latex(products[i])
        } else {
          product.side <- 
            paste0(stoich.products[i], "*", Var2Latex(products[i]))
        }
      } else {
        if (stoich.products[i] == 1) {
          product.side <- paste0(product.side, " + ", Var2Latex(products[i]))
        } else {
          new.to.add <- paste0(stoich.products[i], "*", Var2Latex(products[i]))
          product.side <- paste0(product.side, " + ", new.to.add)
        }
      }
    }
  } else {
    product.side  <- ""
  }
  
  # Build Modifier Strings in LaTeX
  if (isTruthy(modifiers)) {
    mods <- paste0(sapply(modifiers, Var2Latex), collapse = ", ")
    mods <- paste0("[", mods, "]")
  } else {
    mods <- ""
  }
  
  # Build Arrow Type in LaTeX
  if (reversible) {
    direction <- "\\xrightleftharpoons"
  } else {
    direction <- "\\xrightarrow"
  }
  
  # Build Parameter Versions in LaTeX
  if (isTruthy(parameters)) {
    pars <- paste0(sapply(parameters, Var2Latex), collapse = ", ")
    pars <- paste0("(", pars, ")")
  } else {
    pars <- ""
  }
  
  # Build Arrow in LaTeX
  arrow <- paste0(direction, "[", pars, "]", "{", mods, "}")
  
  # Build Final Reaction in LaTeX
  latex.reaction <- paste0(reactant.side, arrow, product.side)
  
  
  # BUILD MATHJAX REACTION______________________________________________________
  # Build reaction strings
  if (length(reactants) > 0 && isTruthy(reactants)) {
    for (i in seq_along(reactants)) {
      if (i == 1) {
        if (stoich.reactants[i] == 1) {
          reactant.side <- Var2MathJ(reactants[i])
        } else {
          reactant.side <- paste0(stoich.reactants[i], "*", 
                                  Var2MathJ(reactants[i]))
        }
      } else {
        if (isTruthy(reactants[i])) {
          if (stoich.reactants[i] == 1) {
            reactant.side <- paste0(reactant.side, " + ", 
                                    Var2MathJ(reactants[i]))
          } else {
            new.to.add <- paste0(stoich.reactants[i], "*", 
                                 Var2MathJ(reactants[i]))
            reactant.side <- paste0(reactant.side, " + ", new.to.add)
          }
        }
      }
    }
  } else {
    reactant.side <- ""
  }
  
  # Build Product Strings in MathJax
  if (prodDegSymbol) {
    product.side <- "\\bigotimes"
  } else if (length(products) > 0 && isTruthy(products)) {
    for (i in seq_along(products)) {
      if (i == 1) {
        if (stoich.products[i] == 1) {
          product.side <- Var2MathJ(products[i])
        } else {
          product.side <- paste0(stoich.products[i], "*", 
                                 Var2MathJ(products[i]))
        }
      } else {
        if (isTruthy(products[i])) {
          if (stoich.products[i] == 1) {
            product.side <- paste0(product.side, " + ", 
                                   Var2MathJ(products[i]))
          } else {
            new.to.add <- paste0(stoich.products[i], "*", 
                                 Var2MathJ(products[i]))
            product.side <- paste0(product.side, " + ", new.to.add)
          }
        }
      }
    }
  } else {
    product.side <- ""
  }
  
  # Build Modifier Strings in MathJax
  if (isTruthy(modifiers)) {
    mods <- paste0(sapply(modifiers, Var2MathJ), collapse = ", ")
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
  mj.reaction <- paste0("$$",
                        reactant.side,
                        arrow,
                        product.side,
                        "$$")
  
  out <- list("text" = text.reaction,
              "latex" = latex.reaction,
              "mathjax" = mj.reaction)
}