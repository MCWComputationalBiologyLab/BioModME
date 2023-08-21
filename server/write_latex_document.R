AddBracketsToLatexEqns <- function(eqn){
  split.eqn <- trimws(str_split(eqn, ""))[[1]]
  nletters <- length(split.eqn)
  idx.left.bracket <- vector()
  idx.right.bracket.replace <- vector()
  idx.right.bracket.add <- vector()
  for (i in seq(nletters)) {
    if (split.eqn(i) == "*") {
      idx.left.bracket <- c(idx.left.bracket, i)
      for (j in seq(i:nletters)) {
        if (split.eqn[j] == "*")
        if (split.eqn[j] %in% c("*", "+", "-", "/")) {
          
        }
      }
    }
  }
}

IO2Latex <- function(eqn) {
  split.eqn <- trimws(str_split(eqn, "\\*")[[1]])
  print(split.eqn)
  out <- vector()
  for (var in split.eqn) {
    out <- c(out, Var2Latex(var))
  }
  print(out)
  out <- paste(out, collapse = "*")
  return(out)
}

massActionEqn2Latex <- function(eqn) {
  split.eqn <- trimws(str_split(eqn, "")[[1]])
  count = 0
  lhs <- c()
  rhs <- c()
  two.sided = FALSE
  for (letter in split.eqn) {
    count = count + 1
    if (count != 1) { #skip check on first var incase first var is negative
      ifelse(two.sided, rhs <- c(rhs, letter), lhs <- c(lhs, letter))
      if (letter == "+" | letter == "-") {
        two.sided = TRUE
      }
    } else {
      skipped.letter <- letter
    } 
  }
  lhs <- paste0(c(skipped.letter, lhs), "", collapse = "")
  rhs <- paste0(rhs, "", collapse = "")
  #convert vars to latex form
  if (two.sided) {
    # Prepping left hand side of the equation
    latex.vars.lhs <- c()
    lhs.split <- str_split(lhs, "\\*")[[1]]
    for (var in lhs.split) {
      latex.vars.lhs <- c(latex.vars.lhs, Var2Latex(var, mathMode = FALSE))
    }
    latex.vars.lhs <- paste0("\\text{", latex.vars.lhs, "}")
    for (i in seq(length(latex.vars.lhs))) {
      if (i == 1) {
        eqn.out.lhs <- latex.vars.lhs[i] 
      } else {
        eqn.out.lhs <- paste0(eqn.out.lhs, "*", latex.vars.lhs[i])
      }
    }
    # Prepping RHS of mass action equation
    latex.vars.rhs <- c()
    rhs.split <- str_split(rhs, "\\*")[[1]]
    for (var in rhs.split) {
      print(var)
      latex.vars.rhs <- c(latex.vars.rhs, Var2Latex(var, mathMode = FALSE))
    }
    latex.vars.rhs <- paste0("\\text{", latex.vars.rhs, "}")
    for (i in seq(length(latex.vars.rhs))) {
      if (i == 1) {
        eqn.out.rhs <- latex.vars.rhs[i] 
      } else {
        eqn.out.rhs <- paste0(eqn.out.rhs, "*", latex.vars.rhs[i])
      }
    }
    eqn.out <- paste0(eqn.out.lhs, eqn.out.rhs)
    print(eqn.out)
  } else {
    latex.vars.lhs <- c()
    lhs.split <- str_split(lhs, "\\*")[[1]]
    for (var in lhs.split) {
      latex.vars.lhs <- c(latex.vars.lhs, Var2Latex(var, mathMode = FALSE))
    }
    latex.vars.lhs <- paste0("\\text{", latex.vars.lhs, "}")
    for (i in seq(length(latex.vars.lhs))) {
      if (i == 1) {
        eqn.out.lhs <- latex.vars.lhs[i] 
      } else {
        eqn.out.lhs <- paste0(eqn.out.lhs, "*", latex.vars.lhs[i])
      }
    }
    print(eqn.out.lhs)
    eqn.out <- eqn.out.lhs
  }
  return(eqn.out)
}

enzymeEqn2Latex <- function(eqn) {
  #check if equations starts with a minus
  minus.at.start = startsWith(eqn, "-")
  if (minus.at.start) {
    eqn <- substring(eqn, 2) #remove minus sign
  }
  #split by division first
  split.fraction <- trimws(str_split(eqn, "/")[[1]])
  top.of.fraction <- split.fraction[1]
  bottom.of.fraction <- split.fraction[2]
  #remove parenthesis from phrase (remove 1st and last letter)
  split.bottom <- str_split(bottom.of.fraction, "")[[1]]
  split.bottom <- split.bottom[2:(length(split.bottom) - 1)]
  bottom.of.fraction <- paste0(split.bottom, "", collapse = "")
  
  #convert top of equation
  latex.vars.top <- c()
  top.split <- str_split(top.of.fraction, "\\*")[[1]]
  latex.vars.top <- sapply(top.split, Var2Latex, mathMode = FALSE, USE.NAMES = FALSE)
  latex.vars.top <- paste0("\\text{", latex.vars.top, "}")

  for (i in seq(length(latex.vars.top))) {
    if (i == 1) {
      eqn.out.top <- latex.vars.top[i] 
    } else {
      eqn.out.top <- paste0(eqn.out.top, "*", latex.vars.top[i])
    }
  }
  
  #convert bottom of equation
  bottom.split <- str_split(bottom.of.fraction, "\\+")[[1]]
  latex.vars.bottom <- sapply(bottom.split, Var2Latex, mathMode = FALSE, USE.NAMES = FALSE)
  latex.vars.bottom <- paste0("\\text{", latex.vars.bottom, "}")
  eqn.out.bottom <- paste0(latex.vars.bottom[1], "+", latex.vars.bottom[2])
  
  #final equation
  if (minus.at.start) {
    eqn.out <- trimws(paste0("-\\frac{", eqn.out.top, "}{", eqn.out.bottom, "}"))
  } else {
    eqn.out <- trimws(paste0("\\frac{", eqn.out.top, "}{", eqn.out.bottom, "}"))
  }
  print(eqn.out)
  return(eqn.out)
}

#TODO: Determine if parameter needs $param$ or not based on if itll
#be in math mode of not
Var2Latex <- function(variable, mathMode = TRUE, noDollarSign = TRUE) {
  # Takes input variable and changes it to a pretty latex form for latex reader
  # Inputs:
  #   @variable - string variable to be changed to nice formating
  
  # Output: Variable in latex form
  # Example: "My_var" --> "My_{var}
  
  split.var <- str_split(variable, "")[[1]]
  length.of.var <- length(split.var)
  has.underscore = FALSE
  count = 0
  for (letter in split.var) {
    count = count + 1
    if (letter == "_" & count != length.of.var & count != 1) { #prevent splitting if first/last letter is _
      idx <- count
      has.underscore = TRUE
      break
    }
  }
  if (has.underscore) {
    if (mathMode) {
      if (noDollarSign) {
        before <- paste0(split.var[1:idx], collapse = "")
        after <- paste0(split.var[(idx + 1):length.of.var], collapse = "")
        new.var <- paste0(before, "{", after, "}")
      } else{
        before <- paste0(split.var[1:idx], collapse = "")
        before <- paste0("$", before)
        after <- paste0(split.var[(idx + 1):length.of.var], collapse = "")
        new.var <- paste0(before, "{", after, "}$")
      }
    } else {
      #if underscores are to be used in a text phrase
      before <- paste0(split.var[1:(idx - 1)], collapse = "")
      after <- paste0(split.var[(idx + 1):length.of.var], collapse = "")
      new.var <- paste0(before, "\\textsubscript{", after, "}")
    }
  }
  else {
    new.var <- variable
  }
  out <- new.var
  return(out)
}

VarToLatexForComment <- function(string) {
  # Takes sentence and adds appropriate underscores to it
  # Input 
  #   @string - string phrase to be wrapped
  # Output
  #   -string phase with appropriate underscores
  words.in.string <- str_split(string, " ")[[1]]
  num.words <- length(words.in.string)
  new.string <- c()
  for (word in words.in.string) {
    new.word <- Var2Latex(word, mathMode = FALSE)
    new.string <- c(new.string, new.word)
  }
  new.string <- paste0(new.string, collapse = " ")
  return(new.string)
}

WrapInText <- function(string){
  #wraps string in \text{}; useful for words in equations otherwise 
  #they are too spaced
  #note using text would need mathMode = FALSE
  # Input 
  #   @string - string phrase to be wrapped
  # Output
  #   -string of string phase wrapped in latex text
  out <- paste0("\\text{", string, "}")
  return(out)
}
 

GenerateDifferentialEquations <- function(differentialEqns) {
  #outputs a table for parameters in latex form for the user
  #inputs:
  # @differentialEqns - vector of strings of differential eqns
  #Outputs:
  # @out - string containing latex version of differential eqns
  #Example:
   # differntialEqns = c("d(CDK4)/dt = -k_f1*CCND*CDK4+k_r1*CCND.CDK4",
   #                     "d(CDC25A)/dt = -kf_p2*CCNE.CDK2*CDC25A+kr_p2*CDC14*
   #                                  CDC25Ap-kcat_1*SCF*CDC25A/(Km_1+CDC25A)"
   #                     )
   # out = c("\\frac{d(CDK4)}{dt} = -k_{f1}*CCND*CDK4 + k_{r1}*CCND.CDK4"
   #        \\frac{d(CDC25A)}{dt} = -kf_{p2}*CCNE.CDK2*CDC25A+kr_{p2}*CDC14*
   #                        CDC25Ap-\\frac{kcat_1*SCF*CDC25A}/{(Km_1+CDC25A)}
   #        )
  
  for (eqn in differntialEqns) {
    #make LHS of equation into fraction
    eqn.split <- trimws(str_split(eqn, "=")[[1]])
    lhs <- eqn.split[1]
    #split into its fractions
    lhs.split <- str_split(lhs, "/")[[1]]
    lhs.out <- paste0("\\frac{", lhs.split[1], "}", "{", lhs.split[2], "}")
    print(lhs.out)
    
    #Make RHS of equation into fraction
    #split into chunks
  }
} 

GenerateParameterTable <- function(parameters, values, descriptions) {
  #outputs a table for parameters in latex form for the user
  #inputs:
  # @parameters - vector of parameter names
  # @values - vector of parameter values corresponding to their names
  # @descriptions - vector of param descriptions corresponding to their names
  #Outputs:
  # @out - string containing latex table for parameter information
  num.parameters <- length(parameters)
  
  out <- "\n \\section*{\\underline{Parameters}}\n"
  out <- paste0(out, "\\begin{longtable}{lcl} \n")
  out <- paste0(out, 
                "Parameter & Value & \\multicolumn{1}{c}{Description} 
                \\\\ \\hline \n \\endhead \n")
  
  for (i in seq(num.parameters)) {
    if (i != num.parameters) {
      line.to.add <-
        paste0(Var2Latex(parameters[i], noDollarSign = FALSE),
               " & ",
               values[i],
               " & ",
               VarToLatexForComment(descriptions[i]),
               "\\\\ \n")
    } else {
      line.to.add <-
        paste0(Var2Latex(parameters[i], noDollarSign = FALSE),
               " & ",
               values[i],
               " & ",
               VarToLatexForComment(descriptions[i]),
               "\n"
        )
    }
    out <- paste0(out, line.to.add)
  }
  out <- paste0(out, "\\end{longtable} \n \\newpage")
}

OutputSideOfEquation <- function(coefs, vars){
  # Computes string that is equivalent to one side of chemical equation
  #
  # Args:
  #   coefs: vector containing coefficients of species in eqn
  #   vars: vector containing the species in the eqn
  #   
  #   Ex: Eqn: A + 2B, coefs = [1,2], vars = [A,B]
  #
  # Returns:
  #   string version of the equation
  
  out <- ""
  #generate eqn when there is only one species
  if (length(coefs) == 1) {
    if (coefs != "1") { #only want to show coefs if they aren't one
      out <- paste0(out, 
                    coefs, 
                    "*", 
                    WrapInText(Var2Latex(vars, mathMode = FALSE)))
    }
    else{
      out <- paste0(out, 
                    WrapInText(Var2Latex(vars, mathMode = FALSE)))
    }
  }
  else{#add coefs together when there are multiple
    for (i in seq(length(coefs))) {
      if (i == length(coefs)) { #this is the last vars to add so no "+"
        if (coefs[i] != "1") {
          out <- paste0(out, 
                        coefs[i], 
                        "*", 
                        WrapInText(Var2Latex(vars[i], mathMode = FALSE)))
        }else{
          out <- paste0(out, 
                        WrapInText(Var2Latex(vars[i], mathMode = FALSE)))
        }
      }else{#these should all have a plus after them
        if (coefs[i] != "1") {
          out <- paste0(out, 
                        coefs[i], 
                        "*", 
                        WrapInText(Var2Latex(vars[i], mathMode = FALSE)), 
                        " + ")
        }else{
          out <- paste0(out[i], 
                        WrapInText(Var2Latex(vars[i], mathMode = FALSE)), 
                        " + ")
        }
      }
    }
  }
  return(out)
}

OutputArrowType <- function(eqnType, arrowType, kr, kf, 
                            frBool = FALSE, #if forward regulator in chem rxn
                            frRC = NULL, #forward regulator rate constant
                            frSpecies = NULL, #forward regulator species
                            rrBool = FALSE, #if reverse regulator in chem rxn
                            rrRC = NULL, #reverse regulator rate constant
                            rrSpecies = NULL){ #reverse regulator species 
  # determines arrow output type for in latex form
  #
  # Args:
  #   eqnType: Type of equation such as enzyme_rxn or chem_rxn
  #   arrowType: "forward_only" or "both_directions" to generate the eqn arrow
  #   kr: rates that will appear on bottom of arrow in latex
  #   kf: rates that will appear on top of arrow in latex
  #
  # Returns:
  #   string containing latex version for the equation arrow
  frBool = as.logical(frBool)
  rrBool = as.logical(rrBool)
  if (eqnType == "enzyme_rxn") {
    if (arrowType == "forward_only") {
      out <- paste0("\\xrightleftharpoons", 
                    "[", Var2Latex(kr), "]", 
                    "{", Var2Latex(kf), "}")
    }else if (arrowType == "both_directions") {
      out <- paste0("\\xrightleftharpoons", 
                    "[", Var2Latex(kr), "]", 
                    "{", Var2Latex(kf), "}")
    }
  }
  else{
    if (arrowType == "forward_only") {
      if (frBool) {
        out <-
          paste0(
            "\\xrightarrow{",
            "k_f",
            "}"
          )
      }
      else {
        out <- paste0("\\xrightarrow{", Var2Latex(kf), "}")
      }
      
    }else if (arrowType == "both_directions") {
      if (frBool & rrBool) {
        out <-
          paste0(
            "\\xrightleftharpoons",
            "[",
            "k_r",
            "]",
            "{",
            "k_f",
            "}"
          )
      } else if (frBool) {
        out <-
          paste0("\\xrightleftharpoons",
                 "[",
                 Var2Latex(kr),
                 "]",
                 "{",
                 "k_f",
                 "}")
      } else if (rrBool) {
        out <-
          paste0("\\xrightleftharpoons",
                 "[",
                 "k_r",
                 "]",
                 "{",
                 Var2Latex(kf),
                 "}")
      } else {
        out <-
          paste0("\\xrightleftharpoons",
                 "[",
                 Var2Latex(kr),
                 "]",
                 "{",
                 Var2Latex(kf),
                 "}")
      }
    }
  }
}
printEquationDescription <- function(eqnDescription) {
  out <- paste0(eqnDescription, "\n")
  return(out)
}
PrintEquationType <- function(eqnType, FRbool, RRbool) {
  print(FRbool)
  print(RRbool)
  if (eqnType == "chem_rxn") {
    if (FRbool & RRbool) {
      out <- "Forward and Reverse Regulation with Mass Action"
    } else if (FRbool) {
      out <- "Forward Regulation with Mass Action"
    } else if (RRbool) {
      out <- "Reverse Regulation with Mass Action"
    } else {
      out <- "Mass Action"
    }
  }
  else if (eqnType == "enzyme_rxn") {
    out <- "Enzyme Reaction"
  }
  else {
    out <- "I don't know what happened, check helper file for errors"
  }
  out <- paste0(out, "\n")
  return(out)
}

DifferentialEqnsInModel <- function(diffEquationRV,
                                    compartmentRV) {
  # This function changes normal string diffeqns to their latex version to print
  # Inputs:
  # @diffEquationRV - rv containing all diff equation information
  # @compartmentRV - rv containing all compartment information
  # Outputs:
  # @eqns - full differential equation of model
  #ex 
  # species <- c("a", "b")
  # eqns <- c("k_{f1}*a - k-{r1}*b",
  #           "k_{f1}*a - k-{r1}*b - \frac{V_{max}*a}{K_{m}+a}")
  #out <- DifferentialEqnsInModel (species, eqns)
  # out
  # "\frac{d(a)}{dt} = k_{f1}*a - k-{r1}*b"
  # "\frac{d(b)}{dt} = k_{f1}*a - k-{r1}*b - \frac{V_{max}*a}{K_{m}+a}
  # browser()
  # Extract differential equation information
  species.name <- unname(sapply(diffEquationRV,
                                get,
                                x = "Name"))
  
  Compartment.vol <- unname(sapply(diffEquationRV,
                                   get,
                                   x = "Compartment.vol"))
  
  latex.diffeq   <- unname(sapply(diffEquationRV,
                                  get,
                                  x = "ODES.latex.string"))
  
  out <- "\\section*{\\underline{Differential Equations}}\n"
  out <- paste0(out, "\\begin{align*}\n")
  for (i in seq_along(species.name)) {
    new.eqn <- paste0("&",
                      "\\text{", 
                      Var2Latex(Compartment.vol[i], mathMode = FALSE), 
                      "} ",
                      "\\frac{d(", 
                      "\\text{",
                      Var2Latex(species.name[i], mathMode = FALSE),
                      "}",
                      ")}{dt}=", 
                      latex.diffeq[i],
                      "\\\\\n")
    out <- paste0(out, new.eqn)
  }
  out <- paste0(out, "\\end{align*}\n")
  out <- paste0(out, "\\newpage\n\n")
  print(out)
  return(out)
}

SpeciesInModel <- function(variables, descriptions) {
  #input variables - vector of variables in model
  #input: descriptions - vector of variable descriptions
  out <- "\\section*{\\underline{Variables}}\n"
  out <- paste0(out, "\\begin{enumerate}\n")
  for (i in seq(length(variables))) {
    if (length(str_split(descriptions[i], "")[[1]]) > 0) {
      out <-
        paste0(out,
               "\t\\item ",
               Var2Latex(variables[i], mathMode = FALSE),
               " - ",
               descriptions[i],
               "\n")
    } else {
      out <- paste0(out, 
                    "\t\\item ", 
                    Var2Latex(variables[i], mathMode = FALSE), 
                    "\n")
    }
    
  }
  out <- paste0(out, "\\end{enumerate}\n",  "\\newpage\n\n")
}

RegulatorEquation <- function(regulators, rateConstants, forwardBool = TRUE) {
  out <- "\\begin{equation*}\n"
  rxn <- ifelse(forwardBool, "k_{f} = ", "k_{r} = ")
  out <- paste0(out, rxn)
  
  #split regulators to determine how many there are and properly make equation
  regulators <- trimws(str_split(regulators, " ")[[1]])
  rateConstants <- trimws(str_split(rateConstants, " ")[[1]])
  
  n.var <- length(regulators)
  for (i in seq(n.var)) {
    if (i == 1) {
      out <- paste0(out, 
                    Var2Latex(rateConstants[i]) , 
                    "[", 
                    WrapInText(Var2Latex(regulators[i], mathMode = FALSE)), 
                    "]")
    } else {
      out <- paste0(out, 
                    "+", 
                    Var2Latex(rateConstants[i]), 
                    "[", 
                    WrapInText(Var2Latex(regulators[i], mathMode = FALSE)), 
                    "]")
    }
  }
  out <- paste0(out, "\n\\end{equation*}")
  return(out)
}

ReactionsToLatex <- function(latexEqns, 
                             PrintEqnType,
                             printEqnDescription,
                             eqnDescriptions) {
  
  # Writes all eqns out to latex format from the eqn database
  # Args:
  #   eqnInfo: dataframe containing all the eqn information
  #   printEqnType: bool to print the equation type to the output
  #   printeqnDescriptions: bool to print equation description to output
  #   eqnDescriptions: vector of equation descriptions
  #
  # Returns:
  #   string for all latex eqns to be combined with other generated sheets
  #
  n.eqns <- length(latexEqns)
  out <- "\\section*{\\underline{Equations}}\n"
  for (i in seq(n.eqns)) {
    eqn <- ""
    if (printEqnDescription) {
      eqn <- paste0(eqn, eqnDescriptions[i], "\n")
    }
    eqn <- paste0(eqn, "\\begin{equation}\n ")
    eqn <- paste0(eqn, latexEqns[i], " \n ")
    eqn <- paste0(eqn, " \\end{equation}\n ")
    out <- paste0(out, eqn)
    
  }
  out <- paste0(out, "\\newpage\n\n")
  
}

GenerateParameterTable <- function(parameters, values, descriptions) {
  #outputs a table for parameters in latex form for the user
  #inputs:
  # @parameters - vector of parameter names
  # @values - vector of parameter values corresponding to their names
  # @descriptions - vector of param descriptions corresponding to their names
  #Outputs:
  # @out - string containing latex table for parameter information
  num.parameters <- length(parameters)
  
  out <- "\n \\section*{\\underline{Parameters}}\n"
  out <- paste0(out, "\\begin{longtable}{lcl} \n")
  out <- paste0(out, 
                "Parameter & Value & \\multicolumn{1}{c}{Description} 
                \\\\ \\hline \n \\endhead \n")
  
  for (i in seq(num.parameters)) {
    if (i != num.parameters) {
      line.to.add <-
        paste0(Var2Latex(parameters[i], noDollarSign = FALSE),
               " & ",
               values[i],
               " & ",
               VarToLatexForComment(descriptions[i]),
               "\\\\ \n")
    } else {
      line.to.add <-
        paste0(Var2Latex(parameters[i], noDollarSign = FALSE),
               " & ",
               values[i],
               " & ",
               VarToLatexForComment(descriptions[i]),
               "\n"
        )
    }
    out <- paste0(out, line.to.add)
  }
  out <- paste0(out, "\\end{longtable} \n \\newpage")
}

GenerateIOTable <- function(IO.rv) {
  # Create Table of Input/Output Information
  # Input: 
  # @ IO.rv - (list) reactive variable storing all input output information
  print("GenerateIOTable")
  n.IO <- length(IO.rv)
  
  if (n.IO != 0) {
    
    # Extract variables
    type <- unname(sapply(IO.rv,
                          get,
                          x = "Type"))
    
    comp.in <- unname(sapply(IO.rv, 
                             get, 
                             x = "Compartment.In"))
    
    comp.out <- unname(sapply(IO.rv, 
                             get, 
                             x = "Compartment.Out"))
    
    # Create Table
    out <- "\n \\section*{\\underline{Input/Outputs}}\n"
    out <- paste0(out, "\\begin{longtable}{lcl} \n")
    out <- paste0(out, 
                  "Type & Compartment In & Compartment Out 
                \\\\ \\hline \n \\endhead \n")
    
    for (i in seq_along(IO.rv)) {
      if (i != n.IO) {
        line.to.add <- 
          paste0(type[i], " & ",
                 Var2Latex(comp.in[i]), " & ",
                 Var2Latex(comp.out[i]),
                 "\\\\ \n")
      } else {
        line.to.add <- 
          paste0(type[i], " & ",
                 Var2Latex(comp.in[i]), " & ",
                 Var2Latex(comp.out[i]),
                 "\n")
      }
      out <- paste0(out, line.to.add)
    }
    out <- paste0(out, "\\\\ \n \\end{longtable} \n \\newpage")
    
  } else {
    out <- "No Input/Output in Model \n \\newpage"
  }
}

AdditionalEqnsToLatex <- function(additionalEqns){
  # Writes all additional eqns out to latex format from the appropriate vector
  # Args:
  #   additionalEqns: vector of additional eqn strings
  #
  # Returns:
  #   string of all equations parsed into latex format to print

  out <- "\\section*{\\underline{Additional Equations}}\n"
  #find a way to parse equations properly from equations df
  for (eqn in additionalEqns) {
    current.latex.eqn <- "\\begin{equation}\n"
    current.latex.eqn <- paste0(current.latex.eqn, eqn)
    current.latex.eqn <- paste0(current.latex.eqn, "\\end{equation}\n")
    out <- paste0(out, current.latex.eqn)
    }
    out <- paste0(out, "\\newpage\n\n")

  return(out)
}

subsetInputOutput <- function(df){
  index_of_rows_with_var <- vector()
  #search rows of data for the choosen variable and subset them to new df
  for (row in 1:nrow(myModel)) {
    #grabs RHS vars, splits them so they can be searched for wanted variable
    RHS_var <- str_split(myModel[row,3], " ")[[1]] 
    #Does above for LHS variables
    LHS_var <- str_split(myModel[row,5], " ")[[1]] 
    #find indices containing var name
    if (var_to_subset_with %in% RHS_var | var_to_subset_with %in% LHS_var) { 
      #adds index to vector to subset main df later
      index_of_rows_with_var <- c(index_of_rows_with_var, row) 
    }
  }    
  temp_df <- myModel[index_of_rows_with_var, ] #extract var rows
  #print(temp_df)
  return(temp_df)
}



Var2Latex_depreciated <- function(var = NULL, inmathModeBool = TRUE){
  # Converts 
  # Args:
  #   var: variable to change to latex format converting subscripts properly
  #   inmathModeBool: boolean. If true, var takes math mode in latex otherwise
  #                   it uses \textsubscript
  # Returns:
  #   var in latex readable form
  #
  # Ex: var = my_var -> var = my_{var} or my\\textsubscript{var}
  
  # count all "_" in variable
  #if only one then take the start of it to the end of the work and enclose 
  #   in {}
  latex.var = ""
  
  if (!is.null(var)) {
    split.var = strsplit(var, "")[[1]]
    has.underscore = FALSE
    if (inmathModeBool) {
      latex.var = paste0(latex.var, "$")
      for (i in seq(length(split.var))) {
        if (split.var[i] == "_" & !has.underscore) {
          has.underscore = TRUE
          latex.var = paste0(latex.var, split.var[i], "{")
        }else{
          latex.var = paste0(latex.var, split.var[i])
        }
      }
      if (has.underscore) {
        latex.var = paste0(latex.var, "}")
      }
    }else{
      for (i in seq(length(split.var))) {
        if (split.var[i] == "_" & !has.underscore) {
          has.underscore = TRUE
          latex.var = paste0(latex.var, "\\textsubscript{")
        }else{
          latex.var = paste0(latex.var, split.var[i])
        }
      }
      if (has.underscore) {
        latex.var = paste0(latex.var, "}")
      }
    }
  }
  
  return(latex.var)
}

InputOutputToLatex <- function(inputOutputDf){
  # Writes all additional IO out to latex format from the appropriate vector
  # Args:
  #   inputOutputDf: df containing all input/output information
  #
  # Returns:
  #   string of latex form of the inputs and outputs for this equation
  
  out <- "\\section*{\\underline{Inputs \\& Outputs}}\n"
  #find all unique species in inputOutput df
  unique.species <- unique(inputOutputDf[ ,3])
  print(unique.species)
  #for loop for each unique species.  Subset IO df for unique species
  for (var in unique.species) {
    index.unique.var <- vector()
    unique.var.found <- FALSE
    latex.line <- paste0("\\noindent \\underline{", var, "} \\\\\n")
    
    for (row in 1:nrow(inputOutputDf)) {
      if (var == inputOutputDf[row, 3]) {
        index.unique.var <- c(index.unique.var, row)
        unique.var.found <- TRUE
      }
    }
    if (!unique.var.found) {
      latex.line <- paste0(latex.line, "\\tab ", "None \\\\\n\n")
    }else{
      subset.df <- inputOutputDf[index.unique.var, ]
      
      for (new.row in 1:nrow(subset.df)) {
        in.or.out <- subset.df[new.row, 1]
        type <- subset.df[new.row,2]
        rate.constant <- Var2Latex_depreciated(subset.df[new.row,4], FALSE)
        rate.by.species <- subset.df[new.row,5]
        vmax <- ifelse(is.na(subset.df[new.row,6]),
                       subset.df[new.row, 6],
                       Var2Latex_depreciated(subset.df[new.row,6], FALSE))
        kcat <- ifelse(is.na(subset.df[new.row,7]),
                       subset.df[new.row, 7],
                       Var2Latex_depreciated(subset.df[new.row,7], FALSE))
        enzyme <- ifelse(is.na(subset.df[new.row,8]),
                         subset.df[new.row, 8],
                         Var2Latex_depreciated(subset.df[new.row,8], FALSE))
        
        latex.line <- paste0(latex.line, "\\tab ", in.or.out, ": ")
        if (type == "Rate") {
          if (rate.by.species == "FALSE") {
           ifelse(in.or.out == "input",
                  latex.line <-  paste0(latex.line, "Self Synethesis, "),
                  latex.line <-  paste0(latex.line, "Self Degradation, "))
          }
          latex.line <- paste0(latex.line, rate.constant, "\\\\\n")
        }
        else if (type == "Enzyme_Degradation") {
          ifelse(in.or.out == "input",
                 latex.line <- paste0(latex.line, "Enzyme Synthesis, "),
                 latex.line <- paste0(latex.line, "Enzyme Degradation, "))
          if (!is.na(vmax)) {
            latex.line <- paste0(latex.line, "V\\textsubscript{max} = "
                                 , vmax, ", ")
            latex.line <- paste0(latex.line, "K\\textsubscript{m} = ", 
                                 rate.constant, "\\\\\n")
          }else{
            latex.line <- paste0(latex.line, "enzyme = ", enzyme, ", ")
            latex.line <- paste0(latex.line, "k\\textsubscript{cat} = ", 
                                 kcat, ", ")
            latex.line <- paste0(latex.line, "K\\textsubscript{m} = ", 
                                 rate.constant, "\\\\\n")
          }
        }
      }
    }
    out <- paste0(out, latex.line)
  }
  out <- paste0(out, "\n", "\\newpage")
  print(out)
  return(out)
  #run latex loop for each row in subsetted df to create latex strings
  #for(row in 1:nrow())
}


GenerateLatexDocument <- function(latexText){
#GenerateLatexDocument <- function(eqnsLatex){
  # combines eqns, I/O, and additional equations into final latex document
  #
  # Args:
  #   eqnsLatex: string containing all latex eqns
  #   IoLatex: string containing all input/output information in latex form
  #   addEqnsLatex: string containing all additioanl eqns in latex form
  #
  # Returns:
  #   final string eqns for final latex document

  out <- paste0("\\documentclass[12pt]{article}\n",
                "\\usepackage[margin=1in]{geometry}\n",
                "\\usepackage{chemarr}\n",
                "\\usepackage{float}\n",
                "\\usepackage{longtable}\n",
                "\\newcommand\\tab[1][1cm]{\\hspace*{#1}}\n",
                "\\begin{document}\n"
                #"\\tableofcontents\n",
                #"\\newpage\n\n"
                )
  #out <- paste0(out, eqnsLatex)
  out <- paste0(out, latexText)

  out <- paste0(out, "\\end{document}")

  return(out)
}







