#Functions in this file:
# jPrint
# RenameVarInVector
# Var2Latex



SeparateParameters <- function(oldParams, newParams, allParams) {
  # Want to find which parameters are new from old but separate them from those 
  # found in all.  This is used when editing equations
  
  # Difference in new params (to add)
  diff.add     <- setdiff(newParams, oldParams)
  # Difference in old params (to remove)
  diff.remove  <- setdiff(oldParams, newParams)
  
  # Check with overall list
  add.in.all    <- intersect(diff.add, allParams)
  remove.in.all <- intersect(diff.remove, allParams)
  
  to.add    <- setdiff(diff.add, add.in.all)
  to.remove <- setdiff(diff.remove, remove.in.all)
  to.edit   <- c(intersect(diff.add, add.in.all), 
                 intersect(diff.remove, remove.in.all)
                 )
}

convertBlankToNA <- function(valueToChange) {
  # Check function to convert "" to NA
  if (valueToChange == "") {
    out <- NA
  } else {
    out <- valueToChange
  }
  return(out)
}

withConsoleRedirect <- function(containerId, expr) {
  # Change type="output" to type="message" to catch stderr
  # (messages, warnings, and errors) instead of stdout.
  txt <- capture.output(results <- expr, type = "output")
  removeUI(paste0("#", containerId))
  if (length(txt) > 0) {
    insertUI(paste0("#", containerId), 
             where = "beforeEnd",
             ui = paste0(txt, "\n", collapse = "")
    )
  }
  results
}

PassConsoleOutputToVar <- function(var, expr) {
  # Change type="output" to type="message" to catch stderr
  # (messages, warnings, and errors) instead of stdout.
  txt <- capture.output(results <- expr, type = "output")
  removeUI(paste0("#", containerId))
  if (length(txt) > 0) {
    insertUI(paste0("#", containerId), 
             where = "beforeEnd",
             ui = paste0(txt, "\n", collapse = "")
    )
  }
  results
}




VectorizeListValue <- function(l, value, init.mode = "character") {
  # Takes a list item and creates a vector of its components
  # Example:
  # a <- list("gone" = c(1,2,3,4,4),
  #           "girl" = c("not", "too", "hot"))
  # b <- VectorizeListValue(a, gone, init.mode="numeric")
  #     >>> b = c(1,2,3,4,4)
  out <- vector(mode=init.mode, length=length(l))
  for (i in seq_along(l)) {
    out[i] <- eval(parse(text=paste0("l[[i]]$", value)))
  }
  return(out)
}

Unit_Dict_Convert <- function(dict, unit_term) {
  new_split <- UnitBreak(unit_term)
  converted_vec <- 
    sapply(
      new_split, 
      function(x) ifelse(x %in% names(dict), dict[x], x)
    )
  
  new <- paste0(converted_vec, collapse = "")
  return(new)
}

UnitCompare <- function(unitDescriptor, 
                        unitToCompare,
                        possibleUnitsData,
                        useMol = TRUE) {
  # Take in unit descriptor, break it down and make sure it matches new input
  # Input: 
  #   unitDescriptor - word break down of units (num <div> time)
  #   unitToCompare - units to compare to descriptor (1/min)
  #   possibleUnitsData - RV containing all possible units (units$possible.unit)
  #   useMol - if TRUE, uses count measurements, if FALSE uses MASS
  
  # Unpack Units DataStructure
  possibleTimeUnits   <- possibleUnitsData$Duration
  possibleEnergyUnits <- possibleUnitsData$Energy
  possibleLengthUnits <- possibleUnitsData$Length
  possibleMassUnits   <- possibleUnitsData$Mass
  possibleVolumeUnits <- possibleUnitsData$Volume
  possibleFlowUnits   <- possibleUnitsData$Flow
  possibleConcUnits   <- possibleUnitsData$Count
  
  # Split descriptor
  ud.split   <- strsplit(unitDescriptor, " ")[[1]]
  # browser()
  # Need to split power terms here for calculation
  new.vec <- c()
  for (i in seq_along(ud.split)) {
    if (startsWith(ud.split[i], "<power>")) {
      to.add <- strsplit(ud.split[i], ">")[[1]]
      to.add[1] <- paste0(to.add[1], ">")
    } else {to.add <- ud.split[i]}
    new.vec <- c(new.vec, to.add)
  }
  new.vec <- RemoveFromVector(c("(Mol)", "(mol)", "(MOL)",
                                "(Mass)", "(mass)", "(MASS)"),
                              new.vec)
  ud.split <- new.vec
  
  comp.split <- UnitBreak(unitToCompare)
  is.match <- TRUE
  error.message <- "No Error: Unit Matches Descriptor"
  
  # Check if lengths of splits are the same
  if (length(comp.split) != length(ud.split)) {
    out <- list("is.match" = FALSE,
                "message" = "Size Difference in Inputs")
    return(out)
  }
  
  # Perform analysis/comparison
  comp.i <- 0
  skip = FALSE
  for (i in seq_along(ud.split)) {
    
    element <- ud.split[i]

    # Skips unit descriptor for conc
    if (element == "(Mol)" | element == "(Mass)") {
      skip = TRUE
    } else {
      comp.i <- comp.i + 1
      comp    <- comp.split[comp.i]
    }
    
    # Performs comparison of specific unit element
    if (skip) {
      skip = FALSE
    } else {
      if (startsWith(element, "<power>")) {

        if (comp != "^") {
          is.match <- FALSE
          error.message <- "Exponent Does Not Match up"
          break
        }  else {
          next.element <- qdapRegex::ex_between(ud.split[i+1], "(", ")")[[1]]
          next.comp    <- comp.split[comp.i+1]
          i = i + 1
          if (next.element != next.comp) {
            is.match <- FALSE
            error.message <- "Exponent value changed"
          }
        }
        
      } else if (startsWith(element, "<")) {
        # mathematical operators begin with <, checking if math symbols match
        if (element == "<div>") {
          if (comp != "/") {
            is.match <- FALSE
            error.message <- "Division Does Not Match up"
            break
          }
        } else if (element == "<multiply>") {
          if (comp != "*") {
            is.match <- FALSE
            error.message <- "Division Does Not Match up"
            break
          }
        } else if (element == "<addition>") {
          if (comp != "+") {
            is.match <- FALSE
            error.message <- "Addition Does Not Match up"
            break
          }
        } else if (element == "<subtraction>") {
          if (comp != "-") {
            is.match <- FALSE
            error.message <- "Subtraction Does Not Match up"
            break
          }
        } else if (element == "<group>") {
          if (comp != "(") {
            is.match <- FALSE
            error.message <- "Beginning Parenthesis Does Not Match up"
            break
          }
        } else if (element == "<endgroup>") {
          if (comp != ")") {
            is.match <- FALSE
            error.message <- "End Parenthesis Does Not Match up"
            break
          }
        }
      } else if(element == "num") {
        is.num <- as.numeric(comp)
        if (is.na(is.num)) {
          # Return error because not numeric
          is.match <- FALSE
          error.message <- "Number is not a number"
          break
        }
      } else if (element == "conc") {
        # Check if new term is a concentration term
        # Pull list of concentration terms
        if (!(comp %in% possibleConcUnits)) {
          is.match <- FALSE
          error.message <- paste0("Unit: '", 
                                  comp,
                                  "' not a possible concentration unit. ",
                                  "Possible units are: ",
                                  paste0(possibleConcUnits, collapse = ", ")
          )
          break
        }
      } else if (element == "time") {
        if (!(comp %in% possibleTimeUnits)) {
          is.match <- FALSE
          error.message <- paste0("Unit: '", 
                                  comp,
                                  "' not a possible time unit. ",
                                  "Possible units are: ",
                                  paste0(possibleTimeUnits, collapse = ", ")
          )
          break
        }
      } else if (element == "volume") {
        if (!(comp %in% possibleVolumeUnits)) {
          is.match <- FALSE
          error.message <- paste0(
                            "Unit: '", 
                            comp,
                            "' not a possible volume unit. ",
                            "Possible units are: ",
                            paste0(possibleVolumeUnits, collapse = ", ")
                           )
        }
      }
      
    }
  }
  
  out <- list("is.match" = is.match,
              "message" = error.message)
  return(out)
}

UnitBreak <- function(unitFxn,
                      splitExponents = TRUE) {
  
  # Split Parenthesis
  break.terms <- c("(", ")")
  group.terms <- SplitOnValue(unitFxn, break.terms)
  
  # Split on mathematical operators
  operator.terms <- c()
  break.terms <- c("/", "+", "-", "*")
  for (term in group.terms) {
    operator.terms <- c(operator.terms, SplitOnValue(term, break.terms))
  }
  # split.terms <- SplitOnValue(unitFxn, break.terms)

  # Further split terms by powers
  out <- c()
  if (splitExponents) {
    for (term in operator.terms) {
      if ("^" %in% S2V(term)) {
        term.out <- SplitOnValue(term, "^")
        out <- c(out, term.out)
      } else {
        out <- c(out, term)
      }
    }
  } else {
    out <- operator.terms
  }
  
  return(out)
}

S2V <- function(string) {
  # Quick way to split a string to a single term vector
  vec <- strsplit(string, "")[[1]]
  return(vec)
}

SplitOnValue <- function(string, break.terms) {
  # Split string on break.terms but retain the break.terms
  
  split.terms <- c()
  running.terms <- c()
  for (i in seq(nchar(string))) {
    val <- strsplit(string, "",)[[1]][i]
    if (val %in% break.terms) {
      split.terms <- c(split.terms, paste(running.terms, collapse = ""), val)
      running.terms <- c()
    } else if (i == nchar(string)) {
      running.terms <- c(running.terms, val)
      split.terms <- c(split.terms, paste(running.terms, collapse = ""))
    } else {
      running.terms <- c(running.terms, val)
    }
  }
  return(split.terms)
}


UnitConversion <- function(unitDescriptor,
                           previousUnits,
                           newUnits,
                           unitValue) {
  # Take in unit descriptor, break it down and make sure it matches new input
  # Input: 
  #   unitDescriptor - word break down of units (num <div> time)
  #   previousUnits - Units before conversion
  #   newUnits -Units being converted to
  #   unitValue - value of units at previous units
  
  # Split descriptor
  ud.split   <- strsplit(unitDescriptor, " ")[[1]]
  # Need to split power terms here for calculation
  new.vec <- c()
  for (i in seq_along(ud.split)) {
    if (startsWith(ud.split[i], "<power>")) {
      to.add <- strsplit(ud.split[i], ">")[[1]]
      to.add[1] <- paste0(to.add[1], ">")
    } else {to.add <- ud.split[i]}
    new.vec <- c(new.vec, to.add)
  }
  # Remove concentration terms (Mol)/(Mass)
  new.vec <- RemoveFromVector(c("(Mol)", "(Mass)", "(mol)", "(mass)"), new.vec)
  ud.split <- new.vec
  prev.units <- UnitBreak(previousUnits)
  new.units  <- UnitBreak(newUnits)
  unit.terms <- c("time", "conc", "volume")
  
  conversion.val    <- 1
  next.term.div     <- FALSE
  in.group          <- FALSE
  group.convs       <- c()
  power.level       <- 1
  group.end.trigger <- FALSE
  # Conversions for after div and power, have to account for groups
  
  # Basic
  # Perform analysis/comparison
  for (i in seq_along(ud.split)) {
    
    ud   <- ud.split[i]
    prev <- prev.units[i]
    new  <- new.units[i]
    
    if (ud == "<div>") {
      next.term.div = TRUE
    } else if (ud == "<group>") {
      in.group = TRUE
    }
    
    if (ud %in% unit.terms){
      # Check if the term is raised to a power (ignore if last term)
      if (i != length(ud.split)) {
        if (startsWith(ud.split[i+1], "<power>")) {
          power.level <- as.numeric(
            qdapRegex::ex_between(ud.split[i+2], "(", ")")[[1]]
            )
        } else {power.level <- 1}
      } else {power.level <- 1}
      
      # Do group math
      i.conversion.val <- conv_unit(1, prev, new)
      i.conversion.val <- i.conversion.val ^ power.level
      if (in.group) {
        group.convs <- c(group.convs, i.conversion.val)
        
        # Check if group ends - first check if power statement next
        if (i != length(ud.split)-1) {
          if (startsWith(ud.split[i+1], "<power>")) {
            if (ud.split[i+3] == "<endgroup>") {
              group.end.trigger <- TRUE
            }
          }
        } else if (i != length(ud.split)) {
          if (ud.split[i+1] == "<endgroup>") {
            group.end.trigger <- TRUE
          }
        }
        
        # Perform group calculations
        if (group.end.trigger) {
          in.group <- FALSE
          group.end.trigger <- FALSE
          i.conversion.val <- prod(group.convs)
          if (next.term.div) {
            i.conversion.val <- 1/i.conversion.val
            next.term.div <- FALSE
          }
          conversion.val <- conversion.val * i.conversion.val
        }
      } else {
        if (next.term.div) {
          i.conversion.val <- 1/i.conversion.val
          next.term.div <- FALSE
        }
        conversion.val <- conversion.val * i.conversion.val
      }
      
    }
  }
  
  new.val <- unitValue * conversion.val
  
  return(new.val)
}

FindId <- function(varName) {
  # Searches Id database to find ID corresponding to name
  if (!(is.na(varName) | is.null(varName))) {
    idx <- which(rv.ID$id.df[,2] %in% varName)
    var.id <- rv.ID$id.df[idx, 1]
  } else {
    var.id <- NA
  }
  
  return(var.id)
}


regulatorToRate <- function(regulators, rateConstants) {
  #break values from space separated string to vector
  regulators <- str_split(regulators, ", ")[[1]]
  rateConstants <- str_split(rateConstants, ", ")[[1]]
  
  numRegulators <- length(regulators)
  eqnOut <- c()
  for (i in seq(numRegulators)) { #add each regulator equation to a list (regulator*rateConstant)
    eqnForRegulator <- paste0(rateConstants[i], "*", regulators[i])
    eqnOut <- c(eqnOut, eqnForRegulator)
  }
  out <- paste(eqnOut, collapse = "+")
  if (numRegulators > 1) {
    out <- paste0("(", out, ")")
  }
  #out <- paste0("(", out, ")")
  return(out)
}

regulatorToRateLatex <- function(regulators, rateConstants) {
  #break values from space separated string to vector
  regulators <- str_split(regulators, ", ")[[1]]
  rateConstants <- str_split(rateConstants, ", ")[[1]]
  
  numRegulators <- length(regulators)
  eqnOut <- c()
  for (i in seq(numRegulators)) { #add each regulator equation to a list (regulator*rateConstant)
    eqnForRegulator <- paste0(Var2Latex(rateConstants[i]), "*", 
                              Var2Latex(regulators[i]))
    eqnOut <- c(eqnOut, eqnForRegulator)
  }
  out <- paste(eqnOut, collapse = "+")
  if (numRegulators > 1) {
    out <- paste0("(", out, ")")
  }
  #out <- paste0("(", out, ")")
  return(out)
}




# Function to extract non-numerical and non-mathematical operators from an expression
extract_operators <- function(expr_string) {
  # Takes a expression string and extracts all mathematical operators
  # Input
  #   @expr_string - string representing rexpression to be broken up
  # Output
  #   @variables - vector of extracted terms
  #
  # Example:
  # > expr_string <- "3 * x2 + 2 - y_5^2 / z"
  # > extract_operators(expr_string)
  # > [1] "*" "+" "-" "^" "/"
  
  to.ignore <- c(".", "_")
  
  # Define the regular expression pattern to match operators
  operator_pattern <- "[^[:alnum:]\\s.]"
  
  # Extract all matches of the pattern from the expression string
  operators <- regmatches(expr_string, 
                          gregexpr(operator_pattern, 
                                   expr_string, 
                                   perl = TRUE))[[1]]
  
  # Remove operators we don't care about
  operators <- operators[!operators %in% to.ignore]
  
  return(operators)
}

extract_variables <- function(expr_string) {
  # Takes a expression string and extracts all variables that aren't numbers or
  # mathematical operators
  # Input
  #   @expr_string - string representing rexpression to be broken up
  # Output
  #   @variables - vector of extracted terms
  #
  # Example:
  # > expr_string <- "3 * x2 + 2 - y_5^2 / z"
  # > extract_variables(expr_string)a
  # > [1] "x2"  "y_5" "z"
  
  # Define the regular expression pattern to match variables
  # variable_pattern <- "\\b[a-zA-Z_][a-zA-Z0-9_.]*\\b"
  variable_pattern <- "\\b[a-zA-Z0-9_][a-zA-Z0-9_.]*\\b"
  
  # Extract all matches of the pattern from the expression string
  variables <- regmatches(expr_string, 
                          gregexpr(variable_pattern, 
                                   expr_string, 
                                   perl = TRUE))[[1]]
  
  # Remove single numbers that would appear
  variables <- variables[!grepl("^\\d+$", variables)]
  
  # Remove Duplicates
  variables <- unique(variables)

  return(variables)
}

find_legal_math_terms <- function(terms) {
  # This function is meant to define and extract mathematical terms that can 
  # be used in an expression and remove them from possible processing terms
  # These include items like sin, tan, cos, log.
  # 
  # Inputs:
  #   @terms - vector of terms to be searched
  # Outputs
  #   @out$math.terms - vector of legal math terms found in expression
  #   @out$vars - vector of terms to be parsed later that aren't math
  
  legal.terms <- c("abs",
                   "sqrt",
                   "exp",
                   "log",
                   "log10",
                   "log2",
                   "log1p",
                   "cos",
                   "cosh",
                   "sin",
                   "sinh",
                   "tan",
                   "tanh",
                   "acos",
                   "acosh",
                   "asin",
                   "asinh",
                   "atan",
                   "atanh")
  
  math.terms <- terms[terms %in% legal.terms]
  exp.vars   <- terms[!terms %in% legal.terms]
  
  out <- list("math.terms" = math.terms,
              "vars" = exp.vars)
  
  return(out)
}

check_invalid_terms <- function(terms) {
  # Takes a vector of strings and determines if they are valid variables 
  # Inputs:
  # @ terms - vector of strings to be checked for validity.
  # Outputs:
  #   @out$valid.terms - Vector of terms passing validity check
  #   @out$invalid.terms - Vector of terms failing validity check
  #   @out$error.messages - Vector of error messages relating to invalid terms
  
  #Initialize list
  invalid.terms  <- c()
  valid.terms    <- c()
  message.vector <- c()
  
  # Cycle through terms checking cascade of validity statments
  for (i in seq_along(terms)) {
    is.valid <- TRUE
    message  <- NA
    first.letter <- substr(terms[i], 1, 1)
    
    #Checks if variable has first letter has number 
    if (grepl("^([0-9])", first.letter)) {
      is.valid <- FALSE
      message <- paste0(terms[i], ": cannot start with number")
    }
    else if (grepl("[^[:alnum:]_.]", terms[i])) {
      #regrex expression checks if values contains alpha numeric char, _, and .
      is.valid <- FALSE
      message <- paste0(terms[i], ": cannot contain special characters other than
                      . and _")
    }
    else if (grepl("^([[:punct:]])", terms[i])) {
      # Check that it does not start with 
      is.valid <- FALSE
      message <- paste0(terms[i], 
                        ": variable cannot start with special character")
    }
    
    if (is.valid) {
      valid.terms <- c(valid.terms, terms[i])
    } else {
      invalid.terms <- c(invalid.terms, terms[i])
      message.vector <- c(message.vector, message)
    }
  }
  
  out <- list("valid.terms" = valid.terms,
              "invalid.terms" = invalid.terms,
              "error.messages" = message.vector)
  
  return(out)
}

parse_string_expression <- function(expr_string) {
  
  # Pull Variables
  vars <- extract_variables(expr_string)
  
  # Pull Operators
  operators <- extract_operators(expr_string)
  
  # Pull Mathematical terms
  sort.vars <- find_legal_math_terms(vars)
  vars <- sort.vars$vars
  math.terms <- sort.vars$math.terms
  
  # Separate into valid, invalid terms
  validy        <- check_invalid_terms(vars)
  valid.terms   <- validy$valid.terms
  invalid.terms <- validy$invalid.terms
  err.messages  <- validy$error.messages
  
  out <- list("valid.terms" = valid.terms,
              "invalid.terms" = invalid.terms,
              "invalid.messages" = err.messages,
              "all.vars" = vars,
              "operators" = operators)
  
  # Return, valid, invalid, all var, operators, mathematical terms
}

determineFraction <- function(string_input) {
  delimiters <- "(?=[+\\-*/(){}])"
  # Define the operators
  # operators <- c("+", "-", "*", "/", "(", ")", "{", "}")
  
  # Create the regular expression pattern
  # delimiters <- paste0("[", paste0("\\", operators, collapse = ""), "]")
  all.terms <- trimws(
    strsplit(string_input, delimiters, perl = TRUE)[[1]], which = "both")
  
  frac_indices <- which(all.terms == "/")
  # Determin what terms belong in the fraction
  
  # If no fraction terms found we just keep original phase
  if (length(frac_indices) == 0) {
    new.expression <- paste0(all.terms, collapse = "")
  }
  
  # Case, idx before is end parenthesis
  count = 0
  while(length(frac_indices > 0)) {
    count <- count + 1
    if (count > 3) {break}
    idx <- frac_indices[1]
    top.par.remove <- FALSE
    bot.par.remove <- FALSE
    in.top.parenthesis <- FALSE
    in.bot.parenthesis <- FALSE
    # Determine top fraction 
    frac.top.start.idx <- 1
    frac.top.stop.idx <- idx - 1
    if (all.terms[idx-1] == ")") {
      frac.top.stop.idx <- idx - 2
      for (j in seq(idx-1, 1)) {
        if (all.terms[j] == "(") {
          frac.top.start.idx <- j + 1
          if (j != 1) {
            before.frac.idx <- j - 1
          } else {
            before.frac.idx <- j 
          }
          
          
          # Find idx of start parenthesis
          top.par.remove <- TRUE
          top.par.idx <- j
          break
        }
      }
    } else {
      # Case: No parenthesis, search for either beginning or "+" or "-"
      for (j in seq(idx-1, 1)) {
        # Check if at beginning
        if (all.terms[j] == "(") {
          frac.top.start.idx = j + 1
          before.frac.idx <- j
          in.top.parenthesis <- TRUE
          
          break
        }
        else if (j == 1) {
          frac.top.start.idx <- j
          before.frac.idx <- j
        }
        else if (all.terms[j] == "+" || 
                 all.terms[j] == "-" || 
                 all.terms[j] == "{" ) {
          frac.top.start.idx = j + 1
          before.frac.idx <- j
          break
        }
      }
    }
    
    # Determine Bottom Fraction ------------------------------------------------
    # Cases: Parenthesis after fraction
    end.par.idx <- length(all.terms)
    frac.bot.start.idx <- idx + 1
    frac.bot.stop.idx  <- length(all.terms)
    if (all.terms[idx+1] == "(") {
      frac.bot.start.idx <- idx + 2
      # Search for corresponding )
      for(j in seq(idx+1, length(all.terms))) {
        if (all.terms[j] == ")") {
          frac.bot.stop.idx = j-1
          if (j != length(all.terms)) {
            # if paraentheiss is not the last term continue after parenthesis
            after.frac.idx <- j + 1
          } else {
            # End continuation before parenthesis (logic will skip)
            after.frac.idx <- j 
          }
          
          # Find idx of end par
          bot.par.remove <- TRUE
          bot.par.idx <- j
          break
        }
      }
    } else {
      for (j in seq(idx+1, length(all.terms))) {
        if (all.terms[j] == ")") {
          frac.bot.stop.idx = j-1
          after.frac.idx <- j
          in.bot.parenthesis <- TRUE
        }
        else if (j == length(all.terms)) {
          frac.bot.stop.idx = j
          after.frac.idx <- j
        }
        else if (all.terms[j] == "+" || 
                 all.terms[j] == "-" ||
                 all.terms[j] == "}") {
          
          frac.bot.stop.idx = j-1
          after.frac.idx <- j
          break
        }
      }
    }
    
    # Piece Fraction Together --------------------------------------------------
    # Pop index that was used
    frac_indices <- frac_indices[-1]
    
    # Build new expression for all terms
    # top.term    <- all.terms[frac.top.start.idx:frac.top.stop.idx]
    # bottom.term <- all.terms[frac.bot.start.idx:frac.bot.stop.idx]
    top.term <- paste0(all.terms[frac.top.start.idx:frac.top.stop.idx],
                       collapse = "")
    bottom.term <- paste0(all.terms[frac.bot.start.idx:frac.bot.stop.idx],
                          collapse = "")
    
    # if (inside.parenthesis) {
    #   
    # }
    if (before.frac.idx != 1) {
      before.frac <- paste0(all.terms[1:before.frac.idx],
                            collapse = "")
    } 
    else if (before.frac.idx == 1 && in.top.parenthesis) {
      before.frac <- "("
    }
    else {before.frac <- ""}
    
    
    if (after.frac.idx != length(all.terms)) {
      after.frac <- paste0(all.terms[(after.frac.idx):length(all.terms)],
                           collapse = "")
    }
    else if (after.frac.idx == length(all.terms) && in.bot.parenthesis) {
      after.frac <- ")"
    }
    else {after.frac <- ""}
    
    my.frac <- paste0("MathJaxFrac{",
                      top.term,
                      "}{",
                      bottom.term,
                      "}")
    
    new.expression <- paste0(before.frac, my.frac, after.frac)
    all.terms <- trimws(
      strsplit(new.expression, delimiters, perl = TRUE)[[1]], which = "both")
    frac_indices <- which(all.terms == "/")
  }
  
  new.expression <- str_replace_all(new.expression, "MathJaxFrac", "\\\\frac")
  return(new.expression)
}

replace_matching_terms <- function(input_vector, 
                                   search_terms, 
                                   replacement_terms) {
  # Function: Replaces matching terms in the input vector with corresponding
  #replacement terms
  
  # Input:
  # - input_vector: Vector to be checked for matching terms
  # - search_terms: Vector of terms to search for in the input_vector
  # - replacement_terms: Vector of replacement terms to use if a match is found
  # Output:
  # - output_vector: Vector with matching terms replaced by their corresponding 
  #   replacement terms
  
  output_vector <- input_vector
  for (i in 1:length(search_terms)) {
    # Find indices where a match is found in the input_vector
    match_indices <- which(input_vector == search_terms[i])
    
    # Replace matching terms with corresponding replacement terms
    output_vector[match_indices] <- replacement_terms[i]
  }
  return(output_vector)
}

ConvertRateLaw <- function(stringRate) {
  # Takes in a string rate law and converts the result to latex, mathjax, and 
  # mathml.  Function won't catch everything but currently does a decent job
  
  
  # Delimiter term to split equation on
  delimiters <- "(?=[+\\-*/(){}])"
  
  # Convert terms by fractions 
  new <- determineFraction(stringRate)
  
  # Split into parts
  all.terms <- trimws(strsplit(new, delimiters, perl = TRUE)[[1]], 
                      which = "both")
  
  # Find terms again
  a <- parse_string_expression(new)
  
  # Extract valid terms for conversion
  valid <- a$valid.terms
  
  # Remove frac from values
  valid <- valid[!(valid == "frac")]
  
  # Convert to respective var types
  valid.latex <- unname(sapply(valid, Var2Latex))
  valid.mj    <- unname(sapply(valid, Var2MathJ))
  
  result.latex <- paste0(
    replace_matching_terms(all.terms, valid, valid.latex), collapse = "")
  
  result.mathjax <- paste0(
    replace_matching_terms(all.terms, valid, valid.mj), collapse = "")
  
  result.mathml <- katex_mathml(result.latex)
  
  out <- list("latex" = result.latex,
              "mathjax" = result.mathjax,
              "mathml" = result.mathml)
  
  return(out)
}





# SUBSTITUTE RATE LAW FUNCTIONS
SplitEquationString <- function(stingToSplit) {
  delimiters <- "(?=[+\\-*/(){}])"
  all.terms <- trimws(
    strsplit(stingToSplit, delimiters, perl = TRUE)[[1]], which = "both")
  return(all.terms)
}




RemoveWS <- function(stringExpression) {
  out <- str_replace_all(stringExpression, " ", "")
  return(out)
}

extractProdTerms <- function(expression) {
  
  split.prod <- strsplit(expression, split = "prod")[[1]][2]
  removed <- RemoveWS(substr(split.prod, 2, nchar(split.prod) - 1))
  final.vec <- strsplit(removed, ",")[[1]]
  condensed <- paste0(final.vec, collapse = "*")
}

rmParen <- function(e) {
  # This function removes excess parenthesis from an expression
  if (length(e) > 1) {
    if (identical(e[[1]], as.symbol("("))) e <- e[[2]]
    if (length(e) > 1) for (i in 1:length(e)) e[[i]] <- Recall(e[[i]])
  }
  return(e)
}

rmp <- function(s){
  tryCatch({
    paste(deparse(rmParen(parse(text = s)[[1]])), collapse="")
  },
  error = function(cond) {
    return(s)
  })
  
}

RenameVarInDFColumn <- function(oldName, newName, dfcol, isMath = FALSE) {
  # Renaming vars in col, used in SBML conversion to id to name
  # Inputs:
  #   @oldName - String name of the parameter to be changed
  #   @newName - Desired String name of the new parameter
  #   @vectorToCheck - Vector of data to look for string in
  #   @isMath - (bool) true is math expression string to be split
  # Output:   
  #   Returns df with changed name values (if any) 
  
  #check to make sure rows exist as some dateframes in this program are initiated without columns (columsn just extra check)
  if (!isMath) {
    if (length(dfcol) != 0) {
      for (i in seq_along(dfcol)) {
        entry <- strsplit(dfcol[i], ", ")[[1]]
        if (oldName %in% entry) {
          idx <- which(entry %in% oldName)
          entry[idx] <- newName
          dfcol[i] <- paste0(entry, collapse = ", ")
        }
      }
    }
  } else {
    if (length(dfcol) != 0) {
      for (i in seq_along(dfcol)) {
        # split by math term
        entry <- SplitEquationString(dfcol[i])
        if (oldName %in% entry) {
          idx <- which(entry %in% oldName)
          entry[idx] <- newName
          dfcol[i] <- paste0(entry, collapse = " ")
        }
      }
    }
  }
  
  return(dfcol)
}


