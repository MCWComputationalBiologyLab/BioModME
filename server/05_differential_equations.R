############################## DiffEQ Server #################################

# Expose a flag to UI: true if exactly one compartment exists
output$single_compartment_bool <- renderText({
  if (isTruthy(rv.COMPARTMENTS$compartments.df)) {
    if (nrow(rv.COMPARTMENTS$compartments.df) == 1) {
      return("true")
    }
  }
  return("false")
})
outputOptions(output, "single_compartment_bool", suspendWhenHidden = FALSE)

# Function to solve and extract diffeqs ----------------------------------------
solveForDiffEqs <- function() {
  # Solve the differential equations using RVs.
  # Store results to their respective RVs. 
  
  results <- DeriveDifferentialEquations(rv.COMPARTMENTS,
                                         rv.SPECIES,
                                         rv.REACTIONS,
                                         rv.IO,
                                         rv.ID)

  # Extract results to proper reactive variables
  rv.DE$de.equations.list   <- results
  rv.DE$de.string.eqns      <- unname(sapply(results,
                                             get,
                                             x = "ODES.eqn.string"))
  rv.DE$de.latex.eqns       <- unname(sapply(results, 
                                             get,
                                             x = "ODES.latex.string"))
  rv.DE$de.mathjax.eqns     <- unname(sapply(results, 
                                             get,
                                             x = "ODES.mathjax.string"))
  rv.DE$de.eqns.for.solver  <- unname(sapply(results,
                                             get,
                                             x = "ODE.for.solver"))
}

# Events -----------------------------------------------------------------------
observeEvent(rv.SPECIES$species, {
  picker.choices <- c()
  i = 0
  for (var in rv.SPECIES$species.names) {
    i = i + 1
    choice <- paste0(i, ") ", 'd(', var, ")/dt")
    picker.choices <- c(picker.choices, choice)
  }
  updatePickerInput(session, 
                    "diffeq_var_to_custom", 
                    choices = picker.choices)
})

observeEvent(rv.DE$custom.diffeq.var, {
  picker.choices <- rv.DE$custom.diffeq.var
  updatePickerInput(session, 
                    "diffeq_multi_custom_eqns", 
                    choices = picker.choices)
})

observeEvent(input$diffeq_custom_eqn_button, {
  new.eqn <- input$diffeq_custom_eqn
  idx <- as.numeric(strsplit(input$diffeq_var_to_custom, ")")[[1]][1])

  rv.DE$de.eqns[idx] <- new.eqn
  rv.DE$custom.diffeq.var <- c(rv.DE$custom.diffeq.var, 
                               rv.SPECIES$species.names[idx])
  rv.DE$custom.diffeq <- c(rv.DE$custom.diffeq, new.eqn)
  rv.DE$custom.diffeq.df[nrow(rv.DE$custom.diffeq.df)+1, ] <- 
    c(rv.SPECIES$species.names[idx], 
      new.eqn)
})

# Diff Eqn Button --------------------------------------------------------------
observeEvent(input$diffeq_generate_equations, {
  solveForDiffEqs()
})

# Render diffeqn text viewer ---------------------------------------------------
output$diffeq_display_diffEqs <- renderText({
  
  if (length(rv.SPECIES$species) == 0) {
    "No variables entered"
  }
  else {
    n_eqns = length(rv.SPECIES$species)
    eqns_to_display <- c()
    for (i in seq(n_eqns)) {
      # Find Corresponding Volumes for compartments
      comp.of.variable <- rv.SPECIES$species[[i]]$Compartment
      row.idx <- which(rv.COMPARTMENTS$compartments.df$Name %in% comp.of.variable)
      comp.vol <- rv.COMPARTMENTS$compartments.df$Volume[row.idx]
      if (input$diffeq_option_simplify) {
        new_eqn <- paste0("(",i, ") ",
                          comp.vol, "*",
                          'd(', 
                          rv.SPECIES$species.names[i], 
                          ")/dt = ", 
                          Deriv::Simplify(rv.DE$de.string.eqns[i]))
      } else {
        new_eqn <- paste0("(",i, ") ",
                          comp.vol, "*",
                          'd(',
                          rv.SPECIES$species.names[i],
                          ")/dt = ",
                          rv.DE$de.string.eqns[i])
      }
      eqns_to_display <- c(eqns_to_display, new_eqn)
    }
    paste(eqns_to_display, collapse = "<br><br>")
  }
})


# output$diffeq_display_diffEqs_MathJax <- renderUI({
#   withMathJax(
#     differentialEqnsMathjax()
#   )
# })

output$diffeq_display_diffEqs_MathJax <- renderUI({
  
  convert.bool <- FALSE
  convert.df   <- NULL
  pretty.bool  <- FALSE
  pretty.df    <- NULL
  
  # Get species names for removing subscripts (always needed)
  species.names <- unname(sapply(rv.SPECIES$species, get, x = "Name"))
  
  # Check for conversion
  if (input$CBI_diffeq_show_unit_types) {
    
    convert.bool <- TRUE
    # Create conversion df
    # Need - species, and parameters
    param.names   <- unname(sapply(rv.PARAMETERS$parameters, get, x = "Name"))
    species.units <- unname(sapply(rv.SPECIES$species, get, x = "BaseUnit"))
    param.units   <- unname(
                       sapply(rv.PARAMETERS$parameters, get, x = "BaseUnit"))
    search_column <- c(species.names, param.names)
    return_column <- c(species.units, param.units)
    convert.df <- data.frame(search_column, return_column)
  } else if (input$CBI_diffeq_pretty_equations) {
    pretty.bool <- TRUE
    # set up df
    param.names   <- unname(sapply(rv.PARAMETERS$parameters, get, x = "Name"))
    type <- c(rep("species", length(species.names)), 
              rep("param", length(param.names))
            )
    term <- c(species.names, param.names)
    pretty.df <- data.frame(term, type)
  }
  
  lapply(seq(length(rv.DE$de.equations.list)), function(i){
    # Determine if we should hide volume: only if single compartment and option enabled
    hide.volume <- FALSE
    if (isTruthy(rv.COMPARTMENTS$compartments.df)) {
      if (nrow(rv.COMPARTMENTS$compartments.df) == 1 && isTruthy(input$CBI_diffeq_hide_volume)) {
        hide.volume <- TRUE
      }
    }
    # Optional extra cleanup
    clean.paren <- isTruthy(input$CBI_diffeq_clean_parenthesis)
    div(
      style = "overflow-y:auto",
      withMathJax(
        buildMathjaxEqn(rv.DE$de.equations.list[[i]],
                        i,
                        rv.DE$de.equations.list[[i]]$Compartment.vol,
                        input$diffeq_newline_diffeq,
                        convert.vars = convert.bool,
                        convert.df = convert.df,
                        pretty.vars = pretty.bool,
                        pretty.df = pretty.df,
                        hide.volume = hide.volume,
                        clean.paren = clean.paren,
                        species.names = species.names)
      )
    )
  })
})

# Helper: remove subscripts from species names in MathJax strings
# Keeps parameters subscripted, only removes subscripts from species
remove_species_subscripts <- function(mj.string, species.names) {
  if (!isTruthy(mj.string) || !isTruthy(species.names)) {
    return(mj.string)
  }
  
  # Process each species name
  for (species.name in species.names) {
    # Only process species names that have underscores (would be subscripted)
    if (grepl("_", species.name, fixed = TRUE)) {
      # Convert species name to its subscripted MathJax form
      species.mj <- Var2MathJ(species.name)
      # Replace the subscripted version with the plain version (underscore, not subscript)
      # Use word boundaries to avoid partial matches
      # Escape special regex characters in the MathJax version
      species.mj.escaped <- gsub("([{}()*+?.\\^$|\\[\\]])", "\\\\\\1", species.mj, perl = TRUE)
      mj.string <- gsub(species.mj.escaped, species.name, mj.string, fixed = FALSE)
    }
  }
  
  return(mj.string)
}

buildMathjaxEqn <- function(de.entry, 
                            iter, 
                            comp.vol, 
                            newline.reaction.parts,
                            convert.vars = FALSE,
                            pretty.vars = FALSE,
                            convert.df = NULL,
                            pretty.df = NULL,
                            hide.volume = FALSE,
                            clean.paren = FALSE,
                            species.names = NULL) {
  # Takes in the differential equation structures and builds an expression to 
  # display in the mathjax builder.
  # Inputs: 
  # @de.entry - string equation for diff eqn entry
  # @iter - current iterator for equation numbering
  # @comp.vol - volume variable belonging to reaction
  # @newline.reaction.parts - (bool), true inserts newline terms 
  # @convert.vars - (bool), converts mathjax expression
  # @pretty.vars - (bool), converts de expressions to bracked form
  # @convert.df - df, rows: search_column, return_column
  # @pretty.df - df, rows: term, type
  
  if (newline.reaction.parts) {
    separator <- " \\ "
    aligner   <- "&"
  } else {
    separator <- ""
    aligner   <- ""
  }
  

  
  if (newline.reaction.parts) {
    if (hide.volume) {
      begin.frac <- paste0("(", iter, ")  ",
                           "\\frac{d",
                           de.entry$Name,
                           "}{dt} = ")
    } else {
      begin.frac <- paste0("(", iter, ")  ", Var2MathJ(comp.vol),
                           "\\frac{d",
                           de.entry$Name,
                           "}{dt} = ")
    }
    
    if (isTruthy(de.entry$ODES.mathjax.vector)) {
      # Create align function
      # Cycle through all vectors, adding to function
      current.diff <- ""
      
      for (j in seq_along(de.entry$ODES.mathjax.vector)) {
        
        mj.expression <- de.entry$ODES.mathjax.vector[j]
        
        # If hiding volume, strip the leading volume factor from each term
        if (hide.volume) {
          vol.mj <- Var2Latex(comp.vol)
          # Remove direct occurrences of 'vol*(...' and 'vol∗(...'
          mj.expression <- gsub(paste0(vol.mj, "*("), "(", mj.expression, fixed = TRUE)
          mj.expression <- gsub(paste0(vol.mj, "\u2217("), "(", mj.expression, fixed = TRUE)
          # Also remove simple 'vol*' or 'vol∗' anywhere it appears
          mj.expression <- gsub(paste0(vol.mj, "*"), "", mj.expression, fixed = TRUE)
          mj.expression <- gsub(paste0(vol.mj, "\u2217"), "", mj.expression, fixed = TRUE)
          # Clean up duplicate parentheses and duplicated left/right markers
          mj.expression <- gsub("\\\\left\\(\\\\left\\(", "\\\\left(", mj.expression)
          mj.expression <- gsub("\\\\right\\)\\\\right\\)", "\\\\right)", mj.expression)
          mj.expression <- gsub("((", "(", mj.expression, fixed = TRUE)
          mj.expression <- gsub("))", ")", mj.expression, fixed = TRUE)
          # Remove leftover whitespace
          mj.expression <- gsub("[\t\n\r ]+", "", mj.expression)
        }
        # Optional extra parenthesis cleanup
        if (clean.paren) {
          # Remove a single wrapping pair of parentheses, if present
          mj.expression <- sub("^\\((.*)\\)$", "\\1", mj.expression, perl = TRUE)
          # Robust cleanup/balancing
          mj.expression <- clean_parentheses_string(mj.expression)
        }
        
        # Remove subscripts from species names (keep parameters subscripted)
        if (isTruthy(species.names)) {
          mj.expression <- remove_species_subscripts(mj.expression, species.names)
        }
        
        # Convert the terms of the differential equations
        if (convert.vars) {
          term <- mj.expression
          term <- remove_braces(term)
          term <- gsub("\\left(", "", term, fixed = TRUE)
          term <- gsub("\\right)", "", term, fixed = TRUE)
          split.exp <- SplitEquationString(term)
          terms.vector <- extract_variables(term)

          # Find matching rows and extract corresponding values
          matched.indices <- match(terms.vector, convert.df$search_column)
          matched.values <- convert.df$return_column[matched.indices]
        
          # matched.values <- 
          #   convert.df$return_column[convert.df$search_column %in% terms.vector]

          mj.expression <- 
            paste0(
              replace_matching_terms(
                split.exp,
                terms.vector,
                matched.values
                ),
              collapse = ""
            )
        } else if (pretty.vars) {
          mj.expression <- prettyDiffEquations(mj.expression, pretty.df, TRUE)
        }
        
        current.diff <- paste0(current.diff,
                               "&",
                               mj.expression)
        # Add the newline for all equations that aren't the last one
        if (j != length(de.entry$ODES.mathjax.vector)) {
          current.diff <- paste0(current.diff, " \\\\ ")
        }
      }
      # If hiding volume, strip only actual whitespace (not \\ or & which are LaTeX markers)
      if (hide.volume || clean.paren) {
        # Remove spaces but preserve \\ and &
        current.diff <- gsub(" +", " ", current.diff)  # Collapse multiple spaces to one
        current.diff <- gsub("^ +| +$", "", current.diff)  # Trim leading/trailing spaces
        if (clean.paren) current.diff <- clean_parentheses_string(current.diff)
      }
    } else {
      current.diff <- "0"
    }
    
    out <- paste0("\\begin{aligned}", 
                  begin.frac, 
                  current.diff, 
                  "\\end{aligned}")
    } else {
      # begin.frac <- paste0("(", iter, ") \\ \: \: ", Var2MathJ(comp.vol),
      #                      "\\frac{d[",
      #                      de.entry$Name,
      #                      "]}{dt} = ")
      if (hide.volume) {
        begin.frac <- paste0("(", iter, ")  ",
                             "\\frac{d",
                             de.entry$Name,
                             "}{dt} = ")
      } else {
        begin.frac <- paste0("(", iter, ")  ", Var2MathJ(comp.vol),
                             "\\frac{d",
                             de.entry$Name,
                             "}{dt} = ")
      }
      
      if (isTruthy(de.entry$ODES.mathjax.vector)) {
        # Create align function
        # Cycle through all vectors, adding to function
        current.diff <- ""
        for (j in seq_along(de.entry$ODES.mathjax.vector)) {
          mj.expression <- de.entry$ODES.mathjax.vector[j]
          if (hide.volume) {
            vol.mj <- Var2Latex(comp.vol)
            mj.expression <- gsub(paste0(vol.mj, "*("), "(", mj.expression, fixed = TRUE)
            mj.expression <- gsub(paste0(vol.mj, "\u2217("), "(", mj.expression, fixed = TRUE)
            mj.expression <- gsub(paste0(vol.mj, "*"), "", mj.expression, fixed = TRUE)
            mj.expression <- gsub(paste0(vol.mj, "\u2217"), "", mj.expression, fixed = TRUE)
            mj.expression <- gsub("\\\\left\\(\\\\left\\(", "\\\\left(", mj.expression)
            mj.expression <- gsub("\\\\right\\)\\\\right\\)", "\\\\right)", mj.expression)
            mj.expression <- gsub("((", "(", mj.expression, fixed = TRUE)
            mj.expression <- gsub("))", ")", mj.expression, fixed = TRUE)
            mj.expression <- gsub("[\t\n\r ]+", "", mj.expression)
          }
          if (clean.paren) {
            mj.expression <- sub("^\\((.*)\\)$", "\\1", mj.expression, perl = TRUE)
            mj.expression <- clean_parentheses_string(mj.expression)
          }
          
          # Remove subscripts from species names (keep parameters subscripted)
          if (isTruthy(species.names)) {
            mj.expression <- remove_species_subscripts(mj.expression, species.names)
          }
          
          # Convert the terms of the differential equations
          if (convert.vars) {
            term <- mj.expression
            term <- remove_braces(term)
            term <- gsub("\\left(", "", term, fixed = TRUE)
            term <- gsub("\\right)", "", term, fixed = TRUE)
            split.exp <- SplitEquationString(term)
            terms.vector <- extract_variables(term)
            
            # Find matching rows and extract corresponding values
            matched.indices <- match(terms.vector, convert.df$search_column)
            matched.values <- convert.df$return_column[matched.indices]
            # matched.values <- 
            #   convert.df$return_column[convert.df$search_column %in% terms.vector]
            
            mj.expression <- 
              paste0(
                replace_matching_terms(
                  split.exp,
                  terms.vector,
                  matched.values
                ),
                collapse = ""
              )
            
          } else if (pretty.vars) {
            mj.expression <- prettyDiffEquations(mj.expression, pretty.df, TRUE)
          }
          
          current.diff <- paste0(current.diff,
                                 mj.expression)
          # Add the newline for all equations that aren't the last one
          if (j != length(de.entry$ODES.mathjax.vector)) {
            current.diff <- paste0(current.diff, separator)
          }
        }
        # If hiding volume, clean up only actual whitespace (preserve LaTeX structure)
        if (hide.volume || clean.paren) {
          current.diff <- gsub(" +", " ", current.diff)  # Collapse multiple spaces
          current.diff <- gsub("^ +| +$", "", current.diff)  # Trim leading/trailing spaces
          if (clean.paren) current.diff <- clean_parentheses_string(current.diff)
        }
      } else {
        current.diff <- "0"
      }
      
      out <- paste0("\\begin{equation}", 
                    begin.frac, 
                    current.diff, 
                    "\\end{equation}")
  }

  
  return(out)
}


# differentialEqnsMathjax <- reactive({
#   # Displays the differential equations in mathjax form
#   # Have multiple options
#   #   @newline - each differential is displayed with a newline after each step
#   
#   # require equations or IO to be greater than one
#   #req()
#   
#   if (input$diffeq_newline_diffeq) {
#     separator <- " \\\\ "
#     aligner   <- "&"
#   } else {
#     separator <- ""
#     aligner   <- ""
#   }
#   
#   beginning.align <- "\\begin{aligned} "
#   diff.eqns <- vector("character", length = length(rv.DE$de.equations.list))
#   # Cycle through de equations list.
#   for (i in seq_along(rv.DE$de.equations.list)) {
#     
#     # Get compartment vol
#     comp.vol <- rv.DE$de.equations.list[[i]]$Compartment.vol
#     
#     # create fraction for each (d[var1]/dt = )
#     begin.fract <- paste0("&", "(", i, ") \\:\\: ",  Var2MathJ(comp.vol),
#                           "\\frac{d[", 
#                           rv.DE$de.equations.list[[i]]$Name,
#                           "]}{dt} = ")
#     
#     # Check if equations mathjax expressions have been created for this variable
#     if (isTruthy(rv.DE$de.equations.list[[i]]$ODES.mathjax.vector)) {
#       
#       # Create align function
#       current.diff <- "\\begin{aligned}[t] "
#       # Cycle through all vectors, adding to function
#       for (j in seq_along(rv.DE$de.equations.list[[i]]$ODES.mathjax.vector)) {
#         mj.expression <- rv.DE$de.equations.list[[i]]$ODES.mathjax.vector[j]
#         current.diff <- paste0(current.diff, 
#                                aligner,
#                                mj.expression,
#                                " ")
#         # Add the newline for all equations that aren't the last one
#         if (j != length(rv.DE$de.equations.list[[i]]$ODES.mathjax.vector)) {
#           current.diff <- paste0(current.diff, separator)
#         }
#       }
#       
#       current.diff <- paste0(current.diff, "\\end{aligned}")
#     } else {
#       current.diff <- "0"
#     }
#     
#     # Combine fraction with diffeqn
#     current.diff <- paste0(begin.fract, current.diff)
#     diff.eqns[i] <- current.diff
#   }
#   
#   out <- paste0(diff.eqns, collapse = " \\\\\\\\\\ ")
#   # out <- paste0("$$", out, "$$")
#   out <- paste0("$$\\begin{aligned} ", out, "\\end{aligned}$$")
#   
#   
#   
#   
#   # 
#   # # Store each individual in a vector
#   # 
#   # # Collapse vector with mathjax newline (//)
#   # for (i in seq_along(rv.DE$de.equations.list)) {
#   #   if (isTruthy(rv.DE$de.equations.list[[i]]$ODES.mathjax.vector)) {
#   #     textOut <- paste0(textOut, 
#   #                    rv.DE$de.equations.list[[i]]$ODES.mathjax.vector, 
#   #                    "\n")
#   #   }
#   # }
#   # textOut <- paste0("$$", textOut, "$$")

#   return(out)
# })


# Download Button - Modal ------------------------------------------------------

observe({
  
  eqn.choices  <- unname(sapply(rv.DE$de.equations.list,
                         get,
                         x = "Name"))
  
  updatePickerInput(
    session = session,
    inputId = "PI_dde_c_mathml_selection",
    choices = c("View All", eqn.choices)
  )
  
  updatePickerInput(
    session = session,
    inputId = "PI_dde_p_mathml_selection",
    choices = c("View All", eqn.choices)
  )
})

output$vTO_displayEquations_txt <- renderText({
  eqns  <- unname(sapply(rv.DE$de.equations.list,
                         get,
                         x = "ODES.eqn.string"))
  print(paste(eqns, collapse = "\n"))
})

output$vTO_displayEquations_p_mathml <- renderText({
  eqns  <- unname(sapply(rv.DE$de.equations.list,
                         get,
                         x = "ODES.eqn.string"))
  
  equations_vector <- 
    unname(
      sapply(
        rv.DE$de.equations.list,
        get,
        x = "Name"
      )
    )
  
  # Convert to mathml
  mathml_equations <- c()
  for (i in seq_along(eqns)) {
    # print(eqns[i])
    temp <- mathml(eval(parse(text=paste0("quote(", eqns[i], ")"))))
    temp <- gsub("&#x2062;", "*", temp)
    temp <- gsub("&sdot;", "*", temp)
    temp <- gsub("<math>", '<math xmlns=\"http://www.w3.org/1998/Math/MathML\">', temp)
    # sub <math> with <math xmlns="&mathml;">
    # sub &#x2062 & &sdot with *
      # paste0(
      #   "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">",
      #   mathml(quote(term=eqns[i])),
      #   "</math>"
      # )
    mathml_equations <- c(mathml_equations, temp)
  }
  if (input$PI_dde_p_mathml_selection == "View All") {
    formatted_mathml <- sapply(mathml_equations, function(eq) {
      parsed_xml <- read_xml(eq)
      xml_str <- as.character(parsed_xml)
      xml_str <- gsub('<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n',
                      '',
                      xml_str,
                      fixed = TRUE)
    })
    eqns_out <- paste(formatted_mathml, collapse = "\n\n")
  } else {
    index <- which(equations_vector == input$PI_dde_p_mathml_selection)
    parsed_xml <- read_xml(mathml_equations[[index]])
    xml_str <- as.character(parsed_xml)
    eqns_out <- gsub('<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n',
                    '',
                    xml_str,
                    fixed = TRUE)
  }
  
  return(eqns_out)
})

output$vTO_displayEquations_c_mathml <- renderText({
  eqns  <- unname(sapply(rv.DE$de.equations.list,
                         get,
                         x = "ODES.eqn.string"))
  
  equations_vector <- 
    unname(
      sapply(
        rv.DE$de.equations.list,
        get,
        x = "Name"
      )
    )
  
  # Convert to mathml
  mathml_equations <- c()
  for (i in seq_along(eqns)) {
    temp <- 
      paste0(
        "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">",
        string2mathml(eqns[i]),
        "</math>"
      )
    mathml_equations <- c(mathml_equations, temp)
  }
  
  if (input$PI_dde_c_mathml_selection == "View All") {
    formatted_mathml <- sapply(mathml_equations, function(eq) {
      parsed_xml <- read_xml(eq)
      xml_str <- as.character(parsed_xml)
      xml_str <- gsub('<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n', 
                      '', 
                      xml_str,
                      fixed = TRUE)
    })
    paste(formatted_mathml, collapse = "\n\n")
  } else {
    index <- which(equations_vector == input$PI_dde_c_mathml_selection)
    parsed_xml <- read_xml(mathml_equations[[index]])
    xml_str <- as.character(parsed_xml)
    xml_str <- gsub('<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n', 
                    '', 
                    xml_str,
                    fixed = TRUE)
  }
})

output$vTO_displayEquations_latex <- renderText({
  eqns  <- unname(sapply(rv.DE$de.equations.list,
                         get,
                         x = "ODES.eqn.string"))
})

output$dbttn_download_diffequations_specific <- downloadHandler(
  filename = function() {
    # paste0(input$PI_dde_choose_download_type,
    #        "_differential_equation.txt")
    
    # paste("equation.", input$PI_dde_choose_download_type, sep = "")
    switch(input$PI_dde_choose_download_type,
           "txt" = "txt_differential_equations.txt",
           "latex" = "latex_differential_equations.txt",
           "c_mathml" = "mathml_differential_equations.txt")
  },
  content = function(file) {

    
    if(input$PI_dde_choose_download_type == "txt") {
      
      eqns  <- unname(sapply(rv.DE$de.equations.list,
                             get,
                             x = "ODES.eqn.string"))
      eqns  <- paste0(eqns, collapse = "\n")
      # TODO add LHS off equation d[A]/dt = eqns[i]
      writeLines(eqns, file)
      
    } else if(input$PI_dde_choose_download_type == "mathml") {
      
      eqns  <- unname(sapply(rv.DE$de.equations.list,
                             get,
                             x = "ODES.eqn.string"))
      # Convert to mathml
      mathml.eqns <- c()
      for (i in seq_along(eqns)) {
        temp <- 
          paste0(
            "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">",
            string2mathml(eqns[i]),
            "</math>"
          )
        mathml.eqns <- c(mathml.eqns, temp)
      }
      
      mathml.eqns <- paste0(mathml.eqns, collapse = "\n")
      writeLines(mathml.eqns, file)
      
    } else if(input$PI_dde_choose_download_type == "latex") {
      writeLines(equation_as_latex, file)
    }
  }
)

# Helper: robust parenthesis cleanup while preserving LaTeX markers
clean_parentheses_string <- function(x) {
  if (!isTruthy(x)) return(x)
  # Normalize duplicated brackets first
  old <- NULL
  new <- x
  # Preserve LaTeX left/right markers by temporarily removing them
  new <- gsub("\\\\left\\(", "__L__", new)
  new <- gsub("\\\\right\\)", "__R__", new)
  # Collapse duplicated parentheses
  repeat {
    old <- new
    new <- gsub("\\)\\)", ")", new)
    new <- gsub("\\(\\(", "(", new)
    if (identical(new, old)) break
  }
  # Balance parentheses: remove unmatched closing ones
  chars <- strsplit(new, "")[[1]]
  out <- character(length(chars))
  balance <- 0L
  k <- 0L
  for (ch in chars) {
    if (ch == "(") {
      balance <- balance + 1L
      k <- k + 1L; out[k] <- ch
    } else if (ch == ")") {
      if (balance > 0L) {
        balance <- balance - 1L
        k <- k + 1L; out[k] <- ch
      } else {
        # skip unmatched ')'
      }
    } else {
      k <- k + 1L; out[k] <- ch
    }
  }
  if (k > 0L) new <- paste0(out[seq_len(k)], collapse = "") else new <- ""
  # Restore LaTeX markers
  new <- gsub("__L__", "\\\\left(", new)
  new <- gsub("__R__", "\\\\right)", new)
  new
}