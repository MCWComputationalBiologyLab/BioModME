############################## DiffEQ Server #################################

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
  
  # Check for conversion
  if (input$CBI_diffeq_show_unit_types) {
    
    convert.bool <- TRUE
    # Create conversion df
    # Need - species, and parameters
    species.names <- unname(sapply(rv.SPECIES$species, get, x = "Name"))
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
    species.names <- unname(sapply(rv.SPECIES$species, get, x = "Name"))
    param.names   <- unname(sapply(rv.PARAMETERS$parameters, get, x = "Name"))
    type <- c(rep("species", length(species.names)), 
              rep("param", length(param.names))
            )
    term <- c(species.names, param.names)
    pretty.df <- data.frame(term, type)
  }
  
  lapply(seq(length(rv.DE$de.equations.list)), function(i){
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
                        pretty.df = pretty.df)
      )
    )
  })
})

buildMathjaxEqn <- function(de.entry, 
                            iter, 
                            comp.vol, 
                            newline.reaction.parts,
                            convert.vars = FALSE,
                            pretty.vars = FALSE,
                            convert.df = NULL,
                            pretty.df = NULL) {
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
    separator <- " \\\\ "
    aligner   <- "&"
  } else {
    separator <- ""
    aligner   <- ""
  }
  


  
  if (newline.reaction.parts) {
    begin.frac <- paste0("(", iter, ") \\: \\: ", Var2MathJ(comp.vol),
                         "\\frac{d",
                         de.entry$Name,
                         "}{dt} = ")
    
    if (isTruthy(de.entry$ODES.mathjax.vector)) {
      # Create align function
      # Cycle through all vectors, adding to function
      current.diff <- ""
      
      for (j in seq_along(de.entry$ODES.mathjax.vector)) {
        
        mj.expression <- de.entry$ODES.mathjax.vector[j]
        
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
                               mj.expression,
                               " ")
        # Add the newline for all equations that aren't the last one
        if (j != length(de.entry$ODES.mathjax.vector)) {
          current.diff <- paste0(current.diff, " \\\\ ")
        }
      }
    } else {
      current.diff <- "0"
    }
    
    out <- paste0("\\begin{aligned}[t]", 
                  begin.frac, 
                  current.diff, 
                  "\\end{aligned}")
    } else {
      # begin.frac <- paste0("(", iter, ") \\: \\: ", Var2MathJ(comp.vol),
      #                      "\\frac{d[",
      #                      de.entry$Name,
      #                      "]}{dt} = ")
      begin.frac <- paste0("(", iter, ") \\: \\: ", Var2MathJ(comp.vol),
                           "\\frac{d",
                           de.entry$Name,
                           "}{dt} = ")
      
      if (isTruthy(de.entry$ODES.mathjax.vector)) {
        # Create align function
        # Cycle through all vectors, adding to function
        current.diff <- ""
        for (j in seq_along(de.entry$ODES.mathjax.vector)) {
          mj.expression <- de.entry$ODES.mathjax.vector[j]
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
                                 mj.expression,
                                 " ")
          # Add the newline for all equations that aren't the last one
          if (j != length(de.entry$ODES.mathjax.vector)) {
            current.diff <- paste0(current.diff, separator)
          }
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
#   print("Diff.eqns")
#   print(diff.eqns)
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
#   #   print("DE TESTS ITER")
#   #   print(rv.DE$de.equations.list[[i]]$ODES.mathjax.vector)
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
output$dbttn_download_diffequations_specific <- downloadHandler(
  filename = function() {
    # paste0(input$PI_dde_choose_download_type,
    #        "_differential_equation.txt")
    
    # paste("equation.", input$PI_dde_choose_download_type, sep = "")
    switch(input$PI_dde_choose_download_type,
           "txt" = "txt_differential_equations.txt",
           "latex" = "latex_differential_equations.txt",
           "mathml" = "mathml_differential_equations.txt")
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