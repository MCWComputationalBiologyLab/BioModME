

# Change color of Rate Law TextInput if valid/invalid
observeEvent(input$TI_CC_enter_rate_law, {
  a <- parse_string_expression(input$TI_CC_enter_rate_law)
  is.valid <- is_valid_expression(input$TI_CC_enter_rate_law, a$valid.terms)
  print(a$valid.terms)
  print(is.valid)
  if (is.valid) {
    js$backgroundCol("TI_CC_enter_rate_law", "#90ee90")
  } else {
    js$backgroundCol("TI_CC_enter_rate_law", "#ffcccb")
  }
})

# Store Equation Event 
observeEvent(input$bttn_store_custom_reaction, {
  
  # Pull law information from UI  
  law.name <- input$TI_CC_law_name
  law.desc <- input$TI_CC_law_description
  
  # Grab Equation Information
  reactants <- trimws(strsplit(input$PI_CC_reactants, ",")[[1]], which = "both")
  products  <- trimws(strsplit(input$PI_CC_products,  ",")[[1]], which = "both")
  modifiers <- trimws(strsplit(input$PI_CC_modifiers, ",")[[1]], which = "both")
  species   <- c(reactants, products, modifiers)
  
  # Collapse Species
  reactants.collapsed    <- collapseVector(reactants)
  products.collapsed     <- collapseVector(products)
  species.collapsed      <- collapseVector(species)
  modifiers.collapsed    <- collapseVector(modifiers)
  
  # Grab Rate Law Information
  rate.law        <- input$TI_CC_enter_rate_law
  parse.rate.law  <- parse_string_expression(rate.law)
  valid.rate.law  <- is_valid_expression(rate.law, parse.rate.law$valid.terms)
  
  # Perform checks to make sure rate law is valid to enter
  # Checks include:
  #   Title provided
  #   Title not already used
  #   Either Reactants or Products exist
  #   All Variables are legal variables
  #   Rate Law is legal
  
  valid.custom.law    <- FALSE
  is.title.provided   <- FALSE
  title.not.used      <- FALSE
  exists.reactant     <- FALSE
  exists.product      <- FALSE
  exists.r.or.p       <- FALSE
  vars.appropriate    <- FALSE
  
  # Check for Law Title
  if (gsub(" ", "", law.name) != "") {is.title.provided <- TRUE}
  
  # Check if title exists
  if (!(law.name %in% rv.CUSTOM.LAWS$cl.reaction.names)) {title.not.used <- TRUE}
  
  # Check if reactants exist
  if (isTruthy(gsub(" ", "", reactants.collapsed))) {exists.reactant <- TRUE}
  
  # Check if products exist
  if (isTruthy(gsub(" ", "", products.collapsed))) {exists.product <- TRUE}
  
  # rate law needs a reactant or product to apply rate law to (doesnt need both)
  if (exists.reactant || exists.product) {exists.r.or.p <- TRUE}
  
  if (is.title.provided && title.not.used && exists.r.or.p && valid.rate.law) {
    valid.custom.law <- TRUE
  }
  
  # VALID - STORE AND PROCESS LAW
  if (valid.custom.law) {
    
    # Grab Parameters For Law From RhandsonTable
    par.table <- hot_to_r(input$TO_CC_parameter_table)
    parameters <- par.table %>% pull(Variables)
    par.type   <- par.table %>% pull(Type)
    
    # Generate unique reaction ID
    ids <- GenerateId(rv.ID$id.custeqn.seed, "customEqn")
    unique.id <- ids[[2]]
    rv.ID$id.custeqn.seed <- ids[[1]]
    idx.to.add <- nrow(rv.ID$id.df) + 1
    rv.ID$id.df[idx.to.add, ] <- c(unique.id, 
                                   paste0("user_custom_law_", law.name))
    
    # Condense Parameter Vectors
    par.collapsed          <- collapseVector(parameters)
    par.type.collapsed     <- collapseVector(par.type)
    
    
    # Equation Builds
    eqn.builds <- BuildCustomEquationText(reactants,
                                          products,
                                          modifiers,
                                          parameters)
    # Rate Laws
    string.rate <- input$TI_CC_enter_rate_law
    latex.rate    <- NA
    mathjax.rate  <- NA
    mathml.rate   <- NA
    print(string.rate)
    tryCatch(
      expr = {
        law.converted <- ConvertRateLaw(string.rate)
        latex.rate    <- law.converted$latex
        mathjax.rate  <- law.converted$mathjax
        mathml.rate   <- law.converted$mathml
      },
      error = function(e) {
        print("ERROR PLEASE DON'T CRASH")
        print(e)
      },
      warning = function(w) {
        print("WARNING PLEASE DON'T CRASH")
        print(w)
      }
    )
    # law.converted <- ConvertRateLaw(string.rate)
    # latex.rate    <- law.converted$latex
    # mathjax.rate  <- law.converted$mathjax
    # mathml.rate   <- law.converted$mathml
    # 
    # Create content mathml from string
    content.ml <- string2mathml(string.rate)
    # Add Custom Law Data
    to.list <- list("ID" = unique.id,
                    "Type" = "Reaction",
                    "Law.Name" = paste0("user_custom_law_", law.name),
                    "Description" = law.desc,
                    "Reactants" = reactants.collapsed,
                    "Products" = products.collapsed,
                    "Modifiers" = modifiers.collapsed,
                    "Parameters" = par.collapsed,
                    "Parameter.Types" = par.type.collapsed,
                    "Equation.Text" = eqn.builds$text,
                    "Equation.Latex" = eqn.builds$latex,
                    "Equation.Mathjax" = eqn.builds$mathjax,
                    "String.Rate.Law" = string.rate,
                    "Latex.Rate.Law" = latex.rate,
                    "MathJax.Rate.Law" = mathjax.rate,
                    "Content.MathML" = content.ml,
                    "Rate.MathML" = mathml.rate,
                    "Reversible" = FALSE)
    
    rv.CUSTOM.LAWS$cl.reaction[[unique.id]] <- to.list
    
    # Update Custom Law Names
    rv.CUSTOM.LAWS$cl.reaction.names <- unname(sapply(rv.CUSTOM.LAWS$cl.reaction,
                                                   get,
                                                   x = "Law.Name"))
    
    # Add to reaction laws RV
    backend.entry <- paste0("user_custom_law_", law.name)
    row.to.add <- c(law.name, backend.entry, "custom")
    rv.REACTIONLAWS$laws <- rbind(rv.REACTIONLAWS$laws, row.to.add)
    
    reaction.type <- input$eqnCreate_type_of_equation
    reaction.law  <- input$eqnCreate_reaction_law
    
    if (reaction.type == "All") {
      option.names <- rv.REACTIONLAWS$laws %>% pull(Name)
      options      <- rv.REACTIONLAWS$laws %>% pull(BackendName)
    }  else if (reaction.type == "custom_reaction") {
      option.names <- rv.REACTIONLAWS$laws %>% 
        filter(Type == "custom") %>%
        pull(Name)
      options      <- rv.REACTIONLAWS$laws %>%
        filter(Type == "custom") %>%
        pull(BackendName)
    }
    
    names(options) <- option.names
    
    updatePickerInput(
      session = session, 
      inputId = "eqnCreate_reaction_law",
      choices = options,
      selected = reaction.law
    )
    
    # Send Confirm Message
    sendSweetAlert(
      session = session,
      title = "Success",
      text = "Custom Law Added!",
      type = "success"
    )
    
    # Number of custom eqns
    n.cust.eqns <- length(rv.CUSTOM.LAWS$cl.reaction)
    # Clear Custom UI of added information
    updateTextInput(
      session = session,
      inputId = "TI_CC_law_name",
      value = paste0("CustomLaw", as.character(n.cust.eqns + 1))
    )
    
    updateTextAreaInput(
      session = session,
      inputId = "TI_CC_law_description",
      value = "",
      placeholder = "This law describes the interactions of ..."
    )
    
    updateTextInput(
      session = session,
      inputId = "PI_CC_reactants",
      value = "",
      placeholder = "x1, x2"
    )
    
    updateTextInput(
      session = session,
      inputId = "PI_CC_products",
      value = "",
      placeholder = "y1"
    )
    
    updateTextInput(
      session = session,
      inputId = "PI_CC_modifiers",
      value = "",
      placeholder = "mod1"
    )
    
    updateTextInput(
      session = session,
      inputId = "TI_CC_enter_rate_law",
      value = "",
      placeholder = "x1*p1*x2^2/(mod*y1)"
    )
    
  } else {
    # NOT VALID - SEND APPROPRIATE ERROR MESSAGE
    
    # Determine appropriate error,
    if (!is.title.provided) {
      error.message <- "No Law Name provided for custom law."
    } else if (!title.not.used) {
      error.message <- paste0("Law Name: ",
                              input$TI_CC_law_name,
                              ", is already used for different law in program.")
    } else if (!exists.r.or.p) {
      error.message <- "The law needs either a reactant or a product to 
                        be valid. Otherwise, the rate law would not apply to 
                        any variables."
    } else if (!valid.rate.law) {
      error.message <- "Rate law is not a valid equation."
    }
    
    # Send Error message
    sendSweetAlert(
      session = session,
      title = "Error...",
      text = error.message,
      type = "error"
    )
  }
})

# Table render for custom equation build showing parameters
output$TO_CC_parameter_table <- renderRHandsontable({
  
  # Grab variables for reactants, products, modifiers
  reactants <- trimws(strsplit(input$PI_CC_reactants, ",")[[1]], which = "both")
  products  <- trimws(strsplit(input$PI_CC_products,  ",")[[1]], which = "both")
  modifiers <- trimws(strsplit(input$PI_CC_modifiers, ",")[[1]], which = "both")
  
  species.var <- c(reactants, products, modifiers)
  
  a <- parse_string_expression(input$TI_CC_enter_rate_law)
  
  valid <- a$valid.terms
  
  # # Remove species from valid Terms
  Variables <- valid[!valid %in% species.var]
  type.options <- c("Parameter", "Volume", "Time")
  
  Type <- rep("Parameter", length(Variables))
  # Type <- factor(rep(par.types[1], length(Variables)), levels = par.types)
  print(Variables)
  print(Type)
  if (isTruthy(Variables)) {
    df.to.show <- data.frame(Variables,
                             Type
    ) 
    colnames(df.to.show) <- c("Variables", "Type")
    hot <- rhandsontable(df.to.show,
                         height = 200,
                         stretchH = "all",
                         overflow = "visible"
    ) %>%
      hot_col(col = "Type", type = "dropdown", source = type.options) %>%
      hot_col(col = "Variables", readOnly = TRUE)
    
  } else {
    temp <- data.frame(c("Parameters will be extracted from above rate 
                         law here"))
    temp <- transpose(temp)
    colnames(temp) <- c("Reaction Parameters")
    hot <- rhandsontable(temp,
                         overflow = "visible",
                         stretchH = "all",
                         readOnly = TRUE,
                         rowHeaders = NULL,
                         height = 200
    ) %>%
      hot_cols(manualColumnMove = FALSE,
               manualColumnResize = FALSE,
               halign = "htCenter",
               valign = "htMiddle")
  }
  
  hot
  
})
