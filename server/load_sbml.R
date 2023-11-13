waiter_fxn <- function(msg, spinner, bar_value) {
  # hostess <- Hostess$new()
  out <- tagList(eval(parse(text = spinner)),
                 br(),
                 br(),
                 shinyWidgets::progressBar(
                   id = "sbml_load_bar",
                   value = bar_value,
                   total = 100
                 ),
                 h4(msg)
  )
  return(out)
}

sbml_2_biomodme_compartments <- function(sbml.model) {
  # Unpack SBML Compartments
  # Current compartment values used by this program
  # Values:
  #   Name
  #   ID
  #   Value
  #   Volume (volume variable: V1, V2 etc)
  #   par.Id (id associated with volume)
  #   Unit
  #   UnitDescription
  #   BaseUnit
  #   BaseValue
  #   Description
  
  # Have to also add to parameters for volumn
  # Values:
  #   Name
  #   ID
  #   Value
  #   Unit
  #   UnitDescription
  #   BaseUnit
  #   BaseValue
  #   Description
  #   Type
  #   Type.note
  
  # SBML stores the compartment volume as the V_{compartment_name}
  # units come out as the type: which would be "volume".
  # So the only things we really look for here are the name
  # Overwrite ids
  # Assign base units as base volumn
  
  # sleep.time <- 0.5
  
  # Bool that is used in reactions. SBML stores compartment id and we want to 
  # store the name.  If TRUE, we will need to perform a conversion. 
  rv.sbml.load.variables$need.compartment.conversion <- FALSE
  
  compartments <- sbml.model$compartments
  n.compartments <- nrow(compartments)

  # Compartments have the following columns
  #   id, name, size, constant, spatialDimensions
  
  comp.ids   <- compartments %>% pull(id)
  comp.names <- compartments %>% pull(name)
  
  if (!identical(comp.ids, comp.names)) {
    rv.sbml.load.variables$need.compartment.conversion <- TRUE
    rv.sbml.load.variables$comp.df.conv <- data.frame(comp.ids, comp.names)
    colnames(rv.sbml.load.variables$comp.df.conv) <- c("id", "name")
  }
  
  comp.values <- compartments %>% pull(size)
  
  
  # Compartment Volume Names
  # SBML used the comp name for the volume var, which is fine but we want to 
  # create a separate variable for that
  comp.vol.names <- paste0("V_", comp.names)
  
  # Generate Compartment IDs (overwrite sbml ids and create vol ids)
  comp.ids <- c()
  vol.ids  <- c()
  for (i in seq_len(nrow(compartments))) {
    # Generate Compartment IDs
    new.id <- GenerateId(rv.ID$id.comp.seed, "compartment")
    comp.ids <- c(comp.ids, new.id$id)
    rv.ID$id.comp.seed <- new.id$seed
    # Store to id db
    idx.to.add <- nrow(rv.ID$id.df) + 1
    rv.ID$id.df[idx.to.add, ] <- c(new.id$id, comp.names[i])
    
    # Generate Volume IDs
    new.id <- GenerateId(rv.ID$id.param.seed, "parameter")
    vol.ids <- c(vol.ids, new.id$id)
    rv.ID$id.param.seed <- new.id$seed
    # Store id to db
    idx.to.add <- nrow(rv.ID$id.df) + 1
    rv.ID$id.df[idx.to.add, ] <- c(new.id$id, comp.vol.names[i])
    
  }
  
  comp.list     <- vector("list", n.compartments)
  comp.vol.list <- vector("list", n.compartments)
  # Add additional list tags for our problem
  for (i in seq_along(comp.list)) {
    # Build Compartment Entry
    comp.list[[i]]$ID              <- comp.ids[i]
    comp.list[[i]]$Name            <- comp.names[i]
    comp.list[[i]]$Value           <- comp.values[i]
    comp.list[[i]]$Volume          <- comp.vol.names[i]
    comp.list[[i]]$par.id          <- vol.ids[i]
    comp.list[[i]]$Unit            <- rv.UNITS$units.base$Volume
    comp.list[[i]]$UnitDescription <- "volume"
    comp.list[[i]]$BaseUnit        <- rv.UNITS$units.base$Volume
    comp.list[[i]]$BaseValue       <- comp.values[i]
    comp.list[[i]]$Description     <- ""
    
    comp.vol.list[[i]]$Name            <- comp.vol.names[i]
    comp.vol.list[[i]]$ID              <- vol.ids[i]
    comp.vol.list[[i]]$Value           <- comp.values[i]
    comp.vol.list[[i]]$Unit            <- rv.UNITS$units.base$Volume
    comp.vol.list[[i]]$UnitDescription <- "volume"
    comp.vol.list[[i]]$BaseUnit        <- rv.UNITS$units.base$Volume
    comp.vol.list[[i]]$BaseValue       <- comp.values[i]
    comp.vol.list[[i]]$Description     <- ""
    comp.vol.list[[i]]$Type            <- "Compartment"
    comp.vol.list[[i]]$Type.note       <- "Volume"
  }
  # TODO: Store compartment volume to  parameters
  names(comp.list) <- comp.ids
  
  # Assign to RV
  rv.COMPARTMENTS$compartments <- comp.list
  rv.COMPARTMENTS$compartments.df <- bind_rows(rv.COMPARTMENTS$compartments)
}

sbml_2_biomodme_species <- function(sbml.model) {
  # Current compartment values used by this program
  # Values:
  #   Name
  #   ID
  #   Value
  #   Unit
  #   UnitDescription
  #   BaseUnit
  #   BaseValue
  #   Description
  #   Compartment
  #   Compartment ID
  #   boundaryCondition (if true, differential eqn gen is ignored)
  
  rv.sbml.load.variables$need.species.conversion <- FALSE

  species <- sbml.model$species
  n.species <- nrow(species)

  # Species from SBML have the following columns
  #   id, name, initialConcentration, substanceUnits, compartment, constant,
  #   boundaryCondition
  
  species.id     <- species %>% pull(id)
  species.names  <- species %>% pull(name)
  species.values <- as.numeric(species %>% pull(initialConcentration))
  species.comp   <- species %>% pull(compartment)
  
  if (!identical(species.id, species.names)) {
    rv.sbml.load.variables$need.species.conversion <- TRUE
    rv.sbml.load.variables$species.df.conv <- 
      data.frame(species.id, species.names)
    colnames(rv.sbml.load.variables$species.df.conv) <- c("id", "name")
  }
  
  # Convert compartments names 
  if (rv.sbml.load.variables$need.compartment.conversion) {
    new.spec <- vector(mode = "character", length = length(species.comp))
    for (i in seq_along(species.comp)) {
      idx <- which(species.comp[i] %in% rv.sbml.load.variables$comp.df.conv$id)
      new.spec[i] <- rv.sbml.load.variables$comp.df.conv$name[idx]
    }
    species.comp <- new.spec
  }
  
  # Need Compartment Ids
  species.comp.id <- unname(sapply(species.comp, FindId))
  
  # Extract Boundary Condition
  species.bounds <- species %>% pull(boundaryCondition)
  
  # Generate Species IDs
  species.ids <- c()
  for (i in seq_len(nrow(species))) {
    # Generate Compartment IDs
    new.id <- GenerateId(rv.ID$id.var.seed, "var")
    species.ids <- c(species.ids, new.id$id)
    rv.ID$id.var.seed <- new.id$seed
    idx.to.add <- nrow(rv.ID$id.df) + 1
    rv.ID$id.df[idx.to.add, ] <- c(new.id$id, species.names[i])
  }
  
  species.list     <- vector("list", n.species)
  # Add additional list tags for our problem
  for (i in seq_along(species.list)) {
    # Build Compartment Entry
    species.list[[i]]$ID                <- species.ids[i]
    species.list[[i]]$Name              <- species.names[i]
    species.list[[i]]$Value             <- species.values[i]
    species.list[[i]]$Unit              <- rv.UNITS$units.base$For.Var
    species.list[[i]]$UnitDescription   <- "conc (mol)"
    species.list[[i]]$BaseUnit          <- rv.UNITS$units.base$For.Var
    species.list[[i]]$BaseValue         <- species.values[i]
    species.list[[i]]$Description       <- ""
    species.list[[i]]$Compartment       <- species.comp[i]
    species.list[[i]]$Compartment.id    <- species.comp.id[i]
    species.list[[i]]$boundaryCondition <- species.bounds[i]
    species.list[[i]]$Reaction.ids      <- NA
    species.list[[i]]$IO.ids            <- NA
  }
  
  
  names(species.list) <- species.ids
  
  # Assign to RV
  rv.SPECIES$species <- species.list
  rv.SPECIES$species.df <- bind_rows(rv.SPECIES$species)
  var.names <- rv.SPECIES$species.df %>% dplyr::select(Name)
  rv.SPECIES$species.names <- as.vector(unlist(var.names))
  rv.REFRESH$refresh.species.table <- rv.REFRESH$refresh.species.table + 1

}

sbml_2_biomodme_parameters <- function(sbml.model) {
  # Current Parmaeter values used by this program
  # Values:
  #   Name
  #   ID
  #   Value
  #   Unit
  #   UnitDescription
  #   BaseUnit
  #   BaseValue
  #   Description
  #   Type
  #   Type.Note
  
  # SMBL load passes a list with two different parameter dfs.
  # pars$parameters is constant parameters
  # pars$non.constant.parameters are non constant parameters which need to be 
  #     added to the appropriate RV

  rv.sbml.load.variables$need.parameter.conversion <- FALSE
  
  pars    <- sbml.model$parameters$Parameters
  nc.pars <- sbml.model$parameters$Variable.Parameters
  n.pars  <- nrow(pars)
  
  # Parameter Load has the following df columns:
  #   id, name, value, constant
  parameters.id    <- pars %>% dplyr::pull(id)
  parameters.names <- pars %>% dplyr::pull(name)
  par.vals         <- pars %>% dplyr::pull(value)
  par.constant     <- pars %>% dplyr::pull(constant)
  
  if (!identical(parameters.id, parameters.names)) {
    rv.sbml.load.variables$need.parameter.conversion <- TRUE
    rv.sbml.load.variables$parameter.df.conv <- 
      data.frame(parameters.id, parameters.names)
    colnames(rv.sbml.load.variables$species.df.conv) <- c("id", "name")
  }
  
  # Overwrite Ids
  par.ids  <- vector("character", n.pars)
  for (i in seq_len(nrow(pars))) {
    # Generate Parameter IDs
    new.id <- GenerateId(rv.ID$id.param.seed, "parameter")
    par.ids[i] <- new.id$id
    rv.ID$id.param.seed <- new.id$seed
    
    idx.to.add <- nrow(rv.ID$id.df) + 1
    rv.ID$id.df[idx.to.add, ] <- c(new.id$id, parameters.names[i])
  }
  
  par.list <- vector("list", n.pars)
  # TODO add custom to pars (change constant to custom and flip bool propbably)
  # Add additional list tags for our problem
  for (i in seq(n.pars)) {
    par.list[[i]]$Name            <- parameters.names[i]
    par.list[[i]]$ID              <- par.ids[i]
    par.list[[i]]$Value           <- as.numeric(par.vals[i])
    par.list[[i]]$Unit            <- NA
    par.list[[i]]$UnitDescription <- NA
    par.list[[i]]$BaseUnit        <- NA
    par.list[[i]]$BaseValue       <- as.numeric(par.vals[i])
    par.list[[i]]$Description     <- ""
    par.list[[i]]$Type            <- "Loaded From SBML File"
    par.list[[i]]$Type.note       <- ""
    par.list[[i]]$Used.In         <- NA
    par.list[[i]]$Custom          <- FALSE
    par.list[[i]]$ConstantValue   <- par.constant[i]
  }
  
  names(par.list) <- par.ids
  
  # Store information to our parameter tables
  rv.PARAMETERS$parameters <- par.list
  rv.PARAMETERS$parameters.df <- bind_rows(rv.PARAMETERS$parameters)
  
}

sbml_2_biomodme_functions <- function(sbml.model) {  
## Unpack SBML Function-------------------------------------------------------
  # Items in rv.CUSTOM.LAWS$cl.reaction:
  # ID                || Specific equation ID
  # Type              || Type of custom law (reaction, rate, etc)
  # Law.Name          || Display name shown on tables
  # Description       || Equation Description
  # Reactants         || Reactants in custom law
  # Products          || Products in custom law
  # Modifiers         || Modifiers (in reaction but no used for rate)
  # Parameters        || Parameters in equation
  # Parameter.Types   || Correlated parameter type to above (time, vol, param)
  # Equation.Text     || Text version of equation
  # Equation.Latex    || Latex text version of equation
  # Equation.Mathjax  || Mathjax text version of equation
  # String.Rate.Law   || String text for rate law
  # Latex.Rate.Law    || Latex version of rate law
  # MathJax.Rate.Law  || MathJax version of rate law
  # Rate.MathML       || MathMl for rate law
  # Reversible        || Bool if the equation is reversible or not
  
  
  #SBML Function Reader provides the following:
  # id
  # name
  # variables
  # law
  # Reactants
  # Products
  # Modifiers
  # Parameters
  
  reactions <- bind_rows(sbml.model$reactions)
  # Check if Functions Exist
  if (!isTruthy(sbml.model$functions)) {
    rv.CUSTOM.LAWS$cl.reaction <- list()
  } else {
    load.fxns <- sbml.model$functions
    for (i in seq_along(load.fxns)) {
      entry <- load.fxns[[i]]
      # Transfer Information
      law.name   <- entry$name
      backend    <- paste0("user_custom_law_", entry$id)
      reactants  <- entry$Reactants
      products   <- entry$Products
      modifiers  <- entry$Modifiers
      parameters <- entry$Parameters
      string.law <- entry$law
      
      # Build Information not in sbml load
      #-----------------------------------
      # Generate unique reaction ID
      ids <- GenerateId(rv.ID$id.custeqn.seed, "customEqn")
      unique.id <- ids[[2]]
      rv.ID$id.custeqn.seed <- ids[[1]]
      idx.to.add <- nrow(rv.ID$id.df) + 1
      rv.ID$id.df[idx.to.add, ] <- c(unique.id, backend)
      
      # Function Description 
      description <- law.name
      # Parameter Types
      param.type <-  rep("Parameter", length(SplitEntry(parameters)))
      param.type <-  collapseVector(param.type)
      
      # Equations Text
      eqn.builds <- BuildCustomEquationText(reactants,
                                            products,
                                            modifiers,
                                            parameters)
      eqn.text    <- eqn.builds$text
      eqn.latex   <- eqn.builds$latex
      eqn.mathjax <- eqn.builds$mathjax
      
      # Rate Law in latex, mathml, mathjax
      latex.rate    <- NA
      mathjax.rate  <- NA
      mathml.rate   <- NA
      tryCatch(
        expr = {
          law.converted <- ConvertRateLaw(string.law)
          latex.rate    <- law.converted$latex
          mathjax.rate  <- law.converted$mathjax
          mathml.rate   <- law.converted$mathml
        }
      )
      
      
      # Add Custom Law Data
      to.list <- list("ID" = unique.id,
                      "Type" = "Reaction",
                      "Law.Name" = backend,
                      "Description" = description,
                      "Reactants" = reactants,
                      "Products" = products,
                      "Modifiers" = modifiers,
                      "Parameters" = parameters,
                      "Parameter.Types" = param.type,
                      "Equation.Text" = eqn.text,
                      "Equation.Latex" = eqn.latex,
                      "Equation.Mathjax" = eqn.mathjax,
                      "String.Rate.Law" = string.law,
                      "Latex.Rate.Law" = latex.rate,
                      "MathJax.Rate.Law" = mathjax.rate,
                      "Rate.MathML" = mathml.rate,
                      "Reversible" = FALSE)
      
      rv.CUSTOM.LAWS$cl.reaction[[unique.id]] <- to.list
      
      # Add to reaction laws RV
      backend.entry <- backend
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
    }
  }}

sbml_2_biomodme_reactions <- function(sbml.model) {
  # Current Reaction values used by this program
  # ID                || Specific equation ID
  # Eqn.Display.Type  || Display name shown on tables
  # Reaction.Law      || Law that the equation uses
  # Species           || Species in equations
  # Reactants         || Reactants in reactions
  # Products          || Products in reactions
  # Modifiers          || Species in equations that arent involved in diff eqns
  # Rate.Constants    || Parameters in equation
  # Compartment       || Compartment reaction occurs in
  # Description       || Equation Description
  # Species.id        || IDs of species in reaction
  # Reactants.id      || IDs of reactants in reaction
  # Products.id       || IDs of products in reaction
  # Modifiers.id      || IDs of modifiers in model
  # Parameters.id     || IDs of parameters in model
  # Compartment.id    || ID of compartment eqn is in
  # Equation.Text     || Text version of equation
  # Equation.Latex    || Latex text version of equation
  # Equation.MathJax  || Mathjax text version of equation
  # String.Rate.Law   || String text for rate law
  # Latex.Rate.Law    || Latex version of rate law
  # MathJax.Rate.Law  || MathJax version of rate law
  # Rate.MathML       || MathMl for rate law
  # Reversible        || Bool if the equation is reversible or not
  # browser()
  reactions <- bind_rows(sbml.model$reactions)
  print(reactions)
  # Convert ids to names for values in reactions species and pars
  # Want to look at specific columns to convert
  
  # For species
  if (rv.sbml.load.variables$need.species.conversion) {
    for (i in seq_len(nrow(rv.sbml.load.variables$species.df.conv))) {
      old <- rv.sbml.load.variables$species.df.conv[i, 1]
      new <- rv.sbml.load.variables$species.df.conv[i, 2]
      
      # Reactants
      col <- reactions %>% pull(Reactants)
      reactions$Reactants <- RenameVarInDFColumn(old, new, col)
      
      # Products
      col <- reactions %>% pull(Products)
      reactions$Products <- RenameVarInDFColumn(old, new, col)
      
      # Modifiers
      col <- reactions %>% pull(Modifiers)
      reactions$Modifiers <- RenameVarInDFColumn(old, new, col)
      
      # Equation.Text
      col <- reactions %>% pull(Equation.Text)
      reactions$Equation.Text <- 
        RenameVarInDFColumn(old, new, col, isMath = TRUE)
    }
  }
  
  if (rv.sbml.load.variables$need.parameter.conversion) {
    # For parameters
    for (i in seq_len(nrow(rv.sbml.load.variables$parameter.df.conv))) {
      old <- rv.sbml.load.variables$parameter.df.conv[i, 1]
      new <- rv.sbml.load.variables$parameter.df.conv[i, 2]
      
      # Parameters
      col <- reactions %>% pull(Parameters)
      reactions$Parameters <- RenameVarInDFColumn(old, new, col)
    }
  }
  
  # For compartments
  # if (rv.sbml.load.variables$need.compartment.conversion) {
  #   for (i in seq_len(nrow(rv.sbml.load.variables$comp.df.conv))) {
  #     browser()
  #     old <- rv.sbml.load.variables$comp.df.conv[i, 1]
  #     new <- rv.sbml.load.variables$comp.df.conv[i, 2]
  #   
  #     # Equation.Text
  #     col <- reactions %>% pull(Compartments)
  #     reactions$Compartments <- RenameVarInDFColumn(old, new, col)
  #   }
  # }
  
  for (i in seq_len(nrow(reactions))) {
    entry <- reactions[i,]
    
    # Extract Reaction Main Inof
    ID.to.add   <- entry %>% pull(id)
    eqn.display <- entry %>% pull(description)
    
    # Extract Reactants
    reactants  <- SplitEntry(entry %>% pull(Reactants))
    products   <- SplitEntry(entry %>% pull(Products))
    modifiers  <- SplitEntry(entry %>% pull(Modifiers))
    parameters <- SplitEntry(entry %>% pull(Parameters))
    
    law.display   <- entry %>% pull(Reaction.Law)
    law.name      <- paste0("user_custom_law_", law.display)
    law.id        <- FindId(law.name)
    string.law <- entry %>% pull(Equation.Text)
    
    # IDK if this is the best way to really do this but it'll be a bandaid
    # for now.  Search for compartment names in string.law and replace them
    # with the volume term this application generated. 
    vectorized.law <- SplitEquationString(string.law)
    species.descriptions <- unname(sapply(rv.SPECIES$species,
                                          get,
                                          x = "Description"))
    comp.names <- 
      unname(
        sapply(
          rv.COMPARTMENTS$compartments,
          get,
          x = "Name"
        )
      )
    
    comp.vol.names <- 
      unname(
        sapply(
          rv.COMPARTMENTS$compartments,
          get,
          x = "Volume"
        )
      )
    
    for (j in seq_along(vectorized.law)) {
      if (vectorized.law[j] %in% comp.names) {
        # find idx of name and corresponding volume
        idx <- which(comp.names %in% vectorized.law[j])
        # replace in vectorized law
        vectorized.law[j] <- comp.vol.names[idx]
      }
    }
    
    # Condense vectorized law to new string law
    string.law <- collapseVector(vectorized.law, delimiter = " ")
    mathml.law <- entry %>% pull(MathMl.Rate.Law)
    reversible <- entry %>% pull(reversible)
    
    # Generate Reaction IDs
    ID.gen <- GenerateId(rv.ID$id.eqn.seed, "eqn")
    rv.ID$id.eqn.seed <- rv.ID$id.eqn.seed + 1
    ID.to.add <- ID.gen[["id"]]
    idx.to.add <- nrow(rv.ID$id.df) + 1
    rv.ID$id.df[idx.to.add, ] <- c(ID.to.add, paste0(eqn.display, 
                                                     " (",
                                                     string.law,
                                                     ")"))
    
    # Find IDs of species, reactants, products, and modifiers in reaction
    reactants.id <- c()
    for (j in seq_along(reactants)) {
      reactants.id[j] <- FindId(reactants[j])
    }
    
    products.id <- c()
    for (j in seq_along(products)) {
      products.id[j] <- FindId(products[j])
    }
    
    modifiers.id <- c()
    for (j in seq_along(modifiers)) {
      modifiers.id[j] <- FindId(modifiers[j])
    }
    
    parameters.id <- c()
    print("Parameters")
    print(parameters)
    for (j in seq_along(parameters)) {
      parameters.id[j] <- FindId(parameters[j])
    }
    
    species    <- RemoveNA(c(reactants, products))
    species.id <- RemoveNA(c(reactants.id, products.id))
    
    # Find which compartment the species are in and assign that
    compartment    <- rv.SPECIES$species[[species.id[1]]]$Compartment
    compartment.id <- rv.SPECIES$species[[species.id[1]]]$Compartment.id
    
    # Build equation text, latex, and mathjax
    eqn.builds <- BuildCustomEquationText(reactants,
                                          products,
                                          modifiers,
                                          parameters)
    
    text.eqn    <- eqn.builds$text
    latex.eqn   <- eqn.builds$latex
    mathjax.eqn <- eqn.builds$mathjax
    
    # Build rate laws from string
    convert.rate.law <- ConvertRateLaw(string.law)
    p.rate.law       <- NA
    latex.law        <- convert.rate.law$latex
    mathjax.law      <- convert.rate.law$mathjax
    mathml.law       <- katex::katex_mathml(latex.law)
    content.ml       <- entry %>% pull(MathMl.Rate.Law)
    
    par.collapsed          <- collapseVector(parameters)
    par.id.collapsed       <- collapseVector(parameters.id)
    reactants.collapsed    <- collapseVector(reactants)
    reactants.id.collapsed <- collapseVector(reactants.id)
    products.collapsed     <- collapseVector(products)
    products.id.collapsed  <- collapseVector(products.id)
    species.collapsed      <- collapseVector(species)
    species.id.collapsed   <- collapseVector(species.id)
    modifiers.collapsed    <- collapseVector(modifiers)
    modifiers.id.collapsed <- collapseVector(modifiers.id)
    
    # Add overall reaction information
    reaction.entry <- list(
      "ID"               = ID.to.add,
      "Eqn.Display.Type" = law.display,
      "Reaction.Law"     = law.name,
      "Backend.Call"     = law.id,
      "Species"          = species.collapsed,
      "Reactants"        = reactants.collapsed,
      "Products"         = products.collapsed,
      "Modifiers"        = modifiers.collapsed,
      "Parameters"       = par.collapsed,
      "Compartment"      = compartment,
      "Description"      = eqn.display,
      "Species.id"       = species.id.collapsed,
      "Reactants.id"     = reactants.id.collapsed,
      "Products.id"      = products.id.collapsed,
      "Modifiers.id"      = modifiers.id.collapsed,
      "Parameters.id"    = par.id.collapsed,
      "Compartment.id"   = compartment.id,
      "Equation.Text"    = text.eqn,
      "Equation.Latex"   = latex.eqn,
      "Equation.MathJax" = mathjax.eqn,
      "String.Rate.Law"  = string.law,
      "Pretty.Rate.Law"  = p.rate.law,
      "Latex.Rate.Law"   = latex.law,
      "MathJax.Rate.Law" = mathjax.law,
      "MathMl.Rate.Law"  = mathml.law,
      "Content.MathMl"   = content.ml,
      "Reversible"       = reversible
    )

    rv.REACTIONS$reactions[[ID.to.add]] <- reaction.entry
    
    # Add Reaction To Species
    for (jj in seq_along(species.id)) {
      if (is.na(rv.SPECIES$species[[species.id[jj]]]$Reaction.ids)) {
        rv.SPECIES$species[[species.id[jj]]]$Reaction.ids <- ID.to.add
      } else {
        items <- 
          strsplit(
            rv.SPECIES$species[[species.id[jj]]]$Reaction.ids, ", ")[[1]]
        items <- c(items, ID.to.add)
        rv.SPECIES$species[[species.id[jj]]]$Reaction.ids <- 
          paste0(items, collapse = ", ")
      }
    }
    print("REaction entery**************************")
    print(reaction.entry)
  }
  print(rbind(rv.REACTIONS$reactions))
  # for (i in seq_along(rv.REACTIONS$reactions)) {
  #   print(rv.REACTIONS$reactions[[i]]$Reactants)
  #   print(typeof(rv.REACTIONS$reactions[[i]]$Reactants))
  # }
  # 
  # for (i in seq_along(rv.REACTIONS$reactions)) {
  #   print(rv.REACTIONS$reactions[[i]]$Products)
  # }
  # 
  # print(do.call(rbind, rv.REACTIONS$reactions))
}

sbml_2_biomodme_rules <- function(sbml.model) {
  # Items in rv.CUSTOM.EQNS$ce.equations:
  # ID                || Specific equation ID
  # Equation          || Equation Description
  # New.Species       || New Species in this equation
  # New.Species.id    || Corresponding ids to above
  # New.Parameters    || New Parameters in this equation
  # New.Parameters.id || Corresponding ids to above
  # Old.Species       || Old Species in this equation
  # Old.Species.id    || Corresponding ids to above
  # Old.Parameters    || Old Parameters in this equation
  # Old.Parameters.id || Corresponding ids to above
  # Has.Time.Var      || Boolean if time var exists
  
  #SBML Rules Reader provides the following:
  # LHS.var
  # mathml
  # str.law
  
  # Check if rules exist
  if (!isTruthy(sbml.model$rules)) {
    rv.CUSTOM.EQNS$ce.equations <- list()
  } else {
    
    rules <- sbml.model$rules
    for (i in seq_along(rules)) {
      entry <- rules[[i]]
      
      # Unpack entry
      lhs.var <- entry$LHS.var
      rhs.eqn <- entry$str.law
      
      # Generate Unique ID
      ids <- GenerateId(rv.ID$id.custeqnaddional.seed, "custEqnAdditional")
      unique.id <- ids[[2]]
      rv.ID$id.custeqnaddional.seed <- ids[[1]]
      idx.to.add <- nrow(rv.ID$id.df) + 1
      rv.ID$id.df[idx.to.add, ] <- c(unique.id, paste0(lhs.var, "=", rhs.eqn))
      eqn.id <- unique.id
      
      # Build Equation from LHS.var and str.law
      
      eqn.out <- paste0(lhs.var, "=", rhs.eqn)
      
      # TODO: Split the reaction to extract variables.Determine if they are in
      # reaction already. If not assign them to parameters (I guess)
      # If we are loading, we would have to assume that all variables are
      # somewhere.
      
      vars.in.eqn <- parse_string_expression(eqn.out)$valid.terms
      par.names <- unname(sapply(rv.PARAMETERS$parameters,
                                 get,
                                 x = "Name"))
      existing.params <- c()
      existing.species <- c()
      par.ids <- c()
      spec.ids <- c()
      for (j in seq_along(vars.in.eqn)) {
        # Search if its in parameter
        if (vars.in.eqn[j] %in% par.names) {
          existing.params <- c(existing.params, vars.in.eqn[j])
          par.ids <- c(par.ids, FindId(vars.in.eqn[j]))
        } else {
          # Store in species list
          existing.species <- c(existing.species, vars.in.eqn[j])
          spec.ids <- c(spec.ids, FindId(vars.in.eqn[j]))
        }
      }
      
      # TODO: Need to check for time vars
      time.var.exists <- FALSE
      
      # Store to Output
      to.ce.list <- list("ID" = eqn.id,
                         "Equation" = eqn.out,
                         "New.Species" = NA,
                         "New.Species.id" = NA,
                         "New.Parameters" = NA,
                         "New.Parameters.id" = NA,
                         "Old.Species" = collapseVector(existing.species),
                         "Old.Species.id" = collapseVector(spec.ids),
                         "Old.Parameters" = collapseVector(existing.params),
                         "Old.Parameters.id" = collapseVector(par.ids),
                         "Has.Time.Var" = time.var.exists)
      
      rv.CUSTOM.EQNS$ce.equations[[eqn.id]] <- to.ce.list
    }
    
  }
}

LoadSBML_show_progress <- function(sbmlFile, w_sbml, spinner) {
  # This function is the same as LoadSBML but it is designed to show the 
  # progress bar screesn
  sleep.time <- 0.5
  error.in.load <- FALSE
  message.out <- "No ERROR"
  error.message <- "TEST: error in parsing sbml"
  
  out <- list()
  # Set initializers and bools
  
  exists.listOfCompartments        <- FALSE
  exists.listOfSpecies             <- FALSE
  exists.listOfParameters          <- FALSE
  exists.listOfRules               <- FALSE
  exists.listOfReactions           <- FALSE
  exists.listOfFunctionDefinitions <- FALSE
  exists.listOfUnitDefinitions     <- FALSE
  exists.parInReactions            <- FALSE
  
  function.definitions <- NA
  listOfParameters <- NA
  reaction.parameters.df <- NA
  compartment.df <- NA
  species.df <- NA
  rules.list <- NA
  
  message.log <- c()
  w_sbml$update(
    html = waiter_fxn(
      "Reading In SBML File",
      spinner, 
      10
    )
  )
  
  # Keep xml doc to remove eqn maths
  doc <- xmlTreeParse(sbmlFile, ignoreBlanks = TRUE)
  
  # Extract model from sbml file
  sbmlList <- read_xml(sbmlFile) %>% as_list()
  modelList <- sbmlList$sbml$model
  out[["model"]] <- modelList

  # Extract Compartments________________________________________________________
  if (!is.null(modelList$listOfCompartments)) {
    mes <- "Extracting Compartments"
    compartment.df <- Attributes2Tibble(modelList$listOfCompartments)

    # NA check for fidelity of data
    if (any(is.na(compartment.df))) {
      error.in.load <- TRUE
      
      rows_with_na <- which(apply(is.na(compartment.df), 1, any))
      
      concatenated_string <- 
        apply(compartment.df[rows_with_na, ], 
              1, 
              function(row) paste(row, collapse = " "))
      concatenated_string <- 
        paste0(
          "Error loading following compartement lines: ", 
          paste0(concatenated_string, collapse = ", ")
        )
      
      # return(list(model = NULL, error.message = error.message))
      return(list(model = NULL, error.message = concatenated_string))
    }
    
    # Polish Compartment Data
    compartment.df <- FinalizeCompartmentData(compartment.df)
    
    # Error return if compartment can't be finalized
    if (!is.null(compartment.df$out)) {
      compartment.df <- compartment.df$out
    } else {
      return(list(model = NULL, error.message = compartment.df$error))
    }
    
    # Store compartment information to model output
    out[["compartments"]] <- compartment.df
    exists.listOfCompartments <- TRUE
  } else {
    # Return error if sbml contains no compartments
    message <- "We currently do not support loading of models with no 
                compartment information."
    return(list(model = NULL, error.message = message))
  }
  
  # Update waiter to end compartment messages
  message.log <- c(message.log, mes)
  w_sbml$update(html = waiter_fxn(paste0(message.log, collapse = "/n"),
                                  spinner, 
                                  10))
  Sys.sleep(sleep.time)
  w_sbml$update(html = waiter_fxn("Extracting Species", spinner, 20))
  
  # Extract Species_____________________________________________________________
  if (!is.null(modelList$listOfSpecies)) {
    species.df <- Attributes2Tibble(modelList$listOfSpecies)
    # browser()
    # NA check for fidelity of data
    if (any(is.na(species.df))) {
      error.in.load <- TRUE
      rows_with_na <- which(apply(is.na(species.df), 1, any))
      cs <- apply(species.df[rows_with_na, ], 1, 
                  function(row) paste(row, collapse = " "))
      cs <-paste0("Error loading following species lines: ", 
                  paste0(cs, collapse = ", "))
      return(list(model = NULL, error.message = cs))
    }
    
    species.df <- FinalizeSpeciesData(species.df)
    
    # Error return if species can't be finalized
    if (!is.null(species.df$out)) species.df <- species.df$out
     else return(list(model = NULL, error.message = species.df$error))
    
    out[["species"]] <- species.df
    exists.listOfSpecies <- TRUE
  }
  Sys.sleep(sleep.time)
  w_sbml$update(html = waiter_fxn("Extracting Parameters", spinner, 30))
  
  # Extract Parameters__________________________________________________________
  if (!is.null(modelList$listOfParameters)) {
    listOfParameters <- Attributes2Tibble(modelList$listOfParameters)
    exists.listOfParameters <- TRUE
  }
  Sys.sleep(sleep.time)
  
  w_sbml$update(html = waiter_fxn("Extracting Rules", spinner, 35))
  # Extract Rules_______________________________________________________________
  if (!is.null(modelList$listOfRules)) {
    rules.header <- Attributes2Tibble(modelList$listOfRules)
    rules.assignment.vars <- rules.header %>% pull(variable)
    rules.list <- ExtractRulesMathFromSBML(doc, rules.assignment.vars)
    
    out[["rules"]] <- rules.list
    exists.listOfRules <- TRUE
  }
  Sys.sleep(sleep.time)
  
  w_sbml$update(html = waiter_fxn("Extracting Function Definitions", 
                                  spinner, 40))
  # Extract Function Definitions________________________________________________
  if (!is.null(modelList$listOfFunctionDefinitions)) {
    func.info <- Attributes2Tibble(modelList$listOfFunctionDefinitions)
    function.definitions <- ExtractFunctionDefFromSBML(doc, func.info)
    function.definitions <- FindFunctionDefInformation(doc,
                                                       function.definitions,
                                                       sbmlList)
    out[["functions"]] <- function.definitions
  }
  Sys.sleep(sleep.time)
  w_sbml$update(html = waiter_fxn("Extracting Reactions", 
                                  spinner, 50))
  # Extract Reactions___________________________________________________________
  if (!is.null(modelList$listOfReactions)) {
    # browser()
    exists.listOfReactions <- TRUE
    
    # Pull Reaction Tags
    reaction.tags <- ExtractionReactionTagFromSBML(modelList$listOfReactions)
    reaction.ids  <- reaction.tags %>% pull(id)
    
    # Loop through reactions grabbing relevant information
    reaction.list <- vector("list", length(modelList$listOfReactions))
    for (i in seq_along(modelList$listOfReactions)) {
      current.reaction <- modelList$listOfReactions[[i]]
      reaction.list[[i]] <- ExtractReactionBaseFromSBML(current.reaction)
      names(reaction.list)[i] <- reaction.ids[i]
    }
    
    # Check if Reaction Parameters Exist
    if (!is.na(reaction.list[[1]]$Parameter.Values)) {
      exists.parInReactions <- TRUE
      
      reaction.pars.name <- c()
      reaction.pars.id <- c()
      reaction.pars.vals <- c()
      for (ii in seq_along(reaction.list)) {
        reaction.pars.name <- 
          c(
            reaction.pars.name,
            SplitEntry(reaction.list[[ii]]$Parameters.name)
          )
        reaction.pars.id <- 
          c(
            reaction.pars.id,
            SplitEntry(reaction.list[[ii]]$Parameters.id)
          )
        reaction.pars.vals <- 
          c(
            reaction.pars.vals,
            SplitEntry(reaction.list[[ii]]$Parameter.Values)
          )
      }
      constant <- rep(TRUE, length(reaction.pars.vals))
      reaction.parameters.df <- data.frame(reaction.pars.id,
                                           reaction.pars.name, 
                                           reaction.pars.vals,
                                           constant)
      colnames(reaction.parameters.df) <- c("id", "name", "value", "constant")
    }
    print(reaction.list)
    # Add math to reactions list
    reaction.list <- ExtractReactionMathFromSBML(doc, 
                                                 reaction.list,
                                                 function.definitions)
    print(reaction.list)
    # Combine Tags With Reaction Math
    reaction.list <- CombineReactionTagsWReactions(reaction.tags,
                                                   reaction.list)
    
    out[["reactions"]] <- reaction.list
    
  }
  Sys.sleep(sleep.time)
  w_sbml$update(html = waiter_fxn("Combining Parameter Information", 
                                  spinner, 60))  # Bind Parameter lists if they both exist
  
  # Finalize Data Outputs to Normalize Output
  final.parameters.df <- FinalizeParameterData(listOfParameters,
                                               reaction.parameters.df,
                                               rules.list)
  out[["parameters"]] <- final.parameters.df
  
  if (error.in.load) {
    out <- NULL
    message.out <- error.message
  }
  
  Sys.sleep(sleep.time)
  to.return <- list(model = out, 
                    error.message = message.out)
  return(to.return)
}

# Load from sbml file (xml)
observeEvent(input$file_input_load_sbml, {

  sleep.time <- 5
  spinner <- RandomHTMLSpinner()
  w_sbml <- Waiter$new(
    id = "import_for_waiter",
    html = waiter_fxn("Loading SBML Model",
                                         spinner, 
                                         0))
  w_sbml$show()
  # TODO: Clear All current model information
  # tryCatch({
  #   sbml.model <- 
  #     LoadSBML_show_progress(
  #       input$file_input_load_sbml$datapath,
  #       w_sbml,
  #       spinner
  #     )
  #   sbml.model <- loaded.sbml$model
  #   if (is.null(sbml.model)) {
  #     sendSweetAlert(
  #       session = session,
  #       title = "Error...",
  #       text = sbml.model$error.message,
  #       type = "error"
  #     )
  #   }
  # }, 
  # error = function(e) {
  #   print(paste0("Error: ", e))
  #   w_sbml$hide()
  # })
  # browser
  loaded.sbml <- 
    LoadSBML_show_progress(
      input$file_input_load_sbml$datapath,
      w_sbml,
      spinner
    )
  sbml.model <- loaded.sbml$model
  if (is.null(sbml.model)) {
    w_sbml$hide()
    sendSweetAlert(
      session = session,
      title = "Error...",
      text = loaded.sbml$error.message,
      type = "error"
    )
    return(NULL)
  }
  ## Unpack SBML Compartments --------------------------------------------------
  
  mes <- "Converting Compartment information to BioModME..."
  w_sbml$update(html = waiter_fxn(mes,
                                  spinner, 65))
  Sys.sleep(sleep.time)
  
  tryCatch({
    sbml_2_biomodme_compartments(sbml.model)
  },
  error = function(e) {
    print(paste0("Error: ", e))
    w_sbml$hide()
  })
  
  print(rv.COMPARTMENTS$compartments.df)
  ## Unpack SBML Species --------------------------------------------------
  
  mes <- "Converting Species information to BioModME..."
  w_sbml$update(
    html = waiter_fxn(
      mes,
      spinner,
      70
    )
  )
  
  tryCatch({
    sbml_2_biomodme_species(sbml.model)
  }, 
  error = function(e) {
    print(paste0("Error: ", e))
    w_sbml$hide()
  })

  print(rv.SPECIES$species.df)

  ## Unpack SBML Params --------------------------------------------------------
  mes <- "Converting Parameter information to BioModME..."
  w_sbml$update(
    html = waiter_fxn(
      mes, 
      spinner, 
      80
    )
  )
  
  tryCatch({
    sbml_2_biomodme_parameters(sbml.model)
  },
  error = function(e) {
    print(paste0("Error: ", e))
    w_sbml$hide()
  })
  
  print(rv.PARAMETERS$parameters.df)
  
  ## Unpack SBML Functions -----------------------------------------------------
 
  
  mes <- "Converting Functions to BioModME..."
  w_sbml$update(
    html = waiter_fxn(
      mes,
      spinner, 
      82
    )
  )
  
  tryCatch({
    sbml_2_biomodme_functions(sbml.model)
  },
  error = function(e) {
    print(paste0("Error: ", e))
    w_sbml$hide()
  })
  print(rv.CUSTOM.LAWS$cl.reaction)
  
  ## Unpack SBML Reaction ____--------------------------------------------------
  mes <- "Converting Reactions to BioModME..."
  w_sbml$update(html = waiter_fxn(mes,
                                  spinner, 85))
  
  tryCatch({
    sbml_2_biomodme_reactions(sbml.model)
  },
  error = function(e) {
    print(paste0("Error: ", e))
    w_sbml$hide()
  })
  # browser()
  # print("reactions finished")
  # for (i in seq_along(rv.REACTIONS$reactions)) {
  #   print(rv.REACTIONS$reactions[[i]]$Modifiers)
  #   print(typeof(rv.REACTIONS$reactions[[i]]$Modifiers))
  # }
  print(as_tibble(
    do.call(rbind, rv.REACTIONS$reactions)))
  print("done bind_rows")

  ## Unpack SBML Rules-------------------------------------------------------
  mes <- "Converting Rules to BioModME..."
  w_sbml$update(html = waiter_fxn(mes,
                                  spinner, 95))
  
  tryCatch({
    sbml_2_biomodme_rules(sbml.model)
  },
  error = function(e) {
    print(paste0("Error: ", e))
    w_sbml$hide()
  })
  
  print(rv.CUSTOM.EQNS$ce.equations)

  # Finish load effects --------------------------------------------------------
  
  # Generate Differential Equations
  solveForDiffEqs()
  print("finish solving diffeqs")
  # End UI Trigger Events
  w_sbml$hide()
  
})
