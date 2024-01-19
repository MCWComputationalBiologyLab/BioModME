# Page contains server for exporting data, tables, latex docs etc. 

# Export RDS -------------------------------------------------------------------
output$export_save_data <- downloadHandler(
  filename = function(){
    paste(input$export_model_file_name, ".rds", sep = "")
  },
  content = function(file){
    mod.info  <- reactiveValuesToList(rv.MODEL.INFO)
    comp.temp <- reactiveValuesToList(rv.COMPARTMENTS)
    spec.temp <- reactiveValuesToList(rv.SPECIES)
    eqns.temp <- reactiveValuesToList(rv.REACTIONS)
    IO.temp   <- reactiveValuesToList(rv.IO)
    pars.temp <- reactiveValuesToList(rv.PARAMETERS)
    diff.temp <- reactiveValuesToList(rv.DE)
    opts.temp <- reactiveValuesToList(rv.SOLVER.OPTIONS)
    rslt.temp <- reactiveValuesToList(rv.RESULTS)
    info.temp <- reactiveValuesToList(rv.PROGRAM.INFO)
    logs.temp <- reactiveValuesToList(rv.LOGS)
    id.temp   <- reactiveValuesToList(rv.ID)
    pe.temp   <- reactiveValuesToList(rv.PAR.ESTIMATION)
    unit.temp <- reactiveValuesToList(rv.UNITS)
    react.law <- reactiveValuesToList(rv.REACTIONLAWS)
    CL.temp   <- reactiveValuesToList(rv.CUSTOM.LAWS)
    CE.temp   <- reactiveValuesToList(rv.CUSTOM.EQNS)
    
    to.save <- c(mod.info,
                 comp.temp,
                 spec.temp,
                 eqns.temp,
                 IO.temp,
                 pars.temp,
                 diff.temp,
                 opts.temp,
                 rslt.temp,
                 info.temp,
                 logs.temp,
                 id.temp,
                 pe.temp, 
                 unit.temp,
                 react.law,
                 CL.temp,
                 CE.temp)

    saveRDS(to.save, file)
  }
)

output$dbttn_header_download_model <- downloadHandler(
  filename = function(){
    # paste0("model", Sys.time(), ".rds")
    "BioModME_model.rds"
  },
  content = function(file){
    mod.info  <- reactiveValuesToList(rv.MODEL.INFO)
    comp.temp <- reactiveValuesToList(rv.COMPARTMENTS)
    spec.temp <- reactiveValuesToList(rv.SPECIES)
    eqns.temp <- reactiveValuesToList(rv.REACTIONS)
    IO.temp   <- reactiveValuesToList(rv.IO)
    pars.temp <- reactiveValuesToList(rv.PARAMETERS)
    diff.temp <- reactiveValuesToList(rv.DE)
    opts.temp <- reactiveValuesToList(rv.SOLVER.OPTIONS)
    rslt.temp <- reactiveValuesToList(rv.RESULTS)
    info.temp <- reactiveValuesToList(rv.PROGRAM.INFO)
    logs.temp <- reactiveValuesToList(rv.LOGS)
    id.temp   <- reactiveValuesToList(rv.ID)
    pe.temp   <- reactiveValuesToList(rv.PAR.ESTIMATION)
    unit.temp <- reactiveValuesToList(rv.UNITS)
    react.law <- reactiveValuesToList(rv.REACTIONLAWS)
    CL.temp   <- reactiveValuesToList(rv.CUSTOM.LAWS)
    CE.temp   <- reactiveValuesToList(rv.CUSTOM.EQNS)
    
    to.save <- c(mod.info,
                 comp.temp,
                 spec.temp,
                 eqns.temp,
                 IO.temp,
                 pars.temp,
                 diff.temp,
                 opts.temp,
                 rslt.temp,
                 info.temp,
                 logs.temp,
                 id.temp,
                 pe.temp, 
                 unit.temp,
                 react.law,
                 CL.temp,
                 CE.temp)
    
    saveRDS(to.save, file)
  }
)

# Export SBML ------------------------------------------------------------------
output$export_save_as_sbml <- downloadHandler(
  filename = function(){
    paste(input$export_model_file_name, ".xml", sep = "")
  },
  content = function(file) {
    
    # Functions to create SBML model
    compartments <- createSBMLCompartmentExport(rv.COMPARTMENTS$compartments)
    species      <- createSBMLSpeciesExport(rv.SPECIES$species)
    parameters   <- createSBMLParameterExport(rv.PARAMETERS$parameters,
                                              rv.COMPARTMENTS$compartments,
                                              rv.ID$id.df)
    reactions    <- createSBMLReactionExport(rv.REACTIONS$reactions,
                                             rv.PARAMETERS$parameters,
                                             rv.COMPARTMENTS$compartments,
                                             rv.ID$id.df)
    print("IOS =-----------=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=")
    IOs          <- createInputOutputExport(rv.IO$InputOutput,
                                            rv.PARAMETERS$parameters,
                                            rv.COMPARTMENTS$compartments,
                                            rv.ID$id.df)
    # 
    # print("FINDISNOH IOS")
    # print(reactions)
    # print(IOs)
    # print(length(reactions))
    # print(length(IOs))
    # print(c(reactions, IOs))
    print(reactions[[1]])
    print(IOs[[1]])
    reactions <- c(reactions, IOs)
    # Once IO is working properly, we should merge reactions and IOs
    functions    <- createSBMLFunctionExport(rv.CUSTOM.LAWS$cl.reaction)
    rules        <- createSBMLRulesExport(rv.CUSTOM.EQNS$ce.equations)
    # Build SBML Output Model
    model <- list("compartments" = compartments,
                  "species" = species,
                  "parameters" = parameters,
                  "reactions" = reactions, 
                  "functions" = functions,
                  "rules" = rules)

    f.name <- paste(input$export_model_file_name, ".xml", sep = "")

    # Write SBML
    sbml.model <- createSBML(model, rv.ID$id.df)
    xml.model  <- xmlParse(sbml.model)
    XML::saveXML(xml.model, file)
  }
)

rename_variables <- function(lst, old_names, new_names) {
  # Converts names of lists of lists 
  # @lst - list of lists to have value changes
  # @old_names - vector of names to be changed
  # @new_names - vector of names to change to
  
  # Example: 
  # my_list <- list(
  #   list(a = 1, b = 2),
  #   list(a = 3, b = 4),
  #   list(a = 5, b = 6)
  # )
  # 
  # old.names <- c("a", "b")
  # new.names <- c("new1", "new2")
  # 
  # new_list <- rename_variables(my_list, old.names, new.names)
  # Output: 
  #   [[1]]
  # [[1]]$new1
  # [1] 1
  # 
  # [[1]]$new2
  # [1] 2
  # 
  # 
  # [[2]]
  # [[2]]$new1
  # [1] 3
  # 
  # [[2]]$new2
  # [1] 4
  # 
  # 
  # [[3]]
  # [[3]]$new1
  # [1] 5
  # 
  # [[3]]$new2
  # [1] 6
  
  for (i in seq_along(old_names)) {
    names(lst)[names(lst) == old_names[i]] <- new_names[i]
  }
  return(lst)
}

createSBMLRulesExport <- function(customEqnsRV) {
  # Our rules would need to extract the following: 
  # varName    <- entry$variable
  # mathml.law <- entry$mathml.eqn
  # 
  # The relevant structure of customEQNSRV is list item: Equation
  # "varName = mathml.law"
  # ex. varProd = Var1 + Var2/Var3
  
  # It would appear we need to break our equation and separate varName 
  # vs mathml.law.
  # We would have to convert the equation split to content mathml. 
  
  rules <- vector(mode = "list", length = length(customEqnsRV))
  
  for (i in seq_along(customEqnsRV)) {
    entry <- customEqnsRV[[i]]
    
    # Build Variables
    eqn <- strsplit(entry$Equation, "=")[[1]]
    varName <- gsub(" ", "", eqn[1])
    string.law <- gsub(" ", "", eqn[2])
    # Convert to mathml 
    mathml.law <- string2mathml(string.law)
    mathml.law <- 
      paste0('<math xmlns=\"http://www.w3.org/1998/Math/MathML\">',
             mathml.law,
             "</math>")
    out <- list(variable = varName,
                mathml.eqn = mathml.law)
    
    rules[[i]] <- out
  }
  return(rules)
}

createSBMLFunctionExport <- function(customLawsRV) {
  # Converts custom laws to sbml exportable form
  # @customLawsRV - (list) of list of parameters (rv.CUSTOM.LAWS$cl.reaction)
  
  functions <- vector(mode = "list", length = length(customLawsRV))
  
  for (i in seq_along(customLawsRV)) {
    # Build variables
    reactants  <- SplitEntry(customLawsRV[[i]]$Reactants)
    products   <- SplitEntry(customLawsRV[[i]]$Products)
    modifiers  <- SplitEntry(customLawsRV[[i]]$Modifiers)
    parameters <- SplitEntry(customLawsRV[[i]]$Parameters)
    to.test  <- RemoveNA(c(reactants, products, modifiers, parameters))
    
    # check if variable in law
    law        <- customLawsRV[[i]]$String.Rate.Law
    variables <- c()
    for (j in seq_along(to.test)) {
      if (to.test[j] %in% law.vars) {
        variables <- c(to.test[j], variables)
      }
    }

    # Grab items from RV that correspond to SBML structure
    id         <- customLawsRV[[i]]$ID
    name       <- customLawsRV[[i]]$Law.Name
    
    
    # Store to list entry
    entry <- list(id = id,
                  name = name,
                  law = law,
                  variables = collapseVector(variables))
    if (!is.null(variables)) {
      functions[[i]] <- entry
    }
    
  }

  return(functions)
}

createSBMLReactionExport <- function(reactionRV, 
                                     parameterRV, 
                                     compartmentRV, 
                                     idRV) {
  # Converts reaction reactive variable to sbml exportable form
  # @reactionRV - (list) of list of parameters (rv.REACTIONS$reactions)
  reactions <- vector(mode = "list", length = length(reactionRV))
  for (i in seq_along(reactionRV)) {
    # Grab items from RV that correspond to SBML structure
    id         <- reactionRV[[i]]$ID
    name       <- reactionRV[[i]]$Eqn.Display.Type
    reversible <- reactionRV[[i]]$Reversible
    fast       <- "false"
    
    reactants  <- reactionRV[[i]]$Reactants.id
    products   <- reactionRV[[i]]$Products.id
    modifiers  <- reactionRV[[i]]$Modifiers.id
    parameters <- reactionRV[[i]]$Parameters.id
    eqn.text   <- reactionRV[[i]]$Equation.Text
    func.name  <- reactionRV[[i]]$Reaction.Law
    func.id    <- reactionRV[[i]]$Backend.Call
    if (!isTruthy(func.id)) {
      func.id <- NA
    }
    
    parameter.names <- reactionRV[[i]]$Parameters
    
    par.split <- SplitEntry(parameters)
    # Find Parameter values
    par.vals <- vector(mode = "numeric", 
                       length = length(par.split))
    
    for (j in seq_along(par.split)) {
      par.vals[j] <- parameterRV[[par.split[j]]]$BaseValue
    }
    
    par.vals <- collapseVector(par.vals)
    # browser()
    # I want to replace volumes here with compartment names to match typical 
    # sbml format.
    # Grab vector of compartment volume names
    # result_vector <- sapply(input_vector, function(x) findVarId(x, id.dict))
    comp.vol.names <- unname(sapply(compartmentRV, get, x = "par.id"))
    comp.vol.names <- 
      unname(
        sapply(
          comp.vol.names, 
          function(x) FindIdName(x, idRV)
        )
      )
    comp.names <- unname(sapply(compartmentRV, get, x = "Name"))
    
    # Break string law into components.  
    # Check if components match volume terms
    # Do proper replacing and storing.
    string.law  <- reactionRV[[i]]$String.Rate.Law
    for (j in seq_along(comp.vol.names)) {
      string.law <- 
        SubstituteSingleRateLawTerm(string.law, 
                                    comp.vol.names[j], 
                                    comp.names[j])
    }
    
    # Store to list entry
    entry <- list(id = id,
                  name = name,
                  reversible = reversible,
                  fast = fast,
                  reactants = reactants,
                  products = products,
                  modifiers = modifiers,
                  parameters = parameters,
                  parameter.names = parameter.names,
                  parameter.values = par.vals,
                  eqn.text = eqn.text,
                  string.law = string.law,
                  function.name = func.name,
                  function.id = func.id)
    
    reactions[[i]] <- entry
  }
  
  return(reactions)
}

createInputOutputExport <- function(IORV,
                                    parameterRV,
                                    compartmentRV,
                                    idRV) {
  # Converts the input output reactive variable to an exportable sbml format
  # IO variables: 
  # ID                || ID for the I/O
  # Direction         || (Input, Output, Both)
  # Display.Type      || Display name of the I/O
  # Law -             || Type of law I/O follows
  # Compartment.Out   || Compartment that flow is leaving from 
  # Compartment.In    || Compartment that flow is going to 
  # Species.Out       || Species leaving a compartment
  # Species.In        || Species from inflow to compartment
  # Parameters        || Parameters used in flow
  # Description       || Description of the IO occuring
  # Compartment.id    || ID of compartment eqn is in
  # Species.id        || IDs of species in model
  # Parameter.Ids     || IDs of parameters in model
  # Equation.Text     || Text version of equation
  # Equation.Latex    || Latex text version of equation
  # Equation.MathJax  || Mathjax text version of equation
  # String.Rate.Law   || String text for rate law
  # Latex.Rate.Law    || Latex version of rate law
  # MathJax.Rate.Law  || MathJax version of rate law
  # Rate.MathML       || MathMl for rate law
  # Reversible        || Bool if the equation is reversible or not
  
  # We need to convert these to reaction output for sbml so we need the 
  # following variables: 
  # id = id,
  # name = name,
  # reversible = reversible,
  # fast = fast,
  # reactants = reactants,
  # products = products,
  # modifiers = modifiers,
  # parameters = parameters,
  # parameter.names = parameter.names,
  # parameter.values = par.vals,
  # eqn.text = eqn.text,
  # string.law = string.law,
  # function.name = func.name,
  # function.id = func.id
  
  # It would seem that parameters and parameter.names and taking the same 
  # id values 
  # 
  # reactants would probably be flow out/species (minus) and products would be 
  # flow/species in (plus)
  # 
  # The hard part of this is extracting flow between.

  # Count the total number of entries in reactions
  
  total_entries <- 0
  for (i in seq_along(IORV)) {
    name <- IORV[[i]]$Type

    if (name == "FLOW_BETWEEN") {
      # Split string.law by ","
      laws <- strsplit(IORV[[i]]$String.Rate.Law, ",")[[1]]
      
      # Add the number of laws to the total entries
      total_entries <- total_entries + length(laws)
    } else {
      # Non FLOW_BETWEEN entry
      total_entries <- total_entries + 1
    }
  }
  
  reactions <- vector(mode = "list", length = total_entries)
  reactions_counter <- 1
  
  # Loop through IORV to populate reactions
  for (i in seq_along(IORV)) {
    name <- IORV[[i]]$Type
    
    par.split <- SplitEntry(IORV[[i]]$Parameter.Ids)
    # Find Parameter values
    par.vals <- vector(mode = "numeric", 
                       length = length(par.split))
    
    for (j in seq_along(par.split)) {
      par.vals[j] <- parameterRV[[par.split[j]]]$BaseValue
    }
    
    
    # Standard entry
    if (name != "FLOW_BETWEEN") {
      par.vals <- collapseVector(par.vals)
      
      entry <- list(id = IORV[[i]]$ID,
                    name = name,
                    reversible = "false",
                    fast = "false",
                    reactants = IORV[[i]]$Species.Out.Ids,
                    products = IORV[[i]]$Species.In.Ids,
                    modifiers = NA,
                    parameters = IORV[[i]]$Parameter.Ids,
                    parameter.names = IORV[[i]]$Parameters,
                    parameter.values = par.vals,
                    eqn.text = IORV[[i]]$Equation.Text,
                    string.law = IORV[[i]]$String.Rate.Law,
                    function.name = IORV[[i]]$Type,
                    function.id = ifelse(isTruthy(IORV[[i]]$Backend.Call), 
                                         IORV[[i]]$Backend.Call, 
                                         NA))
      entry <- lapply(entry, function(x) ifelse(x == "NA", NA, x))
      reactions[[reactions_counter]] <- entry
      reactions_counter <- reactions_counter + 1
      
    } else {
      # browser()
      # FLOW_BETWEEN entry
      # These are split so the first is the out term, leaving compartment,
      # and the next term will be the in term . So in this case, the first 
      # term will be its own reaction with the species in being reactant.
      # The next laws will only have a product term with the next string law.
  
      laws <- SplitEntry(IORV[[i]]$String.Rate.Law)
      
      # Perform element extraction
      reactants  <- SplitEntry(IORV[[i]]$Species.Out)
      products   <- SplitEntry(IORV[[i]]$Species.In)
      parameters <- SplitEntry(IORV[[i]]$Parameters)
      
      # Parameters are IDs so we will need to convert them to names here
      # par.names <- unname(sapply(parameters, function(x) FindIdName(x, idRV)))
      
      for (j in seq_along(laws)) {
        law <- laws[j]
        if (j == 1) {
          rct.in.law <- FindId(reactants)
          prd.in.law <- NA
        } else {
          rct.in.law <- NA
          prd.in.law <- FindId(products[j-1])
        }
        
        par.in.law <- parameters[j]
        par.ids.in.law <- FindId(par.in.law)
        par.val.to.add <- as.character(par.vals[j])
        
        entry <- list(id = paste0(IORV[[i]]$ID, "_", reactions_counter),
                      name = name,
                      reversible = "false",
                      fast = "false",
                      reactants = rct.in.law,
                      products = prd.in.law,
                      modifiers = NA,
                      parameters = par.ids.in.law,
                      parameter.names = par.in.law,
                      parameter.values = par.val.to.add,
                      eqn.text = IORV[[i]]$Equation.Text,
                      string.law = law,
                      function.name = IORV[[i]]$Type,
                      function.id = ifelse(isTruthy(IORV[[i]]$Backend.Call), 
                                           IORV[[i]]$Backend.Call, 
                                           NA))
        entry <- lapply(entry, function(x) ifelse(x == "NA", NA, x))
        reactions[[reactions_counter]] <- entry
        reactions_counter <- reactions_counter + 1
      }
    }
  }
  print("Printing final IO structure????????????>?<><><><><><><?><?><")
  return(reactions)
}

createSBMLParameterExport <- function(parameterRV,
                                      compartmentRV,
                                      idRV) {
  # Converts parameter reactive variable to sbml exportable form
  # @parameterRV - (list) of list of parameters (rv.PARAMETERS$parameters)
  
  parameters <- vector(mode = "list", length = length(parameterRV))
  comp.vol.names <- unname(sapply(compartmentRV, get, x = "par.id"))
  idx.to.remove <- c()
  
  for (i in seq_along(parameterRV)) {
    
    # Grab items from RV that correspond to SBML structure
    id      <- parameterRV[[i]]$ID
    name    <- parameterRV[[i]]$Name
    value   <- parameterRV[[i]]$BaseValue
    cont <- if (parameterRV[[i]]$Custom) "false" else "true"
    
    # Store to list entry
    entry <- list(id = id,
                  name = name,
                  value = value,
                  constant = cont)
    parameters[[i]] <- entry
    
    if (id %in% comp.vol.names) {
      idx.to.remove <- c(idx.to.remove, i)
    }
  }
  
  parameters <- parameters[-idx.to.remove]
  return(parameters)
}

createSBMLSpeciesExport <- function(speciesRV) {
  # Converts species reactive variable to sbml exportable form
  # @ speciesRV - (list) of list of species (rv.SPECIES$species)
  
  species <- vector(mode = "list", length = length(speciesRV))
  
  for (i in seq_along(speciesRV)) {
    
    # Grab items from RV that correspond to SBML structure
    id         <- speciesRV[[i]]$ID
    name       <- speciesRV[[i]]$Name
    init.conc  <- speciesRV[[i]]$BaseValue
    sub.units  <- "species"
    compart    <- speciesRV[[i]]$Compartment.id
    cont       <- "false"
    bc         <- ifelse(speciesRV[[i]]$BoundaryCondition, "true", "false")
    
    # Store to list entry
    entry <- list(id = id,
                  name = name,
                  initialConcentration = init.conc,
                  substanceUnits = sub.units,
                  compartment = compart,
                  constant = cont,
                  boundaryCondition = bc)
    
    species[[i]] <- entry
  }
  return(species)
}

createSBMLCompartmentExport <- function(compartmentsRV) {
  # @compartmentsRV - compartments list with information 
  # (compartmentsRV$compartments)
  # Takes compartment reactive variable and builds sbml term structure
  compartments <- vector(mode = "list", length = length(compartmentsRV))
  # browser()
  for (i in seq_along(compartmentsRV)) {
    
    id = compartmentsRV[[i]]$ID
    name = compartmentsRV[[i]]$Name
    size = compartmentsRV[[i]]$BaseValue
    constant = "true"
    spatialDimensions = 3
    
    entry <- list(id = compartmentsRV[[i]]$ID,
                  name = compartmentsRV[[i]]$Name,
                  size = compartmentsRV[[i]]$BaseValue,
                  constant = "true",
                  spatialDimensions = 3)
    
    compartments[[i]] <- entry
  }
  
  return(compartments)
 
}

# Export Matlab Code -----------------------------------------------------------
output$export_data_to_matlab_script <- downloadHandler(
  filename = function(){
    paste0(input$export_code_file_name, ".m")
  },
  content = function(file){
    my_matlab_file <- create_matlab_model_function(
      rv.SPECIES$species.names, 
      rv.PARAMETERS$parameters.names,
      rv.DE$de.eqns.for.solver,
      rv.PARAMETERS$parameters.df$BaseValue,
      rv.CUSTOM.EQNS$ce.equations,
      rv.SPECIES$species.df$BaseValue,
      rv.SOLVER.OPTIONS$time.scale.bool,
      rv.SOLVER.OPTIONS$time.scale.value,
      rv.SOLVER.OPTIONS$time.start,
      rv.SOLVER.OPTIONS$time.end,
      rv.SOLVER.OPTIONS$time.step)
    writeLines(my_matlab_file, file)
  }
)

# Export to R Script -----------------------------------------------------------
output$export_data_to_R_script <- downloadHandler(
  filename = function(){
    paste0(input$export_code_file_name, ".R")
  },
  content = function(file){
    my.R.file <- CreateRModel(rv.SPECIES$species.names,
                              rv.PARAMETERS$parameters.names, 
                              rv.PARAMETERS$parameters.df$BaseValue,
                              rv.SPECIES$species.df$BaseValue,
                              rv.CUSTOM.EQNS$ce.equations,
                              rv.DE$de.eqns.for.solver,
                              rv.SOLVER.OPTIONS$time.scale.bool,
                              rv.SOLVER.OPTIONS$time.scale.value,
                              rv.SOLVER.OPTIONS$ode.solver.type,
                              rv.SOLVER.OPTIONS$time.start,
                              rv.SOLVER.OPTIONS$time.end,
                              rv.SOLVER.OPTIONS$time.step)
    writeLines(my.R.file, file)
  }
)

# Download Latex Document ------------------------------------------------------
output$export_latex_document <- downloadHandler(
  filename = function(){"latex_test_script.txt"},
  content = function(file){
    add.eqn.headers <- FALSE
    add.eqn.descriptions <- FALSE
    #pull values from checkboxgroups
    if ("show_eqn_type" %in% input$latex_additional_options) {
      add.eqn.headers <- TRUE
    }
    if ("show_eqn_description" %in% input$latex_additional_options) {
      add.eqn.descriptions <- TRUE
    }
    
    #bools for pages to add for latex doc
    page.add.var <- FALSE
    page.add.eqns <- FALSE
    page.add.add.eqns <- FALSE
    page.add.IO <- FALSE
    page.add.param <- FALSE
    page.add.diffeqs <- FALSE
    
    if ("Variable" %in% input$latex_pages_to_add) {
      page.add.var <- TRUE
    }
    if ("Equations" %in% input$latex_pages_to_add) {
      page.add.eqns <- TRUE
    }
    if ("Additional Equations" %in% input$latex_pages_to_add) {
      page.add.add.eqns <- TRUE
    }
    if ("Input/Output" %in% input$latex_pages_to_add) {
      page.add.IO <- TRUE
    }
    if ("Parameter Table" %in% input$latex_pages_to_add) {
      page.add.param <- TRUE
    }
    if ("Differential Eqns" %in% input$latex_pages_to_add) {
      page.add.diffeqs <- TRUE
    }
    
    # Pull all species descriptions
    species.descriptions <- unname(sapply(rv.SPECIES$species,
                                          get,
                                          x = "Description"))
    latex.species <- SpeciesInModel(rv.SPECIES$species.names, 
                                    species.descriptions)
    
    # Pull latex equations
    reaction.vec <- unname(sapply(rv.REACTIONS$reactions,
                                  get,
                                  x = "Equation.Latex"))
    descript.vec <- unname(sapply(rv.REACTIONS$reactions,
                                  get,
                                  x = "Description"))
    
    latex.eqns <- ReactionsToLatex(reaction.vec,
                                   add.eqn.headers,
                                   add.eqn.descriptions,
                                   descript.vec)
    
    additional.eqns <- unname(sapply(rv.CUSTOM.EQNS$ce.equations,
                                     get,
                                     x = "Equation"))
    # 
    latex.IO <- GenerateIOTable(rv.IO$InputOutput)
    latex.addEqns <- AdditionalEqnsToLatex(additional.eqns)
    latex.paramTable <-
      GenerateParameterTable(rv.PARAMETERS$parameters.df$Name,
                             rv.PARAMETERS$parameters.df$Value,
                             rv.PARAMETERS$parameters.df$Description)
    latex.diffEqs <-
      DifferentialEqnsInModel(rv.DE$de.equations.list)
    
    out <- ""
    if (page.add.var)      {out <- paste0(out, latex.species)}
    if (page.add.eqns)     {out <- paste0(out, latex.eqns)}
    if (page.add.add.eqns) {out <- paste0(out, latex.addEqns)}
    if (page.add.IO)       {out <- paste0(out, latex.IO)}
    if (page.add.param)    {out <- paste0(out, latex.paramTable)}
    if (page.add.diffeqs)  {out <- paste0(out, latex.diffEqs)}

    latex.file <- GenerateLatexDocument(out)
    #latex.file <- GenerateLatexDocument(latex.eqns)
    writeLines(latex.file, file)
  }
)

# Tables To View/Print ---------------------------------------------------------
# Here we render specific model tables to view and export

## Species ---------------------------------------------------------------------
output$table_species_export <- renderDT({
  for.table <- rv.SPECIES$species.df %>%
    select("Name", "Value", "Unit", "Compartment", "Description")

  DT::datatable(
    for.table,
    rownames = FALSE,
    class = "cell-border stripe",
    extensions = c('Buttons', "RowReorder", "ColReorder"),
    options = list(
      pageLength = -1,
      # autoWidth = TRUE,
      # ordering = TRUE,
      # order = list(c(0 , 'asc')),
      columnDefs = list(
        list(width = "20%", targets = 0),
        list(width = "10%", targets = c(1,2,3)),
        list(width = "50%", targets = 4),
        list(className = 'dt-center', targets = c(0,1,2,3)),
        list(className = 'dt-left', targets = 4)
      ),
      dom = 'Bt',
      buttons = list("copy",
                     list(extend = "csv",   filename = "Species"),
                     list(extend = "excel", filename = "Species"),
                     list(extend = "pdf",   filename = "Species"),
                     "print"
      )
    )
  )
})
## Compartments ----------------------------------------------------------------
output$table_compartments_export <- renderDT({
  for.table <- rv.COMPARTMENTS$compartments.df %>%
    select("Name", "Volume", "Value", "Unit", "Description")
  
  DT::datatable(
    for.table,
    rownames = FALSE,
    editable = TRUE,
    class = "cell-border stripe",
    extensions = c('Buttons', "RowReorder", "ColReorder"),
    options = list(
      # autoWidth = TRUE,
      # ordering = TRUE,
      # order = list(c(0 , 'asc')),
      columnDefs = list(
        list(width = "20%", targets = 0),
        list(width = "10%", targets = c(1,2,3)),
        list(width = "50%", targets = 4),
        list(className = 'dt-center', targets = c(0,1,2,3)),
        list(className = 'dt-left', targets = 4)
      ),
      dom = 'Bt',
      buttons = list("copy",
                     list(extend = "csv",   filename = "Compartments"),
                     list(extend = "excel", filename = "Compartments"),
                     list(extend = "pdf",   filename = "Compartments"),
                     "print"
      )
    )
  )
})

## Parameters ------------------------------------------------------------------
output$table_parameters_export <- renderDT({
  for.table <- rv.PARAMETERS$parameters.df %>% 
    select("Name", "Value", "Unit", "Description")
  DT::datatable(
    for.table,
    class = "cell-border stripe",
    extensions = c('Buttons', "RowReorder", "ColReorder"),
    rownames = FALSE,
    options = list(
      # autoWidth = TRUE,
      pageLength = -1,
      ordering = TRUE,
      colReorder = TRUE,
      columnDefs = list(
        list(width = "20%", targets = 0),
        list(width = "15%", targets = c(1,2)),
        list(width = "50%", targets = 3),
        list(className = 'dt-center', targets = c(0, 1, 2)),
        list(className = 'dt-left', targets = 3)
      ),
      dom = 'Bt',
      buttons = list("copy",
                      list(extend = "csv",   filename = "Parameters"),
                      list(extend = "excel", filename = "Parameters"),
                      list(extend = "pdf",   filename = "Parameters"),
                      "print"
      )
    )
  )
})

## Reactions -------------------------------------------------------------------
output$table_reactions_export <- renderDT({
  t <- rv.REACTIONS$reactions.df %>%
    select(Equation.Text,
           Eqn.Display.Type,
           Compartment)

  t <- as.data.frame(t)
  colnames(t) <- c("Equation",
                   "Type",
                   "Compartment")
  datatable(
    t,
    rownames = FALSE,
    class = "cell-border stripe",
    extensions = 'Buttons',
    options = list(
      dom = 'Bt',
      lengthMenu = list(c(-1), c("All")),
      buttons = list(
        "copy",
        list(extend = "csv",   filename = "Reactions"),
        list(extend = "excel", filename = "Reactions"),
        list(extend = "pdf",   filename = "Reactions"),
        "print"
      )
    )
  )
})

## Diff Eqns -------------------------------------------------------------------
output$table_differential_equation_export <- renderDT({

  species <- unname(sapply(rv.DE$de.equations.list,
                           get,
                           x = "Name"))
  eqn <- unname(sapply(rv.DE$de.equations.list,
                       get,
                       x = "ODES.eqn.string"))

  tab <- data.frame(species, eqn)
  colnames(tab) <- c("Species", "Differential Equation")
  datatable(
    tab,
    rownames = FALSE,
    class = "cell-border stripe",
    extensions = 'Buttons',
    options = list(
      dom = 'Bt',
      lengthMenu = list(c(-1), c("All")),
      buttons = list(
        "copy",
        list(extend = "csv",   filename = "DifferentialEquations"),
        list(extend = "excel", filename = "DifferentialEquations"),
        list(extend = "pdf",   filename = "DifferentialEquations"),
        "print"
      )
    )
  )
})


output$Dbttn_export_diffeqn_mathml <- downloadHandler(
  filename = function(){
    "mathml_diff_eqns.txt"
  },
  content = function(file) {
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
  }
)


