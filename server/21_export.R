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
    parameters   <- createSBMLParameterExport(rv.PARAMETERS$parameters)
    reactions    <- createSBMLReactionExport(rv.REACTIONS$reactions,
                                             rv.PARAMETERS$parameters)
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
    print("Model processed from export to sbml")
    
    # Write SBML
    sbml.model <- createSBML(model)
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
  
}

createSBMLFunctionExport <- function(customLawsRV) {
  # Converts custom laws to sbml exportable form
  # @customLawsRV - (list) of list of parameters (rv.CUSTOM.LAWS$cl.reaction)
  
  functions <- vector(mode = "list", length = length(customLawsRV))
  
  for (i in seq_along(customLawsRV)) {
    print("LOOPING")
    print(i)
    print(customLawsRV[[i]])
    # Build variables
    reactants  <- SplitEntry(customLawsRV[[i]]$Reactants)
    products   <- SplitEntry(customLawsRV[[i]]$Products)
    modifiers  <- SplitEntry(customLawsRV[[i]]$Modifiers)
    parameters <- SplitEntry(customLawsRV[[i]]$Parameters)
    to.test  <- RemoveNA(c(reactants, products, modifiers, parameters))
    
    # check if variable in law
    law        <- customLawsRV[[i]]$String.Rate.Law
    law.vars  <- SplitEquationString(law)
    print(law)
    print(law.vars)
    print(to.test)
    variables <- c()
    for (j in seq_along(to.test)) {
      if (to.test[j] %in% law.vars) {
        variables <- c(to.test[j], variables)
      }
    }
    print(variables)
    
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
  print("Finished Function Export")
  print(functions)
  return(functions)
}

createSBMLReactionExport <- function(reactionRV, parameterRV) {
  # Converts reaction reactive variable to sbml exportable form
  # @reactionRV - (list) of list of parameters (rv.REACTIONS$reactions)
  print("STARTED REACTION ESPORTS")
  reactions <- vector(mode = "list", length = length(reactionRV))
  # browser()
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
    func.name  <- reactionRV[[i]]$Reaction.Law
    func.id    <- reactionRV[[i]]$Backend.Call
    if (!isTruthy(func.id)) {
      func.id <- NA
    }
    
    parameter.names <- reactionRV[[i]]$Parameters
    
    print(parameters)
    print(parameter.names)
    par.split <- SplitEntry(parameters)
    # Find Parameter values
    par.vals <- vector(mode = "numeric", 
                       length = length(par.split))
    
    for (j in seq_along(par.split)) {
      par.vals[j] <- parameterRV[[par.split[j]]]$BaseValue
    }
    
    par.vals <- collapseVector(par.vals)
    
    string.law  <- reactionRV[[i]]$String.Rate.Law
    
    
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
                  string.law = string.law,
                  function.name = func.name,
                  function.id = func.id)
    
    reactions[[i]] <- entry
  }
  print(reactions)
  print("FINISHED REACTION ESPORTS")
  return(reactions)
}

createSBMLParameterExport <- function(parameterRV) {
  # Converts parameter reactive variable to sbml exportable form
  # @parameterRV - (list) of list of parameters (rv.PARAMETERS$parameters)
  
  parameters <- vector(mode = "list", length = length(parameterRV))
  
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
  }
  
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
    # print(i)
    # print(compartmentsRV)
    # print(compartmentsRV[[i]])
    
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
        list(extend = "csv",   filename = "DifferentialEquations"),
        list(extend = "excel", filename = "DifferentialEquations"),
        list(extend = "pdf",   filename = "DifferentialEquations"),
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
    print(eqns)
    # Convert to mathml
    mathml.eqns <- c()
    for (i in seq_along(eqns)) {
      temp <- 
        paste0(
          "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">",
          string2mathml(eqns[i]),
          "</math>"
          )
      print(temp)
      mathml.eqns <- c(mathml.eqns, temp)
    }
    
    mathml.eqns <- paste0(mathml.eqns, collapse = "\n")
    writeLines(mathml.eqns, file)
  }
)


