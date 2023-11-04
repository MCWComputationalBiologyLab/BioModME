
Attributes2Tibble <- function(xmlAttributeStruct) {
  # When parsing sbml things get weird. Convert these structures to df
  out.list <- list()
  for (i in seq_along(xmlAttributeStruct)) {
    out.list[[i]] <- unlist(attributes(xmlAttributeStruct[[i]]))
  }
  
  return(bind_rows(out.list))
}

listToXml <- function(item, tag){
  if(typeof(item)!='list')
    return(xmlNode(tag, item))
  xml <- xmlNode(tag)
  for(name in names(item)){
    xml <- append.xmlNode(xml, listToXml(item[[name]], name))
  }
  return(xml)
}

LoadSBML <- function(sbmlFile) {
  # This function is an overall load of an smbl file using two different
  # models. It creates an xml tree and parses that when needed, usually, for 
  # importing anything in mathml. Otherwise, the SBML file can be read in 
  # using read_xml and converted to a list and have its components extracted
  # from the list, usually by converting relevant list components to tibbles.
  
  # Check if certain structures exist:
  # Search For
  #     Compartments
  #     Species
  #     Parameters
  #     Function Definitions
  #     Reactions
  #     Rules
  #     Unit Definitions
  #     Model Information
  print("Running Load SBML Function")
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
  
  # Keep xml doc to remove eqn maths
  doc <- xmlTreeParse(sbmlFile, ignoreBlanks = TRUE)
  
  # Extract model from sbml file
  sbmlList <- read_xml(sbmlFile) %>% as_list()
  modelList <- sbmlList$sbml$model
  out[["model"]] <- modelList
  # print(names(modelList))
  
  print("COMPARTMENTS")
  # Extract Compartments
  if (!is.null(modelList$listOfCompartments)) {
    compartment.df <- Attributes2Tibble(modelList$listOfCompartments)
    compartment.df <- FinalizeCompartmentData(compartment.df)
    out[["compartments"]] <- compartment.df
    exists.listOfCompartments <- TRUE
  }

  print("SPECIES")
  # Extract Species
  if (!is.null(modelList$listOfSpecies)) {
    species.df <- Attributes2Tibble(modelList$listOfSpecies)
    species.df <- FinalizeSpeciesData(species.df)
    out[["species"]] <- species.df
    exists.listOfSpecies <- TRUE
  }
  
  print("PARAMETERS")
  # Extract Parameters
  if (!is.null(modelList$listOfParameters)) {
    listOfParameters <- Attributes2Tibble(modelList$listOfParameters)
    exists.listOfParameters <- TRUE
  }
  
  print("RULES")
  # Extract Rules
  if (!is.null(modelList$listOfRules)) {
    rules.header <- Attributes2Tibble(modelList$listOfRules)
    rules.assignment.vars <- rules.header %>% pull(variable)
    rules.list <- ExtractRulesMathFromSBML(doc, rules.assignment.vars)
    
    out[["rules"]] <- rules.list
    exists.listOfRules <- TRUE
  }
  
  print("FUNCTION DEFINTIONS")
  # Extract Function Definitions
  if (!is.null(modelList$listOfFunctionDefinitions)) {
    func.info <- Attributes2Tibble(modelList$listOfFunctionDefinitions)
    function.definitions <- ExtractFunctionDefFromSBML(doc, func.info)
    function.definitions <- FindFunctionDefInformation(doc,
                                                       function.definitions,
                                                       sbmlList)
    out[["functions"]] <- function.definitions
  }
  
  print("REACTIONS")
  # Extract Reactions
  if (!is.null(modelList$listOfReactions)) {
    exists.listOfReactions <- TRUE
    
    print("REACTION TAGS")
    # Pull Reaction Tags
    reaction.tags <- ExtractionReactionTagFromSBML(modelList$listOfReactions)
    reaction.ids  <- reaction.tags %>% pull(id)
    
    print("REACTION BASE")
    # Loop through reactions grabbing relevant information
    reaction.list <- vector("list", length(modelList$listOfReactions))
    for (i in seq_along(modelList$listOfReactions)) {
      current.reaction <- modelList$listOfReactions[[i]]
      reaction.list[[i]] <- ExtractReactionBaseFromSBML(current.reaction)
      names(reaction.list)[i] <- reaction.ids[i]
    }
    
    print("REACTION PARAMETER EXTRACTION")
    # Check if Reaction Parameters Exist
    if (!is.na(reaction.list[[1]]$Parameter.Values)) {
      exists.parInReactions <- TRUE
      reaction.pars <- c()
      reaction.pars.vals <- c()
      for (ii in seq_along(reaction.list)) {
        reaction.pars <- c(reaction.pars,
                           SplitEntry(reaction.list[[ii]]$Parameters))
        reaction.pars.vals <- c(reaction.pars.vals,
                                SplitEntry(reaction.list[[ii]]$Parameter.Values))
      }
      reaction.parameters.df <- data.frame(reaction.pars, reaction.pars.vals)
      colnames(reaction.parameters.df) <- c("Parameters", "Values")
    }
    
    print("REACTION MATH")
    # Add math to reactions list
    reaction.list <- ExtractReactionMathFromSBML(doc, 
                                                 reaction.list,
                                                 function.definitions)
    
    print("COMBINE REACTION TAGS AND MATH")
    # Combine Tags With Reaction Math
    reaction.list <- CombineReactionTagsWReactions(reaction.tags,
                                                   reaction.list)
    
    out[["reactions"]] <- reaction.list
    
  }
  
  # Finalize Data Outputs to Normalize Output
  final.parameters.df <- FinalizeParameterData(listOfParameters,
                                               reaction.parameters.df,
                                               rules.list)
  
  out[["parameters"]] <- final.parameters.df
  print(final.parameters.df)
  return(out)
}

# Parameter Finalizing ---------------------------------------------------------

FinalizeSpeciesData <- function(speciesFromSBML) {
  # Finalize the Output of species data specifying outputs
  # Inputs: 
  #   @speciesFromSBML - Main load from sbml listOfSpecies
  # Outputs: 
  #   (tibble) id, name, initialConcentration, substanceUnits, compartment, 
  #            constant, boundaryCondition
  
  # Throw error if compartments don't exist
  if (isTruthy(speciesFromSBML)) {
    if (nrow(speciesFromSBML) == 0) {
      stop("SBML file contains no species")
    }
  } else {
    stop("SBML file contains no species")
  }
  
  out <- speciesFromSBML
  n.species <- nrow(out)
  
  # The most basic smbl files seem to have id, compartment so we
  # can assume those are in load and check for remaining terms
  # Terms to check for:
  # name
  # substanceUnits
  # constant
  # boundaryCondition
  # initialConcentration
  
  # Check for name
  if (!isTruthy(speciesFromSBML$name)) {
    name <- out %>% pull(id)
    # Bind to output
    out <- cbind(out, name)
  }
  
  # Check for initialConcentration - the issue here is some files use 
  # initialConcentration and some use initialAmount
  if (isTruthy(speciesFromSBML$initialAmount)) {
    initialConcentration <- out %>% pull(initialAmount)
    # Bind to output
    out <- cbind(out, initialConcentration)
  }
  
  # Check for substanceUnits
  if (!isTruthy(speciesFromSBML$substanceUnits)) {
    substanceUnits <- rep("species", n.species)
    # Bind to output
    out <- cbind(out, substanceUnits)
  }
  
  # Check for constant
  if (!isTruthy(speciesFromSBML$constant)) {
    constant <- rep(FALSE, n.species)
    # Bind to output
    out <- cbind(out, constant)
  } else {
    # Convert from string to bool
    out$constant <- as.logical(out$constant)
  }
  
  # Convert boundaryCondition to bool
  if (!isTruthy(speciesFromSBML$boundaryCondition)) {
    boundaryCondition <- rep(FALSE, n.species)
    out <- cbind(out, boundaryCondition)
  } else {
    out$boundaryCondition <- as.logical(out$boundaryCondition)
  }
  
  # print("Species")
  # print(out)
  # Sort Column Order and remove excess columns
  column.order <- c("id", 
                    "name", 
                    "initialConcentration", 
                    "substanceUnits", 
                    "compartment", 
                    "constant",
                    "boundaryCondition")
  
  out <- out %>% select(column.order)
  # print("SPECIS OUT")
  # print(out)
  # Return Output
  return(out)
}

FinalizeCompartmentData <- function(compartmentsFromSBML) {
  # Finalize the Output of compartment data specifying outputs
  # Inputs: 
  #   @compartmentsFromSBML - Main load from sbml listOfCompartments
  # Outputs: 
  #   (tibble) id, name, size, units, constant
  
  # Throw error if compartments don't exist
  if (isTruthy(compartmentsFromSBML)) {
    if (nrow(compartmentsFromSBML) == 0) {
      stop("SBML file contains no compartments")
    }
  } else {
    stop("SBML file contains no compartments")
  }
  
  out <- compartmentsFromSBML
  n.compartments <- nrow(out)
  
  # Need to check that all outputs exist, otherwise add them with standards
  
  # Most sbmls seem to have size and id so I will ignore those
  if (!isTruthy(compartmentsFromSBML$name)) {
    name <- out %>% pull(id)
    # Bind to output
    out <- cbind(out, name)
  }
  
  if (!isTruthy(out$units)) {
    units <- rep("volume", n.compartments)
    out <- cbind(out, units)
  }
  
  if (!isTruthy(out$spatialDimensions)) {
    spatialDimensions <- rep("3", n.compartments)
    out <- cbind(out, spatialDimensions)
  }
  
  if (!isTruthy(out$constant)) {
    constant <- rep(TRUE, n.compartments)
    out <- cbind(out, constant)
  } else {
    # Convert from string to bool
    out$constant <- as.logical(out$constant)
  }

  # Sort Column Order
  column.order <- c("id", "name", "size", "constant", "spatialDimensions")
  out <- out %>% select(column.order)
  
  # Return Output
  return(out)
}

FinalizeParameterData <- function(parsFromSBMLMain,
                                  parsFromReactions,
                                  rulesFromSBML) {
  
  # The purpose of this function is to create a standardized data structure
  # regardless of how the parameter information is stores in sbml.  SBML can 
  # store the data in different places, with different notations, and, to me, 
  # this does not appear to be level dependent. Same level/versions have 
  # different structure storage (could be from different programs making them)
  
  # Inputs: 
  #   @parsFromSBMLMain: Parameter database extracted from <listOfParameters>
  #   @parsFromReactions: Parameter db extracted from reactions > kineticlaw
  #   @rulesFromSBML: Custom Rules extracted from <listOfRules>
  
  # Outputs: 
  # Two values: constant and non constant parameters.
  # Dataframe consisting of relevant parameter df in the following structure: 
  # id, name, value, constant
  # Non-constant (maybe just a vector of string expressions)
  
  main.par.exist  <- FALSE
  react.par.exist <- FALSE
  rules.exist     <- FALSE
  
  browser()
  print("Finalize Parameter Information")
  # Check which of the inputs exist
  if (isTruthy(parsFromSBMLMain)) {
    if (nrow(parsFromSBMLMain) > 0) {
      main.par.exist <- TRUE
      
      out <- parsFromSBMLMain
      # Always seem to have id, value
      
      # Add name and constant if not 
      
      if (!isTruthy(out$name)) {
        name <- out %>% pull(id)
        out <- cbind(out, name)
      }
      if (!isTruthy(out$constant)) {
        constant <- rep(TRUE, nrow(out))
        out <- cbind(out, constant)
        # print("REP DONE")
      }
      out <- out %>% select(c("id", 
                                     "name",
                                     "value",
                                     "constant"))
    }
  }
   print(out)
  # Check for reaction parameters
  if (isTruthy(parsFromReactions)) {
    if (nrow(parsFromReactions) > 0) {
      react.par.exist <- TRUE
      df <- parsFromReactions %>% select(any_of(c("Parameters",
                                                  "Values")))
      colnames(df) <- c("id", "value")
      if (main.par.exist) {
        # Merge the two dfs
        name <- df %>% pull(id)
        constant <- rep(TRUE, length(name))
        df <- cbind(df, data.frame(name, constant))
        # print(df)
        # print(out)
        out <- rbind(out, df)
      } else {
        # add name and constant column
        name <- df %>% pull(id)
        constant <- rep(TRUE, length(name))
        out <- cbind(df, data.frame(name, constant))
      }
    }
  }
  
  # Deal with constant values, they pull from sbml as lowercase true/false which
  # will register as a string, and we want them to be bools.  As well some 
  # instances get pulled as NA and we want those to be TRUE. 
  
  out$constant[is.na(out$constant)] <- "true"
  # Convert to logical
  out$constant <- as.logical(out$constant)
  
  # Pull out all nonconstant parameters
  constant.parameters <- out %>% filter(constant)
  non.constant.parameters <- out %>% filter(!constant)
  
  # Assign rules to the value of nonconstant parameters
  if (isTruthy(rulesFromSBML)) {
    if (length(rulesFromSBML) > 0) {
      rules.vars <- unname(sapply(rulesFromSBML, get, x = "LHS.var"))
      rules.law  <- unname(sapply(rulesFromSBML, get, x = "str.law"))
      # Check if rules.var in non constant parameters
      for (i in seq_along(rules.vars)) {
        if (rules.vars[i] %in% non.constant.parameters$name) {
          # Add the rules law to the "value column" after finding idx
          idx <- which(non.constant.parameters$name %in% rules.vars[i])
          non.constant.parameters$value[idx] <- rules.law[i]
        } else {
          # Add it to the dataframe
          row.to.add <- c(rules.vars[i], rules.vars[i], rules.law[i], FALSE)
          non.constant.parameters <- rbind(non.constant.parameters, row.to.add)
        }
      }
      colnames(non.constant.parameters) <- c("id", "name", "value", "constant")
    }
  }
  
  column.order <- c("id", "name", "value", "constant")
  constant.parameters <- constant.parameters %>% 
                         select(column.order)%>%
                         dplyr::distinct()
  
  out <- list("Parameters" = constant.parameters,
              "Variable.Parameters" = non.constant.parameters)
  
  return(out)
}

# Reaction Pull Functions ------------------------------------------------------
ExtractionReactionTagFromSBML <- function(reactionXML) {
  # Extract the tagline on Reactions that contains information that can 
  # includes id, reversible, name, fast
  # Inputs:
  # @ reactionXML: modelList$listOfReactions
  
  # Create Tags Tibble
  tags <- Attributes2Tibble(reactionXML)
  # print(head(tags))
  # Check which terms exist
  to.pull <- c()
  if (!is.null(tags$id)) {to.pull <- c(to.pull, "id")}
  if (!is.null(tags$reversible)) {to.pull <- c(to.pull, "reversible")}
  if (!is.null(tags$name)) {to.pull <- c(to.pull, "name")}
  if (!is.null(tags$fast)) {to.pull <- c(to.pull, "fast")}
  # PrintVar(to.pull)
  out <- tags %>% select(to.pull)
  return(out)
}

ExtractReactionBaseFromSBML <- function(reactionEntry) {
  # Inputs: 
  #   @reaction.entry: current.reaction <- modelList$listOfReactions[[i]]
  # Cycle through reaction entry tags pull reaction information
  
  # Some SBML files have parameter information below the kinetic law in 
  # reaction entries but some don't and instead list that information in a 
  # XML node "listOfParameters" on the base level with all parameters. So,
  # we need to check for that. Some seem to have both.
  
  out.list <- list("Reactants"  = NA,
                   "Products"   = NA,
                   "Modifiers"  = NA,
                   "Parameters" = NA,
                   "Parameter.Values" = NA)
  
  for (i in seq_along(reactionEntry)) {
    current.node <- reactionEntry[i]
    node.name <- names(current.node)
    
    if (node.name == "listOfReactants") {
      # Convert node to Tibble
      node.reactants <- Attributes2Tibble(current.node$listOfReactants)
      
      # Grab the species from tibble, collapse, add to output
      out.list$Reactants <- collapseVector(node.reactants %>% pull(species),
                                           convertBlank = TRUE)
    } else if (node.name == "listOfProducts") {
      # Convert node to Tibble
      node.products <- Attributes2Tibble(current.node$listOfProducts)
      
      # Grab the species from tibble, collapse, add to output
      out.list$Products <- collapseVector(node.products %>% pull(species),
                                          convertBlank = TRUE)
    } else if (node.name == "listOfModifiers") {
      # Convert node to Tibble
      node.modifiers <- Attributes2Tibble(current.node$listOfModifiers)
      
      # Grab the species from tibble, collapse, add to output
      out.list$Modifiers <- collapseVector(node.modifiers %>% pull(species),
                                          convertBlank = TRUE)
    } else if (node.name == "kineticLaw") {
      # Check if parameter node exists
      node.par <- Attributes2Tibble(current.node$kineticLaw$listOfParameters)
      browser()
      if (ncol(node.par) != 0) {
        # IF PARAMETER INFORMATION IN REACTION XML INFO

        out.list$Parameters <- collapseVector(node.par %>% pull(id), 
                                              convertBlank = TRUE)
        out.list$Parameter.Values <- collapseVector(node.par %>% pull(value), 
                                                    convertBlank = TRUE)
        if (!is.null(node.par$name)) {
          out.list$Parameters <- collapseVector(node.par %>% pull(name),
                                                convertBlank = TRUE)
        } else {
          out.list$Parameters <- collapseVector(node.par %>% pull(id), 
                                                convertBlank = TRUE)
        }
      } 
    } 
  }
  return(out.list)
}

ExtractReactionMathFromSBML <- function(doc, 
                                        reactionList, 
                                        functionList) {
  # I want this function to grab all relevant reaction information from the 
  # sbml but nothing more.  So we will look at extraction the following 
  # reaction information:
  # Name, Id, Reactants, Products, Modifiers, Parameters, String Rate Law
  
  # xmlDoc - parsed xml doc from xmltreeparse
  # reactionList - list of reactions to update
  
  # Check to see if function definitions exist
  functions.exist <- FALSE
  if (isTruthy(functionList)) {
    if (length(functionList) > 0) {
      functions.exist <- TRUE
      functions.names <- unname(sapply(functionList, get, x = "id"))
    }
  }
  
  # Pull Reaction Information from reactionList input
  reactions <- doc$doc$children$sbml[["model"]][["listOfReactions"]]
  n.reactions <- length(reactions)
  
  for (i in seq_along(reactions)) {
    
    # Grab information on Reactants, Products, Modifiers
    # This information should already be in reactionlist from base extraction
    reactants  <- SplitEntry(reactionList[[i]]$Reactants)
    products   <- SplitEntry(reactionList[[i]]$Products)
    modifiers  <- SplitEntry(reactionList[[i]]$Modifiers)
    # PrintVar(modifiers)
    
    # Grab string of mathml.exp for function check
    mathml.string <- toString(reactions[[i]][["kineticLaw"]][["math"]])
    
    # Grab mathml expression for processing to rate law
    mathml.exp <- reactions[[i]][["kineticLaw"]][["math"]][[1]]
    
    equation.uses.function <- FALSE
    if (functions.exist) {
      # Check to see if entry uses a function definition
      for (j in seq_along(functions.names)) {
        fxn.check <- CheckForTermInMathml(mathml.string, functions.names[j])
        if (fxn.check$term.found) {
          # Perform reaction extraction
          equation.uses.function <- TRUE
          function.terms <- fxn.check$function.terms
          function.id <- functions.names[j]
          
          # Extract function information
          function.entry <- functionList[[j]]
          function.vars  <- SplitEntry(function.entry$variables)
          reaction.law   <- function.entry$id
          
          # Grab Function Variables adn Rate law
          function.rate.law   <- function.entry$law
          function.reactants  <- SplitEntry(function.entry$Reactants)
          function.products   <- SplitEntry(function.entry$Products)
          function.modifiers  <- SplitEntry(function.entry$Modifiers)
          function.parameters <- SplitEntry(function.entry$Parameters)
          
          # Check to see if reaction parameters were already extracted and if
          # not then extract them
          if (!is.na(reactionList[[i]]$Parameters)) {
            parameters <- SplitEntry(reactionList[[i]]$Parameters)
          } else {
            species <- c(reactants, products, modifiers)
            species <- RemoveNA(species)
            if (isTruthy(which(function.terms %in% species))) {
              parameters <- 
                function.terms[-(which(function.terms %in% species))]
            } else {
              parameters <- function.terms
            }
          }
          
          # Calculate Rate Law By Substitution
          string.rate.law <- SubstituteRateLawTerms(function.rate.law,
                                                    function.reactants,
                                                    function.products,
                                                    function.modifiers,
                                                    function.parameters,
                                                    reactants,
                                                    products,
                                                    modifiers,
                                                    parameters)
          break
        }
      }
    }
    
    # Extraction of reaction information if not function based
    if (!equation.uses.function) {
      reaction.law <- "CUSTOM"
      # Convert mathml to string rate law for r
      string.rate.law <- rmp(gsub(" ", "", convertML2R(mathml.exp)))
      
      # Grab Parameters
      if (!is.na(reactionList[[i]]$Parameters)) {
        parameters <- SplitEntry(reactionList[[i]]$Parameters)
      } else {
        species <- c(reactants, products, modifiers)
        species <- RemoveNA(species)
        def.terms <- extract_variables(string.rate.law)
        if (isTruthy(which(def.terms %in% species))) {
          parameters <- 
            def.terms[-(which(def.terms %in% species))]
        } else {
          parameters <- def.terms
        }
      }
    }
    
    # Condense Variables
    par.collapsed       <- collapseVector(parameters, convertBlank = TRUE)
    reactants.collapsed <- collapseVector(reactants, convertBlank = TRUE)
    products.collapsed  <- collapseVector(products, convertBlank = TRUE)
    modifiers.collapsed <- collapseVector(modifiers, convertBlank = TRUE)
    
    reactionList[[i]] <- list(
      "Reaction.Law"     = reaction.law,
      "Reactants"        = reactants.collapsed,
      "Products"         = products.collapsed, 
      "Modifiers"        = modifiers.collapsed,
      "Parameters"       = par.collapsed,
      "Equation.Text"    = string.rate.law,
      "MathMl.Rate.Law"  = mathml.string
    )
    # reactionList[[i]] <- to.add
  }
  
  return(reactionList)
}

CombineReactionTagsWReactions <- function(reactionTags,
                                          reactionList) {
  
  n.reactions <- length(reactionList)
  
  # Check for tags we need to grab
  if (!is.null(reactionTags$id)) {
    ids <- reactionTags %>% pull(id)
  } else {
    ids <- rep(NA, n.reactions)
  }
  
  if (!is.null(reactionTags$reversible)) {
    is.reversible <- reactionTags %>% pull(reversible)
  } else {
    is.reversible <- rep(FALSE, n.reactions)
  }
  
  if (!is.null(reactionTags$name)) {
    description <- reactionTags %>% pull(name)
  } else {
    description <- rep("Custom Load Reaction", n.reactions)
  }
  
  if (!is.null(reactionTags$fast)) {
    fast <- reactionTags %>% pull(fast)
  } else {
    fast <- rep(FALSE, n.reactions)
  }
  
  for (i in seq_along(reactionList)) {
    reactionList[[i]]$id <- ids[i]
    reactionList[[i]]$description <- description[i]
    reactionList[[i]]$reversible <- is.reversible[i]
    reactionList[[i]]$fast <- fast[i]
  }
  
  return(reactionList)
}

# Function Definition Pull Functions -------------------------------------------
FindFunctionDefInformation <- function(doc, functionDefList, sbmlList) {
  # This is meant to assign reactants, products, modifiers, and parameters to 
  # functionDefList so we have these variables for the loaded model.
  # Inputs: 
  #   @functionDefList: (list) of function definitions 
  #                     (from ExtractFunctionDefFromSBML)
  #   @sbmlList: (list) sbml components 
  #              (from read_xml(pathToXMLFile) %>% as_list()) 
  modelList <- sbmlList$sbml$model
  # Iterating function definitions, the iterating reactions to find matching 
  # function id in the reaction. From there we will extract reaction info to 
  # build up the proper function definition.
  idx.to.remove  <- c()
  name.to.remove <- c()
  for (i in seq_along(functionDefList)) {
    function.id <- functionDefList[[i]]$id
    match.found <- FALSE
    for (j in seq_along(modelList$listOfReactions)) {
      # Separate current reaction node
      current.reaction <- modelList$listOfReactions[[j]]
      # print(current.reaction)
      # Pull math law and check if it contains the current search fxn
      reactions <- doc$doc$children$sbml[["model"]][["listOfReactions"]]
      
      # Extract mathml expression and make string
      mathml.exp <- reactions[[j]][["kineticLaw"]][["math"]][[1]]
      mathml.exp.string <- toString(reactions[[j]][["kineticLaw"]][["math"]])
      # Search if the function id exists in the mathml string
      if (grepl(function.id, mathml.exp.string, fixed = TRUE)) {
        # Extract from mathml string block
        # There is probably a much better way to do this but I'm straped for time
        # We will push the mathml string through an expression solver getting a 
        # results like "V1*funcDef(var1,var2)" and will extract var1/2 from funcDef
        solved.expr <- toString(mathml2R(mathml.exp))
        # Extract terms between parentheses
        terms <- str_extract_all(solved.expr, "\\((.*?)\\)")[[1]]
        # Remove the parentheses from the extracted terms
        terms <- gsub("\\(|\\)", "", terms)
        # Split the terms by commas and trim white space
        terms <- trimws(strsplit(terms, ",")[[1]])
        
        # Pull reaction information
        reactants.exists <- FALSE
        products.exists   <- FALSE
        modifiers.exists  <- FALSE
        parameters.exists <- FALSE
        reaction.list <- vector("list", 1)
        found.terms <- c()
        
        for (k in seq_along(current.reaction)) {
          cur.node <- current.reaction[k]
          node.name <- names(cur.node)
          if (node.name == "listOfReactants") {
            reactants.exists <- TRUE
            node.reactants <- Attributes2Tibble(cur.node$listOfReactants)
            # Grab the species from tibble
            spec.grab <- node.reactants %>% pull(species)
            found.terms <- c(found.terms, spec.grab)
            # Condense multiple values to be comma separated
            collapsed.grab <- paste(spec.grab, collapse = ", ");
            reaction.list[[1]]$reactants <- collapsed.grab
          } else if (node.name == "listOfModifiers") {
            modifiers.exists <- TRUE
            node.modifiers <- Attributes2Tibble(cur.node$listOfModifiers)
            modifier.grab <- node.modifiers %>% pull(species)
            found.terms <- c(found.terms, modifier.grab)
            
            reaction.list[[1]]$modifiers <- paste(modifier.grab,
                                                  collapse = ", ")
          } else if (node.name == "listOfProducts") {
            products.exists <- TRUE
            node.products <- Attributes2Tibble(cur.node$listOfProducts)
            product.grab <- node.products %>% pull(species)
            found.terms <- c(found.terms, product.grab)
            reaction.list[[1]]$products <- paste(product.grab,
                                                 collapse = ", ")
          } else if (node.name == "kineticLaw") {
            # Check if parameter node exists
            node.par <- Attributes2Tibble(cur.node$kineticLaw$listOfParameters)
            # Build Parameter df to join with parameters
            if (nrow(node.par)> 0) {
              parameters.exists <- TRUE
              # Condense parameter data to build with equations table
              reaction.list[[1]]$parameters <- paste(node.par %>% pull(id),
                                                     collapse = ", ")
              reaction.list[[1]]$parameters.val <- 
                paste(node.par %>% pull(value),
                      collapse = ", ")
            } else {
              # assign all remaining variables to parameters
              parameters.exists <- TRUE
              
              pars.grab <- terms[which(!(terms %in% found.terms))]
              reaction.list[[1]]$parameters <- paste0(pars.grab, 
                                                      collapse = ", ")
            }
            
            
          }
        }
        # Check for null cases 
        if (!reactants.exists)  {reaction.list[[1]]$reactants  <- NA}
        if (!products.exists)   {reaction.list[[1]]$products   <- NA}
        if (!modifiers.exists)  {reaction.list[[1]]$modifiers  <- NA}
        if (!parameters.exists) {reaction.list[[1]]$parameters <- NA}
        
      # Perform model extraction for fxn definitions
      # Here we know the mathml code looks like 
      # <apply> <ci>lawname</ci><ci>var1</ci><ci>var2</ci></apply>
      # We want to extract the var names (var1, var2)
      
      # So now we have terms <- c("var1", "var2") We need to pull our original 
      # fxn variables in and compare them to these to see which are what kind of 
      # variable.  
      # For example, fdef$var <- c("sub", "v"), fdef$law <- "v*sub" 
      # Reaction dat: rdat$reactions <- var1, rdat$par <- v2
      # results fdef$reactants <- sub, fdef$par <- v
      # Notes: Need to account for when reactions have reactants/products that 
      #        exist but are not found in the law.
      # Pull function information
        
        fxn.reactants  <- NA
        fxn.products   <- NA
        fxn.modifiers  <- NA
        fxn.parameters <- NA
        
        n.reactants  <- 0
        n.products   <- 0
        n.modifiers  <- 0
        n.parameters <- 0
        
        fxn.vars <- SplitEntry(functionDefList[[i]]$variables)
        for (ii in seq_along(terms)) {
          # check if the var is in elements
          if (terms[ii] %in% SplitEntry(reaction.list[[1]]$reactants)) {
            if (anyNA(fxn.reactants)) {fxn.reactants <- c()}
            fxn.reactants <- c(fxn.reactants, fxn.vars[ii])
            n.reactants <- n.reactants + 1
          } else if (terms[ii] %in% SplitEntry(reaction.list[[1]]$products)) {
            if (anyNA(fxn.products)) {fxn.products <- c()}
            fxn.products <- c(fxn.products, fxn.vars[ii])
            n.products <- n.products + 1
          } else if (terms[ii] %in% SplitEntry(reaction.list[[1]]$modifiers)) {
            if (anyNA(fxn.modifiers)) {fxn.modifiers <- c()}
            fxn.modifiers <- c(fxn.modifiers, fxn.vars[ii])
            n.modifiers <- n.modifiers + 1
          } else if (terms[ii] %in% SplitEntry(reaction.list[[1]]$parameters)) {
            if (anyNA(fxn.parameters)) {fxn.parameters <- c()}
            fxn.parameters <- c(fxn.parameters, fxn.vars[ii])
            n.parameters <- n.parameters + 1
          }
        }
        
        # Take into account possible variables that aren't in law (react/prod)
        if (!is.na(reaction.list[[1]]$reactants)) {
          react.i <- 1
          while (n.reactants < length(reaction.list[[1]]$reactants)) {
            if (anyNA(fxn.reactants)) {fxn.reactants <- c()}
            n.reactants <- n.reactants + 1
            to.add <- paste0("reactant_", react.i)
            fxn.reactants <- c(fxn.reactants, to.add)
            react.i <- react.i + 1
          }
        }
        
        if (!is.na(reaction.list[[1]]$products)) {
          prod.i <- 1
          while (n.products < length(reaction.list[[1]]$products)) {
            if (anyNA(fxn.products)) {fxn.products <- c()}
            n.products <- n.products + 1
            to.add <- paste0("product_", prod.i)
            fxn.products <- c(fxn.products, to.add)
            prod.i <- prod.i + 1
          }
        }
        
        
        functionDefList[[i]]$Reactants  <- collapseVector(fxn.reactants)
        functionDefList[[i]]$Products   <- collapseVector(fxn.products)
        functionDefList[[i]]$Modifiers  <- collapseVector(fxn.modifiers)
        functionDefList[[i]]$Parameters <- collapseVector(fxn.parameters)
        
        match.found <- TRUE
        break
      }
    }
    if (!match.found) {
      idx.to.remove  <- c(idx.to.remove, i)
      name.to.remove <- c(name.to.remove, function.id)
    }
  }
  
  # Remove Functions that were not used
  if (length(idx.to.remove) > 0) {
    functionDefList <- functionDefList[-idx.to.remove]
    print(paste0("The functions removed are: ", 
                 paste0(name.to.remove, collapse = ", ")))
  }
  
  return(functionDefList)
}

ExtractFunctionDefFromSBML <- function(doc, functionTibble) {
  # Extracts function definitions from sbml document
  # Inputs: 
  #   doc - parsed xml doc from xmltreeparse
  #   functionTibble - tibble that as function information
  # Function tibble is calculated as below:
  # sl <- read_xml(sbmlFile) %>% as_list()
  # functionTibble <- Attributes2Tibble(sl$sbml$model$listOfFunctionDefinitions)
  
  # browser()
  # Grab function definition tree
  func.ids <- functionTibble$id
  func.names <- functionTibble$name
  
  functions <- doc$doc$children$sbml[["model"]][["listOfFunctionDefinitions"]]
  n.funcs <- length(functions)
  
  # funcList <- vector("list", n.funcs)
  funcList <- list()
  # Extract Functions
  for (i in seq_along(functions)) {
    func.def <- functions[[i]][["math"]][["lambda"]]
    # print(func.def)
    # Extract variables from definition and remove them
    var.names <- names(func.def)
    
    # Initialize naming variables
    bvars <- c()
    bvars.idx <- c()
    
    for (j in seq_along(var.names)) {
      if (var.names[j] == "bvar") {
        bvars.idx <- c(bvars.idx, j)
        # child grabs lambad, i grabs current bvar, 1 goes to ci, 1 goes to name
        bvars <- c(bvars, func.def[[j]][[1]][[1]]$value)
      }
    }
    
    # Remove bvars from func.def
    func.def <- func.def[-bvars.idx]
    # Create func.def string
    law.func.def <- rmp(convertML2R(func.def))
    
    # package to output
    to.list <- list("id" = func.ids[i],
                    "name" = func.names[i],
                    "variables" = collapseVector(bvars),
                    "law" = law.func.def)
    
    funcList[[func.ids[i]]] <- to.list
  }
  
  return(funcList)
}

# Math Rules Pull Functions ----------------------------------------------------
ExtractRulesMathFromSBML <- function(doc, assignmentVars) {
  # Extracts mathmatical rules from sbml document that use assignment
  # An instance of this is a parameter that is not constant: V1 = 5*V1i
  #
  # Inputs: 
  #   doc - parsed xml doc from xmltreeparse
  #   assignmentVars - vars on left hand side of rules (V1)

  # Parse to rules section
  rules <- doc$doc$children$sbml[["model"]][["listOfRules"]]
  n.rules <- length(rules)
  
  rulesList <- vector("list", n.rules)
  # Extract mathml for each rule and store info to list
  for (i in seq_along(rules)) {
    
    mathml    <- rules[[i]][["math"]][[1]]
    # print(mathml)
    e <- convertML2R(mathml)
    # print(e)
    e.str.law <- Deriv::Simplify(e)
    # print(e.str.law)
    e.str.law <- rmp(e)
    # print(e.str.law)
    test         <- mathml2R(mathml)
    # print(test)
    # e.exp.law <- e[[1]]
    # e.str.law <- gsub(" ","",toString(e[1]))

    rulesList[[i]]$LHS.var <- assignmentVars[i]
    rulesList[[i]]$mathml  <- toString(mathml)
    rulesList[[i]]$str.law <- e.str.law
  }
  # print(rulesList)
  return(rulesList)
}


CheckForTermInMathml <- function(mathml.exp,
                                 search.term) {
  # Search for string term in mathml expression. 
  # Inputs: 
  # @mathml.exp - (str) mathml terms to search for keyword in
  # @search.term- (str) term to search for in mathml.exp
  # Output:
  # @ (bool) TRUE if search term exists, false if it doesn't
  # @ (vec)  vector of string terms that occur after function defintion
  in.expression <- FALSE
  terms.in.function <- c()
  
  # Regex pattern to remove tags
  pattern <- "<[^>]+>"
  # Replace tags with empty space
  result <- gsub(pattern, "", mathml.exp)
  # Remove newlines
  result <- gsub("\n", "", result)
  # Split on spaces and clear all empty strings from vector
  result <- strsplit(result, " ")[[1]]
  result <- result[nzchar(result)]
  
  if (search.term %in% result) {
    in.expression <- TRUE
    idx.for.search <- which(result %in% search.term)
    terms.in.function  <- result[(idx.for.search+1):length(result)]
  } 
  
  out <- list(term.found = in.expression,
              function.terms = terms.in.function)
  return(out)
}

convertReactionVarsFromSBML <- function(var2Convert) {
  
  out <- c()
  for (i in seq_along(var2Convert)) {
    if (!is.na(var2Convert[i])) {
      # Split Var on Comma
      split.var <- strsplit(var2Convert[i], ",")[[1]]
      # Remove Excess white space from var names if they exist
      subbed.var <- gsub(" ", "", split.var, fixed = TRUE)
      # Recondense with space delmiter
      condensed.var <- paste0(subbed.var, collapse = " ")
      out <- c(out, condensed.var)
    } else {
      out <- c(out, NA)
    }
    
  }
  
  return(out)
}

FindIdSplit <- function(string2Search) {
  
  out.ids <- c()
  split <- strsplit(string2Search, " ")[[1]]
  
  for (i in seq_along(split)) {
    out.ids <- c(out.ids, FindId(split[i]))
  }
  
  return(out.ids)
} 

FindIDReactionStructure <- function(structure2Search) {
  # browser()
  out.ids <- c()
  for (i in seq_along(structure2Search)) {
    if ( !is.na(structure2Search[i])) {
      # split it 
      split.struc <- strsplit(structure2Search[i], " ")[[1]]
      # Convert each component
      row.ids <- c()
      for (j in seq_along(split.struc)) {
        # print(j)
        # print(split.struc)
        # print(length(split.struc))
        # print(split.struc[j])
        row.ids <- c(row.ids, FindId(split.struc[j]))
      }
      out.ids <- c(out.ids, paste0(row.ids, " "))
    } else {
      out.ids <- c(out.ids, NA)
    }
    
  }
}


# ConvertML2R s3 Method (Creates String Law) -----------------------------------
# Create s3 method convertML2R based on mathml2R from the SBMLR package.
# This is a recursive method that takes in a mathml xml node and parses it to 
# convert it to a proper string to be used in the BioModME application. 
# convertML2R - main call
# convertML2R.default - builds expression for a node, passing the node children 
#                       back into the recursive function for the XMLNode parser
# convertML2R.XMLNode - looks at the individual nodes, converting them to the 
#                       proper term and then building it after apply.

# The expression to be passed through would be a mathml law starting on the 
# first actual node of the expression. If actual expression is: 
# <assignmentRule metaid="rule1" variable="V1">
#   <math xmlns="http://www.w3.org/1998/Math/MathML">
#     <apply>
#       <times/>
#       <ci>C</ci>
#       <ci>V1p</ci>
#       <apply>
#         <power/>
#         <apply>
#           <plus/>
#           <ci>C</ci>
#           <ci>K6</ci>
#         </apply>
#         <cn type="integer">-1</cn>
#       </apply>
#     </apply>
#   </math>
# </assignmentRule>

# Then the input would be starting at the first apply: 
# <apply>
#   <times/>
#   <ci>C</ci>
#   <ci>V1p</ci>
#   <apply>
#     <power/>
#     <apply>
#       <plus/>
#       <ci>C</ci>
#       <ci>K6</ci>
#     </apply>
#     <cn type="integer">-1</cn>
#   </apply>
# </apply>

# Resulting in following result: 
# "C*V1p*(C+K6)^-1"


convertML2R <- function(node) {
  UseMethod("convertML2R", node)
}


convertML2R.default <- function(children) {  
  # this gets used when a "list" of children nodes are sent in
  n <- length(children)
  expr <- c() 
  for(i in 1:n) {
    expr <- c(expr, convertML2R(children[[i]]))
  }   
  return(expr)
}

convertML2R.XMLNode <-function(node){
  # print("mathml2R.XMLNode")
  # print(node)
  nm <- xmlName(node) 
  # cat("XMLNode: node name is ",nm," and the node class is",class(node),"\n")
  if(nm=="power"||
     nm == "divide"||
     nm =="times"||
     nm=="plus"||
     nm=="minus" ||
     nm=="exp") {
    op <- switch(nm, 
                 power="^", 
                 divide="/",
                 times="*",
                 plus="+",
                 minus="-",
                 exp="exp")
    out <- as.character(op)
    
  } else if (nm == "ci" || nm == "csymbol") {
    # Character node, grab variable
    out <- node$children[[1]]$value
    
  } else if (nm == "cn") {
    # Numerical code, convert to character
    out <- as.character(node$children[[1]]$value)
    
  } else if(nm == "apply") {
    # If apply, recurse function to solve
    val <- convertML2R(node$children)
    # Once recursive term has ended condense the expression
    # print(val)
    # First term is our condense term
    condense.term <- val[1]
    # If mathematical operator, condense with that as collapse term
    if (condense.term %in% c("*", "+", "-")) {
      # print(condense.term)
      # print(val)
      # print(length(val))
      if (length(val) == 2) {
        out <- paste0(condense.term, val[2])
      } else {
        to.condense <- val[2:length(val)]
        out <- paste0(to.condense, collapse = condense.term)
        out <- paste0("(", out, ")")
      }
      
    } else if (condense.term == "/") {
      # Wrap second term in parenthesis
      denominator <- paste0("(", val[3], ")")
      numerator   <- val[2]
      out <- paste0(numerator, condense.term, denominator)
    } else if (condense.term == "^") {
      # Create exponent term
      to.condense <- val[2:(length(val)-1)]
      last.term <- val[length(val)]
      out <- paste0(to.condense, collapse = "")
      out <- paste0("(", out, ")", "^", last.term)
    } else if (condense.term == "exp") {
      to.condense <- val[2:length(val)]
      out <- paste0(to.condense, collapse = "")
      out <- paste0(condense.term, "(", out, ")")
    }
    else {
      out <- paste0(val, collapse ="")
    }
  } else  {
    out <- NA
    cat("error: nm =",nm," not in set!\n")
  }
  
  return(out)
}


# Convert MathML to R Original Function From Previous Made Package -------------
# These function come direction from Bioconduction - SBMLR. They read mathml
# and convert it to an expression. This was not optimal for what I was trying
# to do so I rewrote the function.  I kept both as I still find this one has
# its uses.
mathml2R <-function(node)  {
  UseMethod("mathml2R", node)
}

mathml2R.XMLDocument <-function(doc) {
  return(mathml2R(doc$doc$children))
}

mathml2R.default<-function(children) {  
  # this gets used when a "list" of children nodes are sent in
  n=length(children)
  expr <- expression() 
  for(i in 1:n) {
    expr=c(expr, mathml2R(children[[i]]))
  }   
  if (n>3) {
    #print("n>3")  # this fixes libsbml problem that times is not binary
    # in R, prod takes arb # of args
    if (expr[[1]]=="*") {
      expr[[1]]=as.name("prod")
    }
    # similary for sum
    if (expr[[1]]=="+") {
      expr[[1]]=as.name("sum")
    }
  }
  return(expr)
}

mathml2R.XMLNode <-function(node){
  nm <- xmlName(node) 
  # print(nm)
  if(nm=="power"||
     nm == "divide"||
     nm =="times"||
     nm=="plus"||
     nm=="minus" ||
     nm=="exp") {
    op <- switch(nm, 
                 power="^", 
                 divide="/",
                 times="*",
                 plus="+",
                 minus="-",
                 exp="exp")
    val <- as.name(op)
  } else if(nm == "ci"||
            nm == "cn"||
            nm == "csymbol") {
    if(nm == "ci" || nm == "csymbol") {
      val <- as.name(node$children[[1]]$value)
    } 
    if(nm == "cn") {
      val <- as.numeric(node$children[[1]]$value)
    } 
  }  else if(nm == "apply") {
    val <- mathml2R(node$children)
    mode(val) <- "call"
  } else  {cat("error: nm =",nm," not in set!\n")}
  # print(val)
  return(as.expression(val))
}

# The next two functions are used by rules and were taken straight from read.SBML
# The idea is that SBML doesn't provide a list of atoms/leaves with rules, so we have to create them
# to place them in their model slots, and to use them to create the R function definition for the rule
# using makeLaw with a null for parameters, since they are passed global for rules.
# map MathML operator symbols into R symbols
ML2R <- function(type) {
  switch(type,
         "times" = "*",
         "divide" = "/",
         "plus" = "+",
         "minus" = "-",
         "power" = "^",
         "exp" = "exp",
         "ln" = "log",
         "not found") 
}   

getRuleLeaves <- function(math) { 
  n=length(math)
  S=c(NULL)
  op=ML2R(xmlName(math[[1]]))
  for (j in 2:n ) {
    if ((xmlName(math[[j]])=="ci")|(xmlName(math[[j]])=="cn")) {
      S=c(S,as.character(xmlValue(math[[j]])))
    } else {
      S=c(S,Recall(math[[j]])  )
    } 
  }
  
  
  return(S)
}

