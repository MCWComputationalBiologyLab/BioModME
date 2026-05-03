
sanitize_to_r_identifier <- function(names) {
  # Convert SBML species/parameter/compartment names into valid R identifiers
  # so they can be used as bare symbols in rate-law text, state vector
  # names, eval(parse(...)) expressions, Shiny input IDs, and elsewhere.
  # The original names are preserved separately as $DisplayName by the
  # SBML loaders for UI rendering and SBML export round-trip.
  #
  # Substitution rules:
  #   - Hyphens become underscores ("14-3-3_s" -> "14_3_3_s") since they're
  #     a common SBML convention and underscores read more naturally than
  #     the dots make.names() inserts.
  #   - The remaining transformation is delegated to make.names() which
  #     prefixes leading digits with "X", replaces spaces and other
  #     non-identifier chars with ".", and renames reserved words.
  #   - unique = TRUE disambiguates collisions ("a", "a" -> "a", "a.1").
  if (length(names) == 0) return(names)
  names <- gsub("-", "_", names, fixed = TRUE)
  make.names(names, unique = TRUE)
}

Attributes2Tibble <- function(xmlAttributeStruct) {
  # When parsing sbml things get weird. Convert these structures to df.
  # attributes() on a parsed element returns both the XML attributes and the
  # `names` attribute (which lists child element names like "annotation"). We
  # only want the XML attributes — keeping `names` causes bind_rows to fill NA
  # for elements without child nodes (e.g. a species with no <annotation>).
  out.list <- list()
  for (i in seq_along(xmlAttributeStruct)) {
    attrs <- attributes(xmlAttributeStruct[[i]])
    attrs$names <- NULL
    out.list[[i]] <- unlist(attrs)
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

  # Extract Compartments
  if (!is.null(modelList$listOfCompartments)) {
    compartment.df <- Attributes2Tibble(modelList$listOfCompartments)
    compartment.df <- FinalizeCompartmentData(compartment.df)
    out[["compartments"]] <- compartment.df
    exists.listOfCompartments <- TRUE
  }

  # Extract Species
  if (!is.null(modelList$listOfSpecies)) {
    species.df <- Attributes2Tibble(modelList$listOfSpecies)
    species.df <- FinalizeSpeciesData(species.df)
    out[["species"]] <- species.df
    exists.listOfSpecies <- TRUE
  }
  
  # Extract Parameters
  if (!is.null(modelList$listOfParameters)) {
    listOfParameters <- Attributes2Tibble(modelList$listOfParameters)
    exists.listOfParameters <- TRUE
  }
  
  # Extract Rules
  if (!is.null(modelList$listOfRules)) {
    rules.header <- Attributes2Tibble(modelList$listOfRules)
    rules.assignment.vars <- rules.header %>% pull(variable)
    rules.list <- ExtractRulesMathFromSBML(doc, rules.assignment.vars)
    
    out[["rules"]] <- rules.list
    exists.listOfRules <- TRUE
  }
  
  # Extract Unit Definitions
  if (!is.null(modelList$listOfUnitDefinitions)) {
    out[["unit_definitions"]] <- ParseUnitDefinitions(modelList$listOfUnitDefinitions)
    exists.listOfUnitDefinitions <- TRUE
  } else {
    out[["unit_definitions"]] <- list()
  }

  # Extract Function Definitions
  if (!is.null(modelList$listOfFunctionDefinitions)) {
    func.info <- Attributes2Tibble(modelList$listOfFunctionDefinitions)
    function.definitions <- ExtractFunctionDefFromSBML(doc, func.info)
    function.definitions <- FindFunctionDefInformation(doc,
                                                       function.definitions,
                                                       sbmlList)
    out[["functions"]] <- function.definitions
  }
  
  # Extract Reactions
  if (!is.null(modelList$listOfReactions)) {
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
    
    # Add math to reactions list
    reaction.list <- ExtractReactionMathFromSBML(doc, 
                                                 reaction.list,
                                                 function.definitions)
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
  return(out)
}

# Unit Definitions -------------------------------------------------------------

# SBML unit kinds we know how to display. Unrecognized kinds (kelvin, ampere,
# pascal, ...) fall through to a warning and an unparsed display.
SBML_KIND_TO_DIMENSION <- list(
  mole          = list(short = "mol", dim = "Count"),
  item          = list(short = "item", dim = "Count"),
  avogadro      = list(short = "mol", dim = "Count"),
  gram          = list(short = "g",   dim = "Mass"),
  kilogram      = list(short = "kg",  dim = "Mass"),
  metre         = list(short = "m",   dim = "Length"),
  meter         = list(short = "m",   dim = "Length"),
  litre         = list(short = "L",   dim = "Volume"),
  liter         = list(short = "L",   dim = "Volume"),
  second        = list(short = "s",   dim = "Duration"),
  minute        = list(short = "min", dim = "Duration"),
  hour          = list(short = "hr",  dim = "Duration"),
  day           = list(short = "day", dim = "Duration"),
  joule         = list(short = "J",   dim = "Energy"),
  dimensionless = list(short = "",    dim = NA)
)

# Apply an SI scale prefix where idiomatic (mol, mol/L, g, m, s).
SCALE_PREFIX <- list(
  `-12` = "p", `-9` = "n", `-6` = "u", `-3` = "m",
  `0`   = "",
  `3`   = "k", `6` = "M", `9` = "G"
)

sbml_kind_dim <- function(kind) {
  entry <- SBML_KIND_TO_DIMENSION[[kind]]
  if (is.null(entry)) NA_character_ else entry$dim
}

sbml_unit_short_name <- function(kind, scale = 0) {
  entry <- SBML_KIND_TO_DIMENSION[[kind]]
  if (is.null(entry)) {
    warning(sprintf("Unrecognized SBML unit kind '%s' -- treated as opaque", kind))
    return(kind)
  }
  if (entry$short == "") return("")
  prefix <- SCALE_PREFIX[[as.character(scale)]]
  if (is.null(prefix)) prefix <- ""
  paste0(prefix, entry$short)
}

ParseUnitDefinitions <- function(listOfUnitDefinitions) {
  # Parse SBML <listOfUnitDefinitions> (xml2 as_list shape) into a named list
  # keyed by unit id. Each entry has $id, $display (e.g. "mol/L"), $description
  # (a one-word category like "concentration") and $base_dim.
  if (is.null(listOfUnitDefinitions)) return(list())

  out <- list()
  for (i in seq_along(listOfUnitDefinitions)) {
    ud <- listOfUnitDefinitions[[i]]
    ud_attrs <- attributes(ud)
    id <- ud_attrs$id
    if (is.null(id)) next

    units_list <- ud$listOfUnits
    if (is.null(units_list)) {
      out[[id]] <- list(id = id, display = "dimensionless",
                        description = "", base_dim = NA_character_)
      next
    }

    numerator   <- character(0)
    denominator <- character(0)
    dims_pos    <- character(0)
    dims_neg    <- character(0)

    for (j in seq_along(units_list)) {
      unit_attrs <- attributes(units_list[[j]])
      kind       <- unit_attrs$kind
      exponent   <- if (is.null(unit_attrs$exponent)) 1 else as.numeric(unit_attrs$exponent)
      scale      <- if (is.null(unit_attrs$scale))    0 else as.numeric(unit_attrs$scale)

      short <- sbml_unit_short_name(kind, scale)
      if (is.na(short) || short == "") next

      token <- if (exponent == 1 || exponent == -1) short else paste0(short, "^", abs(exponent))
      if (exponent > 0) {
        numerator <- c(numerator, token)
        dims_pos  <- c(dims_pos, sbml_kind_dim(kind))
      } else if (exponent < 0) {
        denominator <- c(denominator, token)
        dims_neg    <- c(dims_neg, sbml_kind_dim(kind))
      }
    }

    num_str <- if (length(numerator) > 0) paste(numerator, collapse = "*") else "1"
    if (length(denominator) > 0) {
      den_str <- if (length(denominator) > 1) {
        paste0("(", paste(denominator, collapse = "*"), ")")
      } else {
        denominator
      }
      display <- paste0(num_str, "/", den_str)
    } else {
      display <- num_str
    }

    description <- biomodme_unit_description(dims_pos, dims_neg)
    base_dim    <- if (length(dims_pos) == 1 && length(dims_neg) == 0) dims_pos else NA_character_

    out[[id]] <- list(id = id, display = display,
                      description = description, base_dim = base_dim)
  }

  out
}

biomodme_unit_description <- function(pos_dims, neg_dims) {
  # Heuristic: name common compound dimensions. Examples:
  #   Count / Volume         -> "concentration"
  #   Count / Duration       -> "rate"
  #   Mass / Volume          -> "mass concentration"
  #   Volume / Duration      -> "flow"
  #   Volume                 -> "volume"
  if (length(pos_dims) == 1 && length(neg_dims) == 0) return(tolower(pos_dims))
  if (identical(pos_dims, "Count")    && identical(neg_dims, "Volume"))   return("concentration")
  if (identical(pos_dims, "Mass")     && identical(neg_dims, "Volume"))   return("mass concentration")
  if (identical(pos_dims, "Count")    && identical(neg_dims, "Duration")) return("rate")
  if (identical(pos_dims, "Volume")   && identical(neg_dims, "Duration")) return("flow")
  if (identical(pos_dims, "Mass")     && identical(neg_dims, "Duration")) return("mass rate")
  ""
}

ResolveUnitRef <- function(unit_id, unit_definitions) {
  # Look up an SBML unit reference. Returns NULL if not found.
  if (is.null(unit_id) || is.na(unit_id) || unit_id == "") return(NULL)
  unit_definitions[[unit_id]]
}

# Parameter Finalizing ---------------------------------------------------------

FinalizeSpeciesData <- function(speciesFromSBML) {
  # Finalize the Output of species data specifying outputs
  # Inputs: 
  #   @speciesFromSBML - Main load from sbml listOfSpecies
  # Outputs: 
  #   (tibble) id, name, initialConcentration, substanceUnits, compartment, 
  #            constant, boundaryCondition
  
  message <- NULL
  
  # Throw error if species don't exist
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
  
  if (!isTruthy(out$id)) {
    message <- "SBML doesn't contain species id information."
    return(list(out = NULL, error = message))
  }
  
  has_col <- function(col) col %in% colnames(speciesFromSBML)

  # Check for name
  if (!has_col("name") || !isTruthy(speciesFromSBML$name)) {
    name <- out %>% pull(id)
    # Bind to output
    out <- cbind(out, name)
  }

  # Check for initialConcentration - the issue here is some files use
  # initialConcentration and some use initialAmount
  if (has_col("initialAmount") && isTruthy(speciesFromSBML$initialAmount)) {
    initialConcentration <- out %>% pull(initialAmount)
    # Bind to output
    out <- cbind(out, initialConcentration)
  }

  # Check for substanceUnits
  if (!has_col("substanceUnits") || !isTruthy(speciesFromSBML$substanceUnits)) {
    substanceUnits <- rep("species", n.species)
    # Bind to output
    out <- cbind(out, substanceUnits)
  }

  # Check for constant
  if (!has_col("constant") || !isTruthy(speciesFromSBML$constant)) {
    constant <- rep(FALSE, n.species)
    # Bind to output
    out <- cbind(out, constant)
  } else {
    # Convert from string to bool
    out$constant <- as.logical(out$constant)
  }

  # Convert boundaryCondition to bool
  if (!has_col("boundaryCondition") || !isTruthy(speciesFromSBML$boundaryCondition)) {
    boundaryCondition <- rep(FALSE, n.species)
    out <- cbind(out, boundaryCondition)
  } else {
    out$boundaryCondition <- as.logical(out$boundaryCondition)
  }
  # Sort Column Order and remove excess columns
  column.order <- c("id",
                    "name",
                    "initialConcentration",
                    "substanceUnits",
                    "compartment",
                    "constant",
                    "boundaryCondition")

  out <- out %>% select(all_of(column.order))
  
  # Return Output
  return(list(out = out, error = message))
}

FinalizeCompartmentData <- function(compartmentsFromSBML) {
  # Finalize the Output of compartment data specifying outputs
  # Inputs: 
  #   @compartmentsFromSBML - Main load from sbml listOfCompartments
  # Outputs: 
  #   (tibble) id, name, size, units, constant
  message <- NULL
  
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
  
  has_col <- function(col) col %in% colnames(out)

  if (!has_col("id") || !isTruthy(out$id)) {
    message <- "SBML doesn't contain compartment id information."
    return(list(out = NULL, error = message))
  }
  # Most sbmls seem to have size and id so I will ignore those
  if (!has_col("name") || !isTruthy(compartmentsFromSBML$name)) {
    name <- out %>% pull(id)
    # Bind to output
    out <- cbind(out, name)
  }

  if (!has_col("units") || !isTruthy(out$units)) {
    units <- rep("volume", n.compartments)
    out <- cbind(out, units)
  }

  if (!has_col("spatialDimensions") || !isTruthy(out$spatialDimensions)) {
    spatialDimensions <- rep("3", n.compartments)
    out <- cbind(out, spatialDimensions)
  }

  if (!has_col("constant") || !isTruthy(out$constant)) {
    constant <- rep(TRUE, n.compartments)
    out <- cbind(out, constant)
  } else {
    # Convert from string to bool
    out$constant <- as.logical(out$constant)
  }

  # Sort Column Order
  column.order <- c("id", "name", "size", "constant", "spatialDimensions", "units")
  column.order <- intersect(column.order, colnames(out))
  out <- out %>% select(all_of(column.order))

  # Return Output
  return(list(out = out, error = message))
}

normalize_constant_column <- function(df) {
  # Coerce a parameter table's `constant` column to logical regardless of how
  # SBML stored it ("true"/"false" strings, R logicals, or NA from missing
  # attribute). Tables that don't have the column at all are returned as-is.
  if (!"constant" %in% names(df)) return(df)
  v <- df$constant
  if (is.logical(v)) return(df)
  v_chr <- as.character(v)
  df$constant <- toupper(v_chr) %in% c("TRUE", "T", "1")
  df$constant[is.na(v_chr)] <- NA
  df
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
  
  # Check which of the inputs exist
  if (isTruthy(parsFromSBMLMain)) {
    if (nrow(parsFromSBMLMain) > 0) {
      main.par.exist <- TRUE
      
      out <- parsFromSBMLMain
      # Always seem to have id, value

      # Add name and constant if not
      out_cols <- names(out)
      if (!"name" %in% out_cols || !isTruthy(out$name)) {
        name <- out %>% pull(id)
        out <- cbind(out, name)
      }
      if (!"constant" %in% out_cols || !isTruthy(out$constant)) {
        constant <- rep(TRUE, nrow(out))
        out <- cbind(out, constant)
      }
      out <- out %>% select(any_of(c("id",
                                     "name",
                                     "value",
                                     "constant",
                                     "units")))
      # Normalize `constant` to logical before any bind_rows: SBML stores it
      # as the string "true"/"false", but reaction-built params arrive as
      # logical -- bind_rows refuses to combine columns of different types.
      out <- normalize_constant_column(out)
    }
  }

  # Check for reaction parameters
  if (isTruthy(parsFromReactions)) {
    if (nrow(parsFromReactions) > 0) {
      react.par.exist <- TRUE
      df <- parsFromReactions %>%
        select(any_of(c("id", "name", "value", "constant", "units")))
      df <- normalize_constant_column(df)
      if (main.par.exist) {
        # bind_rows tolerates a column subset on either side (e.g. main has
        # `units` but reaction-extracted parameters don't), filling NA where
        # a column is missing -- base rbind would error.
        out <- dplyr::bind_rows(out, df)
      } else {
        out <- df
      }
    }
  }

  # `constant` is already logical post-normalization; just default any
  # remaining NAs to TRUE (SBML default for parameters with no constant attr).
  out$constant[is.na(out$constant)] <- TRUE
  
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
  
  column.order <- c("id", "name", "value", "constant", "units")
  column.order <- intersect(column.order, colnames(constant.parameters))
  constant.parameters <- constant.parameters %>%
                         select(all_of(column.order)) %>%
                         dplyr::distinct(id, .keep_all = TRUE)

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
  # Check which terms exist (use names() to avoid tibble warnings on $-access
  # of missing columns).
  cols <- names(tags)
  to.pull <- intersect(c("id", "reversible", "name", "fast"), cols)
  out <- tags %>% select(all_of(to.pull))
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
  # browser()
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
      # SBML L2: stoichiometry defaults to 1 when the attribute is absent.
      stoich.r <- if ("stoichiometry" %in% names(node.reactants)) {
        node.reactants$stoichiometry
      } else {
        rep("1", nrow(node.reactants))
      }
      out.list$Reactants.Stoich <- collapseVector(stoich.r)
    } else if (node.name == "listOfProducts") {
      # Convert node to Tibble
      node.products <- Attributes2Tibble(current.node$listOfProducts)

      # Grab the species from tibble, collapse, add to output
      out.list$Products <- collapseVector(node.products %>% pull(species),
                                          convertBlank = TRUE)
      stoich.p <- if ("stoichiometry" %in% names(node.products)) {
        node.products$stoichiometry
      } else {
        rep("1", nrow(node.products))
      }
      out.list$Products.Stoich <- collapseVector(stoich.p)
    } else if (node.name == "listOfModifiers") {
      # Convert node to Tibble
      node.modifiers <- Attributes2Tibble(current.node$listOfModifiers)
      
      # Grab the species from tibble, collapse, add to output
      out.list$Modifiers <- collapseVector(node.modifiers %>% pull(species),
                                          convertBlank = TRUE)
    } else if (node.name == "kineticLaw") {
      # Check if parameter node exists
      node.par <- Attributes2Tibble(current.node$kineticLaw$listOfParameters)
      # browser()
      if (ncol(node.par) != 0) {
        # IF PARAMETER INFORMATION IN REACTION XML INFO

        out.list$Parameters.id <- collapseVector(node.par %>% pull(id), 
                                              convertBlank = TRUE)
        out.list$Parameter.Values <- collapseVector(node.par %>% pull(value), 
                                                    convertBlank = TRUE)
        # Assign name if name exists, else assign name as id
        if ("name" %in% names(node.par)) {
          out.list$Parameters.name <- collapseVector(node.par %>% pull(name),
                                                     convertBlank = TRUE)
        } else {
          out.list$Parameters.name <- collapseVector(node.par %>% pull(id),
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
  # browser()
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
    # browser()
    # Grab information on Reactants, Products, Modifiers
    # This information should already be in reactionlist from base extraction
    reactants  <- SplitEntry(reactionList[[i]]$Reactants)
    products   <- SplitEntry(reactionList[[i]]$Products)
    modifiers  <- SplitEntry(reactionList[[i]]$Modifiers)
    
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
          # not then extract them. Guard against NULL (key was never set) as
          # well as NA (initialized but empty).
          rp <- reactionList[[i]]$Parameters
          if (!is.null(rp) && length(rp) > 0 && !any(is.na(rp))) {
            parameters <- SplitEntry(rp)
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
          replacement <- SubstituteRateLawTerms(function.rate.law,
                                                function.reactants,
                                                function.products,
                                                function.modifiers,
                                                function.parameters,
                                                reactants,
                                                products,
                                                modifiers,
                                                parameters)
          # browser()
          # Solve mathml xml to string with function
          mathml.w.fun <- rmp(gsub(" ", "", convertML2R(mathml.exp)))
          pattern <- paste0(reaction.law, "\\((.*?)\\)")
          string.rate.law <- gsub(pattern, replacement, mathml.w.fun)
        }
      }
    }
    
    # Extraction of reaction information if not function based
    if (!equation.uses.function) {
      reaction.law <- "CUSTOM"
      
      # Convert mathml to string rate law for r
      string.rate.law <- rmp(gsub(" ", "", convertML2R(mathml.exp)))
      
      # Grab Parameters. Parameters.name is only set when the kineticLaw had
      # an inline <listOfParameters>; treat NULL/NA the same.
      pn <- reactionList[[i]]$Parameters.name
      if (!is.null(pn) && length(pn) > 0 && !any(is.na(pn))) {
        parameters <- SplitEntry(pn)
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
      "Reactants.Stoich" = reactionList[[i]]$Reactants.Stoich,
      "Products.Stoich"  = reactionList[[i]]$Products.Stoich,
      "Modifiers"        = modifiers.collapsed,
      "Parameters"       = par.collapsed,
      "Equation.Text"    = string.rate.law,
      "MathMl.Rate.Law"  = mathml.string
    )
  }
  
  return(reactionList)
}

CombineReactionTagsWReactions <- function(reactionTags,
                                          reactionList) {
  
  n.reactions <- length(reactionList)
  tag_cols <- names(reactionTags)

  # Check for tags we need to grab (use names() membership to avoid tibble
  # warnings on $-access of missing optional columns).
  if ("id" %in% tag_cols) {
    ids <- reactionTags %>% pull(id)
  } else {
    ids <- rep(NA, n.reactions)
  }

  if ("reversible" %in% tag_cols) {
    is.reversible <- reactionTags %>% pull(reversible)
  } else {
    is.reversible <- rep(FALSE, n.reactions)
  }

  if ("name" %in% tag_cols) {
    description <- reactionTags %>% pull(name)
  } else {
    description <- rep("Custom Load Reaction", n.reactions)
  }

  if ("fast" %in% tag_cols) {
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
  #              
  extract_function_name <- function(input_string) {
    # Pull the first <ci> immediately inside an <apply> -- that's the function
    # name in MathML. Returns "" if the rate law has no <apply> (e.g. a bare
    # constant or a single <ci>), so the downstream match() in the caller
    # simply finds no hit instead of crashing.
    m <- regmatches(
      input_string,
      regexec("<apply>\\s*<ci>\\s*(.*?)\\s*</ci>", input_string, perl = TRUE)
    )[[1]]
    if (length(m) < 2) return("")
    RemoveWS(m[[2]])
  }
  
  modelList <- sbmlList$sbml$model
  reaction.info <- vector(mode = "character", 
                          length = length(modelList$listOfReactions))
  # Create reaction df of information
  for (i in seq_along(modelList$listOfReactions)) {
    # Separate current reaction node
    current.reaction <- modelList$listOfReactions[[i]]
    # Pull math law and check if it contains the current search fxn
    reactions <- doc$doc$children$sbml[["model"]][["listOfReactions"]]
    
    # Extract mathml expression and make string
    mathml.exp <- reactions[[i]][["kineticLaw"]][["math"]][[1]]
    mathml.exp.string <- toString(reactions[[i]][["kineticLaw"]][["math"]])
    reaction.info[i] <- extract_function_name(mathml.exp.string)
  }
  # browser()
  # Iterating function definitions, the iterating reactions to find matching 
  # function id in the reaction. From there we will extract reaction info to 
  # build up the proper function definition.
  idx.to.remove  <- c()
  name.to.remove <- c()
  for (i in seq_along(functionDefList)) {
    function.id <- functionDefList[[i]]$id
    match.found <- FALSE
    # if (i ==3) {browser()}
    j <- match(function.id, reaction.info)
    
    if (!is.na(j)) {match.found <- TRUE} else {match.found <- FALSE}
    
    if (match.found) {
      current.reaction <- modelList$listOfReactions[[j]]
      # Pull math law and check if it contains the current search fxn
      reactions <- doc$doc$children$sbml[["model"]][["listOfReactions"]]
      
      # Extract mathml expression and make string
      mathml.exp <- reactions[[j]][["kineticLaw"]][["math"]][[1]]
      mathml.exp.string <- toString(reactions[[j]][["kineticLaw"]][["math"]])
      
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
    } else {
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
  message <- NULL
  
  # Parse to rules section
  rules <- doc$doc$children$sbml[["model"]][["listOfRules"]]
  n.rules <- length(rules)
  
  rulesList <- vector("list", n.rules)
  # Extract mathml for each rule and store info to list
  for (i in seq_along(rules)) {
    
    mathml    <- rules[[i]][["math"]][[1]]
    e <- NULL
    # The converter is the most likely point of this process to fail. 
    # We are missing some mathml keywords (ex root, degree)
    tryCatch({
      e <- convertML2R(mathml)
    }, error = function(cond) {
      e <- NULL
      err.mes <- cond
    }, warning = function(cond) {
      e <- NULL
      err.mes <- cond
    })
    
    if (is.null(e)) {
      message <- "Something went wrong in parsing the 'Rules' Section. It could
                  be possible this file contains conversions we do not yet
                  support."
      return(list(out = NULL, error = message))
    }
    
    e.str.law <- Deriv::Simplify(e)
    e.str.law <- rmp(e)
    # test      <- mathml2R(mathml)
    # e.exp.law <- e[[1]]
    # e.str.law <- gsub(" ","",toString(e[1]))

    rulesList[[i]]$LHS.var <- assignmentVars[i]
    rulesList[[i]]$mathml  <- toString(mathml)
    rulesList[[i]]$str.law <- e.str.law
  }
  
  return(list(out = rulesList, error = message))
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
#
# Test example for my refernece:
# test <- xmlTreeParse(eqn, ignoreBlanks = TRUE)
# print(test[[1]][[1]])
# convertML2R(test[[1]][[1]])

convertML2R <- function(node) {
  UseMethod("convertML2R", node)
}


convertML2R.default <- function(children) {
  # print("DEFAULT")
  # this gets used when a "list" of children nodes are sent in
  n <- length(children)
  expr <- c()
  for(i in 1:n) {
    expr <- c(expr, convertML2R(children[[i]]))
  }
  return(expr)
}

convertML2R.XMLNode <-function(node){
  # print("XMLNODE")
  nm <- xmlName(node)
  # PrintVar(nm)
  op_r <- mathml_tag_to_r(nm)
  if (!is.null(op_r)) {
    out <- as.character(op_r)

  } else if (nm == "ci" || nm == "csymbol") {
    # Character node, grab variable
    out <- node$children[[1]]$value

  } else if (nm == "cn") {
    # Numerical code, convert to character
    out <- as.character(node$children[[1]]$value)

  } else if(nm == "apply") {
    # print("IN APPlY")

    # SBML root + optional degree:
    #   <apply><root/>[<degree>n</degree>]<radicand/></apply>
    # n defaults to 2 (sqrt) when <degree> is absent.
    if (length(node$children) >= 1 &&
        xmlName(node$children[[1]]) == "root") {
      n <- "2"
      radicand <- NULL
      for (i in seq_along(node$children)[-1]) {
        child <- node$children[[i]]
        if (xmlName(child) == "degree") {
          n <- convertML2R(xmlChildren(child)[[1]])
        } else if (is.null(radicand)) {
          radicand <- convertML2R(child)
        }
      }
      if (identical(n, "2")) {
        out <- paste0("sqrt(", radicand, ")")
      } else {
        out <- paste0("(", radicand, ")^(1/(", n, "))")
      }
      return(out)
    }

    # If apply, recurse function to solve
    val <- convertML2R(node$children)
    # Once recursive term has ended condense the expression
    # First term is our condense term
    condense.term <- val[1]
    # print("AFTER APPLY RECURSIVE")
    # print(val)
    # print(condense.term)
    # If mathematical operator, condense with that as collapse term
    if (condense.term %in% c("*", "+", "-")) {
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
      # This is if it is a function then we condense val[1](val[2], val[n])
      # out <- paste0(val, collapse ="")
      out <- paste0(val[1], "(", paste(val[-1], collapse = ", "), ")")
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
    # this fixes libsbml problem that times is not binary
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
  op_r <- mathml_tag_to_r(nm)
  if (!is.null(op_r)) {
    val <- as.name(op_r)
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
    # SBML root + optional degree (parallel to convertML2R above).
    if (length(node$children) >= 1 &&
        xmlName(node$children[[1]]) == "root") {
      n <- 2
      radicand <- NULL
      for (i in seq_along(node$children)[-1]) {
        child <- node$children[[i]]
        if (xmlName(child) == "degree") {
          n <- mathml2R(xmlChildren(child)[[1]])[[1]]
        } else if (is.null(radicand)) {
          radicand <- mathml2R(child)[[1]]
        }
      }
      if (identical(n, 2) || identical(n, 2L)) {
        val <- bquote(sqrt(.(radicand)))
      } else {
        val <- bquote((.(radicand))^(1 / .(n)))
      }
      return(as.expression(val))
    }

    val <- mathml2R(node$children)
    mode(val) <- "call"
  } else  {cat("error: nm =",nm," not in set!\n")}
  return(as.expression(val))
}


# The next two functions are used by rules and were taken straight from read.SBML
# The idea is that SBML doesn't provide a list of atoms/leaves with rules, so we have to create them
# to place them in their model slots, and to use them to create the R function definition for the rule
# using makeLaw with a null for parameters, since they are passed global for rules.
# map MathML operator symbols into R symbols
ML2R <- function(type) {
  r <- mathml_tag_to_r(type)
  if (is.null(r)) "not found" else r
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

