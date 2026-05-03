
FindIdName <- function(Id, id_df) {
  # Searches Id database to find ID corresponding to name
  if (!(is.na(Id) | is.null(Id))) {
    idx <- which(id_df[,1] %in% Id)
    var.name <- id_df[idx, 2]
  } else {
    var.name <- NA
  }

  return(var.name)
}

# Inverse of sbml_unit_short_name: BioModME display token -> SBML kind+scale.
# Cover the prefix combos BioModME actually emits (mol, mmol, umol, nmol,
# L, mL, uL, g, kg, mg, m, cm, mm, s, min, hr, day, J).
BIOMODME_UNIT_TO_SBML <- list(
  "mol"  = list(kind = "mole",     scale =  0),
  "mmol" = list(kind = "mole",     scale = -3),
  "umol" = list(kind = "mole",     scale = -6),
  "nmol" = list(kind = "mole",     scale = -9),
  "pmol" = list(kind = "mole",     scale = -12),
  "item" = list(kind = "item",     scale =  0),

  "L"    = list(kind = "litre",    scale =  0),
  "mL"   = list(kind = "litre",    scale = -3),
  "uL"   = list(kind = "litre",    scale = -6),
  "nL"   = list(kind = "litre",    scale = -9),

  "g"    = list(kind = "gram",     scale =  0),
  "mg"   = list(kind = "gram",     scale = -3),
  "ug"   = list(kind = "gram",     scale = -6),
  "kg"   = list(kind = "kilogram", scale =  0),

  "m"    = list(kind = "metre",    scale =  0),
  "cm"   = list(kind = "metre",    scale = -2),
  "mm"   = list(kind = "metre",    scale = -3),
  "um"   = list(kind = "metre",    scale = -6),

  "s"    = list(kind = "second",   scale =  0),
  "min"  = list(kind = "minute",   scale =  0),
  "hr"   = list(kind = "hour",     scale =  0),
  "day"  = list(kind = "day",      scale =  0),

  "J"    = list(kind = "joule",    scale =  0),
  "kJ"   = list(kind = "joule",    scale =  3)
)

SbmlUnitsFromString <- function(unit_str) {
  # Decompose a BioModME display string (e.g. "mol/L", "mmol/(L*min)") into a
  # list of <unit/> attribute lists. Returns NULL for anything unparseable, so
  # the caller can fall back to omitting the unit reference.
  if (is.null(unit_str) || length(unit_str) == 0 ||
      is.na(unit_str)   || unit_str == "" || unit_str == "dimensionless") {
    return(NULL)
  }

  parse_token <- function(token, sign_) {
    token <- trimws(token)
    token <- gsub("^\\(|\\)$", "", token)
    sub_terms <- strsplit(token, "*", fixed = TRUE)[[1]]

    out <- list()
    for (st in sub_terms) {
      st <- trimws(st)
      if (st == "" || st == "1") next
      pieces <- strsplit(st, "^", fixed = TRUE)[[1]]
      base   <- pieces[1]
      exp    <- if (length(pieces) >= 2) suppressWarnings(as.numeric(pieces[2])) else 1
      if (is.na(exp)) return(NULL)

      lookup <- BIOMODME_UNIT_TO_SBML[[base]]
      if (is.null(lookup)) return(NULL)

      out[[length(out) + 1]] <- list(
        kind     = lookup$kind,
        exponent = sign_ * exp,
        scale    = lookup$scale
      )
    }
    out
  }

  parts <- strsplit(unit_str, "/", fixed = TRUE)[[1]]
  if (length(parts) > 2) return(NULL)

  num <- parse_token(parts[1], 1)
  if (is.null(num)) return(NULL)
  den <- if (length(parts) == 2) parse_token(parts[2], -1) else list()
  if (length(parts) == 2 && is.null(den)) return(NULL)

  c(num, den)
}

CollectModelUnits <- function(compartments, species, parameters) {
  # Walks the model and returns a tibble of (display, sbml_id) plus the parsed
  # <unit/> structure for each. Units that can't be decomposed are skipped.
  raw <- character(0)
  for (e in compartments) raw <- c(raw, e$Unit)
  for (e in species)      raw <- c(raw, e$Unit)
  for (e in parameters)   raw <- c(raw, e$Unit)
  raw <- unique(raw[!is.na(raw) & raw != "" & raw != "dimensionless"])

  out <- list()
  for (u in raw) {
    decomposed <- SbmlUnitsFromString(u)
    if (is.null(decomposed)) next
    out[[u]] <- list(display = u,
                     id      = paste0("bm_unit_", length(out) + 1),
                     units   = decomposed)
  }
  out
}

EscapeXmlAttr <- function(value) {
  v <- as.character(value)
  v <- gsub("&", "&amp;", v, fixed = TRUE)
  v <- gsub("<", "&lt;",  v, fixed = TRUE)
  v <- gsub(">", "&gt;",  v, fixed = TRUE)
  v <- gsub('"', "&quot;",v, fixed = TRUE)
  v
}


createSBML <- function(model, id_df) {
  # Takes model object of class SBML and converts it to filename.xml
  
  # Open file connection
  # f.id <- file(filename, "w")
  
  # Grab Components of Model
  # sbml=model[["sbml"]]
  # id=model[["id"]]
  # notes=model[["notes"]]
  # htmlNotes=model[["htmlNotes"]]
  compartments <- model[["compartments"]]
  species      <- model[["species"]]
  parameters   <- model[["parameters"]]
  rules        <- model[["rules"]]
  reactions    <- model[["reactions"]]
  functions    <- model[["functions"]]
  
  # Find lengths
  n.compartments <- length(compartments)
  n.species      <- length(species)
  n.parameters   <- length(parameters)
  n.rules        <- length(rules)
  n.reactions    <- length(reactions)
  n.functions    <- length(functions)
  
  # Other variables
  function.names <- unname(sapply(functions, 
                                  get,
                                  x = "id"))

  # Collect unique units used across compartments/species/parameters; each gets
  # an SBML id so entities can reference them.
  unit_table  <- CollectModelUnits(compartments, species, parameters)
  unit_id_for <- function(display) {
    if (is.null(display) || length(display) == 0 || is.na(display) || display == "") return(NULL)
    entry <- unit_table[[display]]
    if (is.null(entry)) NULL else entry$id
  }

  out <- c()
  # Build SBML Beginning Text --------------------------------------
  out <- c(out, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
  out <-
    c(out,
    "<sbml xmlns=\"http://www.sbml.org/sbml/level2\" level=\"2\" version=\"5\">")
  out <- c(out, paste0("<model id=", '"', "NAMETOADD", '"', ">"))

  tryCatch(expr = {
    # Write Unit Definitions ---------------------------------------------------
    if (length(unit_table) > 0) {
      out <- c(out, "<listOfUnitDefinitions>")
      for (entry in unit_table) {
        out <- c(out, paste0('<unitDefinition id="', entry$id, '">'),
                      "<listOfUnits>")
        for (u in entry$units) {
          attrs <- paste0(' kind="',     u$kind,     '"',
                          ' exponent="', u$exponent, '"',
                          ' scale="',    u$scale,    '"',
                          ' multiplier="1"')
          out <- c(out, paste0("<unit", attrs, "/>"))
        }
        out <- c(out, "</listOfUnits>", "</unitDefinition>")
      }
      out <- c(out, "</listOfUnitDefinitions>")
    }

    # Write Functions ----------------------------------------------------------
    if (n.functions > 0) {
      out <- c(out, "<listOfFunctionDefinitions>")
      for (i in seq_along(functions)) {
        entry <- functions[[i]]
        
        id   <- entry$id
        name <- entry$name
        law  <- entry$law
        
        out <- c(out,
                 paste0("<functionDefinition id=", '"', id, '" ',
                        "name=", '"', name, '"', 
                        ">"))
        
        # Build mathml expression
        vars <- strsplit(entry$variables, ", ")[[1]]
        out <- c(out, "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">")
        out <- c(out, "<lambda>")
        # Add function variables to mathml lambda expression
        for (j in seq_along(vars)) {
          out <- c(out, 
                   paste0("<bvar>",
                          "<ci> ",
                          vars[j], 
                          " </ci></bvar>"))
        }
        # Add mathml term
        # out <- c(out, expToMathML(parse(text=law)[[1]]))
        out <- c(out, string2mathml(law))
        out <- c(out, "</lambda>")
        out <- c(out, "</math>")
        out <- c(out, "</functionDefinition>")
      }
      out <- c(out, "</listOfFunctionDefinitions>")
    }
    
    # Write Compartments -------------------------------------------------------
    if (n.compartments > 0) {
      out <- c(out, "<listOfCompartments>")
      for (i in seq_along(compartments)) {
        entry <- compartments[[i]]
        # entry$id is the sanitized R-safe name (used as SBML id);
        # entry$name is the original DisplayName preserved through import.
        id    <- entry$id
        name  <- entry$name
        size  <- entry$size
        cont  <- entry$constant
        s.dim <- entry$spatialDimensions
        unit_id <- unit_id_for(entry$Unit)
        units_attr <- if (!is.null(unit_id)) paste0('units="', unit_id, '" ') else ""

        out <- c(out,
                 paste0("<compartment id=", '"', id, '" ',
                        "size=", '"', size, '" ',
                        "name=", '"', name, '" ',
                        units_attr,
                        "constant=", '"', cont, '" ',
                        "spatialDimensions=", '"', s.dim, '"', "/>")
                 )
      }

      out <- c(out, "</listOfCompartments>")
    }

    # Write Species ------------------------------------------------------------
    if (n.species > 0) {
      out <- c(out, "<listOfSpecies>")
      for (i in seq_along(species)) {
        entry      <- species[[i]]

        id         <- entry$id
        name       <- entry$name
        init.conc  <- entry$initialConcentration
        compart    <- FindIdName(entry$compartment, id_df)
        cont       <- entry$constant
        bc         <- entry$boundaryCondition
        unit_id    <- unit_id_for(entry$Unit)
        sub_attr   <- if (!is.null(unit_id)) paste0('substanceUnits="', unit_id, '" ') else ""

        out <- c(out,
                 paste0("<species id=", '"', id, '" ',
                        "name=", '"', name, '" ',
                        "initialConcentration=", '"', init.conc, '" ',
                        sub_attr,
                        "compartment=", '"', compart, '" ',
                        "constant=", '"', cont, '" ',
                        "boundaryCondition=", '"', bc, '"',
                        "/>")
        )
      }
      out <- c(out, "</listOfSpecies>")
    }

    # Write Parameters ---------------------------------------------------------
    if (n.parameters > 0) {
      out <- c(out, "<listOfParameters>")
      for (i in seq_along(parameters)) {
        entry      <- parameters[[i]]

        id         <- entry$id
        name       <- entry$name
        value      <- entry$value
        cont       <- entry$constant
        unit_id    <- unit_id_for(entry$Unit)
        units_attr <- if (!is.null(unit_id)) paste0('units="', unit_id, '" ') else ""

        out <- c(out,
                 paste0("<parameter id=", '"', id, '" ',
                        "name=", '"', name, '" ',
                        "value=", '"', value, '" ',
                        units_attr,
                        "constant=", '"', cont, '" ',
                        "/>")
        )
      }
      out <- c(out, "</listOfParameters>")
    }
    
    # Write Reactions ----------------------------------------------------------
    if (n.reactions > 0) {
      # browser()
      out <- c(out, "<listOfReactions>")
      for (i in seq_along(reactions)) {
        # print("Cycling Reactions")
        # print(reactions[[i]])
        entry <- reactions[[i]]
        # Create initial meta-tag (id, name, reversible, fast)
        # added tolower because of instances of FALSE slipping through and 
        # this is just a good catch all to have. 
        id         <- entry$id
        name       <- entry$name
        reversible <- tolower(entry$reversible)
        fast       <- tolower(entry$fast)
        func.used  <- entry$function.id
        str.law    <- entry$string.law
        
        out <- c(out,
                 paste0("<reaction id=", '"', id, '" ',
                        "name=", '"', name, '" ',
                        "reversible=", '"', reversible, '" ',
                        "fast=", '"', fast, '" ',
                        ">")
        )
        
        # These are the ids of these
        r.reactants  <- SplitEntry(entry$reactants)
        r.products   <- SplitEntry(entry$products)
        r.modifiers  <- SplitEntry(entry$modifiers)
        r.parameters <- SplitEntry(entry$parameters)
        r.par.name   <- SplitEntry(entry$parameter.names)
        r.par.value  <- SplitEntry(entry$parameter.values)
        
        all.var <- RemoveNA(c(r.reactants, 
                              r.products, 
                              r.modifiers, 
                              r.parameters))
        
        # Determine stoich coefficients
        # browser()
        if (!is.na(entry$eqn.text)) {
          stoich.coef <- extract_coefficients(entry$eqn.text)
          stoic.reactant <- stoich.coef$reactants
          stoic.products <- stoich.coef$products
        } else {
          # Input outputs don't have an eqn text 
          # Find num react/prod and create vect
          stoic.reactant <- rep(1, length(r.reactants))
          stoic.products <- rep(1, length(r.products))
        }

        # Build <listOfSpecies>
        if (!is.na(entry$reactants)) {
          out <- c(out, "<listOfReactants>")
          # reactants <- strsplit(entry$reactants, ", ")[[1]]
          for (j in seq_along(r.reactants)) {
            r <- FindIdName(r.reactants[j], id_df)
            s <- stoic.reactant[j]
            out <- c(out, 
                     paste0("<speciesReference species=", '"', r, '" ',
                            "stoichiometry=", '"', s, '"',
                            "/>"))
          }
          out <- c(out, "</listOfReactants>")
        }
        
        # Build <listOfProducts>
        if (!is.na(entry$products)) {
          out <- c(out, "<listOfProducts>")
          # products <- strsplit(entry$products, ", ")[[1]]
          for (j in seq_along(r.products)) {
            p <- FindIdName(r.products[j], id_df)
            s <- stoic.products[j]
            out <- c(out, 
                     paste0("<speciesReference species=", '"', p, '" ',
                            "stoichiometry=", '"', s, '"',
                            "/>"))
          }
          
          out <- c(out, "</listOfProducts>")
        }
        
        # Build <listOfModifiers>
        if (!is.na(entry$modifiers)) {
          out <- c(out, "<listOfModifiers>")
          # modifiers <- strsplit(entry$modifiers, ", ")[[1]]
          for (j in seq_along(r.modifiers)) {
            m <- FindIdName(r.modifiers[j], id_df)
            out <- c(out, 
                     paste0("<modifierSpeciesReference species=", '"', m, '"',
                            "/>"))
          }
          
          out <- c(out, "</listOfModifiers>")
        }
        
        # Build <kineticLaw>
        # Determine if law used function in function
        write.raw.mathml <- TRUE
        if (n.functions > 0) {
          if (func.used %in% function.names) {
            write.raw.mathml <- FALSE
            fxn.to.write <- function.names[match(func.used, function.names)]
          }
        }
        
        if (write.raw.mathml) {
          # Write rate law in mathml version
          out <- c(out, 
                   paste0("<kineticLaw>",
                          "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">",
                          string2mathml(str.law),
                          "</math>")
          )
          
          # Add parameters to reaction
          if (!is.na(entry$parameters)) {
            param.ml <- c()
            for (j in seq_along(r.parameters)) {
              to.add <- paste0("<parameter id=", '"', r.par.name[j], '" ',
                               "name=", '"', r.par.name[j], '" ',
                               "value=", '"', r.par.value[j], '"',
                               "/>")
              param.ml <- c(param.ml, to.add)
            }
            
            out <- c(out, 
                     paste0("<listOfParameters>",
                            paste0(param.ml, collapse = ""),
                            "</listOfParameters>"))
          }
          out <- c(out, "</kineticLaw>")
        } else {
          # Write function call
          # ignore compartment call for now
          opener <- 
            paste0("<kineticLaw>",
                   "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">")
          
          fxn.ml.opener <- paste0("<apply>",
                                  "<ci> ", fxn.to.write, " </ci>")
          
          fxn.var.ml <- vector(mode = "character", length = length(all.var))
          for (j in seq_along(all.var)) {
            fxn.var.ml[j] <- paste0("<ci> ", all.var[j], " </ci>")
          }
          fxn.closer <- "</apply></math>"
          
          str.to.add <- paste0(
            opener,
            fxn.ml.opener,
            paste0(fxn.var.ml, collapse = ""),
            fxn.closer)
          out <- c(out, 
                   str.to.add)
          
          
          if (!is.na(entry$parameters)) {
            param.ml <- c()
            for (j in seq_along(r.parameters)) {
              to.add <- paste0("<parameter id=", '"', r.parameters[j], '" ',
                               "name=", '"', r.par.name[j], '" ',
                               "value=", '"', r.par.value[j], '"',
                               "/>")
              param.ml <- c(param.ml, to.add)
            }
            
            out <- c(out, 
                     paste0("<listOfParameters>",
                            paste0(param.ml, collapse = ""),
                            "</listOfParameters>"))
          }
          out <- c(out,"</kineticLaw>")
        }

        # End Reaction
        out <- c(out, "</reaction>")
      }
      out <- c(out, "</listOfReactions>")
    }
    print("end writing reactions")
    # Write Rules --------------------------------------------------------------
    if (n.rules > 0) {
      out <- c(out, "<listOfRules>")
      for (i in seq_along(rules)) {
        entry <- rules[[i]]
        
        varName    <- entry$variable
        mathml.law <- entry$mathml.eqn
        
        
        out <- c(out,
                 paste0("<assignmentRule metaid=", '"', paste0("rule", i), '" ',
                        "variable=", '"', varName, '"', 
                        ">",
                        mathml.law))
        

        out <- c(out, "</assignmentRule>")
      }
      out <- c(out, "</listOfRules>")
    }
    
    


  })
  
  out <- c(out, "</model>")
  out <- c(out, "</sbml>")

  out <- paste0(out, collapse = "\n")
  return(out)
}