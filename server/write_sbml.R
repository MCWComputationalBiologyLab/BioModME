
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
  # units        <- model[["units"]]
  
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
  PrintVar(function.names)
  
  out <- c()
  # Build SBML Beginning Text --------------------------------------
  out <- c(out, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
  out <- 
    c(out, 
    "<sbml xmlns=\"http://www.sbml.org/sbml/level2\" level=\"2\" version=\"5\">")
  out <- c(out, paste0("<model id=", '"', "NAMETOADD", '"', ">"))
  
  tryCatch(expr = {
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
        id    <- entry$name
        name  <- entry$name
        size  <- entry$size
        cont  <- entry$constant
        s.dim <- entry$spatialDimensions
        
        out <- c(out,
                 paste0("<compartment id=", '"', name, '" ',
                        "size=", '"', size, '" ',
                        "name=", '"', name, '" ',
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
        
        id         <- entry$name
        name       <- entry$name
        init.conc  <- entry$initialConcentration
        sub.units  <- entry$substanceUnits
        compart    <- FindIdName(entry$compartment, id_df)
        cont       <- entry$constant
        bc         <- entry$boundaryCondition
        
        out <- c(out,
                 paste0("<species id=", '"', name, '" ',
                        "name=", '"', name, '" ',
                        "initialConcentration=", '"', init.conc, '" ',
                        #"substanceUnits=", '"', sub.units, '" ',
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
        
        id         <- entry$name
        name       <- entry$name
        value      <- entry$value
        cont       <- entry$constant
        
        out <- c(out,
                 paste0("<parameter id=", '"', name, '" ',
                        "name=", '"', name, '" ',
                        "value=", '"', value, '" ',
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
        entry <- reactions[[i]]
        # Create initial meta-tag (id, name, reversible, fast)
        id         <- entry$id
        name       <- entry$name
        reversible <- entry$reversible
        fast       <- entry$fast
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
        stoich.coef <- extract_coefficients(entry$eqn.text)
        stoic.reactant <- stoich.coef$reactants
        stoic.products <- stoich.coef$products
        
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