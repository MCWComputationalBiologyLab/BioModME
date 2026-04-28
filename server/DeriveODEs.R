

DeriveDifferentialEquations <- function(compartments.rv,
                                        species.rv,
                                        reactions.rv,
                                        IO.rv,
                                        id.rv) 
{
  # Derive all equation/IO based differential equations from stored model 
  # information. The idea is to loop through each species, deriving its 
  # equations and then moving on to the next species. Each species list has a 
  # variable noting its equations and IOs IDs to derive from. These ids are 
  # looked up in their respective lists and the rate law is taken from there
  # and concatenated to the existing chain.
  
  # Model Description: 
  # Inputs
  # @compartments.rv - reactive variable containing all compartment info
  # @species.rv - reactive variable containing all species info (rv.SPECIES)
  # @reactions.rv - reactive variable containing all reaction info
  # @IO.rv - reactive variable containing all input/output info
  # @id.rv - dataframe (RV) containing all id pairs
  
  # Outputs
  #list:
  # @eqns - vector of differential equations in string form
  # @latex.eqns - vector of differential equations in latex form
  # @eqns.for.calc - vector of diffeqs with proper volume divided

  # Break down var data structure.
  species.list  <- species.rv$species
  species.names <- species.rv$species.names
  species.ids   <- names(species.list)
  
  comp.df   <- compartments.rv$compartments.df
  comp.list <- compartments.rv$compartments
  
  # Initialize Output Vectors
  differential.equations     <- vector()
  differential.eqns.latex    <- vector()
  differential.eqns.for.calc <- vector()
  
  results <- list()
  # Cycle through each speices
  for (i in seq_along(species.list)) {
    # Find specifics of species
    spec.name   <- species.list[[i]]$Name
    spec.id     <- species.list[[i]]$ID
    comp.id     <- species.list[[i]]$Compartment.id
    comp.name   <- species.list[[i]]$Compartment
    comp.vol    <- comp.list[[comp.id]]$Volume
    comp.vol.id <- comp.list[[comp.id]]$par.id
    
    # Initialize bools
    has.eqn.ode <- FALSE
    has.IO.ode  <- FALSE
    
    # Solve for eqn based odes
    if (!is.na(species.list[[i]]$Reaction.ids)) {
      has.eqn.ode <- TRUE
      eqn.ODEs <- DeriveEquationBasedODEs(species.list[[i]],
                                          compartments.rv,
                                          reactions.rv)
    }
    
    
    # Solve for IO based odes 
    if (!is.na(species.list[[i]]$IO.ids)) {
      has.IO.ode <- TRUE
      IO.ODEs <- DeriveIOBasedODEs(species.list[[i]],
                                   compartments.rv,
                                   IO.rv) 
    }
    
    # Combine ODE String if Needed
    if (!has.eqn.ode && !has.IO.ode) {
      out.vector     <- "0"
      out.string     <- "0"
      latex.vector   <- "0"
      latex.string   <- "0"
      mathjax.vector <- "0"
      mathjax.string <- "0"
      descript.vec   <- "No Differential Equations"
    } else if (has.eqn.ode & !has.IO.ode) {
      out.vector     <- eqn.ODEs$eqn.vector
      out.string     <- eqn.ODEs$eqn.string
      latex.vector   <- eqn.ODEs$latex.vector
      latex.string   <- eqn.ODEs$latex.string
      mathjax.vector <- eqn.ODEs$mathjax.vector
      mathjax.string <- eqn.ODEs$mathjax.string
      descript.vec   <- eqn.ODEs$description.vector
    } else if (!has.eqn.ode & has.IO.ode) {
      out.vector     <- IO.ODEs$eqn.vector
      out.string     <- IO.ODEs$eqn.string
      latex.vector   <- IO.ODEs$latex.vector
      latex.string   <- IO.ODEs$latex.string
      mathjax.vector <- IO.ODEs$mathjax.vector
      mathjax.string <- IO.ODEs$mathjax.string
      descript.vec   <- IO.ODEs$description.vector
    } else {
      out.vector     <- c(eqn.ODEs$eqn.vector, IO.ODEs$eqn.vector)
      out.string     <- paste0(eqn.ODEs$eqn.string, 
                               IO.ODEs$eqn.string, 
                               collapse = "")
      latex.vector   <- c(eqn.ODEs$latex.vector, IO.ODEs$latex.vector)
      latex.string   <- paste0(eqn.ODEs$latex.string,
                               IO.ODEs$latex.string,
                               collapse = "")
      mathjax.vector <- c(eqn.ODEs$mathjax.vector, IO.ODEs$mathjax.vector)
      mathjax.string <- paste0(eqn.ODEs$mathjax.string,
                               IO.ODEs$mathjax.string,
                               collapse = "")
      descript.vec   <- c(eqn.ODEs$description.vector,
                          IO.ODEs$description.vector)
    }
    
    # Build ODE for solver expression 
    if (out.string != "0") {
      for.solver <- paste0("(", out.string, ")", "/", comp.vol)
    } else {
      for.solver <- "0"
    }
    
    # Solve for IO based Odes
    results[[species.ids[i]]] <- list(
      "Species.ID"          = spec.id,
      "Name"                = spec.name,
      "Compartment.id"      = comp.id,
      "Compartment.name"    = comp.name,
      "Compartment.vol"     = comp.vol,
      "Compartment.vol.id"  = comp.vol.id,
      "ODES.eqn.vector"     = out.vector,
      "ODES.eqn.string"     = out.string,
      "ODES.latex.vector"   = latex.vector,
      "ODES.latex.string"   = latex.string,
      "ODES.mathjax.vector" = mathjax.vector,
      "ODES.mathjax.string" = mathjax.string,
      "ODE.for.solver"      = for.solver,
      "Description.vector"  = descript.vec
      )
  }

  return(results)
}

DeriveEquationBasedODEs <- function(species.list.entry,
                                    compartments.rv,
                                    reactions.rv) {
  # Derive the ODEs for equation based problems. Here we are passed the entry 
  # For the species list, use that to find corresponding reaction ids.
  # If there are none, NA is returned. Else those reaction ids search
  # the reactions.rv to find the corresponding rate law, which is stored to 
  # a vector. 
  
  # Args
  #   @species.list.entry - specific species entry containing all info 
  #   @compartments.rv - reactive variable containing all compartment info - 
  #   @reactions.rv - reactive variable containing all reaction info
  
  #   @ ODE - vector of rate law expressions for each reaction (NA if none)
  # browser()
  # Find in species.list 
  name <- species.list.entry$Name
  id   <- species.list.entry$ID

  # browser()
  ODE          <- c()
  latex.ODE    <- c()
  mathjax.ODE  <- c()
  descriptions <- c()
  
  if (is.na(species.list.entry$Reaction.ids)) {
    
  } else {
    reactions <- strsplit(species.list.entry$Reaction.ids, ", ")[[1]]
    for (eqn.id in reactions) {
      # Extract equation by ID and appropriate laws
      eqn        <- reactions.rv$reactions[[eqn.id]]
      rate       <- eqn$String.Rate.Law
      latex.rate <- eqn$Latex.Rate.Law
      mj.rate    <- eqn$MathJax.Rate.Law
      law        <- eqn$Reaction.Law
      descript   <- eqn$Description

      # logistic_competition stores per-species rate laws on the lc entry so
      # one reaction row can drive ODEs for both species independently.
      if (!is.null(law) && law == "logistic_competition") {
        lc <- reactions.rv$logisticCompetition[[eqn.id]]
        if (!is.null(lc)) {
          new.rate <- NULL
          if (id == lc$Species.X.id && !is.null(lc$rate.law.x)) {
            new.rate <- lc$rate.law.x
          } else if (id == lc$Species.Y.id && !is.null(lc$rate.law.y)) {
            new.rate <- lc$rate.law.y
          }
          if (!is.null(new.rate)) {
            rate       <- new.rate
            fmt        <- ConvertRateLaw(new.rate)
            latex.rate <- fmt$latex
            mj.rate    <- fmt$mathjax
          }
        }
      }
      # predator_prey stores the FULL net rate law for each species (including
      # both gain and loss terms) on the pp entry. DeriveODEs picks the right
      # one based on which species the ODE is being assembled for. Both prey
      # and predator are flagged as products so the standard sign mechanism
      # gives them "+" — the sign flips for the predation/death terms are
      # already inside the rate law expression.
      if (!is.null(law) && law == "predator_prey") {
        pp <- reactions.rv$predatorPrey[[eqn.id]]
        if (!is.null(pp)) {
          new.rate <- NULL
          if (id == pp$Prey.id && !is.null(pp$rate.law.x)) {
            new.rate <- pp$rate.law.x
          } else if (id == pp$Predator.id && !is.null(pp$rate.law.y)) {
            new.rate <- pp$rate.law.y
          }
          if (!is.null(new.rate)) {
            rate       <- new.rate
            fmt        <- ConvertRateLaw(new.rate)
            latex.rate <- fmt$latex
            mj.rate    <- fmt$mathjax
          }
        }
      }
      # competitive_monod stores per-species (X, Y) growth rate laws and the
      # substrate consumption rate(s) on the cm entry. Each species pulls its
      # own rate law; the substrate combines both species' consumption rates
      # (and gets the negative sign via the standard reactant handling below).
      if (!is.null(law) && law == "competitive_monod") {
        cm <- reactions.rv$competitiveMonod[[eqn.id]]
        if (!is.null(cm)) {
          new.rate <- NULL
          if (id == cm$Species.X.id && !is.null(cm$rate.law.x) &&
              !(length(cm$rate.law.x) == 1 && is.na(cm$rate.law.x))) {
            new.rate <- cm$rate.law.x
          } else if (!is.null(cm$Species.Y.id) && id == cm$Species.Y.id &&
                     !is.null(cm$rate.law.y) &&
                     !(length(cm$rate.law.y) == 1 && is.na(cm$rate.law.y))) {
            new.rate <- cm$rate.law.y
          } else if (id == cm$Substrate.id) {
            sx <- cm$rate.law.s.x
            sy <- cm$rate.law.s.y
            sx.has <- !is.null(sx) && !(length(sx) == 1 && is.na(sx))
            sy.has <- !is.null(sy) && !(length(sy) == 1 && is.na(sy))
            if (sx.has && sy.has) {
              new.rate <- paste0(sx, "+", sy)
            } else if (sx.has) {
              new.rate <- sx
            } else if (sy.has) {
              new.rate <- sy
            }
          }
          if (!is.null(new.rate)) {
            rate       <- new.rate
            fmt        <- ConvertRateLaw(new.rate)
            latex.rate <- fmt$latex
            mj.rate    <- fmt$mathjax
          }
        }
      }

      applyMultiple <- FALSE
      multiple      <- "1"

      # Find if species Entry is in reactant or product
      # Check if id reacantid is even exists
      if (!is.na(eqn$Reactants.id)) {
        inReactant <- id %in% strsplit(eqn$Reactants.id, ", ")[[1]]
      } else {
        inReactant <- FALSE
      }
      # Check for mass action reaction, then check stoich for modifiers
      if (law == "mass_action" || law == "mass_action_w_reg") {
        #if in mass action, search mass action df
        if (law == "mass_action") {
          ma.list <- reactions.rv$massAction[[eqn.id]]
        } else if (law == "mass_action_w_reg") {
          ma.list <- reactions.rv$massActionwReg[[eqn.id]]
        }
        # check for stoich modifier
        if (inReactant) {
          # Determine which index
          reactant.names <- strsplit(ma.list$Reactants, ", ")[[1]]
          idx <- match(name, reactant.names)
          stoich <- strsplit(ma.list$r.stoichiometry, ", ")[[1]]
          if (stoich[idx] != "1") {
            applyMultiple <- TRUE
            multiple <- stoich[idx]
          }
        } else {
          product.names <- strsplit(ma.list$Products, ", ")[[1]]
          idx <- which(product.names %in% name)
          stoich <- strsplit(ma.list$p.stoichiometry, ", ")[[1]]
          if (stoich[idx] != "1") {
            applyMultiple <- TRUE
            multiple <- stoich[idx]
          }
        }
      } else if (law == "user_custom_law_CUSTOM") {
        # browser()
        reaction.eqn <- eqn$Equation.Text
        stoich <- extract_coefficients(reaction.eqn)
        r.stoich <- stoich$reactants
        p.stoich <- stoich$products
        if (inReactant) {
          reactant.names <- strsplit(eqn$Reactants, ", ")[[1]]
          idx <- match(name, reactant.names)
          stoich.to.apply <- as.character(r.stoich[idx])
          if (stoich.to.apply != "1") {
            applyMultiple <- TRUE
            multiple <- stoich.to.apply
          }
        } else {
          product.names <- strsplit(eqn$Products, ", ")[[1]]
          idx <- which(product.names %in% name)
          stoich.to.apply <- as.character(p.stoich[idx])
          if (stoich.to.apply != "1") {
            applyMultiple <- TRUE
            multiple <- stoich.to.apply
          }
        }
      }

      # If this is a degradation reaction with products and a krel parameter,
      # multiply the product-formation rate by krel. Reactant (species being
      # degraded) keeps the unscaled rate.
      krel <- NA
      if (!inReactant && !is.null(law)) {
        if (law == "degradation_rate" &&
            eqn.id %in% names(reactions.rv$degradation.by.rate)) {
          degInfo <- reactions.rv$degradation.by.rate[[eqn.id]]
          if ("krel" %in% names(degInfo) && !is.na(degInfo$krel) && degInfo$krel != "") {
            krel <- degInfo$krel
          }
        } else if (law == "degradation_by_enzyme" &&
                   eqn.id %in% names(reactions.rv$degradation.by.enzyme)) {
          degInfo <- reactions.rv$degradation.by.enzyme[[eqn.id]]
          if ("krel" %in% names(degInfo) && !is.na(degInfo$krel) && degInfo$krel != "") {
            krel <- degInfo$krel
          }
        }
      }

      if (!is.na(krel)) {
        # Enzyme degradation rate has form V*(Vmax*S/(Km+S)) or
        # V*(kcat*E*S/(Km+S)) — has a "/". Rate-based degradation is just
        # V*(k_d*S) or V*(k_d) — no "/". Use that to choose insertion point:
        # before the "/" for enzyme; after the rate constant for rate.
        insert.has.division <- function(s) {
          sub("(\\*[^/]+)(/)", paste0("\\1*", krel, "\\2"), s)
        }
        insert.no.division <- function(s) {
          if (grepl("\\*", s)) {
            sub("(\\([^\\*]+)(\\*)", paste0("\\1*", krel, "\\2"), s)
          } else {
            sub("(\\))", paste0("*", krel, "\\1"), s)
          }
        }

        if (grepl("/", rate)) {
          rate <- insert.has.division(rate)
        } else {
          rate <- insert.no.division(rate)
        }
        if (grepl("/", latex.rate)) {
          latex.rate <- insert.has.division(latex.rate)
        } else {
          latex.rate <- insert.no.division(latex.rate)
        }
        if (grepl("/", mj.rate)) {
          mj.rate <- insert.has.division(mj.rate)
        } else {
          mj.rate <- insert.no.division(mj.rate)
        }
      }

      # Build ODE expression
      if (inReactant) {sign <- "-"} else {sign <- "+"}

      if (applyMultiple) {
        ODE <- c(ODE, 
                 paste0(sign, multiple, "*(", rate,")"))
        
        latex.ODE <- c(latex.ODE, 
                       paste0(sign, multiple, "*(", latex.rate, ")"))
        
        mathjax.ODE <- c(mathjax.ODE, 
                         paste0(sign, multiple, "*", 
                                "\\left(", mj.rate, "\\right)"))
      } else {
        ODE <- c(ODE, 
                 paste0(sign, "(", rate, ")"))
        latex.ODE <- c(latex.ODE, 
                       paste0(sign, "(", latex.rate, ")"))
        mathjax.ODE <- c(mathjax.ODE, 
                         paste0(sign, "\\left(", mj.rate, "\\right)"))
      }
      descriptions <- c(descriptions, descript)
    }
  }
  
  # Output list of ODE values
  out <- list("eqn.vector"         = ODE,
              "eqn.string"         = paste0(ODE, collapse = ""),
              "latex.vector"       = latex.ODE,
              "latex.string"       = paste0(latex.ODE, collapse=""),
              "mathjax.vector"     = mathjax.ODE,
              "mathjax.string"     = paste0(mathjax.ODE, collapse = ""),
              "description.vector" = descriptions
              )
  
  return(out)
}

DeriveIOBasedODEs <- function(species.list.entry, 
                              compartments.rv,
                              IO.rv) {
  # Derive the ODEs for I\O based problems. Here we are passed the entry 
  # For the species list, use that to find corresponding reaction ids.
  # If there are none, NA is returned. Else those reaction ids search
  # the reactions.rv to find the corresponding rate law, which is stored to 
  # a vector. 
  
  # Args
  #   @species.list.entry - specific species entry containing all info 
  #   @compartments.rv - reactive variable containing all compartment info - 
  #   @IO.rv - reactive variable containing all IO info
  
  #   @ ODE - vector of rate law expressions for each reaction (NA if none)
  
  # Find in species.list 
  species.name <- species.list.entry$Name
  species.id   <- species.list.entry$ID
  
  
  ODE          <- c()
  latex.ODE    <- c()
  mathjax.ODE  <- c()
  descriptions <- c()

  IOs <- strsplit(species.list.entry$IO.ids, ", ")[[1]]
  for (io.id in IOs) {
    eqn        <- IO.rv$InputOutput[[io.id]]
    type       <- eqn$Type
    rate       <- eqn$String.Rate.Law
    latex.rate <- eqn$Latex.Rate.Law
    mj.rate    <- eqn$MathJax.Rate.Law
    law        <- eqn$Reaction.Law
    direction  <- eqn$Direction
    descript   <- eqn$Description
    
    # Multple rate flows stored
    if (type == "FLOW_BETWEEN") {
      # The flow between rate laws are stored per variable
      # Ex. 
      # $IO0001$Species.Ids
      # [1] "var0003, var0004, var0001"
      # $IO0001$String.Rate.Law
      # [1] "-(F_out_1*var_1), +(F_in_1*A_2), +(F_in_1.2*A_3)"
      # Where each variable and law is separated by a comma

      # Unsplit Ids and find index of variable id
      ids      <- strsplit(eqn$Species.Ids, ", ")[[1]]
      str.laws <- strsplit(eqn$String.Rate.Law, ", ")[[1]]
      lat.laws <- strsplit(eqn$Latex.Rate.Law, ", ")[[1]]
      mj.laws  <- strsplit(eqn$MathJax.Rate.Law, ", ")[[1]]
      

      # Use index to access rate law.
      idx <- which(ids %in% species.id)
      # Store that rate law as output
      ODE <- c(ODE, str.laws[idx])
      latex.ODE <- c(latex.ODE, lat.laws[idx])
      mathjax.ODE <- c(mathjax.ODE, mj.laws[idx])
      descriptions <- c(descriptions, descript)
      
    } else {
      if (direction == "Input") {
        sign = "+"
      } else if (direction == "Output") {
        sign = "-"
      } else if (direction == "Both") {
        # Need to determine if species is input/output
        # Search Species in, else classify it as species out
        inSpecies <- species.id %in% strsplit(eqn$Species.In.Ids, ", ")[[1]]
        if (inSpecies) {sign = "+"} else {sign = "-"}
      }
      
      ODE <- c(ODE, 
               paste0(sign, "(", rate, ")"))
      latex.ODE <- c(latex.ODE, 
                     paste0(sign, "(", latex.rate, ")"))
      mathjax.ODE <- c(mathjax.ODE, 
                       paste0(sign, "\\left(", mj.rate, "\\right)"))
      descriptions <- c(descriptions, descript)
    }
  }
  
  # Output list of ODE values
  out <- list("eqn.vector"     = ODE,
              "eqn.string"     = paste0(ODE, collapse = ""),
              "latex.vector"   = latex.ODE,
              "latex.string"   = paste0(latex.ODE, collapse=""),
              "mathjax.vector" = mathjax.ODE,
              "mathjax.string" = paste0(mathjax.ODE, collapse = ""),
              "description.vector" = descriptions
  )
  
}
