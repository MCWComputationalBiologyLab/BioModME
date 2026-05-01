# Bipartite-graph derivation for the Model Diagram tab.
#
# Pure R helper that converts the current model state into nodes + edges
# suitable for the modelDiagram htmlwidget. No reactive context here —
# callers pass plain lists. This makes the function easy to test in
# isolation and easy to call from inside isolate() blocks on the server
# side.
#
# Output schema:
#   list(
#     nodes             = data.frame(...),  # species + reaction nodes
#     edges             = data.frame(...),  # reactant / product / modifier edges
#     compartmentGroups = data.frame(...)   # one row per compartment with a color
#   )

# Split a comma-separated id string from rv.REACTIONS into a character vector.
# Handles NA / NULL / "NA" / empty string by returning character(0). Strips
# whitespace; tolerates either ", " or "," as the separator.
DiagramSplitIds <- function(x) {
  if (is.null(x) || length(x) == 0) return(character(0))
  if (length(x) == 1 && (is.na(x) || x == "" || identical(x, "NA"))) {
    return(character(0))
  }
  ids <- unlist(strsplit(as.character(x), ",", fixed = TRUE))
  ids <- trimws(ids)
  ids[!is.na(ids) & nzchar(ids) & ids != "NA"]
}

# Returns list(reactants, products, modifiers) of species ids for a single
# reaction, dispatching on Reaction.Law. The master reaction entry holds
# Reactants.id / Products.id / Modifiers.id directly for chemistry laws;
# the bacterial growth laws keep their species references on companion
# entries (rv.REACTIONS$exponentialGrowth etc.) and leave the master fields
# NA, so we have to look them up there.
DiagramRoleAssignments <- function(reaction, companions) {
  law <- reaction$Reaction.Law
  if (is.null(law) || is.na(law)) law <- ""

  empty <- list(reactants = character(0),
                products  = character(0),
                modifiers = character(0))

  if (law == "exponential_growth") {
    info <- companions$exponentialGrowth[[reaction$ID]]
    if (is.null(info) || is.null(info$Species.id)) return(empty)
    sp <- DiagramSplitIds(info$Species.id)
    return(list(reactants = sp, products = sp, modifiers = character(0)))
  }

  if (law == "monod_growth") {
    info <- companions$monodGrowth[[reaction$ID]]
    if (is.null(info)) return(empty)
    sp  <- DiagramSplitIds(info$Species.id)
    sub <- DiagramSplitIds(info$Substrate.id)
    return(list(reactants = c(sub, sp),
                products  = sp,
                modifiers = character(0)))
  }

  if (law == "logistic_competition") {
    info <- companions$logisticCompetition[[reaction$ID]]
    if (is.null(info)) return(empty)
    sx <- DiagramSplitIds(info$Species.X.id)
    sy <- DiagramSplitIds(info$Species.Y.id)
    both <- c(sx, sy)
    return(list(reactants = both, products = both, modifiers = character(0)))
  }

  if (law == "competitive_monod") {
    info <- companions$competitiveMonod[[reaction$ID]]
    if (is.null(info)) return(empty)
    sx  <- DiagramSplitIds(info$Species.X.id)
    sy  <- DiagramSplitIds(info$Species.Y.id)
    sub <- DiagramSplitIds(info$Substrate.id)
    return(list(reactants = c(sub, sx, sy),
                products  = c(sx, sy),
                modifiers = character(0)))
  }

  if (law == "predator_prey") {
    info <- companions$predatorPrey[[reaction$ID]]
    if (is.null(info)) return(empty)
    pr <- DiagramSplitIds(info$Prey.id)
    pd <- DiagramSplitIds(info$Predator.id)
    both <- c(pr, pd)
    return(list(reactants = both, products = both, modifiers = character(0)))
  }

  # Default path: chemistry / synthesis / degradation laws all populate
  # Reactants.id / Products.id / Modifiers.id directly on the master entry.
  list(
    reactants = DiagramSplitIds(reaction$Reactants.id),
    products  = DiagramSplitIds(reaction$Products.id),
    modifiers = DiagramSplitIds(reaction$Modifiers.id)
  )
}

# Build the bipartite graph. Inputs are plain lists, not reactive values.
#
# species_list:   rv.SPECIES$species
# reactions_list: rv.REACTIONS$reactions
# compartments:   rv.COMPARTMENTS$compartments
# companions:     reactiveValuesToList(rv.REACTIONS) — needed for the
#                 per-law companion entries (exponentialGrowth, monodGrowth,
#                 logisticCompetition, competitiveMonod, predatorPrey)
BuildDiagramGraph <- function(species_list,
                              reactions_list,
                              compartments,
                              companions) {

  # ---- Compartment groups ----
  comp_ids   <- if (length(compartments) > 0) names(compartments) else character(0)
  comp_names <- if (length(compartments) > 0) {
    vapply(compartments, function(c) {
      if (is.null(c$Name) || is.na(c$Name)) "" else as.character(c$Name)
    }, character(1))
  } else character(0)
  comp_colors <- if (length(comp_ids) > 0) {
    # Stable viridis palette across compartments
    pal <- if (requireNamespace("viridisLite", quietly = TRUE)) {
      viridisLite::viridis(max(length(comp_ids), 3), alpha = 0.20)
    } else {
      rep("#cccccc40", length(comp_ids))
    }
    pal[seq_along(comp_ids)]
  } else character(0)

  compartmentGroups <- data.frame(
    compartmentId   = comp_ids,
    compartmentName = comp_names,
    color           = comp_colors,
    stringsAsFactors = FALSE,
    row.names       = NULL
  )

  # ---- Species nodes ----
  species_ids <- if (length(species_list) > 0) names(species_list) else character(0)

  # Defensive lookup helpers — species entries may have NULL / missing fields
  pickStr <- function(x, default = NA_character_) {
    if (is.null(x)) return(default)
    if (length(x) == 0) return(default)
    if (is.na(x[[1]])) return(default)
    as.character(x[[1]])
  }
  pickNum <- function(x, default = NA_real_) {
    if (is.null(x) || length(x) == 0) return(default)
    suppressWarnings(as.numeric(x[[1]]))
  }
  pickLgl <- function(x, default = NA) {
    if (is.null(x) || length(x) == 0) return(default)
    isTRUE(x[[1]])
  }

  if (length(species_ids) > 0) {
    species_nodes <- data.frame(
      id                = species_ids,
      type              = rep("species", length(species_ids)),
      label             = vapply(species_list, function(s) pickStr(s$Name, ""), character(1)),
      compartmentId     = vapply(species_list, function(s) pickStr(s$Compartment.id), character(1)),
      compartmentName   = vapply(species_list, function(s) pickStr(s$Compartment), character(1)),
      boundaryCondition = vapply(species_list, function(s) pickLgl(s$BoundaryCondition, FALSE), logical(1)),
      initialValue      = vapply(species_list, function(s) pickNum(s$Value), numeric(1)),
      reactionLaw       = rep(NA_character_, length(species_ids)),
      latex             = rep(NA_character_, length(species_ids)),
      stringsAsFactors  = FALSE,
      row.names         = NULL
    )
  } else {
    species_nodes <- data.frame(
      id = character(0), type = character(0), label = character(0),
      compartmentId = character(0), compartmentName = character(0),
      boundaryCondition = logical(0), initialValue = numeric(0),
      reactionLaw = character(0), latex = character(0),
      stringsAsFactors = FALSE
    )
  }

  # ---- Reaction nodes + edges ----
  reaction_ids <- if (length(reactions_list) > 0) names(reactions_list) else character(0)

  rxn_node_id <- function(reaction_id) paste0("rxn_", reaction_id)

  if (length(reaction_ids) > 0) {
    reaction_nodes <- data.frame(
      id                = vapply(reaction_ids, rxn_node_id, character(1)),
      type              = rep("reaction", length(reaction_ids)),
      label             = vapply(reactions_list, function(r) {
        # Prefer Description if set, else the reaction.law key, else the id.
        if (!is.null(r$Description) && !is.na(r$Description) && nzchar(r$Description)) {
          as.character(r$Description)
        } else if (!is.null(r$Reaction.Law) && !is.na(r$Reaction.Law)) {
          as.character(r$Reaction.Law)
        } else {
          as.character(r$ID)
        }
      }, character(1)),
      compartmentId     = vapply(reactions_list, function(r) pickStr(r$Compartment.id), character(1)),
      compartmentName   = vapply(reactions_list, function(r) pickStr(r$Compartment), character(1)),
      boundaryCondition = rep(NA, length(reaction_ids)),
      initialValue      = rep(NA_real_, length(reaction_ids)),
      reactionLaw       = vapply(reactions_list, function(r) pickStr(r$Reaction.Law), character(1)),
      latex             = vapply(reactions_list, function(r) pickStr(r$Latex.Rate.Law), character(1)),
      stringsAsFactors  = FALSE,
      row.names         = NULL
    )
  } else {
    reaction_nodes <- species_nodes[0, , drop = FALSE]
  }

  nodes <- rbind(species_nodes, reaction_nodes)

  # Edges: walk reactions, dispatch on law type, emit reactant/product/modifier
  # rows. Filter out edges whose endpoints are not in the species set
  # (orphaned references survive a partial delete; we silently drop them).
  valid_species <- species_ids
  edge_rows <- list()

  if (length(reaction_ids) > 0) {
    for (rid in reaction_ids) {
      reaction <- reactions_list[[rid]]
      if (is.null(reaction)) next
      roles <- DiagramRoleAssignments(reaction, companions)
      rxn_nid <- rxn_node_id(rid)

      addEdges <- function(species_ids, role) {
        for (sid in species_ids) {
          if (!(sid %in% valid_species)) next
          if (role == "reactant") {
            src <- sid; tgt <- rxn_nid
          } else if (role == "product") {
            src <- rxn_nid; tgt <- sid
          } else { # modifier
            src <- sid; tgt <- rxn_nid
          }
          edge_rows[[length(edge_rows) + 1]] <<- data.frame(
            id          = paste(rid, src, tgt, role, sep = "__"),
            source      = src,
            target      = tgt,
            role        = role,
            reactionId  = rid,
            stringsAsFactors = FALSE
          )
        }
      }

      addEdges(roles$reactants, "reactant")
      addEdges(roles$products,  "product")
      addEdges(roles$modifiers, "modifier")
    }
  }

  edges <- if (length(edge_rows) > 0) {
    do.call(rbind, edge_rows)
  } else {
    data.frame(id = character(0), source = character(0), target = character(0),
               role = character(0), reactionId = character(0),
               stringsAsFactors = FALSE)
  }

  list(
    nodes             = nodes,
    edges             = edges,
    compartmentGroups = compartmentGroups
  )
}
