# Server logic for the Model Diagram tab.
#
# Reactive renderModelDiagram that pulls species / reactions /
# compartments out of the master reactive bundle, derives the
# bipartite graph via BuildDiagramGraph (defined in
# server/diagram_graph.R), and feeds the result into the modelDiagram
# htmlwidget.
#
# Layout positions are stored in rv.DIAGRAM$layout (keyed by node id).
# Reading layout inside isolate() prevents drag events from re-triggering
# a full render; only model-structure changes (species/reactions/compartments)
# cause a re-render.

output$modelDiagram <- renderModelDiagram({
  # Non-isolated read: makes the render reactive to Reset Layout clicks.
  reset.tok <- rv.DIAGRAM$reset.token

  graph <- BuildDiagramGraph(
    species_list   = rv.SPECIES$species,
    reactions_list = rv.REACTIONS$reactions,
    compartments   = rv.COMPARTMENTS$compartments,
    companions     = reactiveValuesToList(rv.REACTIONS)
  )
  saved.layout <- isolate(rv.DIAGRAM$layout)
  modelDiagram(
    nodes             = graph$nodes,
    edges             = graph$edges,
    layout            = saved.layout,
    resetToken        = reset.tok,
    compartmentGroups = graph$compartmentGroups,
    width             = "100%",
    height            = "700px"
  )
})

# Keep the simulation alive while the user is on a different tab so
# the layout is settled when they navigate to the diagram.
outputOptions(output, "modelDiagram", suspendWhenHidden = FALSE)

# Drag observer — store pinned position in rv.DIAGRAM$layout.
# Only updates the layout record; does NOT touch rv.SPECIES / rv.REACTIONS
# so no re-render loop is triggered.
observeEvent(input$modelDiagram_node_drag, {
  drag <- input$modelDiagram_node_drag
  req(isTruthy(drag$id))
  rv.DIAGRAM$layout[[drag$id]] <- list(x = drag$x, y = drag$y)
})

# Node click — record selection (null id means background click = deselect).
observeEvent(input$modelDiagram_node_click, {
  click <- input$modelDiagram_node_click
  if (is.null(click) || is.null(click$id) || identical(click$id, "null")) {
    rv.DIAGRAM$selected.id   <- NULL
    rv.DIAGRAM$selected.kind <- NULL
  } else {
    rv.DIAGRAM$selected.id   <- click$id
    rv.DIAGRAM$selected.kind <- "node"
  }
})

# Zoom / fit toolbar buttons — forwarded to the widget via custom message.
observeEvent(input$modelDiagram_zoom_in,  {
  session$sendCustomMessage("modelDiagram_zoom", list(action = "in"))
})
observeEvent(input$modelDiagram_zoom_out, {
  session$sendCustomMessage("modelDiagram_zoom", list(action = "out"))
})
observeEvent(input$modelDiagram_fit_view, {
  session$sendCustomMessage("modelDiagram_zoom", list(action = "fit"))
})

# Reset Layout — clear all saved positions and re-run auto-layout.
observeEvent(input$modelDiagram_reset_layout, {
  rv.DIAGRAM$layout        <- list()
  rv.DIAGRAM$selected.id   <- NULL
  rv.DIAGRAM$selected.kind <- NULL
  rv.DIAGRAM$reset.token   <- rv.DIAGRAM$reset.token + 1L
})

# Edge click — record the reaction the edge belongs to.
observeEvent(input$modelDiagram_edge_click, {
  click <- input$modelDiagram_edge_click
  req(isTruthy(click$reactionId))
  rv.DIAGRAM$selected.id   <- click$reactionId
  rv.DIAGRAM$selected.kind <- "edge"
})

# Shrink-reactions toggle — push the on/off state to the widget. Shrinks
# reaction squares to a small dot and hides their labels so the diagram
# focuses on the species circles. Edges stay visible because they still
# terminate at the (now small) reaction node.
observeEvent(input$modelDiagram_shrink_reactions, {
  session$sendCustomMessage(
    "modelDiagram_shrink_reactions",
    list(shrink = isTRUE(input$modelDiagram_shrink_reactions))
  )
}, ignoreInit = FALSE)

# Highlight depth (hops) — pushes an integer 0..3 to the widget. 0 disables
# the fade/glow entirely; 1..3 set the BFS depth so each hop level can be
# colored differently in the diagram.
observeEvent(input$modelDiagram_highlight_hops, {
  hops <- suppressWarnings(as.integer(input$modelDiagram_highlight_hops))
  if (length(hops) != 1 || is.na(hops)) hops <- 0L
  session$sendCustomMessage(
    "modelDiagram_highlight_hops",
    list(hops = hops)
  )
}, ignoreInit = FALSE)

# Keep the species dropdown choices in sync with rv.SPECIES. Display name
# is used as the label, species id is the value.
observe({
  sp <- rv.SPECIES$species
  if (length(sp) == 0) {
    choices <- c("(no species yet)" = "")
  } else {
    nm <- vapply(sp, function(s) {
      if (!is.null(s$Name) && nzchar(s$Name))    s$Name
      else if (!is.null(s$ID))                   s$ID
      else                                       ""
    }, character(1))
    choices <- c("(pick a species)" = "",
                 setNames(names(sp), nm))
  }
  current <- isolate(input$modelDiagram_highlight_species)
  if (is.null(current)) current <- ""
  updateSelectInput(
    session, "modelDiagram_highlight_species",
    choices  = choices,
    selected = current
  )
})

# Dropdown -> selection. Picking a species drives the same selection state
# a click would set, then tells the widget to apply the pathway highlight.
# The identity guard avoids loops with the sync-back observer below.
observeEvent(input$modelDiagram_highlight_species, {
  sid <- input$modelDiagram_highlight_species
  if (!isTruthy(sid)) return()
  cur.sid  <- isolate(rv.DIAGRAM$selected.id)
  cur.kind <- isolate(rv.DIAGRAM$selected.kind)
  if (identical(cur.sid, sid) && identical(cur.kind, "node")) return()
  rv.DIAGRAM$selected.id   <- sid
  rv.DIAGRAM$selected.kind <- "node"
  session$sendCustomMessage(
    "modelDiagram_highlight_species", list(id = sid)
  )
}, ignoreInit = TRUE)

# Selection -> dropdown. When the user clicks a species in the diagram
# (or dblclicks the background to clear), keep the dropdown in sync so it
# always reflects the current focus.
observe({
  sid  <- rv.DIAGRAM$selected.id
  kind <- rv.DIAGRAM$selected.kind
  cur  <- isolate(input$modelDiagram_highlight_species)
  if (is.null(cur)) cur <- ""
  new <- if (!is.null(sid) && identical(kind, "node") &&
             !startsWith(sid, "rxn_")) {
    sid
  } else {
    ""
  }
  if (!identical(cur, new)) {
    updateSelectInput(session, "modelDiagram_highlight_species",
                      selected = new)
  }
})

# Info panel — rendered whenever the selection changes.
output$modelDiagram_info_panel <- renderUI({
  kind <- rv.DIAGRAM$selected.kind
  sid  <- rv.DIAGRAM$selected.id

  if (is.null(kind) || is.null(sid)) {
    return(helpText("Click a node or edge to see details."))
  }

  retypeset <- tags$script(HTML(
    "setTimeout(function(){MathJax.Hub.Queue(['Typeset',MathJax.Hub]);},50);"
  ))

  # Helper: coerce NULL / NA / "" to a fallback string.
  val_or <- function(x, fallback = "—") {
    if (is.null(x) || (length(x) == 1 && (is.na(x) || !nzchar(trimws(as.character(x)))))) fallback
    else as.character(x)
  }

  info_row <- function(label, value) {
    tags$tr(
      tags$td(style = "white-space:nowrap; padding-right:12px; color:#666;",
              tags$em(label)),
      tags$td(value)
    )
  }

  info_table <- function(...) {
    tags$table(
      class = "table table-sm table-borderless",
      style = "font-size:90%; margin-bottom:0;",
      tags$tbody(...)
    )
  }

  if (kind == "node" && !startsWith(sid, "rxn_")) {
    # ---- Species node ----
    sp <- rv.SPECIES$species[[sid]]
    if (is.null(sp)) return(helpText("Species not found."))

    # Find all reactions involving this species by scanning every reaction
    # with DiagramRoleAssignments — the same function used by the diagram
    # renderer. This captures modifiers, enzymes, and growth-law companions
    # that sp$Reaction.ids does not reliably track.
    companions <- reactiveValuesToList(rv.REACTIONS)
    rxn.table.rows <- list()
    for (rid in names(rv.REACTIONS$reactions)) {
      rxn.entry <- rv.REACTIONS$reactions[[rid]]
      if (is.null(rxn.entry)) next
      roles <- DiagramRoleAssignments(rxn.entry, companions)
      role.label <- if (sid %in% roles$reactants && sid %in% roles$products) {
        "reactant / product"
      } else if (sid %in% roles$reactants) {
        "reactant"
      } else if (sid %in% roles$products) {
        "product"
      } else if (sid %in% roles$modifiers) {
        "modifier"
      } else {
        NULL
      }
      if (!is.null(role.label)) {
        rxn.table.rows <- c(rxn.table.rows, list(
          tags$tr(
            tags$td(val_or(rxn.entry$Equation.Text,   val_or(rxn.entry$Eqn.Display.Type, rid))),
            tags$td(val_or(rxn.entry$Eqn.Display.Type, "")),
            tags$td(val_or(rxn.entry$Compartment,      "")),
            tags$td(
              tags$span(role.label,
                style = switch(role.label,
                  "reactant"           = "color:#c0392b;",
                  "product"            = "color:#27ae60;",
                  "modifier"           = "color:#2980b9;",
                  "reactant / product" = "color:#8e44ad;",
                  ""
                )
              )
            )
          )
        ))
      }
    }

    has.desc <- nzchar(trimws(val_or(sp$Description, "")))

    tagList(
      tags$h5(tags$strong(sp$Name), style = "margin-top:0;"),
      fluidRow(
        column(width = if (has.desc) 6 else 12,
          info_table(
            info_row("ID",            tags$code(val_or(sp$ID))),
            info_row("Compartment",   val_or(sp$Compartment)),
            info_row("Initial value", paste0(val_or(sp$Value), " ", val_or(sp$Unit, ""))),
            info_row("Base value",    paste0(val_or(sp$BaseValue), " ", val_or(sp$BaseUnit, ""))),
            info_row("Boundary",      if (isTRUE(sp$BoundaryCondition)) "Fixed" else "Dynamic")
          )
        ),
        if (has.desc) {
          column(width = 6,
            tags$p(tags$em("Description:"),
                   style = "margin-bottom:2px; color:#666;"),
            tags$p(sp$Description)
          )
        }
      ),
      tags$p(tags$em("Involved in reactions:"),
             style = "margin-top:14px; margin-bottom:6px; color:#666;"),
      if (length(rxn.table.rows) > 0) {
        tags$table(
          class = "table table-sm table-hover table-bordered",
          style = "font-size:90%; margin-bottom:0;",
          tags$thead(
            tags$tr(
              tags$th("Equation",    style = "width:45%;"),
              tags$th("Type",        style = "width:25%;"),
              tags$th("Compartment", style = "width:18%;"),
              tags$th("Role",        style = "width:12%;")
            )
          ),
          tags$tbody(rxn.table.rows)
        )
      } else {
        tags$span("— not yet part of any reaction", style = "color:#999;")
      }
    )

  } else {
    # ---- Reaction node or edge ----
    rxn.id <- if (kind == "node") sub("^rxn_", "", sid) else sid
    rxn    <- rv.REACTIONS$reactions[[rxn.id]]
    if (is.null(rxn)) return(helpText("Reaction not found."))

    latex.law <- if (isTruthy(rxn$Latex.Rate.Law)) rxn$Latex.Rate.Law else ""

    # Build participants list, skipping blank entries.
    participant_rows <- list(
      info_row("Reactants",  val_or(rxn$Reactants)),
      info_row("Products",   val_or(rxn$Products))
    )
    if (nzchar(trimws(val_or(rxn$Modifiers, "")))) {
      participant_rows <- c(participant_rows,
        list(info_row("Modifiers", rxn$Modifiers)))
    }
    if (nzchar(trimws(val_or(rxn$Parameters, "")))) {
      participant_rows <- c(participant_rows,
        list(info_row("Parameters", rxn$Parameters)))
    }

    tagList(
      tags$h5(tags$strong(val_or(rxn$Eqn.Display.Type)), style = "margin-top:0;"),
      fluidRow(
        column(width = 6,
          tags$p(tags$em("Identity:"),
                 style = "margin-bottom:4px; color:#666;"),
          info_table(
            info_row("Law",        val_or(rxn$Reaction.Law)),
            info_row("Compartment",val_or(rxn$Compartment)),
            info_row("Reversible", if (isTRUE(rxn$Reversible)) "Yes" else "No"),
            if (nzchar(trimws(val_or(rxn$Description, "")))) {
              info_row("Description", rxn$Description)
            }
          )
        ),
        column(width = 6,
          tags$p(tags$em("Participants:"),
                 style = "margin-bottom:4px; color:#666;"),
          do.call(info_table, participant_rows)
        )
      ),
      tags$p(tags$em("Rate law:"),
             style = "margin-top:14px; margin-bottom:6px; color:#666;"),
      if (nzchar(trimws(latex.law))) {
        tagList(
          tags$p(paste0("$$", latex.law, "$$"),
                 style = "overflow-x:auto; font-size:110%;"),
          retypeset
        )
      } else {
        tags$span("No rate law stored.", style = "color:#999;")
      }
    )
  }
})

# ---- Animation: scale species circles by simulation results ---------------
# The diagram tab listens for results in rv.RESULTS$results.model.final and
# turns the user's scrubbing of the time slider into a {speciesId: radius}
# map that the widget applies to its circle nodes. Conditional panels in the
# UI hide everything until results exist.

# Boolean output that drives the conditionalPanel visibility.
output$modelDiagram_has_results <- reactive({
  isTRUE(rv.RESULTS$results.model.has.been.solved)
})
outputOptions(output, "modelDiagram_has_results", suspendWhenHidden = FALSE)

# When new results arrive, update the time slider's range to match.
observe({
  req(rv.RESULTS$results.model.has.been.solved)
  res <- rv.RESULTS$results.model.final
  if (is.null(res) || !"time" %in% colnames(res)) return()
  times <- res[, "time"]
  if (length(times) < 2) return()
  t.min <- min(times, na.rm = TRUE)
  t.max <- max(times, na.rm = TRUE)
  if (!is.finite(t.min) || !is.finite(t.max) || t.max <= t.min) return()
  step  <- (t.max - t.min) / 200
  unit  <- if (isTruthy(rv.RESULTS$results.time.units)) {
    rv.RESULTS$results.time.units
  } else {
    ""
  }
  current <- isolate(input$modelDiagram_anim_time)
  if (is.null(current) || !is.finite(current) || current < t.min || current > t.max) {
    current <- t.min
  }
  updateSliderInput(
    session, "modelDiagram_anim_time",
    label = paste0("Time", if (nzchar(unit)) paste0(" (", unit, ")") else ""),
    min   = t.min,
    max   = t.max,
    value = current,
    step  = step
  )
})

# Compute and push radii whenever sync is on AND any of (slider, log,
# results matrix) change.
observe({
  if (!isTRUE(input$modelDiagram_anim_sync))             return()
  if (!isTRUE(rv.RESULTS$results.model.has.been.solved)) return()
  res <- rv.RESULTS$results.model.final
  if (is.null(res) || !"time" %in% colnames(res))        return()

  t.target <- input$modelDiagram_anim_time
  use.log  <- isTRUE(input$modelDiagram_anim_log)
  if (is.null(t.target) || !is.finite(t.target))         return()

  # Pick the row closest to the requested time.
  times   <- res[, "time"]
  row.idx <- which.min(abs(times - t.target))
  row     <- res[row.idx, , drop = TRUE]

  # Build species name -> id lookup. deSolve sometimes mangles names through
  # make.names() so we register both spellings.
  species_list <- rv.SPECIES$species
  if (length(species_list) == 0) return()
  ids   <- names(species_list)
  names_ <- vapply(species_list, function(s) {
    if (!is.null(s$Name) && nzchar(s$Name)) s$Name else (if (!is.null(s$ID)) s$ID else "")
  }, character(1))
  name_to_id <- setNames(ids, names_)
  safe_to_id <- setNames(ids, make.names(names_))
  resolve.id <- function(col) {
    if (col %in% names(name_to_id)) return(name_to_id[[col]])
    if (col %in% names(safe_to_id)) return(safe_to_id[[col]])
    NULL
  }

  # Global max across the whole result matrix (excluding time) so the scale
  # is consistent across the animation.
  species_cols <- setdiff(colnames(res), "time")
  if (length(species_cols) == 0) return()
  vals.all <- as.numeric(unlist(res[, species_cols, drop = FALSE]))
  vals.all <- vals.all[is.finite(vals.all)]
  if (length(vals.all) == 0) return()
  global.max <- max(vals.all, na.rm = TRUE)
  if (!is.finite(global.max) || global.max <= 0) return()

  r.min <- 8
  r.max <- 35
  if (use.log) {
    eps.floor <- max(1e-12, global.max * 1e-9)
    log.min   <- log10(eps.floor)
    log.max   <- log10(global.max)
    log.range <- log.max - log.min
  }

  radii <- list()
  for (col in species_cols) {
    sid <- resolve.id(col)
    if (is.null(sid)) next
    v <- as.numeric(row[[col]])
    if (!is.finite(v) || v < 0) v <- 0
    norm <- if (use.log) {
      log.v <- log10(max(v, eps.floor))
      (log.v - log.min) / log.range
    } else {
      v / global.max
    }
    norm <- max(0, min(1, norm))
    radii[[sid]] <- r.min + norm * (r.max - r.min)
  }

  if (length(radii) == 0) return()
  session$sendCustomMessage(
    "modelDiagram_set_radii",
    list(radii = radii)
  )
})

# Reset all species circles to default size when sync turns off.
observeEvent(input$modelDiagram_anim_sync, {
  if (!isTRUE(input$modelDiagram_anim_sync)) {
    session$sendCustomMessage("modelDiagram_reset_radii", list())
  }
}, ignoreInit = TRUE)
