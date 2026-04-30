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

# Highlight mode toggle — push the on/off state to the widget. JS clears
# the fade/glow when this turns off and skips applying it on subsequent
# clicks; turning it back on does NOT auto-restore highlight on the prior
# selection — the user clicks again or picks from the dropdown.
observeEvent(input$modelDiagram_highlight_mode, {
  session$sendCustomMessage(
    "modelDiagram_highlight_mode",
    list(enabled = isTRUE(input$modelDiagram_highlight_mode))
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
