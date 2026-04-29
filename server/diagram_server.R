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

  if (kind == "node" && !startsWith(sid, "rxn_")) {
    # ---- Species node ----
    sp <- rv.SPECIES$species[[sid]]
    if (is.null(sp)) return(helpText("Species not found."))
    tagList(
      tags$strong(sp$Name),
      tags$br(),
      tags$small(paste0("Compartment: ", sp$Compartment)),
      tags$hr(style = "margin: 6px 0;"),
      tags$table(
        class = "table table-sm table-borderless",
        style = "font-size:90%;margin-bottom:0;",
        tags$tbody(
          tags$tr(
            tags$td(tags$em("Initial value")),
            tags$td(paste0(sp$Value, " ", sp$Unit))
          ),
          tags$tr(
            tags$td(tags$em("Boundary")),
            tags$td(if (isTRUE(sp$BoundaryCondition)) "Fixed" else "Dynamic")
          )
        )
      )
    )
  } else {
    # ---- Reaction node or edge ----
    rxn.id <- if (kind == "node") sub("^rxn_", "", sid) else sid
    rxn    <- rv.REACTIONS$reactions[[rxn.id]]
    if (is.null(rxn)) return(helpText("Reaction not found."))

    latex.law <- if (isTruthy(rxn$Latex.Rate.Law)) rxn$Latex.Rate.Law else ""

    tagList(
      tags$strong(rxn$Eqn.Display.Type),
      tags$br(),
      tags$small(tags$em(rxn$Reaction.Law)),
      tags$hr(style = "margin: 6px 0;"),
      if (nzchar(trimws(latex.law))) {
        tagList(
          tags$p(tags$em("Rate law:"), style = "margin-bottom:2px;"),
          tags$p(paste0("$$", latex.law, "$$")),
          retypeset
        )
      } else {
        tags$p(tags$em("No rate law stored."))
      }
    )
  }
})
