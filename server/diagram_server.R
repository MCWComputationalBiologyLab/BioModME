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
