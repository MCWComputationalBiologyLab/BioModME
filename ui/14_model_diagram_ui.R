TAB_MODEL_DIAGRAM <-
  tabItem(
    tabName = "TAB_MODEL_DIAGRAM",
    fluidRow(
      column(
        width = 12,
        h4("Model Diagram"),
        helpText("Bipartite view of the current model: species are circles, ",
                 "reactions are squares. Reactant edges flow into reaction ",
                 "nodes; product edges flow out; modifier influences are ",
                 "shown dashed.")
      )
    ),
    fluidRow(
      column(
        width = 12,
        actionButton(
          inputId = "modelDiagram_reset_layout",
          label   = "Reset Layout",
          icon    = icon("rotate-left"),
          class   = "btn btn-sm btn-default",
          style   = "margin-bottom: 6px;"
        ),
        actionButton(
          inputId = "modelDiagram_zoom_in",
          label   = NULL,
          icon    = icon("magnifying-glass-plus"),
          class   = "btn btn-sm btn-default",
          style   = "margin-bottom: 6px;",
          title   = "Zoom in"
        ),
        actionButton(
          inputId = "modelDiagram_zoom_out",
          label   = NULL,
          icon    = icon("magnifying-glass-minus"),
          class   = "btn btn-sm btn-default",
          style   = "margin-bottom: 6px;",
          title   = "Zoom out"
        ),
        actionButton(
          inputId = "modelDiagram_fit_view",
          label   = "Fit",
          icon    = icon("expand"),
          class   = "btn btn-sm btn-default",
          style   = "margin-bottom: 6px;",
          title   = "Fit all nodes into view"
        )
      )
    ),
    fluidRow(
      column(
        width = 9,
        box(
          width = 12,
          solidHeader = FALSE,
          collapsible = FALSE,
          modelDiagramOutput("modelDiagram",
                             width  = "100%",
                             height = "700px")
        )
      ),
      column(
        width = 3,
        box(
          width = 12,
          title = "Selection",
          solidHeader = FALSE,
          collapsible = FALSE,
          helpText("Click a node or edge to see details.",
                   "(Detail panel content lands in a follow-up commit.)"),
          uiOutput("modelDiagram_info_panel")
        )
      )
    )
  )
