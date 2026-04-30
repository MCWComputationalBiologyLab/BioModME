TAB_MODEL_DIAGRAM <-
  tabItem(
    tabName = "TAB_MODEL_DIAGRAM",
    # Toolbar CSS — compacts checkboxInput / selectInput so they sit on the
    # same row as the action buttons.
    tags$style(HTML(
      ".modelDiagram-toolbar { display: flex; align-items: center;
         flex-wrap: wrap; gap: 6px; margin-bottom: 6px; }
       .modelDiagram-toolbar > .form-group { margin-bottom: 0; }
       .modelDiagram-toolbar .checkbox { margin: 0; padding-left: 0; }
       .modelDiagram-toolbar .checkbox label { padding-left: 0; margin: 0;
         font-weight: normal; cursor: pointer; }
       .modelDiagram-toolbar .checkbox input[type='checkbox'] {
         position: static; margin: 0 4px 0 0; }
       .modelDiagram-toolbar .modelDiagram-species-select { min-width: 240px; }
       .modelDiagram-toolbar .modelDiagram-species-select .form-group {
         margin-bottom: 0; }
       .modelDiagram-toolbar .modelDiagram-divider {
         border-left: 1px solid #ccc; height: 22px; margin: 0 6px; }"
    )),
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
        div(
          class = "modelDiagram-toolbar",
          actionButton(
            inputId = "modelDiagram_reset_layout",
            label   = "Reset Layout",
            icon    = icon("rotate-left"),
            class   = "btn btn-sm btn-default"
          ),
          actionButton(
            inputId = "modelDiagram_zoom_in",
            label   = NULL,
            icon    = icon("magnifying-glass-plus"),
            class   = "btn btn-sm btn-default",
            title   = "Zoom in"
          ),
          actionButton(
            inputId = "modelDiagram_zoom_out",
            label   = NULL,
            icon    = icon("magnifying-glass-minus"),
            class   = "btn btn-sm btn-default",
            title   = "Zoom out"
          ),
          actionButton(
            inputId = "modelDiagram_fit_view",
            label   = "Fit",
            icon    = icon("expand"),
            class   = "btn btn-sm btn-default",
            title   = "Fit all nodes into view"
          ),
          tags$span(class = "modelDiagram-divider"),
          checkboxInput(
            inputId = "modelDiagram_highlight_mode",
            label   = "Highlight pathways",
            value   = TRUE
          ),
          tagAppendAttributes(
            conditionalPanel(
              condition = "input.modelDiagram_highlight_mode == true",
              selectInput(
                inputId  = "modelDiagram_highlight_species",
                label    = NULL,
                choices  = c("Focus a species..." = ""),
                selected = "",
                width    = "240px"
              )
            ),
            class = "modelDiagram-species-select"
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        box(
          width      = 12,
          solidHeader = FALSE,
          collapsible = FALSE,
          modelDiagramOutput("modelDiagram",
                             width  = "100%",
                             height = "700px")
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        box(
          width       = 12,
          title       = "Selection Details",
          solidHeader = FALSE,
          collapsible = TRUE,
          uiOutput("modelDiagram_info_panel")
        )
      )
    )
  )
