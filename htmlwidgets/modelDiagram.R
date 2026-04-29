# R-side factory and Shiny bindings for the Model Diagram htmlwidget.
#
# BioModME is a Shiny app, not an R package, so the standard
# inst/htmlwidgets/<name>.yaml autoload is not available. We wire
# dependencies manually via htmltools::htmlDependency() and pass
# package = NULL to htmlwidgets::createWidget() to bypass the package
# lookup. The JS side registers the widget under name "modelDiagram"
# via HTMLWidgets.widget({name: "modelDiagram", ...}).

modelDiagram <- function(nodes = NULL,
                         edges = NULL,
                         layout = NULL,
                         compartmentGroups = NULL,
                         width = NULL,
                         height = NULL,
                         elementId = NULL) {

  x <- list(
    nodes             = nodes,
    edges             = edges,
    layout            = layout,
    compartmentGroups = compartmentGroups
  )

  htmlwidgets::createWidget(
    name         = "modelDiagram",
    x            = x,
    width        = width,
    height       = height,
    package      = NULL,
    elementId    = elementId,
    dependencies = list(
      htmltools::htmlDependency(
        name      = "d3",
        version   = "7.9.0",
        src       = c(file = normalizePath("htmlwidgets/lib/d3-7.9.0",
                                           mustWork = TRUE)),
        script    = "d3.min.js",
        all_files = FALSE
      ),
      htmltools::htmlDependency(
        name       = "modelDiagram",
        version    = "1.0",
        src        = c(file = normalizePath("htmlwidgets/lib/modelDiagram-1.0",
                                            mustWork = TRUE)),
        script     = "modelDiagram.js",
        stylesheet = "modelDiagram.css",
        all_files  = FALSE
      )
    )
  )
}

modelDiagramOutput <- function(outputId, width = "100%", height = "600px") {
  htmlwidgets::shinyWidgetOutput(outputId, "modelDiagram",
                                 width, height,
                                 package = NULL)
}

renderModelDiagram <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) expr <- substitute(expr)
  htmlwidgets::shinyRenderWidget(expr, modelDiagramOutput, env, quoted = TRUE)
}
