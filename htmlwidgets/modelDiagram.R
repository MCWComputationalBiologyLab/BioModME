# R-side factory and Shiny bindings for the Model Diagram htmlwidget.
#
# BioModME is a Shiny app, not an R package, so the standard
# inst/htmlwidgets/<name>.yaml autoload is not available. We wire
# dependencies manually via htmltools::htmlDependency().
#
# The output side cannot use htmlwidgets::shinyWidgetOutput() because
# that function calls getDependency(name, package) internally, which
# bottoms out in .getNamespace(NULL) when package is NULL — that
# errors with "invalid type/length (symbol/0) in vector allocation".
# Instead we build the output <div> ourselves with the classes the
# htmlwidgets runtime scans for ("<widgetname> html-widget
# html-widget-output") and attach the dependency list directly via
# htmltools::attachDependencies().

# Returns the htmlDependency list this widget needs. Centralized so the
# manual output container and the createWidget call share one source of
# truth. htmlwidgets.js is included explicitly so the runtime is
# guaranteed to load before the <div> is scanned for binding.
modelDiagramDependencies <- function() {
  list(
    htmltools::htmlDependency(
      name      = "htmlwidgets",
      version   = as.character(utils::packageVersion("htmlwidgets")),
      src       = system.file("www", package = "htmlwidgets"),
      script    = "htmlwidgets.js",
      all_files = FALSE
    ),
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
      # Bump this whenever modelDiagram.js or modelDiagram.css changes — the
      # version is part of the URL Shiny serves the asset under, so bumping
      # forces browsers to fetch the fresh file instead of using a stale cache.
      version    = "1.0.8",
      src        = c(file = normalizePath("htmlwidgets/lib/modelDiagram-1.0",
                                          mustWork = TRUE)),
      script     = "modelDiagram.js",
      stylesheet = "modelDiagram.css",
      all_files  = FALSE
    )
  )
}

modelDiagram <- function(nodes = NULL,
                         edges = NULL,
                         layout = NULL,
                         compartmentGroups = NULL,
                         resetToken = NULL,
                         width = NULL,
                         height = NULL,
                         elementId = NULL) {

  x <- list(
    nodes             = nodes,
    edges             = edges,
    layout            = layout,
    compartmentGroups = compartmentGroups,
    resetToken        = resetToken
  )

  htmlwidgets::createWidget(
    name         = "modelDiagram",
    x            = x,
    width        = width,
    height       = height,
    package      = NULL,
    elementId    = elementId,
    dependencies = modelDiagramDependencies()
  )
}

modelDiagramOutput <- function(outputId, width = "100%", height = "600px") {
  styleStr <- paste0(
    "width:",  htmltools::validateCssUnit(width),
    ";height:", htmltools::validateCssUnit(height), ";"
  )
  out <- shiny::tags$div(
    id    = outputId,
    class = "modelDiagram html-widget html-widget-output shiny-report-size",
    style = styleStr
  )
  htmltools::attachDependencies(out, modelDiagramDependencies())
}

renderModelDiagram <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) expr <- substitute(expr)
  htmlwidgets::shinyRenderWidget(expr, modelDiagramOutput, env, quoted = TRUE)
}
