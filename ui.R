#This contains the overall UI flow of the Application
#-------------------------------------------------------------------------
#  Justin Womack
#  January 5, 2021
#  Last Update: January 5, 2021
#  MCW, Milwaukee, WI, USA
#-------------------------------------------------------------------------

# load.lib<-c("shinydashboard", "bs4Dash", "shiny","ggplot2","gridExtra","shinythemes",
#             "shinyWidgets","shinyjs","DT","tidyverse","dplyr","rhandsontable","data.table","ggpmisc",
#             "colourpicker","shinyBS","shinyjqui", "bsplus", "plotly", "deSolve", "waiter", "ggpubr",
#             "viridis", "Deriv", "shinycssloaders")
# 
# load.lib <- c("shinydashboard", "bs4Dash", "shiny", "ggplot2", "gridExtra","shinythemes",
#               "shinyWidgets", "shinyjs", "DT", "tidyverse", "dplyr", "rhandsontable", "data.table",
#               "ggpmisc", "colourpicker", "shinyBS", "shinyjqui", "bsplus", "deSolve", "plotly",
#               "Deriv", "viridis", "ggpubr", "shinycssloaders", "waiter", "fresh", "readxl",
#               "minpack.lm", "measurements", "qdapRegex", "XML", "xml2", "katex",
#               "reshape2", "clipr", "jsonlite")

# 
# install.lib<-load.lib[!load.lib %in% installed.packages()]
# for(lib in install.lib) install.packages(lib,dependencies=TRUE)
# sapply(load.lib,require,character=TRUE)


# library(htmltools)
# library(htmlwidgets)


fxn.sources <- file.path("ui_functions", list.files("ui_functions"))
sapply(fxn.sources, source)

# Source Modules
source(file.path("modules", "tableDownloadButtonsUI.R"))
source(file.path("modules", "tableDownloadButtons.R"))

# Source in UI tabs
source(file.path("ui", "00_home_ui.R"))
source(file.path("ui", "01_create_model_ui.R"))
source(file.path("ui", "11_run_execute_ui.R"))
source(file.path("ui", "12_run_post_processing_ui.R"))
source(file.path("ui", "13_run_lineplot_ui.R"))
source(file.path("ui", "21_export_ui.R"))
source(file.path("ui", "31_documentation_ui.R"))
source(file.path("ui", "41_summary_ui.R"))
source(file.path("ui", "contributions_ui.R"))
source(file.path("ui", "51_parameter_estimation_ui.R"))
source(file.path("ui", "51_create_custom_law_ui.R"))
source(file.path("ui", "51_create_custom_eqn_ui.R"))
source(file.path("ui", "61_global_options_ui.R"))
source(file.path("ui", "71_import_ui.R"))
source(file.path("ui", "repository_ui.R"))
source(file.path("ui", "debug_ui.R"))
# source(file.path("server", "tableLayout.R"))


jsColChanger <- '
shinyjs.backgroundCol = function(params) {
var defaultParams = {
id : null,
col : "red"
};
params = shinyjs.getParams(params, defaultParams);
var el = $("#" + params.id);
el.css("background-color", params.col);
}'

loading_screen <- tagList(
  spin_pong(), 
  h3("Loading Model...")
)

ui <- dashboardPage(
  header = dashboardHeader(
    title = dashboardBrand(
      title = "BioModME",
      image = "icon.svg"
      #color = "primary",
    ),
    downloadButton(
      outputId = "dbttn_header_download_model",
      label = "Save Model",
      icon = NULL,
      class = "btn btn-lg",
      style = "border: 1.5px solid grey"
    )
  ),
  sidebar = 
    dashboardSidebar(
      skin = "light",
      sidebarMenu(
        id = "sidebar_tabs",
        #menuItem("Home", tabName = "TAB_HOME", icon = icon("home")),
        menuItem(
          "Create Model",
          tabName = "TAB_VAR_CREATE",
          icon = icon("tasks", lib = "glyphicon")
        ),
        menuItem(
          "Execute Model",
          tabName = "TAB_RUN_EXECUTE",
          icon = icon("laptop-code")
        ),
        menuItem(
          "Visualization",
          tabName = "TAB_RUN_LINEPLOT",
          icon = icon("images")
        ),
        menuItem(
          "Modeler's Toolbox",
          tabName = "TAB_Toolbox",
          icon = icon("toolbox"),
          menuSubItem("Parameter Estimation",
                      tabName = "TAB_PARAMETER_ESTIMATION"),
          menuSubItem("Build Custom Law",
                      tabName = "TAB_CREATE_CUSTOM_LAW"),
          menuSubItem("Custom Equations",
                      tabName = "TAB_CREATE_CUSTOM_EQN")
        ),
        menuItem("Export",
                 tabName = "TAB_EXPORT",
                 icon = icon("file-export")),
        menuItem("Import",
                 tabName = "TAB_IMPORT",
                 icon = icon("file-import")),
        menuItem("Summary",
                 tabName = "TAB_SUMMARY",
                 icon = icon("list-alt")),
        menuItem(
          "Options",
          tabName = "TAB_GLOBAL_OPTIONS",
          icon = icon("tags", lib = "glyphicon")
        ),
        menuItem("Documentation",
                 tabName = "TAB_DOCUMENTATION",
                 icon = icon("book")),
        menuItem("Repository",
                 tabName = "TAB_MODEL_REPOSITORY",
                 icon = icon("database")),
        conditionalPanel(
          condition = "input.CB_showDegbugTab",
          menuItem(
            "Debug",
            tabName = "TAB_DEBUG",
            icon = icon("erase", lib = "glyphicon")
          )
        ),
        menuItem("Contributions",
                 tabName = "TAB_CONTRIBUTIONS",
                 icon = icon("clipboard-user")),
        
        absolutePanel(
          "v1.10",
          bottom = 0,
          left = 5,
          fixed = TRUE
        )
      )#end SideBarMenu
     ), #end dashboardSidebar
  
  body = dashboardBody(
    autoWaiter(
      "eqnCreate_equationBuilder_chem",
      color = "white",
      html = spin_refresh()
    ),
    # Apply outside functionalities
    useShinyjs(),
    extendShinyjs(text = jsColChanger, functions = c("backgroundCol")),
    withMathJax(),
    useWaiter(),
    useHostess(),
    useSweetAlert(),
    #apply css
    includeCSS(file.path("www", "css", "nonColorStyling.css")),
    # tags$link(rel = "stylesheet",
    #           type = "text/css",
    #           href = "css/nonColorStyling.css"),
    
    # 'HTML-CSS': { linebreaks: { automatic: true } },
    # SVG: { linebreaks: { automatic: true } },
    
    # Sets mathjax context menu to show up on modals
    tags$div(HTML(
      "<script type='text/x-mathjax-config'>
        MathJax.Hub.Config({
          TeX: {
            Macros: {
            set: ['\\left\\{#1\\right\\}',1],
            }
          },
        MathMenu: {
          styles: {
            '#MathJax_About': {'z-index':1201},
            '.MathJax_Menu': {'z-index':1201}
          }
        }
        });

        MathJax.Hub.Register.StartupHook('MathMenu Ready',function () {
          MathJax.Menu.BGSTYLE['z-index'] = 1200;
        });
      </script>"
    )),
    
    # Apply js functionalites from scripts
    includeScript("www/js/popup.js"),
    includeScript("www/js/select_all.js"),
    includeScript("www/js/remove_all.js"),
    includeScript("www/js/press_enter.js"),
    includeScript(file.path("www", "js", "tablePopup.js")),
    includeScript(file.path("www", "js", "plotPopup.js")),
    includeScript(file.path("www", "js", "evalExpression.js")),
    # Remove help switch icon from the top of the navbar (?) (added in newer
    # versions of bs4dash I guess.)
    tags$head(
      tags$style(HTML("
          .custom-control.custom-switch.mx-2.mt-2 {
              display: none !important;
          }
      "))
    ),
    # Functionality for changing page themes
    #uiOutput("css_themes"),
    
    # Apply tabs
    tabItems(
      TAB_HOME,
      TAB_VAR_CREATE,
      TAB_EXPORT,
      TAB_IMPORT,
      TAB_RUN_EXECUTE,
      TAB_RUN_LINEPLOT,
      TAB_SUMMARY,
      TAB_PARAMETER_ESTIMATION,
      TAB_CREATE_CUSTOM_LAW,
      TAB_CREATE_CUSTOM_EQN,
      TAB_GLOBAL_OPTIONS,
      TAB_DOCUMENTATION,
      TAB_CONTRIBUTIONS,
      TAB_MODEL_REPOSITORY,
      TAB_DEBUG
    )
  ), #end dashboardBody
  
  # Sidebar of main page
  controlbar = dashboardControlbar(
    uiOutput("UIOutput_renderTab_options"),
    checkboxInput(
      inputId = "CB_showDegbugTab",
      label = "Show Debug",
      value = FALSE
    ),
    pickerInput(
      inputId = "css_selector",
      label = "Select Skin",
      choices = c("Default",
                  "Night",
                  "RoyalBlue"),
      select = "Default"
    ),
    div(skinSelector()), 
    "$$\\require{mhchem}$$",
  ),
  #,footer = NULL
  # Needed to remove light/dark switch
  dark = NULL

) #end dashboardPage

