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
# 
# install.lib<-load.lib[!load.lib %in% installed.packages()]
# for(lib in install.lib) install.packages(lib,dependencies=TRUE)
# sapply(load.lib,require,character=TRUE)

library(shinydashboard)
library(bs4Dash)
library(shiny)
library(ggplot2)
library(gridExtra)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)
library(DT)
library(tidyverse)
library(dplyr)
library(rhandsontable)
library(data.table)
library(ggpmisc)
library(colourpicker)
library(shinyBS)
library(shinyjqui)
library(bsplus)
library(deSolve)
library(plotly)
library(Deriv)
library(viridis)
library(ggpubr)
library(shinycssloaders)
library(waiter)
library(fresh)

#load files with UI outputs
source("./ui/00_homeUI.R")
source("./ui/01_model_varCreate_ui.R")
source("./ui/02_model_equationCreate_ui.R")
source("./ui/03_input_outputsUI.R")
source("./ui/04_model_parameters_ui.R")
source("./ui/05_model_ICs_ui.R")
source("./ui/06_model_diffEqs_ui.R")
source("./ui/11_run_executeUI.R")
source("./ui/12_run_post_processing.R")
source("./ui/13_run_lineplotUI.R")

source("./ui/21_export_ui.R")

source("./ui/31_documentationUI.R")
source("./ui/41_SummaryUI.R")
#source("./ui/51_parameter_estimination_UI.R")
source("./ui/contributionsUI.R")

mytheme <- create_theme(
  bs4dash_vars(
    navbar_light_color = "#37a7e7",
    navbar_light_active_color = "#178acc",
    navbar_light_hover_color = "#178acc"
  ),
  # bs4dash_yiq(
  #   contrasted_threshold = 10,
  #   text_dark = "#FFF",
  #   text_light = "#272c30"
  # ),
  bs4dash_layout(
    main_bg = "#ecf0f5"
  ),
  bs4dash_sidebar_light(
    bg = "#222d32",  #background color
    color = "grey",  #text color
    hover_color = "white",
    submenu_bg = "#272c30",
    submenu_color = "grey",
    submenu_hover_color = "white"
  ),
  bs4dash_status(
    primary = "#37a7e7", danger = "#BF616A", light = "#272c30"
  ),
  bs4dash_color(
    gray_900 = "#FFF", white = "white"
  )
)

js1 <- paste0(c(
  "Selectize.prototype.selectall = function(){",
  "  var self = this;",
  "  self.setValue(Object.keys(self.options));",
  "}"), 
  collapse = "\n")

js2 <- paste0(c(
  "var selectinput = document.getElementById('lineplot_yvar');",
  "selectinput.selectize.setValue(-1, false);",
  "selectinput.selectize.selectall();",
  "$('#select + .selectize-control .item').removeClass('active');"),
  collapse = "\n")

loading_screen <- tagList(
  spin_pong(), 
  h3("Loading Model...")
)

ui <- dashboardPage(
  header = dashboardHeader(
    title = dashboardBrand(
      title = "BioModME",
      #color = "primary",
      image = "icon.svg"
    )
    
  ),
  sidebar = dashboardSidebar(skin = "light",
                             sidebarMenu(
                               #menuItem("Home", tabName = "Tab_home", icon = icon("home")),
                               menuItem("Create Model", tabName = "TAB_MODEL_BUILD", startExpanded = FALSE, icon = icon("tasks", lib = "glyphicon")
                                        ,menuSubItem("Define Variables", tabName = "TAB_VAR_CREATE")
                                        ,menuSubItem("Build Equations", tabName = "TAB_Equation_Create")
                                        #,menuSubItem("Add Input/Output", tabName = "TAB_InOut")
                                        ,menuSubItem("Parameter Values", tabName = "TAB_Parameters")
                                        ,menuSubItem("Initial Conditions", tabName = "TAB_ICs")
                                        ,menuSubItem("Differential Equations", tabName = "TAB_diffEqs")
                               )
                               ,menuItem("Execute Model", tabName = "TAB_RUN_EXECUTE", icon = icon("laptop-code"))
                               ,menuItem("Visualization", tabName = "TAB_RUN_LINEPLOT", icon = icon("images"))
                               #,menuItem("Modeler's Toolbox", tabName = "TAB_Toolbox",
                                         #menuSubItem("Parameter Estimation", tabName = "Tab_Parameter_Estimation"))
                               #,menuSubItem("Plot Model", tabName = "TAB_RUN_LINEPLOT"))
                               ,menuItem("Export", tabName = "TAB_export", icon = icon("file-export"))
                               ,menuItem("Summary", tabName = "TAB_SUMMARY", icon = icon("list-alt"))
                               ,menuItem("Documentation", tabName = "TAB_DOCUMENTATION", icon = icon("book"))
                               ,menuItem("Contributions", tabName = "TAB_Contributions")
                               ,absolutePanel("Version 1.0.0", bottom = 0, left = 5, fixed = TRUE)
                               
                             )#end SideBarMenu
  ), #end dashboardSidebar
  body = dashboardBody(
    #use_theme(mytheme),
    autoWaiter("eqnCreate_equationBuilder_chem",
               color = "white",
               html = spin_refresh()
    ),
    #tags$style(js),
    tags$link(rel = "stylesheet", type = "text/css", href = "nonColorStyling.css"),
    #tags$link(rel = "stylesheet", type = "text/css", href = "royalBlue.css"),
    tags$head(tags$script(js1)),
    tags$head(tags$script(js2)),
    tags$head(tags$style("
                       .jhr{
                       display: inline;
                       vertical-align: middle;
                       padding-left: 10px;
                       }")),
    tags$head(tags$style("
                       .main-sidebar { font-size: 20px;}")),
    tags$head(tags$style("
                       .brand-text {font-weight: bold !important; 
                                    font-size: 1.5rem;
                                    padding-left: 15px;};")),
    tags$head(tags$style("
                       .brand-link {line-height: 1.25};")),
    
    #activates shiny javascript so that I can play with vanishing and appearing div files
    useShinyjs()
    ,withMathJax()
    ,useWaiter()
    ,tags$script(src = "popup.js")
    ,tags$script(src = "press_enter.js")
    ,uiOutput("css_themes")
    
    ,tabItems(Tab_home
              ,TAB_VAR_CREATE
              ,TAB_Equation_Create
              ,TAB_InOut
              ,TAB_ICs
              ,TAB_Parameters
              ,TAB_diffEqs
              ,TAB_export
              ,TAB_RUN_EXECUTE
              ,TAB_RUN_LINEPLOT
              ,TAB_SUMMARY
              #,Tab_Parameter_Estimation
              ,TAB_DOCUMENTATION
              ,TAB_Contributions
    )
  ) #end dashboardBody
  
  ,controlbar = dashboardControlbar("version 1.0",
                                    fileInput("load_model"
                                              ,"Load Model"
                                              ,placeholder = "Choose .rds File"
                                              ,multiple = FALSE
                                              ,accept = c(".rds")
                                    ),
                                    checkboxInput("show_debug_tools",
                                                  "Show Debug",
                                                  value = FALSE),
                                    conditionalPanel(
                                      condition = "input.show_debug_tools",
                                      h4("Debugging Tools"),
                                      actionButton(inputId = "refresh_text_eqns",
                                                   label = "Refresh Equations"),
                                      hr(),
                                      numericInput(inputId = "sum_box_size",
                                                   label = "Text Size (px)",
                                                   value = 25,
                                                   step = 1,
                                                   min = 1),
                                      hr(),
                                      numericInput(inputId = "sum_table_font_size",
                                                   label = "Table Font (%)",
                                                   value = 135,
                                                   min = 5,
                                                   max = 200,
                                                   step = 5),
                                      hr(),
                                      actionButton(inputId = "view_eqns_debug",
                                                   label = "View eqns")
                                      ,actionButton(inputId = "view_ids",
                                                    label = "view ids",
                                                    style = "color: #fff; background-color: green; border-color: #2e6da4")
                                      ,actionButton(inputId = "param_view_parameters"
                                                    ,label = "View Parameters"
                                                    ,style = "color: #fff; background-color: green; border-color: #2e6da4")
                                      ,hr()
                                      ,actionButton(inputId = "param_remove_duplicate_parameters"
                                                    ,label = "Delete Duplicate Parameters"
                                                    ,style = "color: #fff; background-color: green; border-color: #2e6da4")
                                      ,hr()
                                      ,actionButton(inputId = "createEqn_refreshEquations"
                                                    ,label = "Refesh"
                                                    ,style = "color: #fff; background-color: green; border-color: #2e6da4")
                                      ,hr()
                                      ,actionButton(inputId = "createEqn_removeFirstRate"
                                                    ,label = "Remove First Rate"
                                                    ,style = "color: #fff; background-color: red; border-color: #2e6da4")
                                      ,hr()
                                      ,actionButton(inputId = "createEqn_removeEqnFromList"
                                                    ,label = "Remove Last Added"
                                                    ,style = "color: #fff; background-color: red; border-color: #2e6da4")
                                      ,pickerInput(inputId = "css_selector",
                                                   label = "Select Skin",
                                                   choices = c("default",
                                                               "royalBlue")
                                                   ,select = "default"
                                      )
                                      ,div(skinSelector())
                                    ),
                                    "$$\\require{mhchem}$$",
  )
  #,footer = NULL
  #,dark = NULL
) #end dashboardPage

