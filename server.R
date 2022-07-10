#This contains the overall server flow of the Application (backend)
#-------------------------------------------------------------------------
#  Justin Womack
#  January 5, 2021
#  Last Update: January 5, 2021
#  MCW, Milwaukee, WI, USA
#-------------------------------------------------------------------------
waiting_screen <- tagList(
  spin_pong(),
  br(),
  h4("Loading Model..."),
)

button_store <- tagList(
  spin_loaders(32),
  h4("Overwriting Parameters")
)

server <- shinyServer(function(input, output, session) {
  #allows for the import of bigger data frames
  options(shiny.maxRequestSize = 30000 * 1024 ^ 2)
  #options(shiny.sanitize.errors = TRUE)
 
  #helpers here (generating_diffEqs_functions)
  source("./server/helpers.R")
  source("./server/helper_createEqns.R")
  source("./server/helper_diffeqs_to_text.R")
  source("./server/helper_diffeq_laws.R")
  source("./server/helper_generate_latex_file.R")
  source("./server/helper_write_matlab_code.R") #load functions to solve differential equations
  source("./server/helper_write_R_code.R")
  source("./server/helper_id_generator.R")
  source("./server/helper_recalc_text_equations.R")
  
  table.header<- reactiveValues(bg = "#3c8dbc",
                                color = 'white')
  
  source(file.path("server", "00_reactive_variables.R"), local = TRUE)$value
  source(file.path("server", "01_modelBuild_createVar.R"), local = TRUE)$value
  source(file.path("server", "02_modelBuild_createEqn.R"), local = TRUE)$value
  source(file.path("server", "02_modelBuild_createEqn_renderUI.R"), local = TRUE)$value
  source(file.path("server", "02_modelBuild_createEqn_textEqn_mathjax.R"), local=TRUE)$value
  source(file.path("server", "02_modelBuild_createEqn_mathjax.R"), local = TRUE)$value
  source(file.path("server", "02_modelBuild_createEqn_edit.R"), local = TRUE)$value
  source(file.path("server", "03_modelBuild_IO.R"), local = TRUE)$value
  source(file.path("server", "04_modelBuild_params.R"), local = TRUE)$value
  source(file.path("server", "05_modelBuild_ICs.R"), local = TRUE)$value
  source(file.path("server", "06_modelBuild_diffeqn.R"), local = TRUE)$value

  source(file.path("server", "11_run_execute.R"), local = TRUE)$value
  source(file.path("server", "12_run_post_processing.R"), local = TRUE)$value
  source(file.path("server", "13_0_run_lineplot.R"), local = TRUE)$value
  source(file.path("server", "13_compare_model.R"), local = TRUE)$value
  source(file.path("server", "13_3_loop_model.R"), local = TRUE)$value #controls the looping mechanism in the lineplot server
  source(file.path("server", "41_summary.R"), local = TRUE)$value
  
  source(file.path("server", "21_export.R"), local = TRUE)$value
  # #additional source pages
  source(file.path("server", "server_load_model.R"), local = TRUE)$value #controls the load function server in the sidebar
  
  output$css_themes <- renderUI({
    tags$head(
      if (input$css_selector == "default") {
        tags$link(rel = 'stylesheet', type = 'text/css', href = 'default.css')
      }  else if (input$css_selector == "royalBlue") {
        tags$link(rel = "stylesheet", type = "text/css", href = "royalBlue.css")
      }
    )
  })
  
  # This changes the colors of the generated DT table UI
  observeEvent(input$css_selector, {
    if (input$css_selector == "default") {
      table.header$bg <- "grey"
      table.header$color <- "black"
      #fresh::use_theme("ocean.css")
    } else if (input$css_selector == "night") {
      #fresh::use_theme("night.css")
    } else if (input$css_selector == "test1") {
      #fresh::use_theme("test1.css")
    } else if (input$css_selector == "test2") {
      # fresh::use_theme("test2.css")
    } else if (input$css_selector == "ocean") {
    } else if (input$css_selector == "royalBlue") {
      table.header$bg <- "#3c8dbc"
      table.header$color <- "white"
    }
  })
  
  # close info boxes
  #start with box removed on load
  updateBox("create_var_info_box", action = "toggle")
  updateBox("create_eqn_info_box", action = "toggle")
  
})#end of server
