
# Find model names from saved models folder



# Update pickerinput with names from that folder


# If select input value changes, extract name and description from model info
selected_base_model <- reactive({
  req(input$SI_repos_base_choices)
  path.to.model <- file.path("base_models", input$SI_repos_base_choices)
  out <- readRDS(path.to.model)
  return(out)
})


# TextOuput to display title of model 
output$TO_repos_model_name <- renderText({
  mod.name <- selected_base_model()$model.name
  header <- "Model Name:\n"
  print(paste0(header, mod.name))
  
})


output$TO_repos_model_description <- renderText({
  descript <- selected_base_model()$model.description
  header <- "Description:\n"
  print(paste0(header, descript))
})

# On Load Model Button Press, Load Model to Main Application
observeEvent(input$bttn_load_model_from_base_repo, {
  rv.LOADBUTTONS$LB.button.name <- "Load_base_model"
  rv.LOADBUTTONS$LB.count <- rv.LOADBUTTONS$LB.count + 1
  
  # Change NavBar to Original Panel
  # updateSidebar(session, "TAB_VAR_CREATE", selected = 1)
  updateTabItems(
    session,
    inputId = "sidebar_tabs",
    selected = "TAB_VAR_CREATE"
  )
})