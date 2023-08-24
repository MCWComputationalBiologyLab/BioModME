# This script will hold any server side processes that need to be run at the 
# initiation of the program.

# Generate first compartment on document load
onStart <- observe({
  shinyjs::click("createVar_add_compartment_button")
  
  # Grab file names from base_models
  base.models <- list.files(file.path("base_models"))
  
  updateSelectInput(
    session = session,
    inputId = "SI_repos_base_choices",
    choices = base.models
  )
  
  shinyjs::hide("GO_species_unit_choice")
  
  
  #Try adding tooltips
  # bs4Dash::addTooltip(
  #   id = "bttn_download_model_results_copy",
  #   options = list(
  #     title = "Copy",
  #     placement = "bottom"
  #   )
  # )
  # bs4Dash::addTooltip(
  #   id = "bttn_download_model_results_csv",
  #   options = list(
  #     title = "Download csv",
  #     placement = "bottom"
  #   )
  # )
  # bs4Dash::addTooltip(
  #   id = "bttn_download_model_results_xlsx",
  #   options = list(
  #     title = "Download xlsx",
  #     placement = "bottom"
  #   )
  # )
  # bs4Dash::addTooltip(
  #   id = "bttn_download_model_results_new_window",
  #   options = list(
  #     title = "Open in new window",
  #     placement = "bottom"
  #   )
  # )
  
  # Remove this observer after first iteration
  onStart$destroy()
})