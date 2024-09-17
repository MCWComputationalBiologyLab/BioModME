# Server for custom logic script

# Observe the "Add" button click
observeEvent(input$button_customLogic_add_custom_code, {
  # Get the input code
  custom_code <- input$TI_customLogic_custom_code
  
  # Add the custom code to the reactive list
  if (nzchar(custom_code)) {  # Check if the input is not empty
    rv.CUSTOM.LOGIC$logic <- append(rv.CUSTOM.LOGIC$logic, list(custom_code))
    
    # Update the choices in the selectInput and select the first choice
    updateSelectInput(session, "SI_customLogic_show_logic",
                      choices = seq_along(rv.CUSTOM.LOGIC$logic),
                      selected = 1)  # Automatically select the first choice
    
    # Clear the text area input
    updateTextAreaInput(session, "TI_customLogic_custom_code", value = "")
  }
})

observeEvent(input$bttn_customLogic_delete_logic, {
  
  if (length(rv.CUSTOM.LOGIC$logic) == 0) {
    return(NULL)
  }
  selected_index <- input$SI_customLogic_show_logic
  #browser()
  # Check if there's a valid selection to delete
  if (nzchar(selected_index)) {
    selected_index <- as.numeric(selected_index)
    
    # Remove the selected code
    if (selected_index > 0 && selected_index <= length(rv.CUSTOM.LOGIC$logic)) {
      rv.CUSTOM.LOGIC$logic <- rv.CUSTOM.LOGIC$logic[-selected_index]
      print(length(rv.CUSTOM.LOGIC$logic))
      if (length(rv.CUSTOM.LOGIC$logic) == 0) {
        choice = c()
        updateSelectInput(session, 
                          "SI_customLogic_show_logic",
                          choices = list(),
                          selected = NULL
        )
      } else {
        choice = seq_along(rv.CUSTOM.LOGIC$logic)
        updateSelectInput(session, 
                          "SI_customLogic_show_logic",
                          choices = choice,
                          selected = ifelse(length(rv.CUSTOM.LOGIC$logic) > 0, 1, NULL)) 
      }
      # Update the choices in the selectInput
      # Select the first choice if available
    }
  }
})

# Output the selected custom code in textOutput
output$TO_customLogic_view_logic <- renderText({
  print(rv.CUSTOM.LOGIC$logic)
  print(length(rv.CUSTOM.LOGIC$logic))
  
  if (is.null(input$SI_customLogic_show_logic) || 
      length(rv.CUSTOM.LOGIC$logic)==0) {
    return("No added logic.")
  }
  
  selected_index <- input$SI_customLogic_show_logic
  
  if (nzchar(selected_index)) {
    selected_code <- rv.CUSTOM.LOGIC$logic[[as.numeric(selected_index)]]
    return(selected_code)
  }
  
 
})