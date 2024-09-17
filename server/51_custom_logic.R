# Server for custom logic script

observeEvent(input$button_customLogic_add_custom_code, {
  
  # Store logic to reactive variable
  n.logic <- length(rv.CUSTOM.LAWS$logic)
  rv.CUSTOM.LAWS$logic[n.logic+1] <- test
  
  
})