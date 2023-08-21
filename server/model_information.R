
# Reactive variables about model name
observe({
  rv.MODEL.INFO$model.name <- input$TI_model_name
  rv.MODEL.INFO$model.description <- input$TAI_model_description
})