

#update picker inputs on model run
observeEvent(input$execute_run_model, {
  observe({print("new1")})
  n.rows <- length(colnames(model_output()))
  updatePickerInput(session
                    ,"pp_add_vars"
                    ,choices = colnames(model_output())[2:n.rows]
  )


  updatePickerInput(session
                    ,"pp_sub_vars"
                    ,choices = colnames(model_output())[2:n.rows]
  )
})

#update textouput to fit current equation to be added

ppEqnToAdd <- function(){
  
  if (input$pp_new_var == "") {
    out.eqn <- "Enter Name of New Variable"
    
  }else{
    out.eqn <- paste0(input$pp_new_var, " = ")
    VAR.ADDED <- FALSE
    if (length(input$pp_add_vars > 0)) {
      count = 0
      VAR.ADDED <- TRUE
      for (var in input$pp_add_vars) {
        count = count + 1
        if (count == length(input$pp_add_vars)) {
          out.eqn <- paste0(out.eqn, var)
        }else{
          out.eqn <- paste0(out.eqn, var, " + ")
        }
      }
    }
    
    if (length(input$pp_sub_vars > 0)) {
      if (VAR.ADDED) {out.eqn <- paste0(out.eqn, " - ")}
      count = 0
      for (var in input$pp_sub_vars) {
        count = count + 1
        if (count == length(input$pp_sub_vars)) {
          out.eqn <- paste0(out.eqn, var)
        }else{
          out.eqn <- paste0(out.eqn, var, " - ")
        }
      }
    }
  }
  
  return(out.eqn)
}

ppEqnToAddColumn <- function(){
  
  model <- "model_output()"
  out.eqn <- paste0(input$pp_new_var, " = ")
  VAR.ADDED <- FALSE
  if (length(input$pp_add_vars > 0)) {
    count = 0
    VAR.ADDED <- TRUE
    for (var in input$pp_add_vars) {
      count = count + 1
      if (count == length(input$pp_add_vars)) {
        out.eqn <- paste0(out.eqn, model, "[,'", var, "']")
      }else{
        out.eqn <- paste0(out.eqn, model, "[,'", var, "']", " + ")
      }
    }
  }
  
  if (length(input$pp_sub_vars > 0)) {
    if (VAR.ADDED) {out.eqn <- paste0(out.eqn, " - ")}
    count = 0
    for (var in input$pp_sub_vars) {
      count = count + 1
      if (count == length(input$pp_sub_vars)) {
        out.eqn <- paste0(out.eqn, model, "[,'", var, "']")
      }else{
        out.eqn <- paste0(out.eqn, model, "[,'", var, "']" , " - ")
      }
    }
  }
  
  return(out.eqn)
}

PostProcessModel <- function(){
  #problem is when we rerun and change the time that the dataframe is staying as the old time.
  #this is because it doesn't take the old model.  It probably can each time. 
  if (results$is.pp) {
    #if (!results$is.pp)
    results$pp.model <- as.data.frame(results$model)
    
    n.col.to.add <- length(results$pp.eqns.col)
   #observe({print(n.col.to.add)})
    
    for (i in seq(n.col.to.add)) {
      new.col <- eval(parse(text = results$pp.eqns.col[i]))
      #observe({print(head(new.col))})
      eval(parse(text = paste0("results$pp.model$",results$pp.vars[i], "<-new.col")))
    }
  }
}

output$pp_built_equation <- renderText({ppEqnToAdd()})

#add variable on submit

observeEvent(input$execute_run_model, {
  observe({print("Post-Processing Model")})
  PostProcessModel() 
})

#reset pickerinputs and text inputs on button submit
observeEvent(input$pp_submit_new_var, {
  
  #store variables to their proper place
  results$pp.vars <- c(results$pp.vars, input$pp_new_var)
  results$pp.eqns <- c(results$pp.eqns, ppEqnToAdd())
  results$pp.eqns.col <- c(results$pp.eqns.col, ppEqnToAddColumn())
  PostProcessModel()
  
  results$is.pp = TRUE
  
  n.rows <- length(colnames(model_output()))
  updatePickerInput(session
                    ,"pp_add_vars"
                    ,choices = colnames(model_output())[2:n.rows]
  )
  
  updatePickerInput(session
                    ,"pp_sub_vars"
                    ,choices = colnames(model_output())[2:n.rows]
  )
  
  updateTextInput(session,
                  "pp_new_var"
                  ,value = ""
                  )
})

observeEvent({counts$loading.model
             input$pp_submit_new_var
             input$execute_run_model}, {
  jPrint("Model to run is being processed")
  if (results$is.pp) {
    jPrint("is.pp is true - processed model")
    model.to.use <- results$pp.model
  }else{
    jPrint("is.pp is false - normal model")
    model.to.use <- results$model
  }
  results$model.final <- model.to.use
  jPrint("Final model to use")
  jPrint(results$model.final)
  
  updatePickerInput(session
                    ,"lineplot_xvar"
                    ,choices = colnames(results$model.final[1]))
  updateSelectizeInput(session,
                       "lineplot_yvar"
                       ,choices  = colnames(results$model.final)[2:ncol(results$model.final)]
                       ,selected = colnames(results$model.final)[2:ncol(results$model.final)]
  )
})

# ModelToUse <- reactive({
#   if (results$is.pp) {
#     observe({print("is.pp is true - processed model")})
#     model.to.use <- results$pp.model
#   }else{
#     observe({print("is.pp is false - normal model")})
#     model.to.use <- results$model
#   }
#   return(model.to.use)
# })
# DataTable --------------------------------------------------------------------
output$pp_data_table <- renderRHandsontable({
  rhandsontable(ModelToUse(),
                readOnly = TRUE, 
                contextMenu = FALSE)
})

