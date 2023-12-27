

#update picker inputs on model run
observeEvent(input$execute_run_model, {
  n.rows <- length(colnames(rv.RESULTS$results.model.final))
  updatePickerInput(session
                    ,"pp_add_vars"
                    ,choices = colnames(rv.RESULTS$results.model.final)[2:n.rows]
  )


  updatePickerInput(session
                    ,"pp_sub_vars"
                    ,choices = colnames(rv.RESULTS$results.model.final)[2:n.rows]
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
  
  model <- "rv.RESULTS$results.model.final"
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
  if (rv.RESULTS$results.is.pp) {
    #if (!rv.RESULTS$results.is.pp)
    rv.RESULTS$results.pp.model <- as.data.frame(rv.RESULTS$results.model)
    
    n.col.to.add <- length(rv.RESULTS$results.pp.eqns.col)

    for (i in seq(n.col.to.add)) {
      new.col <- eval(parse(text = rv.RESULTS$results.pp.eqns.col[i]))
      eval(parse(text = paste0("rv.RESULTS$results.pp.model$",
                               rv.RESULTS$results.pp.vars[i], "<-new.col")))
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
  rv.RESULTS$results.pp.vars <- c(rv.RESULTS$results.pp.vars, input$pp_new_var)
  rv.RESULTS$results.pp.eqns <- c(rv.RESULTS$results.pp.eqns, ppEqnToAdd())
  rv.RESULTS$results.pp.eqns.col <- c(rv.RESULTS$results.pp.eqns.col, ppEqnToAddColumn())
  PostProcessModel()
  
  rv.RESULTS$results.is.pp = TRUE
  
  n.rows <- length(colnames(rv.RESULTS$results.model.final))
  updatePickerInput(session
                    ,"pp_add_vars"
                    ,choices = colnames(rv.RESULTS$results.model.final)[2:n.rows]
  )
  
  updatePickerInput(session
                    ,"pp_sub_vars"
                    ,choices = colnames(rv.RESULTS$results.model.final)[2:n.rows]
  )
  
  updateTextInput(session,
                  "pp_new_var"
                  ,value = ""
                  )
})

observeEvent({rv.COUNTS$loading.model
             input$pp_submit_new_var
             input$execute_run_model}, {
  if (rv.RESULTS$results.is.pp) {
    model.to.use <- rv.RESULTS$results.pp.model
  }else{
    model.to.use <- rv.RESULTS$results.model
  }
  rv.RESULTS$results.model.final <- model.to.use
  
  updatePickerInput(session
                    ,"lineplot_xvar"
                    ,choices = colnames(rv.RESULTS$results.model.final[1]))
  updateSelectizeInput(session,
                       "lineplot_yvar"
                       ,choices  = colnames(rv.RESULTS$results.model.final)[2:ncol(rv.RESULTS$results.model.final)]
                       ,selected = colnames(rv.RESULTS$results.model.final)[2:ncol(rv.RESULTS$results.model.final)]
  )
})

# ModelToUse <- reactive({
#   if (rv.RESULTS$results.is.pp) {
#     model.to.use <- rv.RESULTS$results.pp.model
#   }else{
#     model.to.use <- rv.RESULTS$results.model
#   }
#   return(model.to.use)
# })
# DataTable --------------------------------------------------------------------
output$pp_data_table <- renderRHandsontable({
  rhandsontable(ModelToUse(),
                readOnly = TRUE, 
                contextMenu = FALSE)
})

