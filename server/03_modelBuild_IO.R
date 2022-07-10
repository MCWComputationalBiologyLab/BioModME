StoreParamsIO <- function(parameterToAdd, InOrOut) {
  #note InOrOut should be a string that is either "In" or "Out"
  #NEED TO ADD CHECK IF PARAM ALREADY EXISTS
  if (!(parameterToAdd %in% params$vars.all) &&
      !(parameterToAdd %in% params$rate.params)) {
    params$vars.all <- append(params$vars.all, parameterToAdd)
    params$vals.all <- append(params$vals.all, 0)
    params$comments.all <- append(params$comments.all, "")

    if (InOrOut == "In") {
      params$inputs.vars <- append(params$inputs.vars, parameterToAdd)
      params$inputs.vals <- append(params$inputs.vals, 0)
      params$inputs.comments <- append(params$inputs.comments, "")
    } else {#if output
      params$outputs.vars <- append(params$outputs.vars, parameterToAdd)
      params$outputs.vals <- append(params$outputs.vals, 0)
      params$outputs.comments <- append(params$outputs.comments, "")
    }
    
    #add parameter to parameter table
    row.to.add <- c(parameterToAdd, 0, "")
    if (nrow(params$param.table) == 0) {
      params$param.table[1,] <- row.to.add
    } else {
      params$param.table <- rbind(params$param.table, row.to.add)
    }
  }
}


############################### I/O Server ###################################
observeEvent(vars$species, {
  updatePickerInput(session = session
                    ,"InOut_selectVar"
                    ,choices = sort(vars$species))
  
  updatePickerInput(session = session
                    ,"IO_factor_for_syn"
                    ,choices = sort(vars$species))
  
  updatePickerInput(session #updates output substrate choices for enzyme degradation
                    ,"enzyme_deg_substrate"
                    ,choices = sort(vars$species))
  
  updatePickerInput(session
                    ,"enzyme_deg_enzyme"#updates output enzyme choices for enzyme degradation
                    ,choices = sort(vars$species))
  
  updatePickerInput(session,
                    "MA_species"
                    ,choices = sort(vars$species))
})

# adds inputs of model to appropriate df for analysis
observeEvent(input$Inout_addInVarToDf, {
  IO$bool.input.added <- TRUE
  type = input$InOut_typeOfIn #gets the type of the input (rate, diffusion, synthesis, etc)
  speciesName = input$InOut_selectVar #actual name of the species going in (eg. A *rate1 where A is the species)
  if (type == "Rate") { #if input is a simple rate in (species*rate)
    rate.constant.in = input$In_rate_id #name of the rate constant
    # params$inputs.vars <- append(params$inputs.vars, rateConstantIn) #store rateConstant to parameters model
    StoreParamsIO(rate.constant.in, "In")
    row_to_df <- c(type, 
                   speciesName, 
                   rate.constant.in, 
                   input$In_rate_multiply_with_species,
                   NA, 
                   NA, 
                   NA) 
    log_row <- paste0("Input of '", 
                      speciesName, 
                      "' by ", 
                      tolower(type), 
                      " with rate constant, ", 
                      rate.constant.in, 
                      sep = "")
  } else if (type == "Synthesis") {
    rate.constant.in <- input$IO_rc_for_syn
    StoreParamsIO(rate.constant.in, "In")
    row_to_df <- c(type, 
                   speciesName, 
                   rate.constant.in, 
                   FALSE, 
                   NA, 
                   NA, 
                   input$IO_factor_for_syn) 
    log_row <- paste0("Synthesis of '", 
                      speciesName, 
                      "' by ", 
                      tolower(input$IO_factor_for_syn), 
                      " with rate constant, ", 
                      rate.constant.in, sep = "")
  }
  if (IO$bool.input.exists) {
    IO$bool.input.exists <- FALSE
    IO$input.info[1,] <- row_to_df
  }
  else
  {
    IO$input.info <- rbind(IO$input.info, row_to_df)
  }
  
  #log info
  logs$input.logs <- append(logs$input.logs, log_row)
  IO$n.inputs = IO$n.inputs + 1
  #solveForDiffEqs()
  jPrint(IO$input.logs)
})

# adds outputs of model to appropriate df for analysis
observeEvent(input$Inout_addOutVarToDf, {
  IO$bool.output.added <- TRUE
  type = input$InOut_typeOfOut  #gets the type of the output (rate, diffusion, synthesis, etc)
  speciesName = input$InOut_selectVar #actual name of the species going out (eg. A *rate1 where A is the species)
  if (type == "Rate") { #if output is a simple rate out (species*rate)
    rateConstantOut = input$Out_rate_id #name of the rate constant
    #params$outputs.vars <- append(params$outputs.vars, rateConstantOut) #store rateConstant to parameters model
    StoreParamsIO(rateConstantOut, "Out")
    row_to_df <- c(type, speciesName, rateConstantOut, input$Out_rate_multiply_with_species, NA, NA, NA) #create row to add to df read by differential equation solver
    
    log_row <- ifelse(input$Out_rate_multiply_with_species,
                      paste0("Output of '", speciesName, "' by ", tolower(type), " with rate constant, ", rateConstantOut, ", conc dependent", sep = ""),
                      paste0("Output of '", speciesName, "' by ", tolower(type), " with rate constant, ", rateConstantOut, sep = "")
                      )
  }
  else if (type == "Enzyme_Degradation") {
    km = input$enzyme_deg_km #micheal menton Km value
    substrate = speciesName
    if (!input$enzyme_deg_vmax_opt) { #if michealis menton eqn uses vmax
      vmax = input$enzyme_deg_Vmax
      enzyme = NA
      kcat = NA
      # params$outputs.vars <- append(params$outputs.vars, vmax) #store Vmax to parameters model
      # params$outputs.vars <- append(params$outputs.vars, km) #store Michelis Menton Constant to parameters model
      StoreParamsIO(vmax, "Out")
      StoreParamsIO(km, "Out")
      row_to_df <- c(type, substrate, km, NA, vmax, NA, NA) #create row to add to df read by differential equation solver
      log_row <- paste0("Output of '", speciesName, "' by enzyme degradation", " with Vmax, ", vmax, sep = "")
    } else {#if vmax = kcat*enzyme
      vmax = NA
      enzyme = input$enzyme_deg_enzyme
      kcat = input$enzyme_deg_kcat
      # params$outputs.vars <- append(params$outputs.vars, km) #store Michelis Menton Constant to parameters model
      # params$outputs.vars <- append(params$outputs.vars, kcat) #store rateConstant to parameters model
      StoreParamsIO(kcat, "Out")
      StoreParamsIO(km, "Out")
      row_to_df <- c(type, substrate, km, NA, NA, kcat, enzyme) #create row to add to df read by differential equation solver
      log_row <- paste0("Output of '", speciesName, "' by enzyme degradation", " with enzyme, ", enzyme, ", and kcat, ", kcat)
    }
  }
  else if (type == "mass_action") {
    rateConstantOut = input$MA_deg_rate_constant
    params$outputs.vars <- append(params$outputs.vars, rateConstantOut) #store rateConstant to parameters model
    transporter_out <- input$MA_species
    row_to_df <- c(type, speciesName, rateConstantOut, NA, NA, NA, transporter_out)
    log_row <- paste0("Output of '", speciesName, "' by ", tolower(type), " with rate constant, ", rateConstantOut, " by ", transporter_out)
  }
  if (IO$bool.output.exists) {
    IO$bool.output.exists <- FALSE
    IO$output.info[1,] <- row_to_df
  }
  else
  {
    IO$output.info <- rbind(IO$output.info, row_to_df)
  }
  #log info
  logs$output.logs <- append(logs$output.logs, log_row)
  IO$n.outputs = IO$n.outputs + 1
  #solveForDiffEqs()
})

output$IO_Display_Logs <- renderText({
  #check which options is currently displayed: New, Edit, Delete
  #Then check for if input/output is checked
  #change log to match the input output
  
  if ((input$IO_pageOptions == "New" & input$InOut_radio == "Input") |
      (input$IO_pageOptions == "Edit" & input$IO_edit_inOrOut == "Input") |
      (input$IO_pageOptions == "Delete" & input$IO_edit_inOrOut_delete == "Input")) {
      if (length(logs$input.logs) == 0) {
        paste("No Input Entered")
      } else{
        n_eqns = seq(length(logs$input.logs))
        eqns_to_display <- c()
        for (i in n_eqns) {
          new_eqn <- paste0("(",i, ") ", logs$input.logs[i])
          eqns_to_display <- c(eqns_to_display, new_eqn)
        }
        paste(eqns_to_display, collapse = "<br>")
      }
    } else {
      jPrint(length(logs$output.logs))
      if (length(logs$output.logs) == 0) {
        paste("No Outputs Entered")
      } else{
        n_eqns = seq(length(logs$output.logs))
        eqns_to_display <- c()
        for (i in n_eqns) {
          new_eqn <- paste0("(",i, ") ", logs$output.logs[i])
          eqns_to_display <- c(eqns_to_display, new_eqn)
        }
        paste(eqns_to_display, collapse = "<br>")
      }
    }
})

#-------------------------------------------------------------------------------

#Delete Equation 

#-------------------------------------------------------------------------------

#updates picker input to delete input output equations with the number of input/output equations there are
observeEvent(input$Inout_addInVarToDf, {
  shinyjs::enable("Inout_delete_input_eqn")
  updatePickerInput(session
                    ,"Inout_delete_input_eqn"
                    ,choices = seq(IO$n.inputs))
  
})

observeEvent(input$Inout_addOutVarToDf, {
  shinyjs::enable("Inout_delete_output_eqn")
  updatePickerInput(session,
                    "Inout_delete_output_eqn",
                    choices = seq(IO$n.outputs))
})
#deletes the selected I/O equation from the model
observeEvent(input$Inout_button_delete_IO_eqn, {
  
  if (input$IO_edit_inOrOut_delete == "Input") {
    if (IO$n.inputs > 0) {
      idx <- as.numeric(input$Inout_delete_input_eqn)
      
      #remove parameters if options is checked
      if (input$InOut_delete_eqn_delete_parameters) {
        #find all parameters in IO
        Var1 <- IO$input.info[idx, 3]
        Var2 <- IO$input.info[idx, 5]
        Var3 <- IO$input.info[idx, 6]
        vars.to.check <- c(Var1, Var2, Var3)
        for (var in vars.to.check) {
          if (!is.na(var)) {
            idx.params <- match(var, params$vars.all)
            idx.params.input <- match(var, params$inputs.vars)
            if (!is.na(idx.params)) {
              params$vars.all <- params$vars.all[-idx.params]
              params$vals.all <- params$vals.all[-idx.params]
              params$comments.all <- params$comments.all[-idx.params]
            }
            if (!is.na(idx.params.input)) {
              params$inputs.vars <- params$inputs.vars[-idx.params.input]
              params$inputs.vals <- params$inputs.vals[-idx.params.input]
              params$inputs.comments <- params$inputs.comments[-idx.params.input]
            }
            idx.param.table <- match(var, params$param.table[,1])
            if (!is.na(idx.param.table)) {
              params$param.table <- params$param.table[-idx.param.table, 1:ncol(params$param.table)]
            }
          }
        }
      }
      
      IO$input.info <- IO$input.info[-idx, 1:ncol(IO$input.info)]
      IO$n.inputs <- IO$n.inputs - 1
      logs$input.logs <- logs$input.logs[-idx]
      
      val.to.show <- ifelse(IO$n.inputs > 0, seq(IO$n.inputs), "No Inputs")
      if (IO$n.inputs <= 0) shinyjs::disable("Inout_delete_input_eqn")
      updatePickerInput(session
                        ,"Inout_delete_input_eqn"
                        ,choices = val.to.show)
    }
    
  } else if (input$IO_edit_inOrOut_delete == "Output") {
    if (IO$n.outputs > 0) {
      idx <- as.numeric(input$Inout_delete_output_eqn)
      
      #remove parameters if options is checked
      if (input$InOut_delete_eqn_delete_parameters) {
        #find all parameters in IO
        Var1 <- IO$output.info[idx, 3]
        Var2 <- IO$output.info[idx, 5]
        Var3 <- IO$output.info[idx, 6]
        vars.to.check <- c(Var1, Var2, Var3)
        for (var in vars.to.check) {
          if (!is.na(var)) {
            idx.params <- match(var, params$vars.all)
            if (!is.na(idx.params)) {
              params$vars.all <- params$vars.all[-idx.params]
              params$vals.all <- params$vals.all[-idx.params]
              params$comments.all <- params$comments.all[-idx.params]
            }
            idx.params.output <- match(var, params$outputs.vars)
            if (!is.na(idx.params.output)) {
              params$outputs.vars <- params$outputs.vars[-idx.params.output]
              params$outputs.vals <- params$outputs.vals[-idx.params.output]
              params$outputs.comments <- params$outputs.comments[-idx.params.output]
            }
            idx.param.table <- match(var, params$param.table[,1])
            if (!is.na(idx.param.table)) {
              params$param.table <- params$param.table[-idx.param.table, 1:ncol(params$param.table)]
            }
          }
        }
      }
      
      IO$output.info <- IO$output.info[-idx, 1:ncol(IO$output.info)]
      IO$n.outputs <- IO$n.outputs - 1
      logs$output.logs <- logs$output.logs[-idx]
      
      #remove parameters if options is checked
      val.to.show <- ifelse(IO$n.outputs > 0, seq(IO$n.outputs), "No Outputs")
      if (IO$n.outputs <= 0) shinyjs::disable("Inout_delete_output_eqn")
      updatePickerInput(session,
                        "Inout_delete_output_eqn",
                        choices = val.to.show)
    }
  }

  jPrint(IO$input.info)
  jPrint(IO$output.info)
  #solveForDiffEqs()
})

#-------------------------------------------------------------------------------

# Edit Input/Output 

#-------------------------------------------------------------------------------

# Update edit inputs that update with variable additions/deletions
observeEvent(vars$species, {
  updatePickerInput(session = session
                    ,"InOut_edit_selectVar"
                    ,choices = sort(vars$species))
  
  updatePickerInput(session = session
                    ,"IO_factor_for_syn_edit"
                    ,choices = sort(vars$species))
  
  updatePickerInput(session #updates output substrate choices for enzyme degradation
                    ,"enzyme_deg_substrate_edit"
                    ,choices = sort(vars$species))
  
  updatePickerInput(session
                    ,"enzyme_deg_enzyme_edit"#updates output enzyme choices for enzyme degradation
                    ,choices = sort(vars$species))
  
  updatePickerInput(session,
                    "MA_species_edit"
                    ,choices = sort(vars$species))
})

#update IO box with number of input/outputs when IO is added or deleted
observeEvent(input$Inout_addInVarToDf | input$Inout_addOutVarToDf | input$Inout_button_delete_IO_eqn, {
  updatePickerInput(session
                    ,"IO_selectIO2Edit"
                    ,choices = seq(IO$n.IO))
})


# Change UI based on IO equation selected to edit
observeEvent(input$IO_selectIO2Edit, {
  #require an IO to have been added
  req(input$Inout_addInVarToDf | input$Inout_addOutVarToDf)
  #get selection of row
  row <- as.numeric(input$IO_selectIO2Edit)
  # Extract data from IO dataframe
  in.or.out <- IO$IO.info[row, 1]
  type <- IO$IO.info[row, 2]
  species <- IO$IO.info[row, 3]
  rate.constant <- IO$IO.info[row, 4]
  species.dep <- IO$IO.info[row, 5]
  Vmax <- IO$IO.info[row, 6]
  kcat <- IO$IO.info[row, 7]
  enzyme <- IO$IO.info[row, 8]
  
  in.out.selected = ifelse(in.or.out == "input", "Input", "Output")
  # if (in.or.out == "input") {in.out.selected = "Input"}
  # else {in.out.selected = "Output"}
  
  updateRadioGroupButtons(session,
                          "IO_edit_inOrOut",
                          selected = in.out.selected)
  
  #solveForDiffEqs()
})


