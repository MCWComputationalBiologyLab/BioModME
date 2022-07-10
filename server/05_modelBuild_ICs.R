############################### ICs Server ###################################

################################################################################
#Server Section that controls editable table of variables
# needs to create table that is editable and changes the respectable RVs.
# should control the parameters: 

output$ICs_RHT <- renderRHandsontable({
  rhandsontable(ICs$ICs.table,
                colHeaderWidth = 100,
                stretchH = "all",
                overflow = "visible"
                ) %>%
    hot_cols(colWidth = c(30, 30, 90),
             manualColumnMove = FALSE,
             manualColumnResize = TRUE,
             halign = "htCenter",
             valign = "htMiddle",
             renderer = "
           function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.NumericRenderer.apply(this, arguments);
             if (row % 2 == 0) {
              td.style.background = '#f9f9f9';
             } else {
              td.style.background = 'white';
             };
           }") %>%
    hot_col("Variable", readOnly = TRUE) %>%
    #hot_col("Description", halign = "htLeft", valign = "htMiddle") %>%
    hot_rows(rowHeights = 40) %>%
    hot_context_menu(allowRowEdit = FALSE,
                     allowColEdit = FALSE
    ) %>%
    hot_validate_numeric(col = 2, min = 0)
})


observeEvent(input$ICs_RHT$changes$changes, {
  xi = input$ICs_RHT$changes$changes[[1]][[1]]
  yi = input$ICs_RHT$changes$changes[[1]][[2]]
  old = input$ICs_RHT$changes$changes[[1]][[3]]
  new = input$ICs_RHT$changes$changes[[1]][[4]]
  
  jPrint(yi)
  if (yi == 1) {
    new <- as.character(new)
    jPrint(str_split(new, "")[[1]][1])
    if (str_split(new, "")[[1]][1] == ".") {
      new <- as.numeric(paste0("0", new))
      jPrint(new)
    }
  }
  #copying table to dataframe
  ICs$ICs.table[xi+1, yi+1] <- new
  ICs$vals[xi+1] <- ICs$ICs.table[xi+1, 2]
  ICs$comments[xi+1] <- ICs$ICs.table[xi+1, 3]
  jPrint(ICs$vals)
  jPrint(ICs$comments)
  jPrint(ICs$ICs.table)
  loop$ICs <- ICs$ICs.table
})
