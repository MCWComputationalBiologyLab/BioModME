# Handsontable to Reactable Migration Guide

## Overview
This guide explains how to migrate from `rhandsontable` (security issues, outdated) to `reactable` (modern, secure, actively maintained).

## Key Differences

### 1. Rendering Output

**OLD (Handsontable):**
```r
output$myVariables_DT <- renderRHandsontable({
  rhandsontable(df.by.comp,
                overflow = "visible",
                colHeaderWidth = 100,
                stretchH = "all") %>%
    hot_cols(...) %>%
    hot_rows(...)
})
```

**NEW (Reactable):**
```r
output$myVariables_DT <- renderReactable({
  reactable(df.by.comp,
            editable = TRUE,
            selection = "multiple",  # If you need row selection
            onClick = JS("function(rowInfo, column) {
              Shiny.setInputValue('myVariables_DT_select', 
                {r: rowInfo.index, c: column.id});
            }"),
            columns = list(
              Name = colDef(minWidth = 120),
              Value = colDef(minWidth = 80),
              Unit = colDef(minWidth = 80),
              Compartment = colDef(minWidth = 100, editable = FALSE),
              Description = colDef(minWidth = 200)
            ),
            striped = TRUE,
            compact = TRUE)
})
```

### 2. UI Output

**OLD:**
```r
rHandsontableOutput("myVariables_DT")
```

**NEW:**
```r
reactableOutput("myVariables_DT")
```

### 3. Change Detection

**OLD (Handsontable):**
```r
observeEvent(input$myVariables_DT$changes$changes, {
  xi = input$myVariables_DT$changes$changes[[1]][[1]]  # row
  yi = input$myVariables_DT$changes$changes[[1]][[2]]  # col
  old = input$myVariables_DT$changes$changes[[1]][[3]] # old value
  new = input$myVariables_DT$changes$changes[[1]][[4]] # new value
  
  # Process change...
})
```

**NEW (Reactable):**
```r
observeEvent(input$myVariables_DT_edit, {
  # Reactable sends edit info as a list
  edit_info <- input$myVariables_DT_edit
  row_idx <- edit_info$row
  col_name <- edit_info$column
  new_value <- edit_info$value
  
  # Get old value from current data
  old_value <- rv.SPECIES$plotted.var.table[row_idx, col_name]
  
  # Map column index to column name if needed
  # (Reactable uses column names directly)
  
  # Process change...
})
```

### 4. Row Selection

**OLD:**
```r
observeEvent(input$myVariables_DT_select$select$r, {
  selected_row <- input$myVariables_DT_select$select$r
  # ...
})
```

**NEW:**
```r
# For custom JavaScript click detection:
observeEvent(input$myVariables_DT_select, {
  selected_row <- input$myVariables_DT_select$r  # 0-indexed
  selected_col <- input$myVariables_DT_select$c  # 0-indexed or col name
  # ...
})

# OR use built-in selection:
observeEvent(input$myVariables_DT_selected, {
  # Returns vector of selected row indices
  selected_rows <- input$myVariables_DT_selected
})
```

## Step-by-Step Migration Template

### For an Editable Table Like `myVariables_DT`:

1. **Install reactable:**
```r
install.packages("reactable")
```

2. **Update UI** (already done in this migration):
```r
# Change from:
rHandsontableOutput("myVariables_DT")
# To:
reactableOutput("myVariables_DT")
```

3. **Create a new edit handler:**
```r
# This replaces the input$myVariables_DT$changes$changes observer
observeEvent(input$myVariables_DT_edit, {
  edit_info <- input$myVariables_DT_edit
  
  if (is.null(edit_info)) return()
  
  row_idx <- edit_info$row
  col_name <- edit_info$column
  new_value <- edit_info$value
  
  # You can map column index to name if needed
  col_mapping <- c("Name", "Value", "Unit", "Compartment", "Description")
  col_name <- col_mapping[edit_info$column]
  
  old_value <- rv.SPECIES$plotted.var.table[row_idx, col_name]
  
  # Your existing validation/processing logic stays the same
  # Just replace yi (column index) with col_name
  
  if (col_name == "Name") {
    # SPECIES NAME CHANGE
    # ...existing logic...
  } else if (col_name == "Value") {
    # CHANGE SPECIES VALUE
    # ...existing logic...
  } else if (col_name == "Unit") {
    # CHANGE SPECIES UNIT
    # ...existing logic...
  } else if (col_name == "Compartment") {
    # CHANGE SPECIES COMPARTMENT (but we set editable = FALSE)
    # ...existing logic...
  } else if (col_name == "Description") {
    # CHANGE SPECIES DESCRIPTION
    # ...existing logic...
  }
})
```

4. **Update the renderer:**
See example below for complete implementation

## CSS Styling Considerations

Your current CSS targets `#myVariables_DT` which will still work with reactable.

To customize reactable styling, you can:
```r
# In your renderReactable:
reactable(...,
  theme = reactableTheme(
    rowSelectedStyle = list(backgroundColor = "#eee"),
    cellPadding = "8px 12px",
    stripedColor = "#f9f9f9"
  )
)
```

## Testing Checklist

After migration:
- [ ] Table renders with correct data
- [ ] Table cells are editable (except read-only ones)
- [ ] Changes are captured and processed correctly
- [ ] Row/column selection works
- [ ] Existing validations work
- [ ] Related equations/parameters update correctly
- [ ] Error messages display
- [ ] CSS styling looks acceptable

## Migration Priority

Based on usage frequency (most editable first):
1. `myVariables_DT` - High interactivity
2. `createVar_compartment_table` - High interactivity  
3. `main_eqns_table` - High interactivity
4. `parameters_DT` - Read-only display
5. `createModel_IO_logs_table` - Editable
6. Others - Lower priority or read-only

## Advantages of This Migration

✅ **Security**: No old JavaScript vulnerabilities  
✅ **Maintenance**: Actively developed, regular updates  
✅ **Performance**: Lighter weight than Handsontable  
✅ **Developer Experience**: Simpler API, less boilerplate  
✅ **Backend Compatibility**: Your R logic stays mostly the same  
✅ **Styling**: More CSS-friendly  

