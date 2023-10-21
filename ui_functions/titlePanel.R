#' titlePanel: A Custom Panel for Shiny UI with Various Styling Options
#'
#' The `titlePanel` function provides a way to create a styled panel
#' in Shiny applications. This custom panel can include widgets, 
#' an optional title, and several style customizations.
#' 
#' Parameters:
#' - `...`: Widgets to be included in the panel.
#' - `title`: An optional title for the panel. Default is NULL.
#' - `bg_color`: Background color for the panel. Default is transparent.
#' - `header_font`: Font for the title. Default is "Arial".
#' - `header_weight`: Weight for the title font. Default is "normal".
#' - `header_font_size`: Font size for the title. Default is "18px".
#' - `header_color`: Color for the title. Default is "black".
#' - `header_bg_color:`: Color of header background box
#' - `panel_id`: User-defined ID for the panel. If not provided, a unique ID will be generated.
#' - `border_thickness`: Thickness of the panel border. Default is "1px".
#' - `border_color`: Color of the panel border. Default is "lightgrey".
#' - `border_type`: Type of the panel border. Default is "solid".
#' - `margin_top`: Top margin for the panel. Default is "10px".

# Create a counter outside the function for unique ID generation
.counter <- new.env()
.counter$val <- 0

titlePanel <- function(..., title = NULL, bg_color = NULL, 
                       header_font = "Arial", header_weight = "normal", 
                       header_font_size = "18px", header_color = "grey",
                       header_bg_color = "lightgrey",
                       header_trans_x = "-50%",
                       header_trans_y = "-60%",
                       panel_id = NULL, border_thickness = "1px", 
                       border_color = "lightgrey", border_type = "solid",
                       margin_top = "20px",
                       padding = "10px") {
  # If no ID provided, generate a unique ID based on counter and timestamp
  if (is.null(panel_id)) {
    .counter$val <- .counter$val + 1
    panel_id <- paste0("custom-panel-", 
                       .counter$val, 
                       "-", 
                       format(Sys.time(), "%Y%m%d%H%M%OS"))
  }
  
  # Widgets to be included in the panel
  widgets <- list(...)
  
  # Optional title for the panel
  title_div <- if (!is.null(title)) {
    div(class = "custom-panel-header", title)
  } else {
    NULL
  }
  
  # Background color styling
  bg_css <- if (!is.null(bg_color)) {
    paste0("background-color: ", bg_color, ";")
  } else {
    "background-color: transparent;"
  }
  
  # Create and return the custom panel with its styles
  tagList(
    tags$style(HTML(paste0(
      "#", panel_id, " {
            ", bg_css, "
            border-radius: 10px;
            border: ", border_thickness, " ", border_type, " ", border_color, ";
            padding:", padding, ";
            position: relative;
            margin-top: ", margin_top, ";
        }
        
        #", panel_id, " .custom-panel-header {
            position: absolute;
            top: 0;
            left: 50%;
            transform: translate(", header_trans_x, ",", header_trans_y, ");
            background-color:", header_bg_color, ";
            padding: 5px 10px;
            border-radius: 5px;
            font-size: ", header_font_size, ";
            font-weight: ", header_weight, ";
            font-family: '", header_font, "';
            color: ", header_color, ";
        }"
    ))),
    div(id = panel_id, class = "custom-panel",
        title_div,
        widgets
    )
  )
}

#' Example Usage:
#'
#' titlePanel(
#'     sliderInput("slider1", "Slider Input:", min = 0, max = 100, value = 50),
#'     textInput("text1", "Text Input:"),
#'     title = "Custom Title",
#'     bg_color = "red",
#'     header_font = "Times New Roman",
#'     header_weight = "bold",
#'     header_font_size = "24px",
#'     header_color = "blue", 
#'     panel_id = "myCustomID",  # User-defined ID
#'     border_thickness = "2px", 
#'     border_color = "blue",
#'     border_type = "dashed",
#'     margin_top = "15px"
#' )
