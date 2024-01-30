#' Update MathML string by replacing content associated with a target function.
#'
#' This function takes a MathML string and a target function definition,
#' finds the corresponding <apply> tag, and replaces the content within
#' that <apply> tag with a modified <ci> tag.
#'
#' @param mathml_string The input MathML string.
#' @param target_function The target function definition to identify and replace.
#'
#' @return Updated MathML string with replaced content, or NA if an error occurs.
#' @export
#'
#' @examples
#' # Your MathML string
#' mathml_string <- "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n 
#' <apply>\n  <times/>\n  <ci>cell</ci>\n  <apply>\n   
#' <ci>scaled_Michaelis_Menten_1_1</ci>\n   <ci>GF</ci>\n   <ci>Kagf</ci>\n  
#' <ci>eps</ci>\n   <ci>vsap1</ci>\n  </apply>\n </apply>\n</math>"
#'
#' # Function definition to extract and remove
#' target_function <- "scaled_Michaelis_Menten_1_1"
#'
#' # Call the function
#' updated_mathml_string <- replace_fxn_in_mathml(mathml_string, target_function)
#'
#' # Print the updated MathML string
#' print(updated_mathml_string)
#'
#' @export
replace_fxn_in_mathml <- function(mathml_string, target_function) {
  # Find the positions of all occurrences of <apply> in the MathML string
  apply_positions <- gregexpr("<apply>", mathml_string, fixed = TRUE)[[1]]
  
  if (length(apply_positions) > 0) {
    # Find the position of the target function in the MathML string
    target_function_position <- 
      regexpr(paste0("<ci>", target_function, "</ci>"), mathml_string)
    
    if (target_function_position != -1) {
      # Find the opening <apply> tag that corresponds to the target function
      opening_apply_pos <- 
        max(apply_positions[apply_positions < target_function_position])
      
      if (opening_apply_pos != -1) {
        end_pos <- regexpr("</apply>\n", mathml_string, fixed = TRUE)
        extracted_content <- 
          substr(mathml_string, 
                 opening_apply_pos, 
                 end_pos + attr(end_pos, "match.length") - 1)
        
        # Extract the content between opening and closing apply tags
        # Remove the extracted content from the MathML string
        updated_mathml_string <- 
          gsub(extracted_content, 
               paste0("<ci>", "fxn_", target_function, "</ci>\n"), 
               mathml_string
          )
        
        # Return the updated MathML string
        return(updated_mathml_string)
      }
    }
  }
  
  # Return NA if any error occurs
  return(NA)
}
