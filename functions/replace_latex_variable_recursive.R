# replace_latex_variable_recursive
#
# Purpose:
#   This function is designed to recursively search and replace LaTeX-style variables
#   within a nested list structure. It works on character vectors, data frames, and lists.
#   If the target does not resemble a LaTeX-style variable (e.g., lacks an underscore),
#   the function will return the input list unchanged.
#
# Inputs:
#   - lst: A list, which can be nested and can contain character vectors and data frames.
#   - target: The LaTeX-style variable you wish to replace (e.g., "A_{1}").
#   - replacement: The word or phrase you want to use as a replacement.
#
# Output:
#   - Returns a list structure similar to the input but with all occurrences of the target
#     replaced by the replacement word or phrase.

replace_latex_variable_recursive <- function(lst, target, replacement) {
  
  # Check if target contains an underscore; if not, return the original list.
  if (!grepl("_", target)) {
    return(lst)
  }
  
  # Check if the input is a single atomic element (e.g., a double) and return it unchanged.
  if (is.atomic(lst) && !is.character(lst)) {
    return(lst)
  }
  # The target is taken as a literal string to match.
  target_pattern <- target
  
  # Apply function to each item in the list
  lapply(lst, function(item) {
    # If item is a character vector
    if (is.character(item)) {
      # Using gsub to replace all occurrences
      return(gsub(target_pattern, replacement, item, fixed = TRUE))
    }
    # If item is a dataframe
    else if (is.data.frame(item)) {
      return(data.frame(lapply(item, function(col) {
        if (is.character(col)) {
          return(gsub(target_pattern, replacement, col, fixed = TRUE))
        } else {
          return(col)
        }
      })))
    }
    # If item is a list
    else if (is.list(item)) {
      return(replace_latex_variable_recursive(item, target, replacement))
    }
    # If item is none of the above
    else {
      return(item)
    }
  })
}


# EXAMPLE
# ---------------------------------------------
# lst <- list(
#   vec = c("A_{1}", "B_{2}", "C_{1} * A_{1}"),
#   df = data.frame(X = c("A_{1} + B_{2}", "D_{1}"), Y = c("E_{2} * A_{1}", "F_{3}")),
#   inner_list = list(
#     vec2 = c("G_{2} / A_{1}", "H_{4}")
#   ),
#   vec2 = c("-(F*A_{1})", "+(F_{1}*A_{1})", "+(F_{2}*A_{1})"),
#   vec3 = c("-\\left(F*A_{1}\\right)", "+\\left(F_{1}*A_{1}\\right)", "+\\left(F_{2}*A_{1}\\right)")
# )
# 
# lst <- replace_latex_variable_recursive(lst, "A_{1}", "NewVar")
# print(lst)
