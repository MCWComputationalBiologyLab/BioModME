# replace_word_recursive 
#
# Purpose:
# This function recursively replaces instances of a target word within a list,
#  which can contain sublists, dataframes, and vectors. 
# The target word is replaced only if it appears as an isolated word, not as 
# part of another word.
#
# Inputs:
# - lst: The list where replacements should be made.
# - target: The word that needs to be replaced.
# - replacement: The word with which the target word should be replaced.
# - to.latex: (bool) if TRUE, converts target/replacement to latex
# - to.mathjax: (bool) if TRUE, converts target/replacement to mathjax
#
# Returns:
# A modified version of the input list with the target word replaced with the 
# specified replacement


# The target word is placed between two instances of this pattern, ensuring 
# that the word is surrounded by any of these specified characters.

replace_word_recursive <- function(lst, 
                                   target, 
                                   replacement, 
                                   to.latex = FALSE, 
                                   to.mathjax = FALSE) {
  
  # Apply Var2Latex or Var2Mathjax transformations based on the provided flags
  # These functions should be found in the "functions" folder
  if (to.latex) {
    target.mod <- Var2Latex(target)
    replacement.mod <- Var2Latex(replacement)
  } else if (to.mathjax) {
    target.mod <- Var2MathJ(target)
    replacement.mod <- Var2MathJ(replacement)
  } else {
    target.mod <- target
    replacement.mod <- replacement
  }

  # Constructing the regular expression patterns for the target and 
  # its replacement.
  # Constructing the regular expression patterns for the target and its replacement.
  # Escape the square brackets, underscores, and curly braces to match them literally
  target_pattern <- paste0("(^|\\s|,|\\*|\\(|\\)|\\+|-|/|\\[|\\]|\\{|\\}|_)", 
                           target, 
                           "($|\\s|,|\\*|\\(|\\)|\\+|-|/|\\[|\\]|\\{|\\}|_)")
  # target_pattern <- paste0("(^|\\s|,|\\*|\\(|\\)|\\+|-|/)", 
  #                          target.mod, 
  #                          "($|\\s|,|\\*|\\(|\\)|\\+|-|/)")
  replacement_pattern <- paste0("\\1", replacement.mod, "\\2")
  
  # Apply function to each item in the list
  lapply(lst, function(item) {
    # If item is a character vector
    if (is.character(item)) {
      return(replace_word_vector(item, target_pattern, replacement_pattern))
    }
    # If item is a dataframe
    else if (is.data.frame(item)) {
      return(replace_word_df(item, target_pattern, replacement_pattern))
    }
    # If item is a list
    else if (is.list(item)) {
      return(replace_word_recursive(item, 
                                    target, 
                                    replacement, 
                                    to.latex, 
                                    to.mathjax))
    }
    # If item is none of the above
    else {
      return(item)
    }
  })
}


# replace_word_vector 
#
# Purpose:
# This function replaces instances of a target word within a vector.
# The target word is replaced only if it appears as an isolated word, 
# not as part of another word.
#
# Inputs:
# - vec: The vector where replacements should be made.
# - target: The word that needs to be replaced.
# - replacement: The word with which the target word should be replaced.
#
# Returns:
# A modified version of the input vector with the target word replaced with
#  the specified replacement
replace_word_vector <- function(vec, target, replacement) {
  # Apply the replacement to each element of the vector using sub
  replaced_vec <- sapply(vec, function(item) gsub(target, replacement, item))
  return(unname(replaced_vec))
}

# replace_word_df 

# Purpose:
# This function replaces instances of a target word within a dataframe.
# The target word is replaced only if it appears as an isolated word, 
# not as part of another word.
#
# Inputs:
# - df: The dataframe where replacements should be made.
# - target: The word that needs to be replaced.
# - replacement: The word with which the target word should be replaced.
#
# Returns:
# A modified version of the input dataframe with the target word replaced
# with the specified replacement.

replace_word_df <- function(df, target, replacement) {
  df[] <- lapply(df, function(col) {
    if (is.character(col)) {
      return(sapply(col, function(cell) gsub(target, replacement, cell)))
    }
    return(col)
  })
  return(df)
}

# # Test Code: Changing "Prot" in the list
# 
# lst <- list(
#   sub_list = list(
#     vec = c("Prot", "I.Prot", "Prot again"),
#     df = data.frame(A = c("Prot A", "I.Prot B", "Prot C"), B = c("No Prot", "I.Prot D", "E"))
#   ),
#   df = data.frame(X = c("Prot X", "I.Prot Y"), Y = c("Prot Z", "I.Prot W")),
#   vec = c("Another Prot", "Last Prot", "I.Prot End"),
#   Species = "I, Prot, I.Prot",
#   Reactants = "I, Prot",
#   new.test = "V_cell*(k_f4*I*Prot)"
# )
# 
# # before replacement
# print(lst) 
# lst <- replace_word_recursive(lst, "Prot", "Replacement")
# # after replacement
# print(lst) 