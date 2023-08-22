a <- c("Prot", "I.Prot", "I.Prot.Prot", "I_Prot", "Protein")

# Sample vector
a <- c("Prot", "I.Prot", "I.Prot.Prot", "I_Prot", "Protein")

# Replace 'Prot' with 'Replacement' for demonstration purposes
# (change 'Replacement' to whatever you want)
a <- sub("\\bProt\\b", "Replacement", a)

print(a)
# Sample vector
a <- c("Prot", "I.Prot", "I.Prot.Prot", "I_Prot", "Protein")

# Replace 'Prot' with 'Replacement' for demonstration purposes
# (change 'Replacement' to whatever you want)
a <- sub("(^|\\s)Prot($|\\s)", "\\1Replacement\\2", a)

print(a)


a <- c("Prot", "I.Prot", "I.Prot.Prot", "I_Prot", "Protein")
a <- ifelse(a == "Prot", "Replacement", a)
print(a)

b <- c("Prot I.Prot I.ProtProt I_Prot Protien")

# Sample data
a <- c("Prot", "I.Prot", "I.Prot.Prot", "I_Prot", "Protein", "Prot23")
b <- "Prot I.Prot I.ProtProt I_Prot Protein"
lst <- list(a, b)

# Replacement function
replace_word <- function(item, target, replacement) {
  target_pattern <- paste0("(^|\\s)", target, "($|\\s)")
  replacement_pattern <- paste0("\\1", replacement, "\\2")
  
  if (is.character(item) && length(item) == 1) {  # if it's a single string
    words <- unlist(strsplit(item, " "))
    replaced <- sapply(words, function(x) sub(target_pattern, replacement_pattern, x))
    return(paste(replaced, collapse = " "))
  } else if (is.vector(item)) {  # if it's a vector
    replaced <- sapply(item, function(x) sub(target_pattern, replacement_pattern, x))
    return(unname(replaced))
  }
  return(item)
}

# Apply the function to each item in the list, replacing "Prot" with "Replacement"
result_lst <- lapply(lst, replace_word, target = "Prot", replacement = "new")

print(result_lst)



# replace_word_df Function
#
# Purpose:
# This function replaces instances of a target word within a dataframe.
# The target word is replaced only if it appears as an isolated word, not as part of another word.
#
# Inputs:
# - df: The dataframe where replacements should be made.
# - target: The word that needs to be replaced.
# - replacement: The word with which the target word should be replaced.
#
# Returns:
# A modified version of the input dataframe with the target word replaced with the specified replacement.

replace_word_df <- function(df, target, replacement) {
  
  # Constructing the regular expression patterns for the target and its replacement.
  # The pattern ensures that the target word is isolated (i.e., not part of another word).
  target_pattern <- paste0("(^|\\s)", target, "($|\\s)")
  replacement_pattern <- paste0("\\1", replacement, "\\2")
  
  # Iterate over each column in the dataframe
  df[] <- lapply(df, function(col) {
    # Check if the column is of character type
    if (is.character(col)) {
      return(sapply(col, function(cell) sub(target_pattern, replacement_pattern, cell)))
    }
    return(col)  # Return original column if not of character type
  })
  
  return(df)
}

# Example usage:
df <- data.frame(
  A = c("Prot is here", "I.Prot", "Nothing here"),
  B = c("Prot", "I.Prot.Prot", "I_Prot"),
  C = 1:3  # numeric column to demonstrate mixed types
)

print(df)  # before replacement
df <- replace_word_df(df, "Prot", "Replacement")
print(df)  # after replacement


# replace_word_vector Function
#
# Purpose:
# This function replaces instances of a target word within a vector.
# The target word is replaced only if it appears as an isolated word, not as part of another word.
#
# Inputs:
# - vec: The vector where replacements should be made.
# - target: The word that needs to be replaced.
# - replacement: The word with which the target word should be replaced.
#
# Returns:
# A modified version of the input vector with the target word replaced with the specified replacement.

replace_word_vector <- function(vec, target, replacement) {
  
  # Constructing the regular expression patterns for the target and its replacement.
  target_pattern <- paste0("(^|\\s)", target, "($|\\s)")
  replacement_pattern <- paste0("\\1", replacement, "\\2")
  
  # Apply the replacement to each element of the vector
  replaced_vec <- sapply(vec, function(item) sub(target_pattern, replacement_pattern, item))
  
  # Remove any names and return the modified vector
  return(unname(replaced_vec))
}

# Example usage:
vec <- c("Prot", "I.Prot", "I.Prot.Prot", "I_Prot", "Protein", "Prot is here")

print(vec)  # before replacement
vec <- replace_word_vector(vec, "Prot", "Replacement")
print(vec)  # after replacement


replace_word_recursive <- function(lst, target, replacement) {
  
  # Constructing the regular expression patterns for the target and its replacement.
  target_pattern <- paste0("(^|\\s)", target, "($|\\s)")
  replacement_pattern <- paste0("\\1", replacement, "\\2")
  
  # Apply function to each item in the list
  lapply(lst, function(item) {
    # If item is a character vector
    if (is.character(item)) {
      return(replace_word_vector(item, target, replacement))
    }
    # If item is a dataframe
    else if (is.data.frame(item)) {
      return(replace_word_df(item, target, replacement))
    }
    # If item is a list
    else if (is.list(item)) {
      return(replace_word_recursive(item, target, replacement))
    }
    # If item is none of the above
    else {
      return(item)
    }
  })
}

# Test code remains the same.
a <- c("Prot", "I.Prot", "I.Prot.Prot", "I_Prot", "Protein", "Prot23")
b <- "Prot I.Prot I.ProtProt I_Prot Protein"
lst2 <- list(a, b)

lst <- list(
  test = lst2,
  sub_list = list(
    vec = c("Prot", "I.Prot", "Prot again"),
    df = data.frame(A = c("Prot A", "I.Prot B", "Prot C"), B = c("No Prot", "I.Prot D", "E"))
  ),
  df = data.frame(X = c("Prot X", "I.Prot Y"), Y = c("Prot Z", "I.Prot W")),
  vec = c("Another Prot", "Last Prot", "I.Prot End")
)

print(lst)  # before replacement
lst <- replace_word_recursive(lst, "Prot", "Replacement")
print(lst)  # after replacement

                           
                           
replace_word_recursive <- function(lst, target, replacement) {
  
  # Define the replace_word_vector function
  replace_word_vector <- function(vec, target, replacement) {
    target_pattern <- paste0("(^|\\s)", target, "($|\\s)")
    replacement_pattern <- paste0("\\1", replacement, "\\2")
    replaced_vec <- sapply(vec, function(item) sub(target_pattern, replacement_pattern, item))
    return(unname(replaced_vec))
  }
  
  # Define the replace_word_df function
  replace_word_df <- function(df, target, replacement) {
    target_pattern <- paste0("(^|\\s)", target, "($|\\s)")
    replacement_pattern <- paste0("\\1", replacement, "\\2")
    df[] <- lapply(df, function(col) {
      if (is.character(col)) {
        return(sapply(col, function(cell) sub(target_pattern, replacement_pattern, cell)))
      }
      return(col)
    })
    return(df)
  }
  
  # Now the actual logic of the replace_word_recursive function
  lapply(lst, function(item) {
    if (is.character(item)) {
      return(replace_word_vector(item, target, replacement))
    } else if (is.data.frame(item)) {
      return(replace_word_df(item, target, replacement))
    } else if (is.list(item)) {
      return(replace_word_recursive(item, target, replacement))
    } else {
      return(item)
    }
  })
}

# Test code remains the same

lst <- list(
  sub_list = list(
    vec = c("Prot", "I.Prot", "Prot again"),
    df = data.frame(A = c("Prot A", "I.Prot B", "Prot C"), B = c("No Prot", "I.Prot D", "E"))
  ),
  df = data.frame(X = c("Prot X", "I.Prot Y"), Y = c("Prot Z", "I.Prot W")),
  vec = c("Another Prot", "Last Prot", "I.Prot End")
)

print(lst)  # before replacement
lst <- replace_word_recursive(lst, "Prot", "Replacement")
print(lst)  # after replacement
