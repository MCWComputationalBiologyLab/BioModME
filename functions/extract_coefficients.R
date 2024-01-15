#' Extract coefficients from a chemical equation.
#'
#' This function takes a chemical equation as input and extracts the stoichiometry coefficients.
#'
#' @param equation A character string representing the chemical equation.
#' 
#' @return A list with two numeric vectors: reactants and products.
#' 
#' @examples
#' equation <- "2*A  [kfg_67]<->(kf1) 4*C + 2*D"
#' result <- extract_coefficients(equation)
#' print(result$reactants)
#' print(result$products)
#'
#' @export
extract_coefficients <- function(equation) {
  # Split the equation into reactants and products
  parts <- strsplit(equation, "\\+|<->|->|-->")[[1]]

  # Remove terms in brackets or parentheses
  cleaned_parts <- str_replace_all(parts, "\\([^\\)]+\\)|\\[[^\\]]+\\]", "")

  # Extract coefficients for each term
  coefficients <- sapply(str_extract_all(cleaned_parts, "\\b\\d+\\b"), function(x) ifelse(length(x) > 0, as.numeric(x), 1))

  # Flatten the result to a vector
  coefficients <- unlist(coefficients)
  
  # Calculate the number of reactants and products
  eqn_split <- gsub("\\s", "", strsplit(equation, "\\<->|->|-->")[[1]])
  search.parts <- gsub("\\s", "", cleaned_parts)
  found.in.reactant <- 
    sapply(search.parts, 
           function(pattern) any(grepl(pattern, eqn_split[1], fixed = TRUE)))
  
  found.in.product <- 
    sapply(search.parts, 
           function(pattern) any(grepl(pattern, eqn_split[2], fixed = TRUE)))
  
  
  n.reactants <- length(which(found.in.reactant))
  n.products <- length(which(found.in.product))
  
  # Split coefficients into reactants and products
  reactants <- head(coefficients, n.reactants)
  products <- tail(coefficients, n.products)
  
  return(list(reactants = reactants, products = products))
}
