SubstituteRateLawTerms <- function(rateLaw,
                                   reactants,
                                   products, 
                                   modifiers,
                                   parameters,
                                   new.reactants,
                                   new.products,
                                   new.modifiers,
                                   new.parameters) {
  
  GetVarIndices <- function(allTerms, searchTerms) {
    # Search allTerms for each var in searchTerms and return indices
    # Inputs: 
    #   @allTerms - vector of split terms c("a", "*", "Vmax", "/" "t_1")
    #   @searchTerms - vector of terms to find c("a", "t_1")
    # Outputs: 
    #   List of serach terms with vector of indices as the result
    #     out <- list("a" = c(1), "t_1" = c(5))
    
    locations <- list()
    if (isTruthy(searchTerms)) {
      for (var in searchTerms) {
        loc <- which(allTerms %in% var)
        if (isTruthy(loc)) {
          locations[[var]] <- loc
        } else {
          locations[[var]] <- NA
        }
      }
    }
    return(locations)
  }
  
  ReplaceVarIndices <- function(allTerms, idxList, replacementVars) {
    
    if (length(idxList) != 0) {
      for (i in seq_along(idxList)) {
        if (isTruthy(idxList[[i]])) {
          new.term <- replacementVars[i]
          for (j in idxList[[i]]) {
            allTerms[j] <- new.term
          }
        }
      }
    }
    return(allTerms)
  }
  
  split.rate.law <- SplitEquationString(rateLaw)
  
  
  reactant.locations  <- GetVarIndices(split.rate.law, reactants)
  product.locations   <- GetVarIndices(split.rate.law, products)
  modifier.locations  <- GetVarIndices(split.rate.law, modifiers)
  parameter.locations <- GetVarIndices(split.rate.law, parameters)
  
  new.rate.law <- split.rate.law
  new.rate.law <- ReplaceVarIndices(new.rate.law, 
                                    reactant.locations, 
                                    new.reactants)
  new.rate.law <- ReplaceVarIndices(new.rate.law, 
                                    product.locations, 
                                    new.products)
  
  new.rate.law <- ReplaceVarIndices(new.rate.law, 
                                    modifier.locations, 
                                    new.modifiers)
  
  new.rate.law <- ReplaceVarIndices(new.rate.law, 
                                    parameter.locations, 
                                    new.parameters)
  
  new.text.law <- paste0(new.rate.law, collapse = "")
  
  return(new.text.law)
  
}