SubstituteSingleRateLawTerm <- function(rateLaw,
                                        term.to.replace,
                                        replacement.term) {
  
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
  
  
  locations  <- GetVarIndices(split.rate.law, term.to.replace)

  
  new.rate.law <- split.rate.law
  new.rate.law <- ReplaceVarIndices(new.rate.law, 
                                    locations, 
                                    replacement.term)
  
  new.text.law <- paste0(new.rate.law, collapse = "")
  
  return(new.text.law)
  
}