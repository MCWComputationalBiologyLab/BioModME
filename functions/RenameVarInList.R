RenameVarInList <- function(oldName, newName, listToSearch) {
  # Changes a name in a list to new.  Searches mathjax and latex versions.
  # 
  out <- listToSearch
  if (length(listToSearch) > 0) {
    # Search for Variable Name
    latex.name <- Var2Latex(oldName)
    mathjax.name <- Var2MathJ(oldName)
    regex_oldName <- paste0("\\b", oldName, "\\b")

    for (i in seq_along(out)) {
      var.indices <- which(grepl(regex_oldName, out[[i]], fixed = TRUE))
      if (length(var.indices) > 0) {
        for (idx in var.indices) {
          out[[i]][[idx]] <- 
            gsub(oldName, newName, out[[i]][[idx]],  fixed = TRUE)
        }
      }
      
      # Search for var in latex term
      latex.indices <- which(grepl(latex.name, out[[i]], fixed = TRUE))
      if (length(latex.indices) > 0) {
        for (idx in latex.indices) {
          out[[i]][[idx]] <- 
            gsub(latex.name, 
                 Var2Latex(newName), 
                 out[[i]][[idx]], 
                 fixed = TRUE)
        }
      }
      
      # Search for var in mathjax terms
      mathjax.indices <- which(grepl(mathjax.name, out[[i]], fixed = TRUE))
      if (length(mathjax.indices) > 0) {
        for (idx in mathjax.indices) {
          out[[i]][[idx]] <- 
            gsub(mathjax.name, 
                 Var2MathJ(newName), 
                 out[[i]][[idx]],  
                 fixed = TRUE)
        }
      }
    }
  }
  return(out)
}