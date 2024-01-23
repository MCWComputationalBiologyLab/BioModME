RenameVarInVector <- function(oldName, newName, vectorToSearch) {
  # When the parameter is renamed it needs to be renamed in many places including
  # all parameter tables, eqns, eqn tables, differential eqns and the such.
  # function is used on vectors
  # Inputs:
  #   @oldName - String name of the parameter to be changed
  #   @newName - Desired String name of the new parameter
  #   @vectorToCheck - Vector of data to look for string in
  # Output:   
  #   Returns vector with changed named values (if any) 
  # Places that Variables need to be changed:
  #   rv.REACTIONSmain, rv.REACTIONSadditional.eqns, rv.REACTIONSrate.eqns, rv.REACTIONStime.dep.eqns, 
  
  #   DE$de.eqns?
  #   PP section?
  #   params - pretty much all sections
  regex_oldName <- paste0("\\b", oldName, "\\b")
  
  idx = 0
  for (string.var in vectorToSearch) {
    idx = idx + 1 
    has.var <- grepl(regex_oldName, string.var, fixed = TRUE)
    if (has.var) {
      new.eqn <- gsub(regex_oldName, newName, string.var) #replace old name with new and place in new variable
      vectorToSearch[idx] <- new.eqn #replace string in variable its stored
    }
  }
  return(vectorToSearch)
}