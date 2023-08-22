RenameVarInDF <- function(oldName, newName, dfToSearch) {
  # When the parameter is renamed it needs to be renamed in many places 
  # Function is used on dataframes
  # Inputs:
  #   @oldName - String name of the parameter to be changed
  #   @newName - Desired String name of the new parameter
  #   @vectorToCheck - Vector of data to look for string in
  # Output:   
  #   Returns df with changed name values (if any) 
  regex_oldName <- paste0("\\b", oldName, "\\b")
  
  n.rows <- nrow(dfToSearch)
  n.cols <- ncol(dfToSearch)
  new.df <- dfToSearch
  #check to make sure rows exist as some dateframes in this program are initiated without columns (columsn just extra check)
  if (n.rows != 0 & n.cols != 0) {
    for (i in seq(n.rows)) {
      for (j in seq(n.cols)) {
        has.var <- grepl(regex_oldName, dfToSearch[i,j], fixed = TRUE)
        if (has.var) {
          new.df[i,j] <- gsub(regex_oldName, newName, dfToSearch[i,j])
        }
      }
    }
  }
  return(new.df)
}