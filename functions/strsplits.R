strsplits <- function(x, splits, ...)
  #splits string on multiple inputs
  #used strsplits(a, c(",", " ")) for space and comma splits of c
  #returns vector of split variables
  #https://stackoverflow.com/questions/10738729/r-strsplit-with-multiple-unordered-split-arguments
{
  for (split in splits)
  {
    x <- unlist(strsplit(x, split, ...))
  }
  return(x[!x == ""]) # Remove empty values
}