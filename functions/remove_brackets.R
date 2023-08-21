remove_braces <- function(input_str) {

  n <- nchar(input_str)
  i <- 1
  result <- character(0)
  
  while (i <= n) {
    char <- substr(input_str, i, i)
    if (char == "_" && substr(input_str, i+1, i+1) == "{") {
      result <- c(result, "_")
      brace_count <- 1
      i <- i + 2
      while (brace_count > 0 && i <= n) {
        if (substr(input_str, i, i) == "{") {
          brace_count <- brace_count + 1
        }
        if (substr(input_str, i, i) == "}") {
          brace_count <- brace_count - 1
        }
        if (brace_count > 0) {
          result <- c(result, substr(input_str, i, i))
        }
        i <- i + 1
      }
    } else {
      result <- c(result, char)
      i <- i + 1
    }
  }
  
  return(paste(result, collapse = ""))
}