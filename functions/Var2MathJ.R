Var2MathJ <- function(var = NULL){
  # Converts a BioModME variable name to a MathJax/KaTeX-safe form. The
  # first underscore opens a subscript group; subsequent underscores in
  # the same name are escaped as `\_` so KaTeX does not interpret them
  # as nested subscripts (which produces a "double subscript" error).
  #
  # Examples:
  #   "my_var"     -> "my_{var}"
  #   "X14_3_3_s"  -> "X14_{3\\_3\\_s}"

  if (is.null(var)) return("")
  split.var <- strsplit(var, "")[[1]]
  has.underscore <- FALSE
  latex.var <- ""

  for (i in seq_along(split.var)) {
    ch <- split.var[i]
    if (ch == "_") {
      if (!has.underscore) {
        has.underscore <- TRUE
        latex.var <- paste0(latex.var, "_{")
      } else {
        # Already inside the subscript group -- escape so KaTeX treats
        # this underscore as a literal character, not a subscript op.
        latex.var <- paste0(latex.var, "\\_")
      }
    } else {
      latex.var <- paste0(latex.var, ch)
    }
  }
  if (has.underscore) latex.var <- paste0(latex.var, "}")
  latex.var
}