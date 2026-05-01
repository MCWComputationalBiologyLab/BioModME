expToMathML <- function(e) {
  # Recursive function to build content mathml expression from a string
  # expression.
  # @e - string expression, expression (use quote, or parse(text=X)[[1]])
  # Output:
  # Example:
  # Input: "Vmax*S/(Km+S)"
  # Output:
  # [1] "<apply>"   "<divide/>" "<apply>"   "<times/>"  "<ci>"      "Vmax"
  # [7] "</ci>"     "<ci>"      "S"         "</ci>"     "</apply>"  "<apply>"
  # [13] "<plus/>"   "<ci>"      "Km"        "</ci>"     "<ci>"      "S"
  # [19] "</ci>"     "</apply>"  "</apply>"

  if (is.symbol(e))
    return(c("<ci> ", as.character(e), " </ci>"))
  if (is.numeric(e))
    return(c("<cn> ", as.character(e), " </cn>"))
  if (!is.call(e))
    return(NULL)

  op_str <- as.character(e[[1]])
  if (op_str == "(")
    return(Recall(e[[2]]))

  op_def <- mathml_r_to_op(op_str)
  if (is.null(op_def))
    return(NULL)

  args <- unlist(lapply(as.list(e)[-1], expToMathML))
  c("<apply>", paste0("<", op_def$tag, "/>"), args, "</apply>")
}
