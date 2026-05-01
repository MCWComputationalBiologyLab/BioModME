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

  # sqrt(x) -> <apply><root/>x</apply>  (degree defaults to 2)
  if (op_str == "sqrt" && length(e) == 2)
    return(c("<apply>", "<root/>", expToMathML(e[[2]]), "</apply>"))

  # x^(1/n) -> <apply><root/><degree>n</degree>x</apply>
  # Strict pattern only: literal numeric 1 in the numerator. x^0.5 stays as <power/>.
  if (op_str == "^" && length(e) == 3) {
    exponent <- e[[3]]
    while (is.call(exponent) && identical(exponent[[1]], as.symbol("(")) &&
           length(exponent) == 2) {
      exponent <- exponent[[2]]
    }
    if (is.call(exponent) &&
        identical(exponent[[1]], as.symbol("/")) &&
        length(exponent) == 3 &&
        is.numeric(exponent[[2]]) && exponent[[2]] == 1) {
      return(c("<apply>", "<root/>",
               "<degree>", expToMathML(exponent[[3]]), "</degree>",
               expToMathML(e[[2]]), "</apply>"))
    }
  }

  op_def <- mathml_r_to_op(op_str)
  if (is.null(op_def))
    return(NULL)

  args <- unlist(lapply(as.list(e)[-1], expToMathML))
  c("<apply>", paste0("<", op_def$tag, "/>"), args, "</apply>")
}
