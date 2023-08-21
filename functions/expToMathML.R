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
    c("<ci> ", as.character(e), " </ci>")
  else if (is.numeric(e))
    c("<cn> ", as.character(e), " </cn>")
  else if (identical(e[[1]], as.symbol("+")))
    c("<apply>", "<plus/>", Recall(e[[2]]), Recall(e[[3]]), "</apply>")
  else if (identical(e[[1]], as.symbol("-"))) {
    if (length(e) == 3) # Binary minus
      c("<apply>", "<minus/>", Recall(e[[2]]), Recall(e[[3]]), "</apply>")
    else # Unary minus
      c("<apply>", "<minus/>", Recall(e[[2]]), "</apply>")
  } else if (identical(e[[1]], as.symbol("*")))  
    c("<apply>", "<times/>", Recall(e[[2]]), Recall(e[[3]]), "</apply>")
  else if (identical(e[[1]], as.symbol("/")))  
    c("<apply>", "<divide/>", Recall(e[[2]]), Recall(e[[3]]), "</apply>")
  else if (identical(e[[1]], as.symbol("^")))
    c("<apply>", "<power/>", Recall(e[[2]]), Recall(e[[3]]), "</apply>")
  else if (identical(e[[1]], as.symbol("("))) Recall(e[[2]])
}
