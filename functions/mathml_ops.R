# Single source of truth for MathML <-> R operator translation.
#
# Read by:
#   functions/expToMathML.R         -- R expression -> MathML (writer)
#   server/sbml_fxns.R::convertML2R -- MathML -> R string
#   server/sbml_fxns.R::mathml2R    -- MathML -> R expression
#   server/sbml_fxns.R::ML2R        -- MathML tag -> R symbol (rule leaves)
#
# Adding a new operator: add an entry here; for non-uniform shapes (e.g.
# <root> with an optional <degree> child) add a custom branch in the
# consuming site that needs special assembly.

MATHML_OPS <- list(
  power  = list(r = "^",   tag = "power",  arity = 2,      fixity = "infix"),
  divide = list(r = "/",   tag = "divide", arity = 2,      fixity = "infix"),
  times  = list(r = "*",   tag = "times",  arity = "n",    fixity = "infix"),
  plus   = list(r = "+",   tag = "plus",   arity = "n",    fixity = "infix"),
  minus  = list(r = "-",   tag = "minus",  arity = "1or2", fixity = "infix"),
  exp    = list(r = "exp", tag = "exp",    arity = 1,      fixity = "function"),
  ln     = list(r = "log", tag = "ln",     arity = 1,      fixity = "function")
)

mathml_tag_to_r <- function(tag) {
  entry <- MATHML_OPS[[tag]]
  if (is.null(entry)) NULL else entry$r
}

mathml_r_to_op <- function(r_symbol) {
  for (nm in names(MATHML_OPS)) {
    if (identical(MATHML_OPS[[nm]]$r, r_symbol)) return(MATHML_OPS[[nm]])
  }
  NULL
}
