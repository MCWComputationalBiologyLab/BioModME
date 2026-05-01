# Round-trip helpers for SBML import -> export -> re-import tests.
# Filled in further during Steps 1-3 (root/degree, units, function recognition).
# For now: a thin helper that parses a content-MathML <apply> from a string
# and returns the XMLNode for the first child of <math>.

mathml_apply_node <- function(inner_xml) {
  full <- paste0(
    "<math xmlns='http://www.w3.org/1998/Math/MathML'>",
    inner_xml,
    "</math>"
  )
  doc <- XML::xmlTreeParse(full, asText = TRUE)
  XML::xmlRoot(doc)[[1]]
}
