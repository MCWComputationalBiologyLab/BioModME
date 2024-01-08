gh <- "-(V_cell*(k_f4*I*Prot))"

library(mathml)
y = quote(-(V_cell*(k_f4*I*Prot)))
y = quote(Vmax*S/(Km+S))
mathml(term=y)

mathml(gh)

mathml(quote(gh))

library(mathml)

library(mathml)

equations <- c("-(V_cell*(k_f4*I*Prot))", "Vmax*s/(Km+S)")

# Evaluate each parsed expression and wrap the result with quote
quoted_results <- lapply(equations, function(eq) {
  expr <- parse(text = eq)
  result <- eval(expr)
  quote(expr = result)
})

# Use the mathml function on the quoted results
mathml(quoted_results)


# Parse the equations into R expressions
expressions <- lapply(equations, function(eq) parse(text = eq))

# Create a named list with expressions
named_list <- setNames(expressions, paste0("eq", seq_along(equations)))
quote(named_list[[1]])
# Use the mathml function on the named list
mathml(named_list[[1]])

library(latex2exp)

equations <- c("-(V_cell*(k_f4*I*Prot))", "Vmax*s/(Km+S)")

# Convert to LaTeX format
latex_equations <- sapply(equations, function(eq) toLatex(parse(text = eq)))

# Print LaTeX equations
print(latex_equations)

# You can use latex2exp to convert LaTeX to expressions
expressions <- sapply(latex_equations, function(latex) parse(text = as.character(parse(text = paste0('expression("', latex, '")')))))

# Use the expressions as needed
print(expressions)

help("quote")

yt <- quote(eval(parse(text = equations[1])))
yt

term <- call(equations[1], as.name("V_cell"))
term
mathml(quote(term))

library(mathml)

equations <- c("-(V_cell*(k_f4*I*Prot))", "Vmax*s/(Km+S)")

mathml(eval(parse(text=paste0("quote(", equations[1], ")"))))

# Evaluate each parsed expression and wrap the result with quote
quoted_results <- lapply(equations, function(eq) {
  expr <- parse(text = eq)
  result <- eval(expr)
  quote(expr = result)
})

# Use the mathml function on the quoted results
mathml(quoted_results)


equations <- c("-(V_cell*(k_f4*I*Prot))", "Vmax*s/(Km+S)")

for (eq in equations) {
  mathml_output <- toMathML(eq)
  cat("Original Equation:", eq, "\n")
  cat("MathML Representation:", mathml_output, "\n\n")
}

g <- equations[1]
mathml(quote(enquote(g)))

(s.e <- substitute(expression(a + b), list(a = 1)))  #> expression(1 + b)
(s.s <- substitute( a + b,            list(a = 1)))  #> 1 + b
c(mode(s.e), typeof(s.e)) #  "call", "language"
c(mode(s.s), typeof(s.s)) #   (the same)
# but:
(e.s.e <- eval(s.e))          #>  expression(1 + b)
c(mode(e.s.e), typeof(e.s.e)) #  "expression", "expression"

substitute(x <- x + 1, list(x = 1)) # nonsense

myplot <- function(x, y)
  plot(x, y, xlab = deparse1(substitute(x)),
       ylab = deparse1(substitute(y)))

## Simple examples about lazy evaluation, etc:

f1 <- function(x, y = x)             { x <- x + 1; y }
s1 <- function(x, y = substitute(x)) { x <- x + 1; y }
s2 <- function(x, y) { if(missing(y)) y <- substitute(x); x <- x + 1; y }
a <- 10
f1(a)  # 11
s1(a)  # 11
s2(a)  # a
typeof(s2(a))  # "symbol"


library(xml2)

# Define the MathML string
mathml_string <- "<math xmlns=\"&mathml;\"><mrow><mrow><mo>-</mo><mrow><mo>[</mo><mrow><mi>V_cell</mi><mo>*</mo><mrow><mo>(</mo><mrow><mrow><mrow><mi>k_f1</mi><mo>*</mo><mi>A</mi></mrow><mo>*</mo><mi>B</mi></mrow><mo>-</mo><mrow><mi>k_r1</mi><mo>*</mo><mi>C_1</mi></mrow></mrow><mo>)</mo></mrow></mrow><mo>]</mo></mrow></mrow><mo>+</mo><mrow><mo>[</mo><mrow><mi>V_cell</mi><mo>*</mo><mrow><mo>(</mo><mrow><mi>k_syn5</mi><mo>*</mo><mi>Prot</mi></mrow><mo>)</mo></mrow></mrow><mo>]</mo></mrow></mrow></math>"

# Define the entity reference
entity_definition <- "<!ENTITY mathml 'http://www.w3.org/1998/Math/MathML'>"

# Combine the entity definition and the MathML string
combined_xml <- paste(entity_definition, mathml_string)

# Read the XML
doc <- read_xml(rawToChar(charToRaw(combined_xml)))

# Print the XML content
print(doc)

library(XML)

# Define the MathML string
mathml_string <- "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><mrow><mrow><mo>-</mo><mrow><mo>[</mo><mrow><mi>V_cell</mi><mo>*</mo><mrow><mo>(</mo><mrow><mrow><mrow><mi>k_f1</mi><mo>*</mo><mi>A</mi></mrow><mo>*</mo><mi>B</mi></mrow><mo>-</mo><mrow><mi>k_r1</mi><mo>*</mo><mi>C_1</mi></mrow></mrow><mo>)</mo></mrow></mrow><mo>]</mo></mrow></mrow><mo>+</mo><mrow><mo>[</mo><mrow><mi>V_cell</mi><mo>*</mo><mrow><mo>(</mo><mrow><mi>k_syn5</mi><mo>*</mo><mi>Prot</mi></mrow><mo>)</mo></mrow></mrow><mo>]</mo></mrow></mrow></math>"

# Parse the MathML string
parsed_xml <- xmlTreeParse(mathml_string, useInternalNodes = TRUE)

# Save XML with proper indentation
formatted_string <- saveXML(parsed_xml, indent = TRUE)

# Print the formatted string
dfs <- cat(formatted_string)
dfs
formatted_string
