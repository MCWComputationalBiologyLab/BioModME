# MathML root + (optional) degree -- import/export of nth-root expressions.
# Closes the documented limitation that the parser fails on <root>/<degree>.

test_that("convertML2R reads <root> with no degree as sqrt", {
  out <- convertML2R(mathml_apply_node(
    "<apply><root/><ci>x</ci></apply>"
  ))
  expect_equal(out, "sqrt(x)")
})

test_that("convertML2R reads <root> with explicit <degree>2 as sqrt", {
  out <- convertML2R(mathml_apply_node(
    "<apply><root/><degree><cn>2</cn></degree><ci>x</ci></apply>"
  ))
  expect_equal(out, "sqrt(x)")
})

test_that("convertML2R reads <root> with numeric <degree>n as x^(1/n)", {
  out <- convertML2R(mathml_apply_node(
    "<apply><root/><degree><cn>3</cn></degree><ci>x</ci></apply>"
  ))
  expect_equal(out, "(x)^(1/(3))")
})

test_that("convertML2R reads <root> with symbolic <degree> n as x^(1/n)", {
  out <- convertML2R(mathml_apply_node(
    "<apply><root/><degree><ci>n</ci></degree><ci>x</ci></apply>"
  ))
  expect_equal(out, "(x)^(1/(n))")
})

test_that("convertML2R tolerates <degree> appearing after the radicand", {
  out <- convertML2R(mathml_apply_node(
    "<apply><root/><ci>x</ci><degree><cn>3</cn></degree></apply>"
  ))
  expect_equal(out, "(x)^(1/(3))")
})

test_that("convertML2R handles a compound radicand", {
  out <- convertML2R(mathml_apply_node(
    "<apply><root/><apply><plus/><ci>x</ci><cn>1</cn></apply></apply>"
  ))
  expect_equal(out, "sqrt((x+1))")
})

test_that("mathml2R reads <root> as an R expression", {
  r <- mathml2R(mathml_apply_node(
    "<apply><root/><ci>x</ci></apply>"
  ))
  expect_equal(deparse(r[[1]]), "sqrt(x)")

  r <- mathml2R(mathml_apply_node(
    "<apply><root/><degree><cn>3</cn></degree><ci>x</ci></apply>"
  ))
  expect_equal(deparse(r[[1]]), "(x)^(1/3)")
})

test_that("expToMathML emits <root/> for sqrt() with no <degree>", {
  out <- expToMathML(quote(sqrt(x)))
  expect_true("<root/>" %in% out)
  expect_false(any(out == "<degree>"))
})

test_that("expToMathML emits <root/> + <degree> for x^(1/n)", {
  out <- expToMathML(quote(x^(1/3)))
  expect_true("<root/>" %in% out)
  expect_true("<degree>" %in% out)
  expect_false("<power/>" %in% out)

  out_sym <- expToMathML(quote(x^(1/n)))
  expect_true("<root/>" %in% out_sym)
  expect_true("<degree>" %in% out_sym)
})

test_that("expToMathML keeps <power/> for x^0.5 and x^(2/3) (strict pattern)", {
  out_half <- expToMathML(quote(x^0.5))
  expect_true("<power/>" %in% out_half)
  expect_false("<root/>" %in% out_half)

  out_two_thirds <- expToMathML(quote(x^(2/3)))
  expect_true("<power/>" %in% out_two_thirds)
  expect_false("<root/>" %in% out_two_thirds)
})

test_that("sqrt round-trips: R expression -> MathML -> R string", {
  e <- quote(sqrt(x))
  ml <- expToMathML(e)
  full <- paste0("<math xmlns='http://www.w3.org/1998/Math/MathML'>",
                 paste(ml, collapse = ""), "</math>")
  apply_node <- XML::xmlRoot(XML::xmlTreeParse(full, asText = TRUE))[[1]]
  expect_equal(convertML2R(apply_node), "sqrt(x)")
})

test_that("x^(1/3) round-trips through MathML root/degree", {
  e <- quote(x^(1/3))
  ml <- expToMathML(e)
  full <- paste0("<math xmlns='http://www.w3.org/1998/Math/MathML'>",
                 paste(ml, collapse = ""), "</math>")
  apply_node <- XML::xmlRoot(XML::xmlTreeParse(full, asText = TRUE))[[1]]
  # Semantically equivalent, parenthesized form.
  expect_equal(convertML2R(apply_node), "(x)^(1/(3))")
})
