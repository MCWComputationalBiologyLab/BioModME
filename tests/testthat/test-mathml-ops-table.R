# Locks the Step 0 operator-table refactor: every currently supported MathML
# operator translates correctly through the four consuming sites and the
# table itself contains the expected entries.

test_that("MATHML_OPS contains the expected operator entries", {
  expected_tags <- c("power", "divide", "times", "plus", "minus", "exp", "ln")
  expect_setequal(names(MATHML_OPS), expected_tags)

  expect_equal(MATHML_OPS$power$r,  "^")
  expect_equal(MATHML_OPS$divide$r, "/")
  expect_equal(MATHML_OPS$times$r,  "*")
  expect_equal(MATHML_OPS$plus$r,   "+")
  expect_equal(MATHML_OPS$minus$r,  "-")
  expect_equal(MATHML_OPS$exp$r,    "exp")
  expect_equal(MATHML_OPS$ln$r,     "log")
})

test_that("mathml_tag_to_r returns the R operator for known tags and NULL otherwise", {
  expect_equal(mathml_tag_to_r("power"), "^")
  expect_equal(mathml_tag_to_r("plus"),  "+")
  expect_equal(mathml_tag_to_r("ln"),    "log")
  expect_null(mathml_tag_to_r("unknown_op"))
})

test_that("mathml_r_to_op finds the entry for a known R symbol", {
  expect_equal(mathml_r_to_op("^")$tag, "power")
  expect_equal(mathml_r_to_op("/")$tag, "divide")
  expect_equal(mathml_r_to_op("log")$tag, "ln")
  expect_null(mathml_r_to_op("not-an-op"))
})

test_that("ML2R returns the legacy mapping for known tags and 'not found' otherwise", {
  expect_equal(ML2R("times"),  "*")
  expect_equal(ML2R("divide"), "/")
  expect_equal(ML2R("plus"),   "+")
  expect_equal(ML2R("minus"),  "-")
  expect_equal(ML2R("power"),  "^")
  expect_equal(ML2R("exp"),    "exp")
  expect_equal(ML2R("ln"),     "log")
  expect_equal(ML2R("not_a_tag"), "not found")
})

test_that("convertML2R produces R strings for canonical operators", {
  expect_equal(
    convertML2R(mathml_apply_node("<apply><times/><ci>Vmax</ci><ci>S</ci></apply>")),
    "(Vmax*S)"
  )
  expect_equal(
    convertML2R(mathml_apply_node("<apply><divide/><ci>S</ci><ci>Km</ci></apply>")),
    "S/(Km)"
  )
  expect_equal(
    convertML2R(mathml_apply_node("<apply><power/><ci>x</ci><cn>2</cn></apply>")),
    "(x)^2"
  )
  expect_equal(
    convertML2R(mathml_apply_node("<apply><plus/><ci>a</ci><ci>b</ci></apply>")),
    "(a+b)"
  )
  expect_equal(
    convertML2R(mathml_apply_node("<apply><minus/><ci>a</ci><ci>b</ci></apply>")),
    "(a-b)"
  )
})

test_that("mathml2R produces R expressions for canonical operators", {
  result <- mathml2R(mathml_apply_node("<apply><times/><ci>Vmax</ci><ci>S</ci></apply>"))
  expect_equal(deparse(result[[1]]), "Vmax * S")

  result <- mathml2R(mathml_apply_node("<apply><divide/><ci>S</ci><ci>Km</ci></apply>"))
  expect_equal(deparse(result[[1]]), "S/Km")

  result <- mathml2R(mathml_apply_node("<apply><power/><ci>x</ci><cn>2</cn></apply>"))
  expect_equal(deparse(result[[1]]), "x^2")
})

test_that("expToMathML emits the right tag for each canonical operator", {
  expect_true("<times/>"  %in% expToMathML(quote(Vmax * S)))
  expect_true("<divide/>" %in% expToMathML(quote(S / Km)))
  expect_true("<power/>"  %in% expToMathML(quote(x ^ 2)))
  expect_true("<plus/>"   %in% expToMathML(quote(a + b)))
  expect_true("<minus/>"  %in% expToMathML(quote(a - b)))
})

test_that("expToMathML handles unary and binary minus distinctly", {
  unary  <- expToMathML(quote(-x))
  binary <- expToMathML(quote(a - b))

  # Both contain exactly one <minus/> tag.
  expect_equal(sum(unary  == "<minus/>"), 1)
  expect_equal(sum(binary == "<minus/>"), 1)

  # Unary form has one operand (one <ci> opener); binary has two.
  expect_equal(sum(unary  == "<ci> "), 1)
  expect_equal(sum(binary == "<ci> "), 2)
})

test_that("expToMathML returns NULL for unrecognized R symbols", {
  expect_null(expToMathML(quote(nosuchop(x))))
})

test_that("convertML2R returns NA and warns on unknown MathML tags", {
  out <- suppressWarnings(
    convertML2R(mathml_apply_node("<unknownTag/>"))
  )
  expect_true(is.na(out))
})
