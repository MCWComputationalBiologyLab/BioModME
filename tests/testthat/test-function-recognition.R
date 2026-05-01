# Recognition of SBML <functionDefinition> bodies against BioModME's
# predefined kinetic-law shapes.
#
# This is annotate-only: we check that the recognizer identifies a known
# shape, but the original String.Rate.Law is never rewritten.

test_that("RecognizeKnownLaw returns the unmatched shape for empty/NA input", {
  for (input in list(NA, NA_character_, "", NULL)) {
    r <- RecognizeKnownLaw(input)
    expect_false(r$matched)
    expect_equal(r$backend.name, "custom")
    expect_equal(r$confidence,   "none")
  }
})

test_that("RecognizeKnownLaw matches single-symbol synthesis", {
  r <- RecognizeKnownLaw("k")
  expect_true(r$matched)
  expect_equal(r$backend.name, "synthesis")
  expect_equal(r$binding$rateConstant, "k")
})

test_that("RecognizeKnownLaw matches degradation_rate (k * X)", {
  r <- RecognizeKnownLaw("k * X")
  expect_true(r$matched)
  expect_equal(r$backend.name, "degradation_rate")
})

test_that("RecognizeKnownLaw matches mass_action with >= 3 factors", {
  r <- RecognizeKnownLaw("kf * A * B")
  expect_true(r$matched)
  expect_equal(r$backend.name, "mass_action")

  r <- RecognizeKnownLaw("kf * A * B * C")
  expect_true(r$matched)
  expect_equal(r$backend.name, "mass_action")
})

test_that("RecognizeKnownLaw allows stoichiometric powers in mass_action", {
  r <- RecognizeKnownLaw("kf * A^2 * B")
  expect_true(r$matched)
  expect_equal(r$backend.name, "mass_action")
})

test_that("RecognizeKnownLaw matches Michaelis-Menten in canonical form", {
  r <- RecognizeKnownLaw("Vmax*S/(Km+S)")
  expect_true(r$matched)
  expect_equal(r$backend.name, "michaelis_menten")
  expect_equal(r$binding$Vmax,      "Vmax")
  expect_equal(r$binding$Km,        "Km")
  expect_equal(r$binding$substrate, "S")
})

test_that("RecognizeKnownLaw recovers MM with renamed variables", {
  r <- RecognizeKnownLaw("V*X/(K+X)")
  expect_true(r$matched)
  expect_equal(r$backend.name, "michaelis_menten")
  expect_equal(r$binding$Vmax,      "V")
  expect_equal(r$binding$Km,        "K")
  expect_equal(r$binding$substrate, "X")
})

test_that("RecognizeKnownLaw is commutative in MM denominator (S+Km == Km+S)", {
  r <- RecognizeKnownLaw("Vmax*S/(S+Km)")
  expect_true(r$matched)
  expect_equal(r$backend.name, "michaelis_menten")
  expect_equal(r$binding$substrate, "S")
  expect_equal(r$binding$Km,        "Km")
})

test_that("RecognizeKnownLaw matches MM-no-Vmax (kcat*E*S/(K+S))", {
  r <- RecognizeKnownLaw("kcat*E*S/(Km+S)")
  expect_true(r$matched)
  expect_equal(r$backend.name, "degradation_by_enzyme")
  expect_equal(r$binding$Km,        "Km")
  expect_equal(r$binding$substrate, "S")
})

test_that("RecognizeKnownLaw rejects almost-MM with extra denominator term", {
  # Regression for the partial-match risk.
  r <- RecognizeKnownLaw("Vmax*S/(Km+S+0.001)")
  expect_false(r$matched)
  expect_equal(r$backend.name, "custom")
})

test_that("RecognizeKnownLaw rejects Hill kinetics", {
  r <- RecognizeKnownLaw("Vmax*S^n/(Km^n+S^n)")
  expect_false(r$matched)
  expect_equal(r$backend.name, "custom")
})

test_that("RecognizeKnownLaw rejects bare sums and ratios without the +", {
  expect_false(RecognizeKnownLaw("a + b")$matched)
  expect_false(RecognizeKnownLaw("a*b/c")$matched)
})

test_that("RecognizeKnownLaw is robust to input that fails to parse", {
  r <- RecognizeKnownLaw("not a valid R expression /// $$$ ")
  expect_false(r$matched)
  expect_equal(r$backend.name, "custom")
})
