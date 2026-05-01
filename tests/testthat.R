# Test runner. From the repo root:
#   R -e 'testthat::test_dir("tests/testthat")'
# Or from R:
#   source("tests/testthat.R")
#
# Requires: install.packages("testthat") (one-time).

if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("Package 'testthat' is required to run tests. Install with: install.packages(\"testthat\")")
}

testthat::test_dir("tests/testthat")
