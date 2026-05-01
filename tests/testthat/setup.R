# Test setup: source the BioModME helpers needed for SBML / MathML tests.
# Working directory while tests run is tests/testthat/, so ../.. is repo root.

suppressPackageStartupMessages({
  library(XML)
  library(xml2)
  library(dplyr)
  library(Deriv)
})

repo_root <- normalizePath("../..", mustWork = TRUE)

source(file.path(repo_root, "functions", "mathml_ops.R"))
source(file.path(repo_root, "functions", "expToMathML.R"))
source(file.path(repo_root, "functions", "string2mathml.R"))
source(file.path(repo_root, "server", "sbml_fxns.R"))

fixture_path <- function(...) {
  file.path(repo_root, "tests", "testthat", "fixtures", "sbml", ...)
}
