# Headless smoke test for the SBML loader.
#
# Walks a folder of .xml/.sbml files and runs each through the same pipeline
# the Shiny Import tab uses (LoadSBML_show_progress in server/load_sbml.R).
# Reports PASS/FAIL per file plus per-file row counts. Exit status is non-zero
# if any file fails to load.
#
# Usage (from the project root):
#   Rscript test_scripts/sbml_batch_load_test.R "C:/path/to/folder"
# If no path is given, defaults to ~/Desktop/SBMLs.

suppressPackageStartupMessages({
  library(xml2)
  library(dplyr)
  library(tibble)
  library(XML)
  library(shiny)
  library(stringr)
})

# Stubs so that sourcing server/load_sbml.R outside of a Shiny session does
# not error on the top-level observeEvent or on the waiter helper.
observeEvent <- function(...) invisible(NULL)
observe      <- function(...) invisible(NULL)
input  <- list()
output <- list()
session <- list()

# Source app helpers, then the SBML modules. Resolve project root from this
# script's location when run via Rscript, otherwise fall back to the cwd.
script_path <- tryCatch({
  args_full <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args_full, value = TRUE)
  if (length(file_arg) > 0) sub("^--file=", "", file_arg[[1]]) else NA_character_
}, error = function(e) NA_character_)
proj_root <- if (!is.na(script_path) && nzchar(script_path)) {
  normalizePath(file.path(dirname(script_path), ".."), mustWork = FALSE)
} else {
  normalizePath(".", mustWork = FALSE)
}
if (!dir.exists(file.path(proj_root, "functions"))) {
  proj_root <- normalizePath(".", mustWork = FALSE)
}
for (f in list.files(file.path(proj_root, "functions"), pattern = "\\.R$", full.names = TRUE)) source(f)
source(file.path(proj_root, "server", "helpers.R"))
source(file.path(proj_root, "server", "sbml_fxns.R"))
source(file.path(proj_root, "server", "sbml_function_recognizer.R"))
source(file.path(proj_root, "server", "load_sbml.R"))

# Mock waiter so LoadSBML_show_progress's UI updates are no-ops.
mock_waiter <- list(
  update = function(...) invisible(NULL),
  show   = function(...) invisible(NULL),
  hide   = function(...) invisible(NULL)
)

run_one <- function(sbml_path) {
  start <- Sys.time()
  warns <- character()
  call_stack <- NULL
  res <- tryCatch(
    withCallingHandlers({
      LoadSBML_show_progress(sbml_path, mock_waiter, spinner = "")
    },
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    },
    error = function(e) {
      # Capture the stack at the moment of error, before the error unwinds.
      call_stack <<- sys.calls()
    }),
    error = function(e) {
      labels <- if (!is.null(call_stack)) utils::limitedLabels(call_stack) else character()
      # Strip outer harness frames only; keep everything once we reach LoadSBML.
      first_real <- which(startsWith(labels, "LoadSBML"))
      if (length(first_real) > 0) labels <- labels[first_real[[1]]:length(labels)]
      # Drop the trailing handler frames so the deepest visible call is the
      # actual source of the error.
      drop_tail <- c(".handleSimpleError", "h(simpleError",
                     "(function (e)", "function (e)", "value")
      labels <- labels[!vapply(labels, function(x)
        any(vapply(drop_tail, function(p) startsWith(trimws(x), p), logical(1))),
        logical(1))]
      tail_n <- if (length(labels) > 25) tail(labels, 25) else labels
      list(model = NULL,
           error.message = conditionMessage(e),
           trace = paste(tail_n, collapse = "\n           "))
    }
  )
  list(
    result   = res,
    warnings = warns,
    elapsed  = as.numeric(Sys.time() - start, units = "secs")
  )
}

format_summary <- function(res) {
  if (is.null(res$model)) {
    base <- sprintf("model=NULL err='%s'", res$error.message)
    if (!is.null(res$trace)) base <- paste0(base, "\n       at: ", res$trace)
    return(base)
  }
  m <- res$model
  parts <- c(
    sprintf("compartments=%d",
            if (!is.null(m$compartments)) nrow(m$compartments) else 0),
    sprintf("species=%d",
            if (!is.null(m$species))      nrow(m$species)      else 0),
    sprintf("reactions=%d",
            if (!is.null(m$reactions))    length(m$reactions)  else 0),
    sprintf("constPars=%d",
            if (!is.null(m$parameters$Parameters))
              nrow(m$parameters$Parameters) else 0),
    sprintf("varPars=%d",
            if (!is.null(m$parameters$Variable.Parameters))
              nrow(m$parameters$Variable.Parameters) else 0)
  )
  paste(parts, collapse = " ")
}

# Argument handling
args <- commandArgs(trailingOnly = TRUE)
target <- if (length(args) >= 1) args[[1]] else file.path(Sys.getenv("USERPROFILE"), "Desktop", "SBMLs")
if (!dir.exists(target)) stop("Folder not found: ", target)

files <- list.files(target, pattern = "\\.(xml|sbml)$", full.names = TRUE, ignore.case = TRUE)
if (length(files) == 0) stop("No .xml/.sbml files in: ", target)

cat(sprintf("Testing %d file(s) from %s\n", length(files), target))
cat(strrep("-", 70), "\n", sep = "")

n_pass <- 0L
n_fail <- 0L
for (f in files) {
  name <- basename(f)
  res <- tryCatch(
    run_one(f),
    error = function(e) list(result = list(model = NULL, error.message = conditionMessage(e)),
                             warnings = character(),
                             elapsed = NA_real_)
  )
  status <- if (!is.null(res$result$model)) "PASS" else "FAIL"
  if (status == "PASS") n_pass <- n_pass + 1L else n_fail <- n_fail + 1L
  cat(sprintf("[%s] %-40s  %s  (%.1fs, %d warning%s)\n",
              status, name, format_summary(res$result),
              ifelse(is.na(res$elapsed), 0, res$elapsed),
              length(res$warnings),
              ifelse(length(res$warnings) == 1, "", "s")))
  if (length(res$warnings) > 0) {
    uniq <- unique(res$warnings)
    for (w in head(uniq, 5)) cat("       warn: ", w, "\n", sep = "")
    if (length(uniq) > 5) cat(sprintf("       ... and %d more unique warning(s)\n", length(uniq) - 5))
  }
}

cat(strrep("-", 70), "\n", sep = "")
cat(sprintf("Result: %d passed, %d failed\n", n_pass, n_fail))
if (n_fail > 0) quit(status = 1)
