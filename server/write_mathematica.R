# Build a Mathematica (.wl / .nb-compatible) script that mirrors the Julia
# and Python exports. Mathematica is fussier about identifiers than the
# others — "_" is a pattern wildcard, "." is not allowed in symbol names,
# and the single letters I, E, C, N, D, O, K are reserved system symbols.
# So we sanitize names up front and remap collisions, then convert every
# species reference in the rate-law strings to a function-of-t form
# (Prot[t]) which is the shape NDSolve expects.

# ---- Identifier sanitization -------------------------------------------------
.mma_reserved_symbols <- c("I", "E", "C", "N", "D", "O", "K", "Pi",
                           "Plot", "True", "False", "List", "Set")

.mma_sanitize_name <- function(name) {
  out <- gsub("[._]", "", as.character(name))
  if (out %in% .mma_reserved_symbols) out <- paste0("var", out)
  out
}

# Replace identifier tokens in an expression string. `name_map` is a named
# character vector (names = original tokens, values = sanitized tokens).
# `species_safe_set` lists which sanitized names are species — those get
# wrapped in [t] when substituted, parameters do not. We sort by descending
# original length so that longer names (e.g. "I_Prot") match before any
# shorter substring (e.g. bare "I").
.mma_apply_substitutions <- function(expr, name_map, species_safe_set) {
  if (length(name_map) == 0 || !nzchar(expr)) return(expr)
  ord <- order(nchar(names(name_map)), decreasing = TRUE)
  for (i in ord) {
    orig <- names(name_map)[i]
    safe <- name_map[[i]]
    if (!nzchar(orig)) next
    repl <- if (safe %in% species_safe_set) paste0(safe, "[t]") else safe
    # BioModME identifiers contain only alphanumerics, dots, and underscores;
    # of those only `.` is a regex metacharacter, so we escape it (as `\.`)
    # and leave the rest alone. `_` is a word character so word boundaries
    # work without further help.
    pat <- paste0("\\b", gsub(".", "\\.", orig, fixed = TRUE), "\\b")
    expr <- gsub(pat, repl, expr, perl = TRUE)
  }
  expr
}

# ---- Section builders --------------------------------------------------------
mma_initial_conditions <- function(species_safe, ICs, comments) {
  lines <- character(length(species_safe))
  for (i in seq_along(species_safe)) {
    sep <- if (i != length(species_safe)) "," else ""
    cmt <- if (length(comments) >= i && nzchar(comments[i])) {
      paste0("  (* ", comments[i], " *)")
    } else ""
    lines[i] <- paste0("  ", species_safe[i], "[0] == ", ICs[i], sep, cmt)
  }
  paste0("ICs = {\n", paste(lines, collapse = "\n"), "\n};")
}

mma_parameters <- function(param_safe, defaults_values, comments) {
  lines <- character(length(param_safe))
  for (i in seq_along(param_safe)) {
    sep <- if (i != length(param_safe)) "," else ""
    cmt <- if (length(comments) >= i && nzchar(comments[i])) {
      paste0("  (* ", comments[i], " *)")
    } else ""
    lines[i] <- paste0("  ", param_safe[i], " -> ", defaults_values[i], sep, cmt)
  }
  paste0("params = {\n", paste(lines, collapse = "\n"), "\n};")
}

mma_diff_equations <- function(species_safe, equations_safe) {
  lines <- character(length(species_safe))
  for (i in seq_along(species_safe)) {
    sep <- if (i != length(species_safe)) "," else ""
    lines[i] <- paste0("  ", species_safe[i], "'[t] == ",
                       equations_safe[i], sep)
  }
  paste0("eqns = {\n", paste(lines, collapse = "\n"), "\n};")
}

mma_rules <- function(rules_lines) {
  if (length(rules_lines) == 0) return("")
  out_lines <- vapply(seq_along(rules_lines), function(i) {
    r   <- rules_lines[i]
    sep <- if (i != length(rules_lines)) "," else ""
    if (grepl("=", r, fixed = TRUE)) {
      parts <- strsplit(r, "=", fixed = TRUE)[[1]]
      lhs   <- trimws(parts[1])
      rhs   <- trimws(paste(parts[-1], collapse = "="))
      paste0("  ", lhs, " -> ", rhs, sep)
    } else {
      paste0("  ", r, sep)
    }
  }, character(1))
  paste0("rules = {\n", paste(out_lines, collapse = "\n"), "\n};")
}

# ---- Orchestrator ------------------------------------------------------------
# Argument names match jl_generate_script() / py_generate_script() so the
# download handler can pass the same vector of inputs.
mma_generate_script <- function(species,
                                parameters,
                                diffEquations,
                                parameterValues,
                                parameterComments,
                                rules,
                                ICs,
                                speciesComments,
                                timeStart,
                                timeEnd) {

  species_safe <- vapply(species,    .mma_sanitize_name, character(1), USE.NAMES = FALSE)
  param_safe   <- vapply(parameters, .mma_sanitize_name, character(1), USE.NAMES = FALSE)

  # Substitution map for the rate-law strings: every original identifier
  # (species or parameter) gets mapped to its sanitized form.
  name_map <- setNames(c(species_safe, param_safe), c(species, parameters))
  species_safe_set <- species_safe

  eqns_safe <- vapply(diffEquations, function(e) {
    .mma_apply_substitutions(e, name_map, species_safe_set)
  }, character(1), USE.NAMES = FALSE)

  rules_safe <- if (length(rules) > 0) {
    vapply(rules, function(r) {
      .mma_apply_substitutions(r, name_map, species_safe_set)
    }, character(1), USE.NAMES = FALSE)
  } else character(0)

  # Banner about any renames so the user can spot what changed.
  all_orig    <- c(species, parameters)
  all_safe    <- c(species_safe, param_safe)
  renamed_idx <- which(all_orig != all_safe)
  rename_notes <- if (length(renamed_idx) > 0) {
    note_lines <- vapply(renamed_idx, function(i) {
      paste0("   ", all_orig[i], "  ->  ", all_safe[i])
    }, character(1))
    paste0(
      "(* The following identifiers were renamed for Mathematica compatibility:\n",
      "   - dots and underscores stripped (Mathematica disallows them in symbols)\n",
      "   - reserved system names (I, E, C, N, D, O, K, Pi) prefixed with 'var'\n\n",
      paste(note_lines, collapse = "\n"),
      "  *)"
    )
  } else ""

  ic_block    <- mma_initial_conditions(species_safe, ICs, speciesComments)
  param_block <- mma_parameters(param_safe, parameterValues, parameterComments)
  eqns_block  <- mma_diff_equations(species_safe, eqns_safe)
  rules_block <- mma_rules(rules_safe)

  # Comma-separated lists for the NDSolve / Plot calls.
  species_list   <- paste(species_safe, collapse = ", ")
  species_t_list <- paste0(species_safe, "[t]", collapse = ", ")
  legend_list    <- paste0("\"", species_safe, "\"", collapse = ", ")
  apply_rules    <- if (length(rules_safe) > 0) " /. rules" else ""

  driver <- paste0(
    "(* ---- Time span ---- *)\n",
    "tStart = ", timeStart, ";\n",
    "tEnd   = ", timeEnd, ";\n\n",
    "(* ---- Solve ---- *)\n",
    "(* First @ pulls the single solution-rule list out of NDSolve's wrapper. *)\n",
    "sol = First @ NDSolve[\n",
    "  Join[eqns /. params", apply_rules, ", ICs],\n",
    "  {", species_list, "},\n",
    "  {t, tStart, tEnd}\n",
    "];\n\n",
    "(* ---- Plot every species against time ---- *)\n",
    "Plot[\n",
    "  Evaluate[{", species_t_list, "} /. sol],\n",
    "  {t, tStart, tEnd},\n",
    "  PlotLegends -> {", legend_list, "},\n",
    "  AxesLabel  -> {\"time\", \"value\"},\n",
    "  PlotRange  -> All\n",
    "]"
  )

  paste0(
    "(* Code generated by BioModME *)\n",
    "(* Created on ", Sys.time(), " *)\n",
    "(* ------------------------- *)\n\n",
    if (nzchar(rename_notes)) paste0(rename_notes, "\n\n") else "",
    "(* ---- Initial conditions ---- *)\n",
    ic_block, "\n\n",
    "(* ---- Parameters ---- *)\n",
    param_block, "\n\n",
    "(* ---- Differential equations ---- *)\n",
    eqns_block, "\n\n",
    if (length(rules_safe) > 0) {
      paste0("(* ---- Rules / custom equations ---- *)\n", rules_block, "\n\n")
    } else "",
    driver, "\n"
  )
}
