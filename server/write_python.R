# Build a Python script that mirrors the Julia export but uses scipy's
# solve_ivp + numpy + matplotlib instead of DifferentialEquations.jl. The
# function signatures intentionally match write_julia.R so the download
# handler in 21_export.R can call py_generate_script() with the same
# arguments it passes to jl_generate_script().

# ---- State -------------------------------------------------------------------
py_state_variables <- function(species, ICs, comments) {
  body <- character(length(species))
  for (i in seq_along(species)) {
    sep <- if (i != length(species)) "," else " "
    body[i] <- paste0(
      "        ", ICs[i], sep, "  # ", species[i],
      if (nzchar(comments[i])) paste0(" — ", comments[i]) else ""
    )
  }
  paste0(
    "def state():\n",
    "    \"\"\"Initial state vector (order matches state_names()).\"\"\"\n",
    "    return np.array([\n",
    paste(body, collapse = "\n"), "\n",
    "    ], dtype=float)"
  )
}

py_state_names <- function(species) {
  body <- vapply(seq_along(species), function(i) {
    sep <- if (i != length(species)) "," else ""
    paste0("        \"", species[i], "\"", sep)
  }, character(1))
  paste0(
    "def state_names():\n",
    "    return [\n",
    paste(body, collapse = "\n"), "\n",
    "    ]"
  )
}

# ---- Parameters --------------------------------------------------------------
py_parameter_variables <- function(param_names, defaults_values, comments) {
  default.lines <- vapply(seq_along(param_names), function(i) {
    sep <- if (i != length(param_names)) "," else ""
    cmt <- if (nzchar(comments[i])) paste0("  # ", comments[i]) else ""
    paste0("        \"", param_names[i], "\": ", defaults_values[i], sep, cmt)
  }, character(1))

  order.lines <- vapply(seq_along(param_names), function(i) {
    sep <- if (i != length(param_names)) "," else ""
    paste0("        \"", param_names[i], "\"", sep)
  }, character(1))

  paste0(
    "def params(**kwargs):\n",
    "    \"\"\"Parameter vector. Override defaults via keyword arguments,\n",
    "    e.g. params(k_f1=5).\"\"\"\n",
    "    defaults = {\n",
    paste(default.lines, collapse = "\n"), "\n",
    "    }\n",
    "    defaults.update(kwargs)\n",
    "    # Order must match the unpacking inside model_diff_eq.\n",
    "    order = [\n",
    paste(order.lines, collapse = "\n"), "\n",
    "    ]\n",
    "    return np.array([defaults[k] for k in order], dtype=float)"
  )
}

# ---- Differential equations --------------------------------------------------
# Wraps a long comma-separated list across lines, prefixing each line with
# `prefix` so the generated Python stays under `width` columns.
.py_wrap_csv <- function(items, prefix, width = 80) {
  if (length(items) == 0) return(prefix)
  lines <- character(0)
  current <- prefix
  for (i in seq_along(items)) {
    item <- items[i]
    sep  <- if (i != length(items)) ", " else ""
    candidate <- paste0(current, item, sep)
    if (nchar(candidate) > width && current != prefix) {
      lines <- c(lines, sub("[ ,]+$", "", current))
      current <- paste0(strrep(" ", nchar(prefix)), item, sep)
    } else {
      current <- candidate
    }
  }
  c(lines, current)
}

py_diff_equations <- function(species, parameters, diffeqs, rules,
                              line_width = 80) {
  # Body section accumulator
  parts <- list()
  parts[[length(parts) + 1]] <- "def model_diff_eq(t, u, p):"
  parts[[length(parts) + 1]] <- "    # Unpack State Variables"
  parts <- c(parts, paste0(
    "    ",
    .py_wrap_csv(species, "", width = line_width - 4)
  ))
  # The last species line gets ` = u` appended
  last <- parts[[length(parts)]]
  parts[[length(parts)]] <- paste0(last, " = u")

  parts[[length(parts) + 1]] <- ""
  parts[[length(parts) + 1]] <- "    # Unpack Parameter Variables"
  parts <- c(parts, paste0(
    "    ",
    .py_wrap_csv(parameters, "", width = line_width - 4)
  ))
  last <- parts[[length(parts)]]
  parts[[length(parts)]] <- paste0(last, " = p")

  if (length(rules) > 0) {
    parts[[length(parts) + 1]] <- ""
    parts[[length(parts) + 1]] <- "    # NonConstant Values (Rules)"
    for (r in rules) parts[[length(parts) + 1]] <- paste0("    ", r)
  }

  if (length(diffeqs) > 0) {
    parts[[length(parts) + 1]] <- ""
    parts[[length(parts) + 1]] <- "    # Differential Equations"
    for (i in seq_along(species)) {
      parts[[length(parts) + 1]] <-
        paste0("    d_", species[i], " = ", diffeqs[i])
    }

    parts[[length(parts) + 1]] <- ""
    parts[[length(parts) + 1]] <- "    return ["
    for (i in seq_along(species)) {
      sep <- if (i != length(species)) "," else ""
      parts[[length(parts) + 1]] <- paste0("        d_", species[i], sep)
    }
    parts[[length(parts) + 1]] <- "    ]"
  }

  paste(unlist(parts), collapse = "\n")
}

# ---- Orchestrator ------------------------------------------------------------
# Argument names match jl_generate_script() so 21_export.R can pass the same
# vector of arguments through.
py_generate_script <- function(species,
                               parameters,
                               diffEquations,
                               parameterValues,
                               parameterComments,
                               rules,
                               ICs,
                               speciesComments,
                               timeStart,
                               timeEnd,
                               line_width = 80) {

  # Python identifiers cannot contain dots; mirror the same substitution the
  # Julia generator does. Also convert Julia-style `^` exponentiation to
  # Python `**` in case the rate-law strings carry it through.
  ConvertVarForPython <- function(x) gsub("\\.", "_", x)
  ConvertExprForPython <- function(x) {
    x <- gsub("\\.", "_", x)
    x <- gsub("\\^", "**", x)
    x
  }

  species   <- vapply(species,    ConvertVarForPython,  character(1), USE.NAMES = FALSE)
  parameters <- vapply(parameters, ConvertVarForPython, character(1), USE.NAMES = FALSE)
  diffEquations <- vapply(diffEquations, ConvertExprForPython, character(1), USE.NAMES = FALSE)
  rules     <- if (length(rules) > 0) {
    vapply(rules, ConvertExprForPython, character(1), USE.NAMES = FALSE)
  } else character(0)

  header <- paste0(
    "# Code generated by BioModME\n",
    "# Created on ", Sys.time(), "\n",
    "# -------------------------------\n\n",
    "import numpy as np\n",
    "from scipy.integrate import solve_ivp\n",
    "import matplotlib.pyplot as plt"
  )

  state.vars     <- py_state_variables(species, ICs, speciesComments)
  state.names    <- py_state_names(species)
  parameter.vars <- py_parameter_variables(parameters, parameterValues, parameterComments)
  diff.eq        <- py_diff_equations(species, parameters, diffEquations, rules,
                                      line_width = line_width)

  # Driver: wire u0 / params / solve_ivp / plotting. One figure, all species.
  driver <- paste0(
    "u0 = state()\n",
    "p = params()\n",
    "# Override defaults like:  p = params(k_f1=1.5, V_cell=2)\n",
    "t_span = (", timeStart, ", ", timeEnd, ")\n\n",
    "sol = solve_ivp(\n",
    "    fun=lambda t, u: model_diff_eq(t, u, p),\n",
    "    t_span=t_span,\n",
    "    y0=u0,\n",
    "    method=\"LSODA\",        # robust for stiff/non-stiff biochemistry\n",
    "    dense_output=True,\n",
    "    rtol=1e-6,\n",
    "    atol=1e-9\n",
    ")\n\n",
    "# Plot every species against time on a single figure.\n",
    "names = state_names()\n",
    "plt.figure()\n",
    "for i, name in enumerate(names):\n",
    "    plt.plot(sol.t, sol.y[i], label=name, linewidth=2)\n",
    "plt.xlabel(\"time\")\n",
    "plt.ylabel(\"value\")\n",
    "plt.legend()\n",
    "plt.show()\n\n",
    "# To plot only a subset, replace the loop above with something like:\n",
    "#     for name in [\"A\", \"B\"]:\n",
    "#         plt.plot(sol.t, sol.y[names.index(name)], label=name, linewidth=2)\n",
    "# Index reference (0-based):\n",
    paste0(
      "# ",
      paste(strwrap(
        paste0(seq_along(species) - 1, "-", species, collapse = ", "),
        width = line_width
      ), collapse = "\n# ")
    )
  )

  paste0(
    header,         "\n\n\n",
    state.vars,     "\n\n\n",
    state.names,    "\n\n\n",
    parameter.vars, "\n\n\n",
    diff.eq,        "\n\n\n",
    driver
  )
}
