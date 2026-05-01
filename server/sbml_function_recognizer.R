# Recognizes whether an imported SBML <functionDefinition> body matches one of
# BioModME's predefined kinetic-law shapes (Michaelis-Menten, mass action, ...).
#
# Annotate-only contract: this never rewrites the user's body. It returns a
# label + variable bindings so the UI can show "Recognized as: ..." next to a
# custom law that still evaluates the user's original expression.
#
# Predefined shapes recognized (matches templates in rate_laws_equations.R):
#   michaelis_menten         V * S / (K + S)
#   degradation_by_enzyme    kcat * E * S / (K + S)
#   mass_action              kf * A * B [* C ...]   (>=2 reactants)
#   degradation_rate         k * X
#   synthesis                k    (single symbol)
#
# Anything else falls through to "custom" with confidence "none".

# AST helpers ------------------------------------------------------------------

unwrap_parens <- function(e) {
  while (is.call(e) && identical(e[[1]], as.symbol("(")) && length(e) == 2) {
    e <- e[[2]]
  }
  e
}

is_call_to <- function(e, op) {
  is.call(e) && identical(e[[1]], as.symbol(op))
}

collect_factors <- function(e) {
  e <- unwrap_parens(e)
  if (is_call_to(e, "*") && length(e) == 3) {
    c(collect_factors(e[[2]]), collect_factors(e[[3]]))
  } else {
    list(e)
  }
}

collect_terms <- function(e) {
  e <- unwrap_parens(e)
  if (is_call_to(e, "+") && length(e) == 3) {
    c(collect_terms(e[[2]]), collect_terms(e[[3]]))
  } else {
    list(e)
  }
}

ast_equal <- function(a, b) {
  identical(deparse(a), deparse(b))
}

is_symbol_or_power <- function(e) {
  # Accept either a plain symbol or a `symbol^integer` factor (mass action with
  # stoichiometry > 1).
  if (is.symbol(e)) return(TRUE)
  if (is_call_to(e, "^") && length(e) == 3 &&
      is.symbol(e[[2]]) && is.numeric(e[[3]])) return(TRUE)
  FALSE
}

# Pattern matchers -------------------------------------------------------------

match_michaelis_menten <- function(law_expr) {
  # V * S / (K + S)
  e <- unwrap_parens(law_expr)
  if (!is_call_to(e, "/") || length(e) != 3) return(NULL)

  num <- unwrap_parens(e[[2]])
  den <- unwrap_parens(e[[3]])

  num_factors <- collect_factors(num)
  den_terms   <- collect_terms(den)
  if (length(num_factors) != 2) return(NULL)
  if (length(den_terms)   != 2) return(NULL)
  if (!all(sapply(num_factors, is.symbol))) return(NULL)
  if (!all(sapply(den_terms,   is.symbol))) return(NULL)

  for (i in 1:2) {
    for (j in 1:2) {
      if (ast_equal(num_factors[[i]], den_terms[[j]])) {
        Vmax <- num_factors[[3 - i]]
        Km   <- den_terms[[3 - j]]
        S    <- num_factors[[i]]
        return(list(
          backend.name = "michaelis_menten",
          confidence   = "structural",
          binding      = list(
            Vmax      = deparse(Vmax),
            Km        = deparse(Km),
            substrate = deparse(S)
          )
        ))
      }
    }
  }
  NULL
}

match_mm_no_vmax <- function(law_expr) {
  # kcat * E * S / (K + S)
  e <- unwrap_parens(law_expr)
  if (!is_call_to(e, "/") || length(e) != 3) return(NULL)

  num <- unwrap_parens(e[[2]])
  den <- unwrap_parens(e[[3]])

  num_factors <- collect_factors(num)
  den_terms   <- collect_terms(den)
  if (length(num_factors) != 3) return(NULL)
  if (length(den_terms)   != 2) return(NULL)
  if (!all(sapply(num_factors, is.symbol))) return(NULL)
  if (!all(sapply(den_terms,   is.symbol))) return(NULL)

  for (i in 1:3) {
    for (j in 1:2) {
      if (ast_equal(num_factors[[i]], den_terms[[j]])) {
        S      <- num_factors[[i]]
        Km     <- den_terms[[3 - j]]
        others <- num_factors[-i]
        return(list(
          backend.name = "degradation_by_enzyme",
          confidence   = "structural",
          binding      = list(
            kcat      = deparse(others[[1]]),
            enzyme    = deparse(others[[2]]),
            Km        = deparse(Km),
            substrate = deparse(S)
          )
        ))
      }
    }
  }
  NULL
}

match_mass_action <- function(law_expr) {
  # kf * A * B [* C ...] -- at least 3 factors so a 2-symbol product
  # (i.e. k * X) routes to degradation_rate instead.
  e <- unwrap_parens(law_expr)
  if (!is_call_to(e, "*")) return(NULL)
  factors <- collect_factors(e)
  if (length(factors) < 3) return(NULL)
  if (!all(sapply(factors, is_symbol_or_power))) return(NULL)

  list(
    backend.name = "mass_action",
    confidence   = "structural",
    binding      = list(
      kf        = deparse(factors[[1]]),
      reactants = sapply(factors[-1], deparse)
    )
  )
}

match_degradation_rate <- function(law_expr) {
  # k * X (or X * k -- both factors are bare symbols)
  e <- unwrap_parens(law_expr)
  if (!is_call_to(e, "*") || length(e) != 3) return(NULL)
  factors <- collect_factors(e)
  if (length(factors) != 2) return(NULL)
  if (!all(sapply(factors, is.symbol))) return(NULL)

  list(
    backend.name = "degradation_rate",
    confidence   = "structural",
    binding      = list(
      rateConstant       = deparse(factors[[1]]),
      degradatedVariable = deparse(factors[[2]])
    )
  )
}

match_synthesis <- function(law_expr) {
  # A bare symbol on its own.
  e <- unwrap_parens(law_expr)
  if (!is.symbol(e)) return(NULL)

  list(
    backend.name = "synthesis",
    confidence   = "structural",
    binding      = list(rateConstant = deparse(e))
  )
}

# Main entry -------------------------------------------------------------------

RecognizeKnownLaw <- function(law_string, bvars = NULL) {
  no_match <- list(
    matched      = FALSE,
    backend.name = "custom",
    confidence   = "none",
    binding      = list()
  )

  if (is.null(law_string) || length(law_string) == 0 ||
      is.na(law_string)   || law_string == "") {
    return(no_match)
  }

  expr <- tryCatch(parse(text = law_string)[[1]], error = function(e) NULL)
  if (is.null(expr)) return(no_match)

  # Normalize via Deriv::Simplify when available; fall back to the raw expr.
  expr_norm <- tryCatch(Deriv::Simplify(expr), error = function(e) expr)

  patterns <- list(
    match_michaelis_menten,
    match_mm_no_vmax,
    match_mass_action,
    match_degradation_rate,
    match_synthesis
  )

  for (p in patterns) {
    result <- p(expr_norm)
    if (!is.null(result)) {
      return(list(
        matched      = TRUE,
        backend.name = result$backend.name,
        confidence   = result$confidence,
        binding      = result$binding
      ))
    }
  }

  no_match
}
