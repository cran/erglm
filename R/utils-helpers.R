
`%||%` <- function(x, y) {
  if (is.null(x)) return(y)
  x
}

.pick_seed <- function() {999 + sample.int(9000, size = 1L)}

# Renders a bad argument value for an error message without risking a
# second error from an unprintable/zero-length/weird-class input.
.fmt_bad_value <- function(x) {
  tryCatch(
    paste(deparse(x), collapse = " "),
    error = function(e) paste0("<", class(x)[1], ">")
  )
}

# `conf_level` must be a single number in [0, 1] for the `qnorm()`-based
# z-score used by `erglm_predict()`/`er_summary.erglm_model()` to be
# meaningful. 0 and 1 are legitimate (degenerate) endpoints -- a 0%
# interval collapses to the point estimate (z = 0), and a 100% interval
# is infinitely wide (z = Inf) -- but values outside [0, 1] aren't valid
# probabilities and currently produce a silently reversed or NaN interval
# rather than erroring.
.erglm_check_conf_level <- function(conf_level) {
  if (!is.numeric(conf_level) || length(conf_level) != 1L || is.na(conf_level) ||
      conf_level < 0 || conf_level > 1) {
    rlang::abort(paste0(
      "`conf_level` must be a single number between 0 and 1 (inclusive), not ",
      .fmt_bad_value(conf_level), "."
    ))
  }
}

# `term` (as passed to `erglm_add_term()`/`erglm_remove_term()`) must be
# a one-sided formula naming exactly one covariate, e.g. `~ sex`. `NULL`
# (or any non-formula) previously reached `stats::terms(term)` directly
# and failed with a cryptic low-level error ("no terms component nor
# attribute", or "$ operator is invalid for atomic vectors"); a
# two-sided formula (e.g. `y ~ x`) previously slipped past silently and
# either produced a misleading "uses variables not in the data" warning
# (`erglm_add_term()`, since the response was checked as if it were a
# predictor) or was tolerated by discarding the response entirely
# (`erglm_remove_term()`) -- neither of which reflects what happened.
# A multi-term formula (e.g. `~ weight + age`) also previously slipped
# past silently in `erglm_add_term()`: the "already in the model" check
# compares term labels elementwise (`==`), which never matches a
# multi-element vector against the model's single-element one, so every
# term in `term` got added at once with no warning.
.erglm_check_term <- function(term) {
  if (is.null(term) || !inherits(term, "formula")) {
    rlang::abort(paste0(
      "`term` must be a one-sided formula naming a single covariate ",
      "(e.g. `~ sex`), not ", .fmt_bad_value(term), "."
    ))
  }
  if (length(term) != 2L) {
    rlang::abort(paste0(
      "`term` must be a one-sided formula (e.g. `~ sex`), not the ",
      "two-sided formula `", deparse(term), "`. erglm_add_term()/",
      "erglm_remove_term() work on plain covariate terms and don't use ",
      "a response."
    ))
  }
  trm_lab <- attr(stats::terms(term), "term.labels")
  if (length(trm_lab) != 1L) {
    rlang::abort(paste0(
      "`term` must name exactly one covariate (e.g. `~ sex`), not ",
      length(trm_lab), ": `", deparse(term), "`."
    ))
  }
}

# `candidates` (as passed to `erglm_scm_forward()`/`erglm_scm_backward()`)
# must be a non-empty character vector where every element names exactly
# one covariate term, e.g. `c("sex", "dose")`. Each element is turned into
# a one-sided formula (`~ <element>`) and only actually tested deep inside
# `.erglm_once_forward()`/`.erglm_once_backward()`, many steps into a
# potentially long search -- so a malformed element (e.g. `"sex + dose"`,
# which parses as two terms; or something that isn't parseable as a
# formula at all) used to only surface once the search reached it,
# aborting the whole search with no indication of which candidate was at
# fault. Validating every element up front catches this before any
# fitting happens.
.erglm_check_candidates <- function(candidates) {
  if (!is.character(candidates) || length(candidates) == 0L || anyNA(candidates)) {
    rlang::abort(paste0(
      "`candidates` must be a non-empty character vector with no missing ",
      "values, not ", .fmt_bad_value(candidates), "."
    ))
  }
  for (cc in candidates) {
    add <- tryCatch(stats::as.formula(paste("~", cc)), error = function(e) NULL)
    if (is.null(add)) {
      rlang::abort(paste0(
        "`candidates` contains an invalid entry: \"", cc, "\" could not ",
        "be parsed as a formula term."
      ))
    }
    trm_lab <- attr(stats::terms(add), "term.labels")
    if (length(trm_lab) != 1L) {
      rlang::abort(paste0(
        "`candidates` contains an invalid entry: \"", cc, "\" names ",
        length(trm_lab), " terms, not exactly one. Each element of ",
        "`candidates` must name a single covariate term (e.g. \"sex\", ",
        "not \"sex + dose\")."
      ))
    }
  }
}

# `nsim` must be a single positive whole number -- used by
# `simulate.erglm_model()`/`.erglm_resample()` and `.erglm_simulate_draws()`
# to size the loop of simulation replicates. Invalid values (0, negative,
# non-integer) otherwise surface as a cryptic "non-conformable arguments"
# error from downstream matrix code.
.erglm_check_nsim <- function(nsim) {
  if (!is.numeric(nsim) || length(nsim) != 1L || is.na(nsim) ||
      nsim < 1 || abs(nsim - round(nsim)) > .Machine$double.eps^0.5) {
    rlang::abort(paste0(
      "`nsim` must be a single positive whole number, not ",
      .fmt_bad_value(nsim), "."
    ))
  }
}

.as_erglm <- function(mod) {
  class(mod) <- c("erglm_model", class(mod)) # append class in case new methods are required
  mod$erglm <- list(type = stats::family(mod)$family) # internal "erglm" list to store erglm-specific info
  mod
}

# simple helpers ----------------------------------------------------------

#' Link and inverse-link functions for a fitted model
#'
#' Every `glm()` family already carries its link and inverse-link
#' functions (`stats::family(mod)$linkfun` / `stats::family(mod)$linkinv`),
#' but many users don't realise these are available for the taking.
#' `erglm_link()` and `erglm_invlink()` are thin, discoverable wrappers
#' around them: `erglm_link()` maps the response scale to the linear
#' predictor scale, and `erglm_invlink()` maps the linear predictor
#' scale back to the response scale.
#'
#' @param mod A fitted model, typically an `erglm_model`/`glm` object.
#' @returns A function of one numeric-vector argument.
#' @examples
#' mod <- erglm_model(ae1 ~ aucss + sex, erglm_data, family = binomial())
#' erglm_link(mod)(0.5)
#' erglm_invlink(mod)(0)
#' erglm_link(mod)(erglm_invlink(mod)(-2:2))
#' @name erglm_link
NULL

#' @export
#' @rdname erglm_link
erglm_link <- function(mod) stats::family(mod)$linkfun

#' @export
#' @rdname erglm_link
erglm_invlink <- function(mod) stats::family(mod)$linkinv
