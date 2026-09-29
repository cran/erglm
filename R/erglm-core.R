

#' Fit an exposure-response model based on `glm()`
#'
#' A thin wrapper around `stats::glm()` that fits the model and tags the
#' result with an extra `erglm_model` class, so downstream erglm functions
#' (and the optional erplots interoperability layer) can recognise it.
#'
#' @param formula Model formula
#' @param data Data set
#' @param family The error distribution and link function to use, as for
#' `stats::glm()`. Defaults to `stats::gaussian()`, matching `stats::glm()`'s
#' own default. Tested and officially supported for `binomial()`,
#' `poisson()`, `gaussian()`, and `Gamma()`; other `glm()` families should
#' work through the same generic mechanisms but are untested.
#' @param ... Other arguments passed to `glm()`. Note that `weights`,
#' `subset`, and `offset` don't work reliably here -- see Details below.
#' @returns A glm object
#'
#' @details The returned object has class `c("erglm_model", "glm", "lm")`:
#' it *is* a `glm` object, with a little extra metadata attached. This
#' means all of the usual `glm`/`lm` methods work unchanged, without
#' needing an erglm-specific equivalent -- e.g. `summary()`, `coef()`,
#' `vcov()`, `confint()`, `predict()`, `AIC()`, `BIC()`, `logLik()`, and
#' `anova()` for comparing nested models. See `vignette("methods",
#' package = "erglm")` for worked examples of these. [erglm_predict()]
#' is a separate, erglm-specific alternative to `predict()` that
#' returns confidence intervals on the response scale in a tidy data
#' frame; the two are complementary, not competing.
#'
#' `weights`, `subset`, and `offset` can't currently be passed through
#' `...` to `stats::glm()`: `glm()` resolves these non-standard-evaluation
#' arguments via `match.call()`, which breaks once they've been forwarded
#' through another function's `...` rather than named directly in the
#' call `glm()` itself sees. This reproduces with a trivial wrapper
#' (`function(formula, data, family, ...) glm(formula, data, family,
#' ...)`) and is a limitation of `glm()`/`lm()`'s NSE, not something
#' specific to erglm's `family` generalisation -- see e.g. the "Note"
#' in `?lm` about wrapping `lm()`. Attempting it currently fails with a
#' low-level error (`"..1 used in an incorrect context, no ... to look
#' in"`). Two workarounds: fold an offset into the formula itself (e.g.
#' `y ~ x + offset(z)`) rather than passing `offset =`, and pre-filter
#' `data` yourself rather than passing `subset =`. There's no similar
#' formula-level workaround for `weights`; call `stats::glm(formula,
#' data, family, weights = ...)` directly instead, then (if you want
#' the `erglm_model` class for consistency, e.g. for `simulate()`'s S3
#' dispatch) run `class(mod) <- c("erglm_model", class(mod))` on the
#' result -- every other erglm function works on a plain `glm` object
#' just as well, since none of them require the class specifically.
#' @export
#' @examples
#' mod <- erglm_model(ae1 ~ aucss, erglm_data, family = binomial())
#' mod
#'
#' # other glm() families are also supported
#' mod_pois <- erglm_model(ae_count ~ aucss, erglm_data, family = poisson())
#' mod_pois
#'
erglm_model <- function(formula, data, family = stats::gaussian(), ...) {
  mod <- stats::glm(formula = formula, data = data, family = family, ...)
  .as_erglm(mod)
}

# extract model predictions and confidence intervals for a new data set.
# should work for any glm, not just logistic. adapted from:
# https://fromthebottomoftheheap.net/2018/12/10/confidence-intervals-for-glms/

#' Predictions and confidence intervals for exposure-response models
#'
#' Computes model-based predictions and confidence intervals on the
#' response scale, returned as a tidy data frame bound to `newdata`.
#'
#' @param object An erglm model, as returned by [erglm_model()]
#' @param newdata Data frame containing cases to be predicted. Defaults to
#' `NULL`, in which case the data the model was originally fitted to
#' (`object$data`) is used.
#' @param conf_level Confidence level for the intervals. Defaults to `0.95`.
#' @returns A data frame
#'
#' @details Computes intervals on the link scale and back-transforms with
#' `stats::family(object)$linkinv`, so this works for any `glm()` family,
#' not just binomial/logistic models. See also [erglm_fun()] for
#' generating predictions at arbitrary (possibly counterfactual)
#' parameters or data. `conf_level` must be a single number between 0
#' and 1 (inclusive); other values error rather than silently producing
#' a reversed or `NaN` interval.
#'
#' This is a tidy, opinionated alternative to calling base R's
#' `predict()` directly on `object` -- since `object` is a genuine
#' `glm` object, `predict()` (and `predict(object, se.fit = TRUE)`, on
#' which this function is based) work unchanged and remain useful for
#' quick point estimates or when a tidy data frame isn't needed. See
#' `vignette("methods", package = "erglm")` for a side-by-side
#' comparison and other inherited `glm`/`lm` methods (`summary()`,
#' `vcov()`, `AIC()`, etc.).
#'
#' @export
#' @examples
#' mod <- erglm_model(ae1 ~ aucss, erglm_data, family = binomial())
#' prd <- erglm_predict(mod, erglm_data)
#' head(prd)
#'
#' mod_gauss <- erglm_model(biomarker_change ~ aucss, erglm_data, family = gaussian())
#' prd_gauss <- erglm_predict(mod_gauss, erglm_data)
#' head(prd_gauss)
#' 
erglm_predict <- function(object, newdata = NULL, conf_level = .95) {
  .erglm_check_conf_level(conf_level)
  if (is.null(newdata)) newdata <- object$data
  inverse_link <- stats::family(object)$linkinv
  z_scale <- -stats::qnorm((1 - conf_level)/2)
  pred <- stats::predict(object, newdata, se.fit = TRUE, type = "link")[1:2]
  out <- newdata
  # plain data frame column assignment strips a vector's own `names()`
  # (an artifact of predict()'s row-indexed names, e.g. "1", "2", ...),
  # unlike dplyr::bind_cols() -- see NEWS.md.
  out$fit_link <- unname(pred$fit)
  out$se_link <- unname(pred$se.fit)
  out <- .verb_mutate(out,
    fit_resp = inverse_link(fit_link),
    ci_lower = inverse_link(fit_link - (z_scale * se_link)),
    ci_upper = inverse_link(fit_link + (z_scale * se_link))
  )
  return(out)
}

#' Prediction function for an exposure-response model
#'
#' Takes a fitted glm object as input and returns a function that
#' evaluates the underlying structural model at user-specified parameters
#' or data (e.g., for VPCs or other counterfactual simulation scenarios).
#'
#' @param object An erglm model, as returned by [erglm_model()]
#'
#' @returns A function with arguments `param`, `data`, and `type`.
#' - The `param` argument should be a vector of coefficients; defaults
#'   to `coef(object)` (the fitted coefficients) if not supplied.
#' - The `data` argument should be a data frame or tibble; defaults to
#'   `object$data` (the data the model was fitted to) if not supplied.
#' - The `type` argument should be a string indicating the type
#'   of prediction to generate (defaults to `"response"`)
#'
#' @details Uses `stats::family(object)$linkinv`, so this works for any
#' `glm()` family, not just binomial/logistic models; tested for
#' binomial, poisson, gaussian, and gamma families. Named `erglm_fun()`
#' for consistency with the companion `emaxnls` package's
#' [emaxnls::emax_fun()], which serves the same purpose for
#' `emaxnls`/`emaxlogistic` models. The returned function checks that
#' `param` is numeric and has one entry per column of the model matrix
#' implied by `data`, erroring informatively rather than failing with a
#' cryptic "non-conformable arguments" error from matrix multiplication.
#'
#' @examples
#' mod1 <- erglm_model(ae2 ~ aucss + sex, erglm_data, family = binomial())
#' mod1_fun <- erglm_fun(mod1)
#' 
#' # no arguments: reproduces the fitted model's own predictions
#' p1 <- mod1_fun()
#' p2 <- unname(predict(mod1, type = "response")) # same result
#' 
#' # user modifies the data set
#' erglm_data2 <- erglm_data[1:20, ]
#' p3 <- mod1_fun(data = erglm_data2) 
#' p4 <- unname(predict(mod1, newdata = erglm_data2, type = "response")) # same result
#' 
#' # user modifies the parameters
#' par2 <- coef(mod1)
#' int1 <- par2["(Intercept)"]
#' par2["(Intercept)"] <- 0
#' p5 <- mod1_fun(param = par2)
#' 
#' @export
#' 
erglm_fun <- function(object) {
  ff <- stats::delete.response(stats::terms(object$formula))
  force(ff)
  function(param = NULL, data = NULL, type = "response") {
    if (is.null(param)) param <- stats::coef(object)
    if (is.null(data)) data <- object$data
    mm <- stats::model.matrix(ff, data)
    if (!is.numeric(param) || length(param) != ncol(mm)) {
      .cond_abort(paste0(
        "`param` must be a numeric vector of length ", ncol(mm),
        " (one entry per column of the model matrix: ",
        paste(colnames(mm), collapse = ", "), "), not length ",
        length(param), "."
      ))
    }
    pred <- as.vector(mm %*% param)
    if (type == "response") pred <- stats::family(object)$linkinv(pred)
    return(pred)
  }
}

# shared helper: draws `nsim` sets of coefficients from the sampling
# distribution implied by the model's variance-covariance matrix, and
# evaluates the linear predictor at each draw for the supplied `newdata`.
# Used directly by the `er_simulate.erglm_model()` method (used by
# erplots, if installed, for both spaghetti-style uncertainty bands via
# `fit_resp`, and for
# `er_vpc_add_simulated(model = ...)` via `sim_resp` -- see `?er_model_interface`
# in erplots for the distinction between the two columns). `sim_resp` adds
# family-appropriate residual/dispersion noise on top of `fit_resp`, via
# the same `.erglm_draw_response()` helper `.erglm_resample()` itself
# uses, so both simulation entry points share one noise model.
.erglm_simulate_draws <- function(object, newdata, nsim = 100, seed = NULL) {
  .erglm_check_nsim(nsim)
  if (is.null(seed)) {
    seed <- .pick_seed()
    .cond_inform(paste0("Using seed = ", seed, ". Pass `seed = ", seed, "` to reproduce this result."))
  }
  fn <- erglm_fun(object)
  family_name <- stats::family(object)$family
  dispersion <- summary(object)$dispersion
  .seed_with_seed(
    seed = seed,
    code = {
      par <- mvtnorm::rmvnorm(
        n = nsim,
        mean = stats::coef(object),
        sigma = stats::vcov(object)
      )
      sim <- list()
      for (ii in seq_len(nsim)) {
        dd_sim <- newdata
        dd_sim$row_id <- seq_len(nrow(newdata))
        dd_sim$sim_id <- ii
        dd_sim$fit_resp <- fn(param = par[ii, ], dd_sim)
        dd_sim$sim_resp <- .erglm_draw_response(family_name, fit = dd_sim$fit_resp, dispersion = dispersion)
        sim[[ii]] <- dd_sim
      }
    }
  )
  do.call(rbind, sim)
}

