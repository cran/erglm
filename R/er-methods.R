
# Interoperability with erplots ------------------------------------------
#
# erglm has no hard dependency on erplots (a modelling package shouldn't need
# to pull in ggplot2/patchwork). But if erplots *is* installed and loaded,
# erglm's model objects should work seamlessly with erplots' model-agnostic
# plotting API (`er_plot_show_model()`, `er_vpc_plot()`, etc.), which relies
# on the `er_predict()`/`er_simulate()`/`er_summary()` generics defined in
# erplots (see `erplots::er_model_interface`).
#
# These methods are registered lazily at load time (via `.onLoad()` below),
# so that neither erplots nor its dependencies need to be installed for
# erglm's modelling functions to work standalone.

er_predict.erglm_model <- function(model, newdata, conf_level = 0.95, ...) {
  erglm_predict(object = model, newdata = newdata, conf_level = conf_level)
}

er_simulate.erglm_model <- function(model, newdata, nsim = 100, seed = NULL, ...) {
  .erglm_simulate_draws(object = model, newdata = newdata, nsim = nsim, seed = seed)
}

er_summary.erglm_model <- function(model, conf_level = 0.95, ...) {
  coefs <- summary(model)$coefficients
  if (nrow(coefs) < 2) return(NULL)
  # column is "Pr(>|z|)" for families with known dispersion (binomial,
  # poisson) but "Pr(>|t|)" for families with an estimated dispersion
  # parameter (gaussian, gamma, inverse.gaussian, quasi*) -- match by
  # pattern rather than exact name.
  p_col <- grep("^Pr\\(", colnames(coefs))[1]

  # Wald intervals, matching erglm_predict()'s approach (a normal-quantile
  # z-score applied to the standard error) rather than profile likelihood --
  # cheap, and consistent with the rest of the package.
  z_scale <- -stats::qnorm((1 - conf_level) / 2)
  estimate <- unname(coefs[, 1])
  std_error <- unname(coefs[, 2])

  coefficients <- tibble::tibble(
    term = rownames(coefs),
    estimate = estimate,
    std_error = std_error,
    statistic = unname(coefs[, 3]),
    p_value = unname(coefs[, p_col]),
    conf_low = estimate - z_scale * std_error,
    conf_high = estimate + z_scale * std_error,
  )

  # r_squared is only meaningful for the classic OLS case (gaussian family,
  # identity link) -- 1 - deviance/null.deviance then coincides with the
  # usual R^2. Left NA for every other family/link combination.
  fam <- stats::family(model)
  r_squared <- if (fam$family == "gaussian" && fam$link == "identity") {
    1 - model$deviance / model$null.deviance
  } else {
    NA_real_
  }

  glance <- tibble::tibble(
    n = stats::nobs(model),
    df_residual = model$df.residual,
    logLik = as.numeric(stats::logLik(model)),
    aic = stats::AIC(model),
    bic = stats::BIC(model),
    deviance = model$deviance,
    r_squared = r_squared,
    converged = model$converged,
  )

  list(
    p_value = unname(coefs[2, p_col]),
    coefficients = coefficients,
    glance = glance
  )
}

.onLoad <- function(libname, pkgname) {
  .s3_register("erplots::er_predict", "erglm_model", er_predict.erglm_model)
  .s3_register("erplots::er_simulate", "erglm_model", er_simulate.erglm_model)
  .s3_register("erplots::er_summary", "erglm_model", er_summary.erglm_model)
}

# Registers `method` as an S3 method for `generic` (given as
# "package::generic") and `class`, without requiring `package` to be
# installed or loaded. If `package` isn't loaded yet, registration is
# deferred until it is (via a load hook). This is the standard pattern used
# across the tidyverse for optional cross-package S3 methods (e.g. as
# implemented in `vctrs::s3_register()`); vendored here to avoid adding a
# dependency for a single small helper.
.s3_register <- function(generic, class, method) {
  pieces <- strsplit(generic, "::")[[1]]
  package <- pieces[[1]]
  generic <- pieces[[2]]

  register <- function(...) {
    envir <- asNamespace(package)
    registerS3method(generic, class, method, envir = envir)
  }

  if (isNamespaceLoaded(package)) {
    register()
  }
  setHook(packageEvent(package, "onLoad"), function(...) register())

  invisible()
}
