
# Family-dispatch helpers shared across SCM (R/erglm-scm.R) and response
# simulation (R/erglm-simulate.R). Officially tested/supported for
# binomial, poisson, gaussian, and gamma; other glm() families work
# through the same generic mechanisms elsewhere in the package
# (erglm_predict(), erglm_fun()) but are not covered by these helpers.

.erglm_supported_response_families <- c("binomial", "poisson", "gaussian", "Gamma")

# Picks the appropriate `stats::anova(..., test = )` flavour for a given
# glm family: a likelihood-ratio chi-squared test is appropriate for
# families with known dispersion (binomial, poisson); an F-test is the
# standard choice for families with an estimated dispersion parameter
# (gaussian, gamma, inverse.gaussian, quasi*).
.erglm_default_test <- function(family_name) {
  if (family_name %in% c("binomial", "poisson")) return("Chisq")
  "F"
}

.erglm_resolve_test <- function(test, family_name) {
  test <- match.arg(test, c("auto", "Chisq", "F"))
  if (test == "auto") return(.erglm_default_test(family_name))
  test
}

# Draws a simulated response value for each element of `fit` (the
# expected response under some sampled parameter vector), incorporating
# family-appropriate residual/dispersion noise. `dispersion` is a single
# point estimate (e.g. from `summary(model)$dispersion`) applied to every
# draw -- parameter uncertainty is already reflected in `fit` varying
# across replicates, but dispersion uncertainty itself is not resampled.
# Used by `simulate.erglm_model()` and `.erglm_simulate_draws()`
# (the latter powering erplots' `er_simulate()` method).
.erglm_draw_response <- function(family_name, fit, dispersion) {
  n <- length(fit)
  switch(
    family_name,
    binomial = stats::rbinom(n, size = 1, prob = fit),
    poisson = stats::rpois(n, lambda = fit),
    gaussian = stats::rnorm(n, mean = fit, sd = sqrt(dispersion)),
    Gamma = stats::rgamma(n, shape = 1 / dispersion, rate = 1 / (dispersion * fit)),
    rlang::abort(
      paste0(
        "erglm does not support simulating responses for family \"",
        family_name, "\" (via simulate() or er_simulate()). ",
        "Supported families are: ",
        paste(.erglm_supported_response_families, collapse = ", "), "."
      )
    )
  )
}
