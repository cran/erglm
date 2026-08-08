# simulate() generic ------------------------------------------------------

#' Simulate responses from an exposure-response model
#'
#' Generates simulated response datasets from a fitted erglm model,
#' propagating uncertainty in the parameter estimates. Useful for
#' simulation-based confidence bands, predictive checks, or bootstrapping
#' downstream analyses. Implements the standard `stats::simulate()`
#' generic, so it is called as `simulate(object, ...)` rather than through
#' an erglm-specific function name.
#'
#' @param object An erglm model, as returned by [erglm_model()]
#' @param nsim Number of replicates. Must be a single positive whole
#' number.
#' @param seed Used to set the RNG seed. If `NULL`, a random seed is
#' chosen and reported.
#' @param ... Ignored
#'
#' @details Samples new parameter values from the multivariate normal
#' distribution implied by the model's variance-covariance matrix (via
#' `mvtnorm::rmvnorm()`), evaluates the expected response at each sampled
#' parameter vector using [erglm_fun()], then draws a simulated
#' response at each prediction using family-appropriate residual noise
#' (the same `.erglm_draw_response()` mechanism used elsewhere in the
#' package: Bernoulli draws for `binomial`, Poisson draws for
#' `poisson`, normal draws for `gaussian`, gamma draws for `Gamma`). The
#' dispersion parameter used for that noise is a single point estimate
#' (`summary(object)$dispersion`), not resampled per replicate. Other
#' `glm()` families are not currently supported and will raise an
#' informative error.
#'
#' For a VPC-style plot comparing observed and simulated response rates,
#' see the companion `erplots` package's `er_vpc_plot()`, which can build
#' its simulated replicates directly from a fitted model (`model =`
#' argument) via the same `.erglm_draw_response()` noise mechanism this
#' method uses, without needing to call `simulate()` yourself.
#'
#' @returns A tibble with one row per observation per simulated
#' replicate, containing:
#' - `dat_id`, `sim_id`: identifiers for the original observation and
#'   the simulation replicate
#' - `mu`: the expected response (response scale) at the sampled
#'   parameter vector
#' - `val`: the simulated response value (`mu` plus family-appropriate
#'   noise)
#' - one `coef_*` column per model coefficient (e.g. `` coef_`(Intercept)` ``,
#'   `coef_aucss`), giving the sampled parameter values used for that
#'   replicate -- prefixed to avoid colliding with predictor columns of
#'   the same name
#' - the model's predictor columns (not including the response)
#'
#' @exportS3Method stats::simulate
#' @examples
#' mod <- erglm_model(ae1 ~ aucss + sex, erglm_data, family = binomial())
#' simulate(mod, nsim = 5, seed = 963)
#'
simulate.erglm_model <- function(object, nsim = 1, seed = NULL, ...) {
  .erglm_resample(object, nsim = nsim, seed = seed)
}

# Shared resampling engine behind `simulate.erglm_model()`. Distinct from
# `.erglm_simulate_draws()` (used only by the erplots `er_simulate()`
# method for spaghetti-style plots), which samples parameters/predictions
# but leaves adding response noise to its caller: this helper does both
# in a single call, and also returns the sampled coefficients, mirroring
# emaxnls's `simulate()` output shape.
.erglm_resample <- function(mod, nsim, seed = NULL) {
  .erglm_check_nsim(nsim)
  if (is.null(seed)) {
    seed <- .pick_seed()
    rlang::inform(paste0("Using seed = ", seed, ". Pass `seed = ", seed, "` to reproduce this result."))
  }

  family_name <- stats::family(mod)$family
  dispersion <- summary(mod)$dispersion

  est <- stats::coef(mod)
  lbl <- names(est)
  nr <- nrow(mod$data)

  vv <- all.vars(mod$formula)
  rsp_var <- vv[1]
  predictors <- setdiff(vv, rsp_var)
  dat <- mod$data[, predictors, drop = FALSE]
  dat$dat_id <- seq_len(nr)

  fn <- erglm_fun(mod)

  withr::with_seed(
    seed = seed,
    code = {
      par <- mvtnorm::rmvnorm(n = nsim, mean = est, sigma = stats::vcov(mod))
      colnames(par) <- lbl

      sim <- vector("list", nsim)
      for (ss in seq_len(nsim)) {
        mu_ss <- fn(param = par[ss, ], data = mod$data, type = "response")
        sim[[ss]] <- tibble::tibble(
          dat_id = seq_len(nr),
          sim_id = ss,
          mu = mu_ss,
          val = .erglm_draw_response(family_name, fit = mu_ss, dispersion = dispersion)
        )
      }
    }
  )

  sim <- dplyr::bind_rows(sim)
  par <- tibble::as_tibble(par)
  # prefix coefficient columns so they can't collide with predictor
  # columns of the same name once joined onto `dat` below
  names(par) <- paste0("coef_", names(par))
  par$sim_id <- seq_len(nsim)

  out <- dplyr::left_join(sim, par, by = "sim_id")
  out <- dplyr::left_join(out, dat, by = "dat_id")
  tibble::as_tibble(out)
}
