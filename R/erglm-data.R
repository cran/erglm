

.make_erglm_data <- function(seed) {
  n <- 300L
  .seed_with_seed(
    seed = seed,
    code = {
      erglm_data <- data.frame(
        check.names = FALSE,
        id = 1:n,
        sex = factor(sample(rep(c("Male", "Female"), c(n/2, n/2)))),
        age = sample(18:35, size = n, replace = TRUE)
      ) |> 
        # ifelse() short-circuits and skips evaluating a branch entirely
        # when a group's condition is uniformly TRUE/FALSE (as it always
        # is here, grouped by sex) -- unlike dplyr::if_else(), which
        # always evaluates both branches. Both runif() draws are forced
        # explicitly here so the RNG stream is consumed in the same
        # order either way, keeping erglm_data reproducible under the
        # same seed.
        .verb_mutate(
          weight = {
            true_val <- (stats::runif(length(sex), .05, .95)) |>
              stats::qlnorm(meanlog = 4.284, sdlog = 0.164) |> 
              round()
            false_val <- (stats::runif(length(sex), .05, .95)) |>
              stats::qlnorm(meanlog = 4.114, sdlog = 0.164) |> 
              round()
            ifelse(sex == "Male", true_val, false_val)
          },
          .by = "sex"
        ) |> 
        .verb_mutate(
          dose = sample(rep(c(0, 100, 200), c(n/3, n/3, n/3))),
          treatment = factor(dose == 0, levels = c(TRUE, FALSE), labels = c("Placebo", "Drug")),
          aucss = (stats::runif(n, .05, .95)) |>
            stats::qlnorm() |>
            (\(x) x * (dose + 10 * weight))() |> 
            (\(x) ifelse(dose == 0, 0, x))() |> 
            round(digits = 2),
          cmaxss = (exp(log(aucss/10) + stats::rnorm(n)/3) + stats::rnorm(n)) |> 
            (\(x) ifelse(dose == 0, 0, x))() |> 
            round(digits = 2),
          ae1 = as.numeric(stats::qlogis(stats::runif(n)) < aucss/200 - 2 + 1 * as.numeric(sex=="Female")),
          ae2 = as.numeric(stats::qlogis(stats::runif(n)) < aucss/500 - 2.0)
        ) |>
        # additional non-binary responses, for demonstrating/testing
        # poisson, gaussian, and gamma families -- appended after the
        # existing columns so their random draws don't perturb ae1/ae2
        # under the same seed.
        .verb_mutate(
          ae_count = stats::rpois(
            n,
            lambda = exp(-1 + aucss / 1000 + 0.3 * as.numeric(sex == "Female"))
          ),
          biomarker_change = stats::rnorm(n, mean = -2 + aucss / 500, sd = 1.5) |>
            round(digits = 2),
          ae_duration = {
            mean_duration <- 5 + dose / 50 + aucss / 500
            stats::rgamma(n, shape = 2, rate = 2 / mean_duration)
          } |>
            round(digits = 2)
        )
    }
  )
  attr(erglm_data$id, "label") <- "Subject"
  attr(erglm_data$sex, "label") <- "Sex"
  attr(erglm_data$age, "label") <- "Age"
  attr(erglm_data$weight, "label") <- "Weight"
  attr(erglm_data$dose, "label") <- "Dose"
  attr(erglm_data$treatment, "label") <- "Treatment"
  attr(erglm_data$aucss, "label") <- "AUCss"
  attr(erglm_data$cmaxss, "label") <- "Cmax,ss"
  attr(erglm_data$ae1, "label") <- "Response 1"
  attr(erglm_data$ae2, "label") <- "Response 2"
  attr(erglm_data$ae_count, "label") <- "AE count"
  attr(erglm_data$biomarker_change, "label") <- "Biomarker change from baseline"
  attr(erglm_data$ae_duration, "label") <- "AE duration"
  return(erglm_data)
}

#erglm_data <- .make_erglm_data(seed = 2407L)
#usethis::use_data(erglm_data, overwrite = TRUE)

#' Sample simulated data for exposure-response models with covariates
#'
#' A synthetic dataset bundled with the package and used throughout its
#' documentation and examples, with response columns illustrating each of
#' erglm's supported `glm()` families.
#'
#' @name erglm_data
#' @format A data frame with columns:
#' \describe{
#' \item{id}{Identifier}
#' \item{sex}{Sex}
#' \item{age}{Age}
#' \item{weight}{Weight}
#' \item{dose}{Nominal dose, units not specified}
#' \item{treatment}{Treatment}
#' \item{aucss}{AUCss}
#' \item{cmaxss}{Cmax,ss}
#' \item{ae1}{Binary response 1 value (for binomial models)}
#' \item{ae2}{Binary response 2 value (for binomial models)}
#' \item{ae_count}{Count response (for poisson models)}
#' \item{biomarker_change}{Continuous response, can be negative (for
#' gaussian models)}
#' \item{ae_duration}{Continuous, strictly positive, right-skewed
#' response (for gamma models)}
#' }
#' @details
#' This simulated dataset is entirely synthetic. See the package source
#' for the data-generating code.
#'
#' @examples
#' head(erglm_data)
"erglm_data"

