
<!-- README.md is generated from README.Rmd. Please edit that file -->

# erglm

<!-- badges: start -->

[![R-CMD-check](https://github.com/djnavarro/erglm/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/djnavarro/erglm/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/djnavarro/erglm/graph/badge.svg)](https://app.codecov.io/gh/djnavarro/erglm)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/erglm)](https://CRAN.R-project.org/package=erglm)
<!-- badges: end -->

Provides estimation tools for exposure-response models based on `glm()`.
It is mostly intended as a convenience package: the core tools are
wrappers around `glm()`, tested and supported for binomial, poisson,
gaussian, and gamma families. For plotting exposure-response models
(including those fitted with erglm), see the companion package
[erplots](https://erplots.djnavarro.net), which supplies a
model-agnostic mini-language for building exposure-response plots. For
an analogous approach to time-to-event models see
[ertte](https://ertte.djnavarro.net), and for Emax regression models see
[emaxnls](https://emaxnls.djnavarro.net).

## Installation

You can install the latest CRAN release of erglm with this:

``` r
install.packages("erglm")
```

Alternatively you can install the development version of erglm like so:

``` r
pak::pak("djnavarro/erglm")
```

## Models

``` r
library(erglm)

head(erglm_data)
#>   id    sex age weight dose treatment   aucss cmaxss ae1 ae2 ae_count
#> 1  1   Male  35     79  200      Drug  673.09  97.33   0   1        1
#> 2  2 Female  22     58  200      Drug 2806.12 300.62   1   1        6
#> 3  3 Female  28     58    0   Placebo    0.00   0.00   0   0        1
#> 4  4 Female  18     57  100      Drug 1169.04 197.78   1   1        0
#> 5  5   Male  28     77  100      Drug  377.29  51.43   0   0        0
#> 6  6 Female  19     76  200      Drug  327.08  25.37   1   0        0
#>   biomarker_change ae_duration
#> 1             1.22       12.40
#> 2             4.87       13.70
#> 3            -1.83        5.26
#> 4             1.90        6.70
#> 5            -1.01        6.15
#> 6            -4.97       12.83

mod <- erglm_model(ae1 ~ aucss, erglm_data, family = binomial())
mod
#> 
#> Call:  stats::glm(formula = formula, family = family, data = data)
#> 
#> Coefficients:
#> (Intercept)        aucss  
#>   -1.791383     0.005497  
#> 
#> Degrees of Freedom: 299 Total (i.e. Null);  298 Residual
#> Null Deviance:       402.1 
#> Residual Deviance: 193.4     AIC: 197.4
```

## Stepwise covariate modelling

``` r
mod1 <- erglm_model(ae1 ~ aucss + sex + dose, erglm_data, family = binomial())
mod2 <- erglm_scm_backward(mod1, candidates = c("sex", "dose"))
erglm_scm_history(mod2)
#>   iteration attempt       step criterion action term_tested
#> 1         0       0 base model      <NA>   <NA>        <NA>
#> 2         1       1   backward   p-value remove       ~dose
#> 3         1       2   backward   p-value remove        ~sex
#> 4         2       3   backward   p-value remove        ~sex
#>               model_tested model_converged term_p_value model_aic model_bic
#> 1 ae1 ~ aucss + sex + dose            TRUE           NA  200.5606  215.3758
#> 2        ae1 ~ aucss + sex            TRUE    0.7405585  198.6703  209.7816
#> 3       ae1 ~ aucss + dose            TRUE    0.4025456  199.2613  210.3727
#> 4              ae1 ~ aucss            TRUE    0.3906289  197.4073  204.8148
#>   model_updated
#> 1            NA
#> 2             1
#> 3             0
#> 4             1
```

## Simulation

``` r
mod <- erglm_model(ae1 ~ aucss + sex, erglm_data, family = binomial())
sim <- simulate(mod, nsim = 5, seed = 1234)
head(sim)
#>   dat_id sim_id        mu val coef_(Intercept)  coef_aucss coef_sexMale   aucss
#> 1      1      1 0.8940479   1        -2.092198 0.006010498    0.1793639  673.09
#> 2      2      1 0.9999996   1        -2.092198 0.006010498    0.1793639 2806.12
#> 3      3      1 0.1098574   0        -2.092198 0.006010498    0.1793639    0.00
#> 4      4      1 0.9928560   1        -2.092198 0.006010498    0.1793639 1169.04
#> 5      5      1 0.5877972   1        -2.092198 0.006010498    0.1793639  377.29
#> 6      6      1 0.4684708   1        -2.092198 0.006010498    0.1793639  327.08
#>      sex
#> 1   Male
#> 2 Female
#> 3 Female
#> 4 Female
#> 5   Male
#> 6 Female
```

To visualise the simulations against the observed data (e.g. as a
VPC-style plot) see erplots’ `er_vpc()` mini-grammar – specifically
`erplots::er_vpc_add_simulated()`, which can build its own simulated
replicates directly from a fitted `erglm_model`
(`er_vpc_add_simulated(model = mod)`).
