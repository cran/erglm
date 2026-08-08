
<!-- README.md is generated from README.Rmd. Please edit that file -->

# erglm

<!-- badges: start -->

[![R-CMD-check](https://github.com/djnavarro/erglm/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/djnavarro/erglm/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/djnavarro/erglm/graph/badge.svg)](https://app.codecov.io/gh/djnavarro/erglm)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

Provides estimation tools for exposure-response models based on `glm()`.
It is mostly intended as a convenience package: the core tools are
wrappers around `glm()`, tested and supported for binomial, poisson,
gaussian, and gamma families. For plotting exposure-response models
(including those fitted with erglm), see the companion package
[erplots](https://github.com/djnavarro/erplots), which supplies a
model-agnostic mini-language for building exposure-response plots.

## Installation

You can install the development version of erglm like so:

``` r
pak::pak("djnavarro/erglm")
```

## Models

``` r
library(erglm)
library(tibble)

erglm_data
#> # A tibble: 300 × 13
#>       id sex      age weight  dose treatment aucss cmaxss   ae1   ae2 ae_count
#>    <int> <fct>  <int>  <dbl> <dbl> <fct>     <dbl>  <dbl> <dbl> <dbl>    <int>
#>  1     1 Male      35     79   200 Drug       673.   97.3     0     1        1
#>  2     2 Female    22     58   200 Drug      2806.  301.      1     1        6
#>  3     3 Female    28     58     0 Placebo      0     0       0     0        1
#>  4     4 Female    18     57   100 Drug      1169.  198.      1     1        0
#>  5     5 Male      28     77   100 Drug       377.   51.4     0     0        0
#>  6     6 Female    19     76   200 Drug       327.   25.4     1     0        0
#>  7     7 Male      30     70     0 Placebo      0     0       0     0        0
#>  8     8 Female    34     60   100 Drug      1208.  133.      1     1        1
#>  9     9 Male      21     89     0 Placebo      0     0       0     0        0
#> 10    10 Female    34     56   200 Drug       254.   31.0     0     0        1
#> # ℹ 290 more rows
#> # ℹ 2 more variables: biomarker_change <dbl>, ae_duration <dbl>

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
#> # A tibble: 4 × 11
#>   iteration attempt step       action term_tested model_tested   model_converged
#>       <int>   <int> <chr>      <chr>  <chr>       <chr>          <lgl>          
#> 1         0       0 base model <NA>   <NA>        ae1 ~ aucss +… TRUE           
#> 2         1       1 backward   remove ~dose       ae1 ~ aucss +… TRUE           
#> 3         1       2 backward   remove ~sex        ae1 ~ aucss +… TRUE           
#> 4         2       3 backward   remove ~sex        ae1 ~ aucss    TRUE           
#> # ℹ 4 more variables: term_p_value <dbl>, model_aic <dbl>, model_bic <dbl>,
#> #   model_updated <int>
```

## Simulation

``` r
mod <- erglm_model(ae1 ~ aucss + sex, erglm_data, family = binomial())
sim <- simulate(mod, nsim = 5, seed = 1234)
sim
#> # A tibble: 1,500 × 9
#>    dat_id sim_id    mu   val `coef_(Intercept)` coef_aucss coef_sexMale aucss
#>     <int>  <int> <dbl> <int>              <dbl>      <dbl>        <dbl> <dbl>
#>  1      1      1 0.894     1              -2.09    0.00601        0.179  673.
#>  2      2      1 1.000     1              -2.09    0.00601        0.179 2806.
#>  3      3      1 0.110     0              -2.09    0.00601        0.179    0 
#>  4      4      1 0.993     1              -2.09    0.00601        0.179 1169.
#>  5      5      1 0.588     1              -2.09    0.00601        0.179  377.
#>  6      6      1 0.468     1              -2.09    0.00601        0.179  327.
#>  7      7      1 0.129     0              -2.09    0.00601        0.179    0 
#>  8      8      1 0.994     1              -2.09    0.00601        0.179 1208.
#>  9      9      1 0.129     1              -2.09    0.00601        0.179    0 
#> 10     10      1 0.362     1              -2.09    0.00601        0.179  254.
#> # ℹ 1,490 more rows
#> # ℹ 1 more variable: sex <fct>
```

To visualise the simulations against the observed data (e.g. as a
VPC-style plot), see `erplots::er_vpc_plot()`, which can build its own
simulated replicates directly from a fitted `erglm_model`
(`er_vpc_plot(model = mod)`).
