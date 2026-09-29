# erglm 0.2.0

* `erglm_scm_forward()`/`erglm_scm_backward()` gain a `criterion`
  argument, supporting `"aic"`/`"bic"`-based term selection in addition
  to the existing `"p-value"` default. The SCM history (`erglm_scm_history()`)
  gains a `criterion` column recording which selection rule was applied
  in each forward/backward step (#7).
* The bundled `erglm_data` dataset's continuous columns (`aucss`, `cmaxss`,
  `biomarker_change`, `ae_duration`) are now rounded to two decimal places,
  a more realistic degree of precision; underlying values are otherwise
  unchanged.
* erglm no longer depends on rlang, withr, tibble, or dplyr (`mvtnorm`
  remains its only runtime dependency besides base `stats`). As a result,
  `erglm_predict()`, `simulate.erglm_model()`, and the bundled `erglm_data`
  dataset now return/are plain data frames rather than tibbles, and
  `erglm_predict()`'s `fit_link`/`se_link`/`fit_resp` columns lose the
  observation-index names they used to carry as a `predict.glm()`
  artifact; underlying values are otherwise unchanged. `tibble` has also
  been dropped from `Suggests` entirely, since the vignettes now build
  their example data with plain `data.frame()` calls too.
* Fixed several documentation gaps across help pages, vignettes, and the
  README: missing descriptions, undocumented argument defaults, and
  broken cross-references, including several references to erplots'
  `er_vpc_plot()`, which has been replaced by `er_vpc_add_simulated()`.

# erglm 0.1.1

CRAN resubmission, addressing reviewer feedback on the 0.1.0 submission:

* Self-references to 'erglm' in the `DESCRIPTION` `Description` field are
  now single-quoted, per CRAN's software-name convention.
* `DESCRIPTION` now declares `Additional_repositories:
  https://djnavarro.r-universe.dev`, the repository from which the
  optional `Suggests` dependency 'erplots' (not on CRAN) can be
  installed, per CRAN policy on declaring where to obtain such
  packages.
* `DESCRIPTION`'s `Description` field now capitalises `Poisson` and
  `Gaussian` (they name the eponymous distributions/families), per
  further reviewer feedback; `binomial` and `gamma` are left
  lowercase, as neither is an eponym.

# erglm 0.1.0

Initial CRAN release.

* Model fitting and prediction for exposure-response models based on
  `glm()` (`erglm_model()`, `erglm_predict()`), supporting arbitrary
  `glm()` families; binomial, poisson, gaussian, and gamma are tested
  and officially supported end to end (fitting, prediction, SCM
  significance testing, and simulation).
* Stepwise covariate modelling (`erglm_scm_forward()`,
  `erglm_scm_backward()`, `erglm_scm_history()`), built on the
  single-term `erglm_add_term()`/`erglm_remove_term()` helpers.
* Simulation support (`erglm_fun()`, `simulate.erglm_model()`) for
  drawing replicate responses from a fitted model, with sampled
  coefficients and both expected and simulated response columns.
* Interoperability with the companion
  [erplots](https://github.com/djnavarro/erplots) package via
  `er_predict()`/`er_simulate()`/`er_summary()` methods, registered
  lazily so erglm has no hard dependency on erplots or on any plotting
  package.
* `erglm_link()`/`erglm_invlink()`, discoverable wrappers around a
  fitted model's link and inverse-link functions.
* An example dataset, `erglm_data`, with binary, count, and
  continuous/right-skewed continuous response columns for
  demonstrating each supported family.
