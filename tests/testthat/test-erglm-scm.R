test_that("erglm_add_term works", {
  mod1 <- erglm_model(ae1 ~ aucss, erglm_data, family = binomial())
  expect_no_error(erglm_add_term(mod1, ~sex, quiet = TRUE))
  mod2 <- erglm_add_term(mod1, ~sex, quiet = TRUE)
  expect_equal(deparse(mod2$formula), "ae1 ~ aucss + sex")
  expect_equal(length(coef(mod2)), length(coef(mod1)) + 1L)
})

test_that("erglm_remove_term works", {
  mod2 <- erglm_model(ae1 ~ aucss + sex, erglm_data, family = binomial())
  expect_no_error(erglm_remove_term(mod2, ~sex, quiet = TRUE))
  mod1 <- erglm_remove_term(mod2, ~sex, quiet = TRUE)
  expect_equal(deparse(mod1$formula), "ae1 ~ aucss")
  expect_equal(length(coef(mod2)), length(coef(mod1)) + 1L)
})

test_that("erglm_add_term warns (unless quiet) when the term already exists", {
  mod1 <- erglm_model(ae1 ~ aucss + sex, erglm_data, family = binomial())
  expect_warning(erglm_add_term(mod1, ~sex), "already exists")
  expect_no_warning(mod2 <- erglm_add_term(mod1, ~sex, quiet = TRUE))
  expect_equal(deparse(mod2$formula), deparse(mod1$formula))
})

test_that("erglm_remove_term warns (unless quiet) when the term isn't in the model", {
  mod1 <- erglm_model(ae1 ~ aucss, erglm_data, family = binomial())
  expect_warning(erglm_remove_term(mod1, ~sex), "does not exist")
  expect_no_warning(mod2 <- erglm_remove_term(mod1, ~sex, quiet = TRUE))
  expect_equal(deparse(mod2$formula), deparse(mod1$formula))
})

test_that("erglm_add_term errors informatively for NULL, non-formula, two-sided, or multi-term term", {
  mod1 <- erglm_model(ae1 ~ aucss, erglm_data, family = binomial())
  expect_error(erglm_add_term(mod1, NULL), "term")
  expect_error(erglm_add_term(mod1, "sex"), "term")
  expect_error(erglm_add_term(mod1, ae1 ~ sex), "two-sided")
  expect_error(erglm_add_term(mod1, ~ weight + age), "exactly one")
  expect_error(erglm_add_term(mod1, ~1), "exactly one")
})

test_that("erglm_remove_term errors informatively for NULL, non-formula, two-sided, or multi-term term", {
  mod2 <- erglm_model(ae1 ~ aucss + sex + weight, erglm_data, family = binomial())
  expect_error(erglm_remove_term(mod2, NULL), "term")
  expect_error(erglm_remove_term(mod2, "sex"), "term")
  expect_error(erglm_remove_term(mod2, ae1 ~ sex), "two-sided")
  expect_error(erglm_remove_term(mod2, ~ sex + weight), "exactly one")
  expect_error(erglm_remove_term(mod2, ~1), "exactly one")
})

test_that("erglm_scm_history works when no scm called", {
  mod1 <- erglm_model(ae1 ~ aucss, erglm_data, family = binomial())
  expect_no_error(erglm_scm_history(mod1))
  hh <- erglm_scm_history(mod1)
  expect_s3_class(hh, "data.frame")
  expect_equal(nrow(hh), 1L)
  expect_named(hh, c(
    "iteration", "attempt", "step", "action", "term_tested", "model_tested",
    "model_converged", "term_p_value", "model_aic", "model_bic", "model_updated"
  ))
  expect_equal(hh$iteration, 0L)
})

test_that(".erglm_once_forward works", {
  mod1 <- erglm_model(ae1 ~ aucss, erglm_data, family = binomial())
  expect_no_error(.erglm_once_forward(mod1, candidates = c("sex", "dose"), threshold = .01, test = "auto"))
  mod2 <- .erglm_once_forward(mod1, candidates = c("sex", "dose"), threshold = .01, test = "auto")
  hh1 <- erglm_scm_history(mod1)
  hh2 <- erglm_scm_history(mod2)
  expect_equal(nrow(hh1) + 2L, nrow(hh2))
})

test_that(".erglm_once_backward works", {
  mod1 <- erglm_model(ae1 ~ aucss + sex + dose, erglm_data, family = binomial())
  expect_no_error(.erglm_once_backward(mod1, candidates = c("sex", "dose"), threshold = .001, test = "auto"))
  mod2 <- .erglm_once_backward(mod1, candidates = c("sex", "dose"), threshold = .001, test = "auto")
  hh1 <- erglm_scm_history(mod1)
  hh2 <- erglm_scm_history(mod2)
  expect_equal(nrow(hh1) + 2L, nrow(hh2))
})

test_that("erglm_scm_forward works", {
  mod1 <- erglm_model(ae1 ~ aucss, erglm_data, family = binomial())
  expect_no_error(erglm_scm_forward(mod1, candidates = c("sex", "dose"), threshold = .01))
  mod2 <- erglm_scm_forward(mod1, candidates = c("sex", "dose"), threshold = .01)
  hh1 <- erglm_scm_history(mod1)
  hh2 <- erglm_scm_history(mod2)
  expect_equal(nrow(hh1) + 2L, nrow(hh2)) 
  expect_equal(max(hh2$iteration), 1L)
})

test_that("erglm_scm_backward works", {
  mod1 <- erglm_model(ae1 ~ aucss + sex + dose, erglm_data, family = binomial())
  expect_no_error(erglm_scm_backward(mod1, candidates = c("sex", "dose"), threshold = .001))
  mod2 <- erglm_scm_backward(mod1, candidates = c("sex", "dose"), threshold = .001)
  hh1 <- erglm_scm_history(mod1)
  hh2 <- erglm_scm_history(mod2)
  expect_equal(nrow(hh1) + 3L, nrow(hh2))
  expect_equal(max(hh2$iteration), 2L)
})

test_that(".erglm_default_test picks Chisq for known-dispersion families and F otherwise", {
  expect_equal(.erglm_default_test("binomial"), "Chisq")
  expect_equal(.erglm_default_test("poisson"), "Chisq")
  expect_equal(.erglm_default_test("gaussian"), "F")
  expect_equal(.erglm_default_test("Gamma"), "F")
  expect_equal(.erglm_default_test("inverse.gaussian"), "F")
})

test_that(".erglm_anova_p auto-selects the test based on family and reads the p-value generically", {
  mod1 <- erglm_model(ae_count ~ aucss, erglm_data, family = poisson())
  mod2 <- erglm_model(ae_count ~ aucss + sex, erglm_data, family = poisson())
  p_auto <- .erglm_anova_p(mod1, mod2, test = "auto")
  p_chisq <- .erglm_anova_p(mod1, mod2, test = "Chisq")
  expect_equal(p_auto, p_chisq)

  mod3 <- erglm_model(biomarker_change ~ aucss, erglm_data, family = gaussian())
  mod4 <- erglm_model(biomarker_change ~ aucss + sex, erglm_data, family = gaussian())
  p_auto_gauss <- .erglm_anova_p(mod3, mod4, test = "auto")
  p_f_gauss <- .erglm_anova_p(mod3, mod4, test = "F")
  expect_equal(p_auto_gauss, p_f_gauss)
  expect_false(isTRUE(all.equal(p_auto_gauss, .erglm_anova_p(mod3, mod4, test = "Chisq"))))
})

test_that("erglm_scm_forward works for a gaussian model with the default auto test", {
  mod1 <- erglm_model(biomarker_change ~ aucss, erglm_data, family = gaussian())
  expect_no_error(erglm_scm_forward(mod1, candidates = c("sex", "dose"), threshold = .01))
  mod2 <- erglm_scm_forward(mod1, candidates = c("sex", "dose"), threshold = .01)
  hh2 <- erglm_scm_history(mod2)
  expect_true(all(family(mod2)$family == "gaussian"))
  expect_true(nrow(hh2) > 1L)
})

test_that("erglm_scm_forward respects an explicit test override", {
  mod1 <- erglm_model(ae1 ~ aucss, erglm_data, family = binomial())
  mod_auto <- erglm_scm_forward(mod1, candidates = c("sex", "dose"), threshold = .01, seed = 5544, test = "auto")
  mod_chisq <- erglm_scm_forward(mod1, candidates = c("sex", "dose"), threshold = .01, seed = 5544, test = "Chisq")
  expect_equal(deparse(mod_auto$formula), deparse(mod_chisq$formula))
})

test_that("erglm_scm_forward is seed-invariant on non-tied data", {
  mod1 <- erglm_model(ae1 ~ aucss, erglm_data, family = binomial())
  seeds <- c(101L, 2202L, 33033L, 4004L, 55055L)
  mods <- lapply(seeds, function(s) {
    erglm_scm_forward(mod1, candidates = c("sex", "dose"), threshold = .01, seed = s)
  })
  formulas <- vapply(mods, function(m) deparse(m$formula), character(1))
  expect_length(unique(formulas), 1L)
  aics <- vapply(mods, stats::AIC, numeric(1))
  expect_equal(aics, rep(aics[1], length(aics)))
})

test_that("erglm_scm_backward is seed-invariant on non-tied data", {
  mod1 <- erglm_model(ae1 ~ aucss + sex + dose, erglm_data, family = binomial())
  seeds <- c(101L, 2202L, 33033L, 4004L, 55055L)
  mods <- lapply(seeds, function(s) {
    erglm_scm_backward(mod1, candidates = c("sex", "dose"), threshold = .001, seed = s)
  })
  formulas <- vapply(mods, function(m) deparse(m$formula), character(1))
  expect_length(unique(formulas), 1L)
  aics <- vapply(mods, stats::AIC, numeric(1))
  expect_equal(aics, rep(aics[1], length(aics)))
})

test_that("erglm_add_term and erglm_remove_term preserve the model's family", {
  mod1 <- erglm_model(ae_count ~ aucss, erglm_data, family = poisson())
  mod2 <- erglm_add_term(mod1, ~sex, quiet = TRUE)
  expect_equal(family(mod2)$family, "poisson")

  mod3 <- erglm_model(ae_count ~ aucss + sex, erglm_data, family = poisson())
  mod4 <- erglm_remove_term(mod3, ~sex, quiet = TRUE)
  expect_equal(family(mod4)$family, "poisson")
})

test_that("erglm_scm_forward skips (with a warning) a candidate aliased with an existing term", {
  dat <- erglm_data
  dat$aucss2 <- dat$aucss # perfectly collinear with aucss, already in the model
  mod1 <- erglm_model(biomarker_change ~ aucss, dat)

  # multiple warnings can fire (one per forward step that re-tests the
  # aliased candidate) -- capture all of them rather than relying on
  # expect_warning(), which only matches the first
  warnings_seen <- character()
  mod2 <- withCallingHandlers(
    erglm_scm_forward(mod1, candidates = c("aucss2", "sex"), threshold = 1, seed = 909),
    warning = function(w) {
      warnings_seen <<- c(warnings_seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("aliased", warnings_seen)))
  # the aliased candidate is never selected, but a genuine candidate still can be
  expect_false("aucss2" %in% attr(stats::terms(mod2), "term.labels"))
  expect_true("sex" %in% attr(stats::terms(mod2), "term.labels"))
})

test_that("erglm_scm_backward skips (with a warning) a candidate aliased with another model term", {
  dat <- erglm_data
  dat$aucss2 <- dat$aucss # perfectly collinear with aucss
  mod1 <- erglm_model(biomarker_change ~ aucss + aucss2 + sex, dat)

  # removing either half of an aliased pair leaves the fit unchanged (the
  # other half absorbs the same information), so both give a Df = 0, NA
  # p-value comparison -- both should warn and be skipped rather than crash
  warnings_seen <- character()
  mod2 <- withCallingHandlers(
    erglm_scm_backward(mod1, candidates = c("aucss", "aucss2", "sex"), threshold = 1, seed = 909),
    warning = function(w) {
      warnings_seen <<- c(warnings_seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(sum(grepl("aliased", warnings_seen)) >= 2L)
  expect_equal(deparse(mod2$formula), deparse(mod1$formula))
})

test_that("erglm_scm_forward/backward validate candidates up front", {
  mod1 <- erglm_model(ae1 ~ aucss, erglm_data, family = binomial())
  mod2 <- erglm_model(ae1 ~ aucss + sex + dose, erglm_data, family = binomial())

  # multi-term entry
  expect_error(erglm_scm_forward(mod1, candidates = c("sex + dose", "weight")), "sex \\+ dose")
  expect_error(erglm_scm_backward(mod2, candidates = c("sex + dose", "weight")), "sex \\+ dose")

  # zero-term entry
  expect_error(erglm_scm_forward(mod1, candidates = c("1", "sex")), "1")

  # unparseable entry
  expect_error(erglm_scm_forward(mod1, candidates = c("not a formula!", "sex")), "not a formula!")

  # not a character vector, empty, or containing NA
  expect_error(erglm_scm_forward(mod1, candidates = NULL), "candidates")
  expect_error(erglm_scm_forward(mod1, candidates = character(0)), "candidates")
  expect_error(erglm_scm_forward(mod1, candidates = c("sex", NA)), "candidates")
  expect_error(erglm_scm_forward(mod1, candidates = list("sex")), "candidates")

  # valid candidates still work, unaffected
  expect_no_error(erglm_scm_forward(mod1, candidates = c("sex", "dose"), seed = 111))
})
