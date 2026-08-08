test_that("simulate.erglm_model works and has the expected shape", {
  mod <- erglm_model(ae1 ~ aucss + sex, erglm_data, family = binomial())
  expect_no_error(simulate(mod, nsim = 2, seed = 111))
  sim <- simulate(mod, nsim = 2, seed = 111)
  expect_s3_class(sim, "data.frame")
  expect_true(all(c("dat_id", "sim_id", "mu", "val", "aucss", "sex") %in% names(sim)))
  expect_true(all(paste0("coef_", names(stats::coef(mod))) %in% names(sim)))
  expect_equal(nrow(sim), nrow(erglm_data) * 2)
})

test_that("simulate.erglm_model is reproducible given a seed", {
  mod <- erglm_model(ae1 ~ aucss + sex, erglm_data, family = binomial())
  sim1 <- simulate(mod, nsim = 5, seed = 222)
  sim2 <- simulate(mod, nsim = 5, seed = 222)
  expect_equal(sim1, sim2)
})

test_that("simulate.erglm_model draws binary 0/1 values for binomial models", {
  mod <- erglm_model(ae1 ~ aucss + sex, erglm_data, family = binomial())
  sim <- simulate(mod, nsim = 5, seed = 333)
  expect_true(all(sim$val %in% c(0, 1)))
})

test_that("simulate.erglm_model draws non-negative integers for poisson models", {
  mod <- erglm_model(ae_count ~ aucss + sex, erglm_data, family = poisson())
  sim <- simulate(mod, nsim = 5, seed = 333)
  expect_true(all(sim$val >= 0))
  expect_equal(sim$val, round(sim$val))
})

test_that("simulate.erglm_model draws real-valued (possibly negative) values for gaussian models", {
  mod <- erglm_model(biomarker_change ~ aucss, erglm_data, family = gaussian())
  sim <- simulate(mod, nsim = 20, seed = 333)
  expect_true(any(sim$val < 0))
})

test_that("simulate.erglm_model draws strictly positive values for Gamma models", {
  mod <- erglm_model(ae_duration ~ aucss + dose, erglm_data, family = Gamma(link = "log"))
  sim <- simulate(mod, nsim = 5, seed = 333)
  expect_true(all(sim$val > 0))
})

test_that("simulate.erglm_model errors informatively for unsupported families", {
  mod <- erglm_model(ae_count ~ aucss, erglm_data, family = quasipoisson())
  expect_error(simulate(mod, nsim = 2, seed = 333), "quasipoisson")
})

test_that("simulate.erglm_model reports a random seed when none is supplied", {
  mod <- erglm_model(ae1 ~ aucss + sex, erglm_data, family = binomial())
  expect_message(simulate(mod, nsim = 2), "Using seed")
})

test_that("simulate.erglm_model rejects invalid nsim", {
  mod <- erglm_model(ae1 ~ aucss + sex, erglm_data, family = binomial())
  expect_error(simulate(mod, nsim = 0, seed = 111), "nsim")
  expect_error(simulate(mod, nsim = -1, seed = 111), "nsim")
  expect_error(simulate(mod, nsim = 1.7, seed = 111), "nsim")
  expect_error(simulate(mod, nsim = NA, seed = 111), "nsim")
  expect_error(simulate(mod, nsim = c(2, 3), seed = 111), "nsim")
  expect_error(simulate(mod, nsim = "2", seed = 111), "nsim")
})
