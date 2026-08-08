test_that("erglm_data is correctly formed", {
  erglm_data2 <- .make_erglm_data(seed = 2407L)
  expect_equal(erglm_data, erglm_data2)
})

test_that("erglm_data has response columns suitable for each supported family", {
  expect_true(all(erglm_data$ae1 %in% c(0, 1)))
  expect_true(all(erglm_data$ae2 %in% c(0, 1)))
  expect_true(is.integer(erglm_data$ae_count))
  expect_true(all(erglm_data$ae_count >= 0))
  expect_true(is.numeric(erglm_data$biomarker_change))
  expect_true(any(erglm_data$biomarker_change < 0)) # can be negative
  expect_true(is.numeric(erglm_data$ae_duration))
  expect_true(all(erglm_data$ae_duration > 0)) # strictly positive
})
