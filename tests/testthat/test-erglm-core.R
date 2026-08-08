test_that("erglm_model works", {
  expect_no_error(erglm_model(ae1 ~ aucss + sex, erglm_data, family = binomial()))
  mod1 <- erglm_model(ae1 ~ aucss + sex, erglm_data, family = binomial())
  expect_s3_class(mod1, "glm")
})

test_that("erglm_model defaults to family = gaussian()", {
  mod <- erglm_model(biomarker_change ~ aucss, erglm_data)
  expect_equal(family(mod)$family, "gaussian")
})

test_that("erglm_fun returns a function", {
  mod1 <- erglm_model(ae1 ~ aucss + sex, erglm_data, family = binomial())
  expect_no_error(erglm_fun(mod1))
  mod1_fun <- erglm_fun(mod1)
  expect_type(mod1_fun, "closure")
})

test_that("erglm_fun works", {

  mod1 <- erglm_model(ae1 ~ aucss + sex, erglm_data, family = binomial())
  par1 <- coef(mod1)
  mod1_fun <- erglm_fun(mod1)

  # no counterfactuals
  p1 <- mod1_fun(param = par1, data = erglm_data) 
  p2 <- unname(predict(mod1, type = "response")) # same result
  expect_equal(p1, p2)

  # user modifies the data set
  erglm_data2 <- erglm_data[1:20, ]
  p3 <- mod1_fun(param = par1, data = erglm_data2) 
  p4 <- unname(predict(mod1, newdata = erglm_data2, type = "response")) # same result
  expect_equal(p3, p4)

  # user modifies the parameters
  par2 <- par1
  int1 <- par1["(Intercept)"]
  par2["(Intercept)"] <- 0
  p5 <- mod1_fun(param = par2, data = erglm_data)
  link1 <- erglm_link(mod1)
  expect_equal(link1(p1), link1(p5) + int1)
  
})

test_that("erglm_fun defaults param to fitted coefficients and data to the fitted data", {
  mod1 <- erglm_model(ae1 ~ aucss + sex, erglm_data, family = binomial())
  mod1_fun <- erglm_fun(mod1)

  p1 <- mod1_fun()
  p2 <- unname(predict(mod1, type = "response"))
  expect_equal(p1, p2)

  # data supplied, param defaulted
  erglm_data2 <- erglm_data[1:20, ]
  p3 <- mod1_fun(data = erglm_data2)
  p4 <- unname(predict(mod1, newdata = erglm_data2, type = "response"))
  expect_equal(p3, p4)

  # param supplied, data defaulted
  par2 <- coef(mod1)
  par2["(Intercept)"] <- 0
  p5 <- mod1_fun(param = par2)
  p6 <- mod1_fun(param = par2, data = erglm_data)
  expect_equal(p5, p6)
})

test_that("erglm_predict works with default data", {
  mod <- erglm_model(ae1 ~ aucss + sex, erglm_data, family = binomial())
  expect_no_error(erglm_predict(mod))
  prd <- erglm_predict(mod)
  pr_resp <- predict(mod, type = "response", se.fit = TRUE)
  pr_link <- predict(mod, type = "link", se.fit = TRUE)
  expect_equal(prd$fit_resp, pr_resp$fit)
  expect_equal(prd$fit_link, pr_link$fit)
  expect_equal(prd$se_link, pr_link$se.fit)
})

test_that("erglm_predict works with modified data", {
  mod <- erglm_model(ae1 ~ aucss + sex, erglm_data, family = binomial())
  dat_1 <- erglm_data[1:20,]
  expect_no_error(erglm_predict(mod, newdata = dat_1))
  prd <- erglm_predict(mod, newdata = dat_1)
  pr_resp <- predict(mod, newdata = dat_1, type = "response", se.fit = TRUE)
  pr_link <- predict(mod, newdata = dat_1, type = "link", se.fit = TRUE)
  expect_equal(prd$fit_resp, pr_resp$fit)
  expect_equal(prd$fit_link, pr_link$fit)
  expect_equal(prd$se_link, pr_link$se.fit)
})

test_that("erglm_predict can adjust confidence level", {
  mod <- erglm_model(ae1 ~ aucss + sex, erglm_data, family = binomial())
  expect_no_error(erglm_predict(mod, conf_level = 0))
  prd0 <- erglm_predict(mod, conf_level = 0)
  expect_equal(prd0$ci_lower, prd0$fit_resp)
  expect_equal(prd0$ci_upper, prd0$fit_resp)
})

test_that("erglm_predict rejects invalid conf_level", {
  mod <- erglm_model(ae1 ~ aucss + sex, erglm_data, family = binomial())
  expect_error(erglm_predict(mod, conf_level = -0.5), "conf_level")
  expect_error(erglm_predict(mod, conf_level = 1.5), "conf_level")
  expect_error(erglm_predict(mod, conf_level = NA), "conf_level")
  expect_error(erglm_predict(mod, conf_level = c(0.9, 0.95)), "conf_level")
  expect_error(erglm_predict(mod, conf_level = "0.95"), "conf_level")
  # 0 and 1 are legitimate (degenerate) endpoints, not errors
  expect_no_error(erglm_predict(mod, conf_level = 0))
  expect_no_error(erglm_predict(mod, conf_level = 1))
})

test_that("erglm_fun's returned function rejects mismatched param length", {
  mod1 <- erglm_model(ae1 ~ aucss + sex, erglm_data, family = binomial())
  mod1_fun <- erglm_fun(mod1)
  expect_error(mod1_fun(param = c(1, 2)), "param")
  expect_error(mod1_fun(param = c(1, 2, 3, 4, 5)), "param")
  expect_error(mod1_fun(param = "not numeric"), "param")
})

test_that("erglm_model supports non-binomial glm families", {
  mod_pois <- erglm_model(ae_count ~ aucss + sex, erglm_data, family = poisson())
  expect_s3_class(mod_pois, "glm")
  expect_equal(family(mod_pois)$family, "poisson")

  mod_gauss <- erglm_model(biomarker_change ~ aucss, erglm_data, family = gaussian())
  expect_equal(family(mod_gauss)$family, "gaussian")

  mod_gamma <- erglm_model(ae_duration ~ aucss + dose, erglm_data, family = Gamma(link = "log"))
  expect_equal(family(mod_gamma)$family, "Gamma")
})

test_that("erglm_predict is family-generic", {
  mod_pois <- erglm_model(ae_count ~ aucss + sex, erglm_data, family = poisson())
  prd <- erglm_predict(mod_pois)
  pr_resp <- predict(mod_pois, type = "response", se.fit = TRUE)
  expect_equal(prd$fit_resp, pr_resp$fit)

  mod_gauss <- erglm_model(biomarker_change ~ aucss, erglm_data, family = gaussian())
  prd_gauss <- erglm_predict(mod_gauss)
  pr_resp_gauss <- predict(mod_gauss, type = "response", se.fit = TRUE)
  expect_equal(prd_gauss$fit_resp, pr_resp_gauss$fit)
})

test_that("erglm_fun is family-generic", {
  mod_pois <- erglm_model(ae_count ~ aucss + sex, erglm_data, family = poisson())
  par1 <- coef(mod_pois)
  fn <- erglm_fun(mod_pois)
  p1 <- fn(param = par1, data = erglm_data)
  p2 <- unname(predict(mod_pois, type = "response"))
  expect_equal(p1, p2)
})
