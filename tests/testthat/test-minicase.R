## Ported from djnavarro/minis' minicase/tests/testthat/test-minicase.R.
## No source() call needed here (unlike upstream): .case_when() lives in
## erglm's own R/minicase.R and is already part of the package's
## namespace when tests run.

test_that(".case_when() matches the first true condition", {
  x <- c(-5, 0, 5)
  out <- .case_when(
    x < 0 ~ "negative",
    x == 0 ~ "zero",
    x > 0 ~ "positive"
  )
  expect_equal(out, c("negative", "zero", "positive"))
})

test_that(".case_when() leaves unmatched positions as NA", {
  x <- c(1, 2, 3)
  out <- .case_when(x == 2 ~ "two")
  expect_equal(out, c(NA, "two", NA))
})

test_that(".case_when() supports a TRUE ~ default catch-all", {
  x <- c(1, 2, 3)
  out <- .case_when(x == 2 ~ "two", TRUE ~ "other")
  expect_equal(out, c("other", "two", "other"))
})

test_that(".case_when() evaluates conditions/values in the caller's environment", {
  y <- c(10, 20)
  out <- .case_when(y > 15 ~ "big", TRUE ~ "small")
  expect_equal(out, c("small", "big"))
})

test_that(".case_when() errors on non-formula input", {
  expect_error(.case_when(TRUE, "x"))
})

test_that(".case_when() errors when no cases are provided", {
  expect_error(.case_when())
})

test_that(".case_when() errors when a condition is not logical", {
  expect_error(.case_when(1 ~ "one"))
})

test_that(".case_when() errors on inconsistent recycling lengths", {
  expect_error(.case_when(c(TRUE, FALSE) ~ c("a", "b", "c")))
})

test_that(".case_when() errors on mismatched factor levels instead of silently producing NA", {
  f1 <- factor(c("a", "b"), levels = c("a", "b"))
  f2 <- factor(c("x", "y"), levels = c("x", "y"))
  expect_error(
    .case_when(c(TRUE, FALSE) ~ f1, c(FALSE, TRUE) ~ f2),
    "Factor levels must match"
  )
})

test_that(".case_when() combines factors that share the same levels", {
  f1 <- factor(c("a", "b"), levels = c("a", "b"))
  f2 <- factor(c("b", "a"), levels = c("a", "b"))
  out <- .case_when(c(TRUE, FALSE) ~ f1, c(FALSE, TRUE) ~ f2)
  expect_equal(as.character(out), c("a", "a"))
  expect_equal(levels(out), c("a", "b"))
})
