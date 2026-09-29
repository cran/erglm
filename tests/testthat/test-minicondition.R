## Ported from djnavarro/minis' minicondition/tests/testthat/test-minicondition.R.
## No source() call needed here (unlike upstream): .cond_*() live in
## erglm's own R/minicondition.R and are already part of the package's
## namespace when tests run.

test_that(".cond_abort() raises an error with the given message", {
  expect_error(.cond_abort("bad input"), "bad input")
})

test_that(".cond_abort() is catchable by a custom class", {
  out <- tryCatch(
    .cond_abort("bad input", class = "my_error"),
    my_error = function(e) paste("caught:", conditionMessage(e))
  )
  expect_equal(out, "caught: bad input")
})

test_that(".cond_abort() without a class is not catchable by an unrelated class", {
  expect_error(
    tryCatch(.cond_abort("bad input"), my_error = function(e) "should not fire"),
    "bad input"
  )
})

test_that(".cond_warn() raises a warning with the given message", {
  expect_warning(.cond_warn("careful"), "careful")
})

test_that(".cond_warn() is catchable by a custom class", {
  out <- tryCatch(
    .cond_warn("careful", class = "my_warning"),
    my_warning = function(w) paste("caught:", conditionMessage(w))
  )
  expect_equal(out, "caught: careful")
})

test_that(".cond_inform() raises a message with the given text", {
  expect_message(.cond_inform("hi there"), "hi there")
})

test_that(".cond_inform()'s conditionMessage() is clean, with no trailing newline", {
  captured <- NULL
  withCallingHandlers(
    .cond_inform("hi there", class = "my_message"),
    my_message = function(m) {
      captured <<- conditionMessage(m)
      invokeRestart("muffleMessage")
    }
  )
  expect_identical(captured, "hi there")
})

test_that(".cond_inform() is catchable by a custom class", {
  out <- withCallingHandlers(
    {
      .cond_inform("hi there", class = "my_message")
      "not caught"
    },
    my_message = function(m) {
      invokeRestart("muffleMessage")
    }
  )
  expect_equal(out, "not caught")
  expect_message(
    withCallingHandlers(
      .cond_inform("hi there", class = "my_message"),
      my_message = function(m) cat("caught:", conditionMessage(m))
    ),
    "hi there"
  )
})

test_that(".cond_assert() passes silently when the condition holds", {
  expect_no_error(.cond_assert(TRUE))
  expect_no_error(.cond_assert(c(TRUE, TRUE)))
})

test_that(".cond_assert() aborts when the condition fails", {
  expect_error(.cond_assert(FALSE, "x must be positive"), "x must be positive")
  expect_error(.cond_assert(c(TRUE, FALSE), "not all true"), "not all true")
})

test_that(".cond_assert() treats NA as a failure, not an error about missing values", {
  expect_error(.cond_assert(c(TRUE, NA)), "Assertion failed")
})

test_that(".cond_assert() passes its class through to the resulting error", {
  out <- tryCatch(
    .cond_assert(FALSE, "bad", class = "my_invalid_input"),
    my_invalid_input = function(e) "caught"
  )
  expect_equal(out, "caught")
})
