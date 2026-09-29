## Ported from djnavarro/minis' miniseed/tests/testthat/test-miniseed.R.
## No source() call needed here (unlike upstream): .seed_*() live in
## erglm's own R/miniseed.R and are already part of the package's
## namespace when tests run. Uses withr as a test-only tool (Suggests)
## to independently verify RNG-state save/restore -- not a runtime
## dependency of erglm itself, which is exactly what these tests exist
## to confirm .seed_with_seed() reproduces without depending on withr.

test_that(".seed_with_seed reproduces the same draws for the same seed", {
  a <- .seed_with_seed(42, runif(5))
  b <- .seed_with_seed(42, runif(5))
  expect_identical(a, b)
})

test_that(".seed_with_seed leaves the ambient RNG stream untouched", {
  withr::with_seed(99, {
    before <- runif(1)
    state_before <- get(".Random.seed", envir = globalenv())
    .seed_with_seed(1, runif(5))
    state_after <- get(".Random.seed", envir = globalenv())
    expect_identical(state_before, state_after)
    after <- runif(1)
    expect_false(identical(before, after))
  })
})

test_that(".seed_with_seed restores 'never touched' RNG state", {
  withr::with_preserve_seed({
    if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    }
    .seed_with_seed(1, runif(1))
    expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
  })
})

test_that(".seed_with_seed restores RNG state on error inside code", {
  withr::with_seed(11, {
    state_before <- get(".Random.seed", envir = globalenv())
    expect_error(.seed_with_seed(1, stop("boom")), "boom")
    state_after <- get(".Random.seed", envir = globalenv())
    expect_identical(state_before, state_after)
  })
})

test_that(".seed_with_preserve_seed restores prior state without setting a seed itself", {
  withr::with_seed(5, {
    state_before <- get(".Random.seed", envir = globalenv())
    result <- .seed_with_preserve_seed({
      set.seed(123)
      runif(2)
    })
    state_after <- get(".Random.seed", envir = globalenv())
    expect_identical(state_before, state_after)
    expect_length(result, 2)
  })
})

test_that(".seed_local_seed reproduces draws and doesn't leak past the caller", {
  f <- function() {
    .seed_local_seed(42)
    runif(3)
  }
  expect_identical(f(), f())

  withr::with_seed(7, {
    state_before <- get(".Random.seed", envir = globalenv())
    f()
    state_after <- get(".Random.seed", envir = globalenv())
    expect_identical(state_before, state_after)
  })
})

test_that(".seed_local_seed restores state even if the caller errors", {
  f <- function() {
    .seed_local_seed(42)
    stop("boom")
  }
  withr::with_seed(7, {
    state_before <- get(".Random.seed", envir = globalenv())
    expect_error(f(), "boom")
    state_after <- get(".Random.seed", envir = globalenv())
    expect_identical(state_before, state_after)
  })
})

test_that(".seed_local_preserve_seed restores prior state without setting a seed itself", {
  g <- function() {
    .seed_local_preserve_seed()
    set.seed(77)
    runif(2)
  }
  withr::with_seed(3, {
    state_before <- get(".Random.seed", envir = globalenv())
    result <- g()
    state_after <- get(".Random.seed", envir = globalenv())
    expect_identical(state_before, state_after)
    expect_length(result, 2)
  })
})

test_that(".seed_local_seed's envir argument can target a different frame", {
  outer <- function() {
    inner <- function(envir) {
      .seed_local_seed(42, envir = envir)
    }
    inner(environment())
    runif(1)
  }
  withr::with_seed(13, {
    state_before <- get(".Random.seed", envir = globalenv())
    outer()
    state_after <- get(".Random.seed", envir = globalenv())
    expect_identical(state_before, state_after)
  })
})

test_that("rng_kind arguments are passed through to set.seed()", {
  old_kind <- RNGkind()[1]
  withr::defer(RNGkind(old_kind))
  kind_inside <- .seed_with_seed(1, RNGkind()[1], rng_kind = "Wichmann-Hill")
  expect_equal(kind_inside, "Wichmann-Hill")
  # restored afterwards, since restoring .Random.seed also restores the kind
  expect_equal(RNGkind()[1], old_kind)
})
