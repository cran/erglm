## minicondition.R -------------------------------------------------------------
## stamp: 42b387fd (2026-09-27)
## Vendored from https://github.com/djnavarro/minis/tree/main/minicondition
## to avoid a hard dependency on rlang for a handful of `abort()`/`warn()`/
## `inform()` calls. Do not add functions here by hand -- re-copy the whole
## file from upstream if erglm's needs grow (e.g. a `class` argument is
## actually used somewhere).
##
## A minimal, dependency-free reimplementation of rlang's `abort()`,
## `warn()`, and `inform()`: signal a classed condition (an error,
## warning, or message carrying a custom class, so callers can catch it
## specifically via `tryCatch()`/`withCallingHandlers()`), plus a small
## `assert()` helper built on top of `abort()`.
##
## Base R only, using base R's own condition system (`structure()` +
## `stop()`/`warning()`/`message()`) rather than `rlang::abort()`/
## `warn()`/`inform()`. Custom condition classes work identically either
## way: `tryCatch(f(), my_class = handler)` catches a condition of class
## `my_class` regardless of whether it was raised via rlang or via a plain
## classed condition object passed to `stop()`.
##
## License: MIT (see LICENSE at the root of the minis repo). This file
## contains no code copied from {rlang}, only equivalent logic built on
## base R's condition system.

.cond_condition <- function(message, class, base_class) {
  structure(
    class = c(class, base_class, "condition"),
    list(message = message, call = NULL)
  )
}

# Signal a classed error. `class` is an optional character vector of
# extra classes, so the condition can be caught specifically via
# `tryCatch(expr, <class> = handler)`.
.cond_abort <- function(message, class = NULL) {
  stop(.cond_condition(message, class, "error"))
}

# Signal a classed warning. See `.cond_abort()`.
.cond_warn <- function(message, class = NULL) {
  warning(.cond_condition(message, class, "warning"))
}

# Signal a classed message. Prints with a trailing newline, like a plain
# `message()` call, but -- unlike passing a pre-built condition straight
# to `message()` -- the newline is never part of the condition's own
# `message` field, so `conditionMessage()` on a caught `.cond_inform()`
# condition is exactly `message`, matching `.cond_abort()`/`.cond_warn()`.
.cond_inform <- function(message, class = NULL) {
  cond <- .cond_condition(message, class, "message")
  withRestarts(
    {
      signalCondition(cond)
      cat(conditionMessage(cond), "\n", sep = "", file = stderr())
    },
    muffleMessage = function() NULL
  )
  invisible(NULL)
}

# Abort with a classed error if `expr` doesn't hold. `NA` counts as a
# failure, same as `FALSE` (unlike a plain `if (any(expr == FALSE))`,
# which would error on `NA` with "missing value where TRUE/FALSE needed"
# instead of raising the intended assertion error).
.cond_assert <- function(expr, message = "Assertion failed.", class = NULL) {
  if (anyNA(expr) || any(expr == FALSE)) .cond_abort(message, class)
}
