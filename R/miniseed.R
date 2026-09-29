## miniseed.R -------------------------------------------------------------
## stamp: c599d527 (2026-09-29)
## Vendored from https://github.com/djnavarro/minis/tree/main/miniseed to
## avoid a hard dependency on withr for `with_seed()`. erglm only ever
## uses `.seed_with_seed()`; the other three functions upstream
## (`.seed_with_preserve_seed()`/`.seed_local_seed()`/
## `.seed_local_preserve_seed()`) are included unchanged for parity with
## upstream, in case a future need arises -- do not hand-edit this file,
## re-copy it from upstream instead.
##
## A minimal, dependency-free reimplementation of the RNG-seed-management
## slice of {withr}: `with_seed()`, `with_preserve_seed()`, `local_seed()`,
## and `local_preserve_seed()`.
##
## License: MIT (see LICENSE at the root of the minis repo). This file
## contains no code copied from {withr}, only equivalent logic.

# Snapshots the current RNG state (`NULL` if `.Random.seed` doesn't
# exist yet, i.e. the RNG has never been touched this session).
.seed_get_state <- function() {
  if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    get(".Random.seed", envir = globalenv())
  } else {
    NULL
  }
}

# Restores a snapshot taken by `.seed_get_state()`. A `NULL` snapshot
# removes `.Random.seed` entirely, putting the RNG back into its
# "never touched" state rather than leaving behind a state introduced
# only while the seed was set.
.seed_set_state <- function(state) {
  if (is.null(state)) {
    if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    }
  } else {
    assign(".Random.seed", state, envir = globalenv())
  }
  invisible(NULL)
}

# Schedules `fn()` (a zero-argument closure, not an unevaluated
# expression) to run when `envir` exits, rather than when
# `.seed_defer()`'s own frame exits. This is what lets
# `.seed_local_seed()`/`.seed_local_preserve_seed()` register their
# restore against their *caller's* frame.
.seed_defer <- function(fn, envir) {
  do.call(on.exit, list(as.call(list(fn)), add = TRUE), envir = envir)
}

# Sets the RNG seed, evaluates `code`, and restores the RNG state as it
# was found -- so calling `.seed_with_seed()` reproducibly is safe
# without disturbing the ambient RNG stream for whatever runs next.
# `code` is only evaluated once, in the caller's own environment, after
# the seed has been set. `rng_kind`/`rng_normal_kind`/`rng_sample_kind`
# are passed straight through to `set.seed()`'s `kind`/`normal.kind`/
# `sample.kind` arguments; `NULL` (the default for all three) leaves the
# corresponding generator unchanged.
.seed_with_seed <- function(seed, code,
                             rng_kind = NULL, rng_normal_kind = NULL,
                             rng_sample_kind = NULL) {
  old_state <- .seed_get_state()
  on.exit(.seed_set_state(old_state))
  set.seed(seed, kind = rng_kind, normal.kind = rng_normal_kind,
           sample.kind = rng_sample_kind)
  code
}

# Like `.seed_with_seed()`, but doesn't itself call `set.seed()` --
# useful when `code` sets its own seed (or draws from the ambient
# stream) and you just want to guarantee the RNG is left as it was found
# once `code` finishes.
.seed_with_preserve_seed <- function(code) {
  old_state <- .seed_get_state()
  on.exit(.seed_set_state(old_state))
  code
}

# Sets the RNG seed immediately, and schedules the prior RNG state to be
# restored when `envir` exits -- by default, when the function that
# called `.seed_local_seed()` returns. Meant to be called directly
# inside a function body (no code block to wrap), unlike
# `.seed_with_seed()`.
.seed_local_seed <- function(seed,
                              rng_kind = NULL, rng_normal_kind = NULL,
                              rng_sample_kind = NULL,
                              envir = parent.frame()) {
  old_state <- .seed_get_state()
  restore <- function() .seed_set_state(old_state)
  .seed_defer(restore, envir)
  set.seed(seed, kind = rng_kind, normal.kind = rng_normal_kind,
           sample.kind = rng_sample_kind)
  invisible(old_state)
}

# Like `.seed_local_seed()`, but doesn't itself call `set.seed()` --
# schedules the restore without touching the current RNG state first.
.seed_local_preserve_seed <- function(envir = parent.frame()) {
  old_state <- .seed_get_state()
  restore <- function() .seed_set_state(old_state)
  .seed_defer(restore, envir)
  invisible(old_state)
}
