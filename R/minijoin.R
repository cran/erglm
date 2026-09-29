## minijoin.R -------------------------------------------------------------
## stamp: 2e7655a5 (2026-09-27)
## Vendored from https://github.com/djnavarro/minis/tree/main/minijoin to
## avoid a hard dependency on dplyr for `left_join()`. erglm only ever
## uses `.join_left_join()`; `.join_inner_join()`/`.join_right_join()`/
## `.join_full_join()` are included unchanged for parity with upstream,
## in case a future need arises -- do not hand-edit this file, re-copy
## it from upstream instead.
##
## A minimal, dependency-free reimplementation of dplyr's mutating joins
## (`inner_join()`, `left_join()`, `right_join()`, `full_join()`), built
## on base `merge()`.
##
## Design notes:
## - Base R only, built on `merge()`.
## - Unlike dplyr, `by` must always be supplied explicitly -- there is
##   no auto-detection of common columns. This avoids a class of silent
##   bugs and keeps the implementation simple.
## - Row order matches `x`'s original row order (`merge()` does not
##   guarantee this on its own); a `.join_id` column is added and
##   removed internally to restore it.
## - Deliberately excluded: dplyr's `relationship`, `multiple`,
##   `unmatched` validation arguments -- this covers plain two-table
##   joins only.
##
## Usage:
##   source("minijoin.R")
##   bands <- data.frame(band = c("Beatles", "Who"), founded = c(1960, 1964))
##   albums <- data.frame(band = c("Beatles", "Kinks"), album = c("Abbey Road", "Arthur"))
##   .join_inner_join(bands, albums, by = "band")
##   .join_left_join(bands, albums, by = "band")
##
## License: MIT (see LICENSE at the root of the minis repo). Logic
## adapted from poorman (MIT licensed), not copied from {dplyr}.

.join_worker <- function(x, y, by, suffix, keep, na_matches, ...) {
  na_matches <- match.arg(arg = na_matches, choices = c("na", "never"), several.ok = FALSE)
  incomparables <- if (na_matches == "never") NA else NULL
  by_x <- if (is.null(names(by))) by else names(by)
  by_y <- if (is.null(names(by))) by else unname(by)

  x[, ".join_id"] <- seq_len(nrow(x))
  if (isTRUE(keep)) {
    # Carry each side's *original* by-column values through under temporary
    # names, as ordinary (non-`by`) columns, so merge() fills them in with
    # real NAs on the side that didn't match -- rather than reconstructing
    # a "y-side" column after the fact by copying the single, already-
    # collapsed by-column merge() produces for actual join keys, which
    # mirrors x's value even for rows with no match in y at all.
    keep_x_nm <- paste0(".join_keep_x_", seq_along(by_x))
    keep_y_nm <- paste0(".join_keep_y_", seq_along(by_y))
    x[, keep_x_nm] <- x[, by_x, drop = FALSE]
    y[, keep_y_nm] <- y[, by_y, drop = FALSE]
  }
  merged <- if (is.null(names(by))) {
    merge(x = x, y = y, by = by, suffixes = suffix, incomparables = incomparables, ...)
  } else {
    merge(x = x, y = y, by.x = names(by), by.y = by, suffixes = suffix, incomparables = incomparables, ...)
  }
  merged <- merged[order(merged[, ".join_id"]), colnames(merged) != ".join_id", drop = FALSE]
  if (isTRUE(keep)) {
    # Both suffixed copies are named after `by`'s x-side name, even when
    # `by` renames the join column (see README) -- e.g. `by = c("band" =
    # "artist")` produces `band.x`/`band.y`, not `band`/`artist`. The
    # original, single collapsed by-column merge() produces (still named
    # `by_x`) is dropped in favour of these two, rather than left behind
    # as a redundant third copy.
    names(merged)[match(keep_x_nm, names(merged))] <- paste0(by_x, suffix[1L])
    names(merged)[match(keep_y_nm, names(merged))] <- paste0(by_x, suffix[2L])
    merged <- merged[, !(names(merged) %in% by_x), drop = FALSE]
  }
  rownames(merged) <- NULL
  merged
}

# Inner join: keep rows present in both `x` and `y`. `by` (column name(s)
# to join on) must always be supplied explicitly; use a named character
# vector (`c("a" = "b")`) to join on differently-named columns. `suffix`
# is a length-2 character vector of suffixes applied to overlapping
# non-join column names. `...` is passed on to `merge()`. `na_matches`
# is `"na"` (default, matches `NA` to `NA`, as in dplyr/SQL joins) or
# `"never"` (never matches `NA` to anything).
.join_inner_join <- function(x, y, by, suffix = c(".x", ".y"), ..., na_matches = c("na", "never")) {
  .join_worker(x = x, y = y, by = by, suffix = suffix, sort = FALSE, ..., keep = FALSE, na_matches = na_matches)
}

# Left join: keep all rows of `x`. See `.join_inner_join()` for
# `by`/`suffix`/`na_matches`. `keep` keeps both join columns (suffixed)
# rather than collapsing them into one.
.join_left_join <- function(x, y, by, suffix = c(".x", ".y"), ..., keep = FALSE, na_matches = c("na", "never")) {
  .join_worker(x = x, y = y, by = by, suffix = suffix, all.x = TRUE, ..., keep = keep, na_matches = na_matches)
}

# Right join: keep all rows of `y`. See `.join_left_join()`.
.join_right_join <- function(x, y, by, suffix = c(".x", ".y"), ..., keep = FALSE, na_matches = c("na", "never")) {
  .join_worker(x = x, y = y, by = by, suffix = suffix, all.y = TRUE, ..., keep = keep, na_matches = na_matches)
}

# Full join: keep all rows of both `x` and `y`. See `.join_left_join()`.
.join_full_join <- function(x, y, by, suffix = c(".x", ".y"), ..., keep = FALSE, na_matches = c("na", "never")) {
  .join_worker(x = x, y = y, by = by, suffix = suffix, all = TRUE, ..., keep = keep, na_matches = na_matches)
}
