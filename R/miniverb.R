## miniverb.R ----------------------------------------------------------------
## stamp: a3e4b120 (2026-09-27)
## Vendored from https://github.com/djnavarro/minis/tree/main/miniverb to
## avoid a hard dependency on dplyr for `filter()`/`mutate()`. erglm only
## ever uses `.verb_mutate()` (with and without `.by`); `.verb_filter()`/
## `.verb_select()`/`.verb_arrange()`/`.verb_summarise()`/`.verb_desc()` are
## included unchanged for parity with upstream, in case a future need
## arises -- do not hand-edit this file, re-copy it from upstream instead.
##
## A minimal, dependency-free reimplementation of five dplyr one-table
## verbs -- `filter()`, `select()`, `mutate()`, `arrange()`, `summarise()`
## -- plus a `.by`-style grouping argument shared across `filter()`,
## `mutate()`, and `summarise()`.
##
## Design notes:
## - Base R only.
## - `.by` accepts a plain character vector of column names only (e.g.
##   `.by = c("cyl", "gear")`) -- no unquoted/tidyselect-lite syntax, unlike
##   dplyr's `.by = sex`. erglm's one `.by` call site was rewritten
##   `.by = "sex"` accordingly.
## - Deliberately excluded, across every verb: `across()`, tidyselect
##   helpers, injection operators (`{{ }}`/`!!`/`!!!`), and any persistent
##   `group_by()` object.
## - `.verb_mutate()` evaluates `name = expr` arguments in the scope of
##   `.data` (plus any already-added columns from earlier arguments in
##   the same call), recycling length-1 results and otherwise relying on
##   base R's own vector-assignment recycling rules. It does not tolerate
##   a trailing comma after the last argument (unlike `dplyr::mutate()`) --
##   it shows up as a genuine extra, unevaluated, unnamed argument in
##   `...`, which fails the "all arguments must be named" check.
## - Unlike `dplyr::mutate()`, there's no `n()`/`row_number()` -- neither
##   has a mini equivalent. erglm's one grouped-`n()` call site
##   (`.by = "sex"`) was rewritten as `length(sex)` (any same-length
##   column of the group works); its one ungrouped `row_number()` call
##   site was rewritten as direct column assignment with
##   `seq_len(nrow(.))` instead of routing through `.verb_mutate()` at
##   all.
##
## Usage:
##   source("miniverb.R")
##   .verb_filter(mtcars, mpg > mean(mpg), .by = "cyl")
##   .verb_select(mtcars, mpg, cylinders = cyl)
##   .verb_mutate(mtcars, mpg_z = (mpg - mean(mpg)) / sd(mpg), .by = "cyl")
##   .verb_arrange(mtcars, .verb_desc(mpg))
##   .verb_summarise(mtcars, mean_mpg = mean(mpg), .by = "cyl")
##
## License: MIT (see LICENSE at the root of the minis repo). `.verb_filter()`
## logic adapted from poorman (MIT licensed), not copied from {dplyr}; the
## other four verbs are original reimplementations of dplyr semantics.

.verb_dotdotdot <- function(...) {
  eval(substitute(alist(...)))
}

# Split row indices of `.data` into groups. `by` is `NULL`, or a character
# vector of column names in `.data` to group by. Returns a list of integer
# vectors of row indices, one per group, in the order each distinct
# combination of `by` columns is first seen in `.data` -- matching dplyr's
# `.by`/`group_by()` semantics, not sorted key order. With `by = NULL`, a
# single-element list holding all row indices in original order.
.verb_split_by <- function(.data, by = NULL) {
  n <- nrow(.data)
  if (is.null(by) || length(by) == 0L) {
    return(list(seq_len(n)))
  }
  if (!is.character(by)) {
    stop(
      ".by must be a character vector of column names (e.g. `.by = c(\"g\")`)",
      call. = FALSE
    )
  }
  keys <- .data[by]
  key_str <- do.call(paste, c(as.list(keys), sep = "\r"))
  # match() assigns each distinct key its first-seen rank (1 for whichever
  # combination appears first, 2 for the next new one, ...), and split()
  # on a small-integer, non-factor vector sorts numerically by that rank
  # -- so groups come back in first-appearance order without needing a
  # separate sort step.
  grp <- match(key_str, unique(key_str))
  unname(split(seq_len(n), grp))
}

# Filter rows of `.data` by unquoted conditions in `...`, evaluated in the
# scope of `.data` (columns can be referred to by bare name). Multiple
# expressions are combined with `&`. `.by` is `NULL` (default), or a
# character vector of column names to evaluate `...` within each group
# separately (e.g. so `x > mean(x)` compares against the group mean, not
# the overall mean). Returns a data frame containing only rows where all
# conditions are `TRUE`; rows where the combined condition is `NA` are
# dropped, as in dplyr. Calling with no conditions returns `.data`
# unchanged.
.verb_filter <- function(.data, ..., .by = NULL) {
  conditions <- .verb_dotdotdot(...)
  if (length(conditions) == 0L) return(.data)
  frame <- parent.frame()
  groups <- .verb_split_by(.data, .by)
  keep <- logical(nrow(.data))
  for (idx in groups) {
    sub <- .data[idx, , drop = FALSE]
    rows <- lapply(conditions, function(cond) eval(cond, sub, frame))
    rows <- Reduce(`&`, rows)
    keep[idx] <- rows & !is.na(rows)
  }
  .data[keep, , drop = FALSE]
}

# Select and optionally rename columns of `.data`. `...` are unquoted
# column names or integer positions to keep, optionally named to rename
# (`new = old`); or `-col`/`-position` to drop columns. Inclusion and
# exclusion cannot be mixed in one call. Returns a data frame with the
# selected columns, in the order given.
.verb_select <- function(.data, ...) {
  dots <- eval(substitute(alist(...)))
  nms <- names(dots)
  if (is.null(nms)) nms <- rep("", length(dots))
  all_names <- names(.data)

  resolve <- function(expr) {
    if (is.symbol(expr)) return(as.character(expr))
    if (is.numeric(expr)) return(all_names[abs(expr)])
    stop(
      ".verb_select: unsupported expression `", deparse(expr), "` -- ",
      "only bare column names, integer positions, and `-` exclusions ",
      "are supported (no tidyselect helpers)",
      call. = FALSE
    )
  }

  include <- character(0)
  exclude <- character(0)
  out_names <- character(0)

  for (i in seq_along(dots)) {
    expr <- dots[[i]]
    nm <- nms[i]
    is_excl <- (is.call(expr) && identical(expr[[1]], as.name("-"))) ||
      (is.numeric(expr) && expr < 0)
    if (is_excl) {
      target <- if (is.call(expr)) expr[[2]] else -expr
      exclude <- c(exclude, resolve(target))
      next
    }
    col <- resolve(expr)
    include <- c(include, col)
    out_names <- c(out_names, if (nzchar(nm)) nm else col)
  }

  if (length(exclude) > 0L) {
    if (length(include) > 0L) {
      stop(".verb_select: cannot mix inclusion and exclusion", call. = FALSE)
    }
    include <- setdiff(all_names, exclude)
    out_names <- include
  }

  out <- .data[include]
  names(out) <- out_names
  out
}

# Add or modify columns of `.data`. `...` are named `name = expr`
# arguments, evaluated in the scope of `.data` (columns added by earlier
# arguments in the same call are visible to later ones). Length-1 results
# are recycled to the number of rows in the group; other lengths rely on
# base R's own vector-assignment recycling. `.by` is `NULL` (default), or
# a character vector of column names to evaluate `...` within each group
# separately. Returns `.data` with the named columns added or overwritten.
.verb_mutate <- function(.data, ..., .by = NULL) {
  dots <- eval(substitute(alist(...)))
  nms <- names(dots)
  if (length(dots) == 0L) return(.data)
  if (is.null(nms) || any(!nzchar(nms))) {
    stop(".verb_mutate: all arguments must be named", call. = FALSE)
  }
  frame <- parent.frame()
  groups <- .verb_split_by(.data, .by)
  out <- .data
  for (i in seq_along(dots)) {
    col <- NULL
    for (idx in groups) {
      sub <- out[idx, , drop = FALSE]
      val <- eval(dots[[i]], sub, frame)
      if (length(val) == 1L) val <- rep(val, length(idx))
      if (is.null(col)) col <- val[rep(NA_integer_, nrow(out))]
      col[idx] <- val
    }
    out[[nms[i]]] <- col
  }
  out
}

# Mark a column for descending order in `.verb_arrange()`. Transforms `x`
# so that sorting it ascending produces descending order on the original
# values.
.verb_desc <- function(x) {
  -xtfrm(x)
}

# Reorder the rows of `.data` by unquoted columns/expressions in `...`
# (e.g. `.verb_desc(col)`), evaluated in the scope of `.data`. Ties are
# broken by later arguments, as in `order()`. `NA`s sort last, matching
# `dplyr::arrange()`. Calling with no arguments returns `.data` unchanged.
.verb_arrange <- function(.data, ...) {
  dots <- eval(substitute(alist(...)))
  if (length(dots) == 0L) return(.data)
  frame <- parent.frame()
  keys <- lapply(dots, function(e) eval(e, .data, frame))
  ord <- do.call(order, keys)
  .data[ord, , drop = FALSE]
}

# Collapse `.data` to one summary row per group. `...` are named
# `name = expr` arguments, each of which must evaluate to a single value
# per group. `.by` is `NULL` (default), or a character vector of column
# names to group by; with `.by`, the grouping columns are included as the
# leading columns of the result. Returns a data frame with one row per
# group (or one row overall, with no `.by`) and one column per named
# argument in `...` (plus the grouping columns, if `.by` is supplied).
.verb_summarise <- function(.data, ..., .by = NULL) {
  dots <- eval(substitute(alist(...)))
  nms <- names(dots)
  if (length(dots) == 0L) {
    stop(".verb_summarise: no summary expressions supplied", call. = FALSE)
  }
  if (is.null(nms) || any(!nzchar(nms))) {
    stop(".verb_summarise: all arguments must be named", call. = FALSE)
  }
  if (!is.null(.by) && any(nms %in% .by)) {
    stop(
      ".verb_summarise: name(s) `", paste(intersect(nms, .by), collapse = "`, `"),
      "` collide with a `.by` grouping column; summary outputs can't reuse ",
      "a grouping column's name",
      call. = FALSE
    )
  }
  frame <- parent.frame()
  groups <- .verb_split_by(.data, .by)
  rows <- lapply(groups, function(idx) {
    sub <- .data[idx, , drop = FALSE]
    values <- lapply(dots, function(e) eval(e, sub, frame))
    if (any(lengths(values) != 1L)) {
      stop(
        ".verb_summarise: expressions must return a single value per group",
        call. = FALSE
      )
    }
    result <- as.data.frame(values, stringsAsFactors = FALSE)
    names(result) <- nms
    if (!is.null(.by)) {
      result <- cbind(sub[1, .by, drop = FALSE], result)
    }
    result
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
