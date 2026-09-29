## minitable.R -------------------------------------------------------------
## stamp: 122e11d0 (2026-09-27)
## Vendored from https://github.com/djnavarro/minis/tree/main/minitable to
## avoid a hard dependency on tibble. erglm only actually uses
## `.table_as_tibble()` (a thin `as.data.frame(x, check.names = FALSE)`
## wrapper); `.table_tibble()`/`.table_add_row()`/
## `.table_rownames_to_column()` are included unchanged for parity with
## upstream, but are NOT used by erglm's own code -- see the warning
## below before reaching for `.table_tibble()`. Do not hand-edit this
## file; re-copy it from upstream instead.
##
## A minimal, dependency-free reimplementation of a few tibble
## construction/coercion helpers: `tibble()`, `as_tibble()`,
## `rownames_to_column()`, `add_row()`.
##
## Design notes:
## - Base R only. Results are plain data.frames, not tibbles -- this
##   mini gives tibble-style *construction* ergonomics (in particular,
##   later columns can refer to earlier ones by name, e.g.
##   `.table_tibble(a = 1:3, b = a * 2)`), not tibble's printing,
##   stricter recycling-length validation, or class. This is a genuine
##   (if narrow) behavior change from real tibble for erglm's public
##   API: `erglm_predict()`/`simulate.erglm_model()` now return plain
##   data frames rather than `tbl_df` objects.
## - WARNING, discovered while integrating this mini into erglm:
##   `.table_tibble()`'s cross-column self-reference trick only resolves
##   a name against columns already built *within the same call*. A
##   value that references an ordinary local variable from the enclosing
##   function (e.g. `.table_tibble(model_tested = deparse(mod$formula))`
##   inside a function with a local `mod`) fails with "object ... not
##   found" -- real `tibble::tibble()` doesn't have this limitation,
##   since it evaluates its arguments as proper quosures carrying the
##   caller's environment, not via `match.call()` + a hand-built `envir`.
##   erglm's own `tibble::tibble()` call sites never actually needed the
##   self-reference feature (nothing referenced one column being built
##   from another within the same call), so they were rewritten as plain
##   base `data.frame(..., check.names = FALSE)` calls instead of routed
##   through `.table_tibble()`. Don't reach for `.table_tibble()` for a
##   new call site without checking this limitation doesn't bite.
## - Unlike real `tibble::add_row()`, `.table_add_row()`'s `...` only
##   accepts name-value pairs, not a whole pre-built row passed as a
##   single unnamed data frame argument (real `add_row()` special-cases
##   that via its own `tibble()` call). erglm's SCM history bookkeeping
##   uses plain `rbind()` instead, since each `history_row` already has
##   identical columns to `history`.
##
## Usage:
##   source("minitable.R")
##   .table_tibble(a = 1:3, b = a * 2)
##   .table_add_row(mtcars, mpg = 99)
##   .table_rownames_to_column(mtcars, var = "model")
##
## License: MIT (see LICENSE at the root of the minis repo). Logic
## adapted from poorman (MIT licensed), not copied from {tibble}.

.table_drop_dup_list <- function(x) {
  list_names <- names(x)
  if (identical(list_names, unique(list_names))) return(x)
  count <- table(list_names)
  dupes <- names(count[count > 1])
  uniques <- names(count[count == 1])
  to_drop <- do.call(c, lapply(dupes, function(nm) {
    matches <- which(list_names == nm)
    matches[-length(matches)]
  }))
  x[uniques] <- Filter(Negate(is.null), x[uniques])
  x[-to_drop]
}

# Build a data frame column by column, sequentially. Like
# `tibble::tibble()`, later columns can refer to earlier ones by name
# (`.table_tibble(a = 1:3, b = a * 2)`). Unlike a real tibble, the
# result is a plain `data.frame`, and recycling/type validation follows
# whatever `as.data.frame()` does, not tibble's stricter rules. `...`
# are name-value pairs of columns; `NULL`/length-0 values become `NA`,
# and unnamed arguments are named after their deparsed expression, as in
# `tibble::tibble()`. Does not tolerate a trailing comma after the last
# argument (unlike `tibble::tibble()`) -- it shows up as a genuine extra,
# unevaluated argument in `...`.
.table_tibble <- function(...) {
  fn_call <- match.call()
  list_to_eval <- as.list(fn_call)[-1]
  out <- vector(mode = "list", length = length(list_to_eval))
  names(out) <- names(list_to_eval)
  exprs <- lapply(substitute(list(...)), deparse)[-1]
  for (element in seq_along(list_to_eval)) {
    value <- list_to_eval[[element]]
    if (is.language(value)) {
      value <- eval(
        value,
        envir = if (element == 1L) list_to_eval else .table_drop_dup_list(out[seq_len(element - 1)])
      )
    }
    if (is.null(value)) {
      out[element] <- list(NULL)
    } else {
      out[[element]] <- value
    }
    invalid_name <- is.null(names(out)[element]) || is.na(names(out)[element]) || names(out)[element] == ""
    if (invalid_name) names(out)[element] <- exprs[[element]]
  }
  out <- lapply(out, function(x) if (is.null(x) || length(x) == 0) NA else x)
  as.data.frame(out, check.names = FALSE)
}

# Coerce `x` to a data frame via `as.data.frame(x, ..., check.names = FALSE)`.
.table_as_tibble <- function(x, ...) as.data.frame(x, ..., check.names = FALSE)

# Move row names into an explicit column named `var`. A no-op if `.data`
# has no row names, or only the default sequential ones ("1", "2", ...).
.table_rownames_to_column <- function(.data, var = "rowname") {
  rn <- rownames(.data)
  is_default_rn <- is.null(rn) || identical(rn, as.character(seq_len(NROW(.data))))
  if (is_default_rn) return(.data)
  rownames(.data) <- NULL
  df_col <- data.frame(rn)
  names(df_col) <- var
  cbind(df_col, .data)
}

# Append a single row to `.data`. `...` are name-value pairs matched to
# `.data`'s columns, supporting the same sequential cross-reference
# evaluation as `.table_tibble()`.
.table_add_row <- function(.data, ...) {
  fn_call <- match.call(expand.dots = FALSE)
  list_to_eval <- fn_call[["..."]]
  out <- vector(mode = "list", length = length(list_to_eval))
  names(out) <- names(list_to_eval)
  exprs <- lapply(substitute(list(...)), deparse)[-1]
  for (element in seq_along(list_to_eval)) {
    value <- list_to_eval[[element]]
    if (is.language(value)) {
      value <- eval(
        value,
        envir = if (element == 1L) list_to_eval else .table_drop_dup_list(out[seq_len(element - 1)])
      )
    }
    if (is.null(value)) {
      out[element] <- list(NULL)
    } else {
      out[[element]] <- value
    }
    invalid_name <- is.null(names(out)[element]) || is.na(names(out)[element]) || names(out)[element] == ""
    if (invalid_name) names(out)[element] <- exprs[[element]]
  }
  out <- lapply(out, function(x) if (is.null(x) || length(x) == 0) NA else x)
  new_row <- as.data.frame(out, check.names = FALSE)

  # rbind.data.frame() matches columns by name, but its error for a
  # mismatched column *set* ("numbers of columns of arguments do not
  # match") is a low-level, unhelpful surprise -- check for that directly
  # so a missing/extra/misspelled column name gets a message that actually
  # says which column is the problem.
  missing_cols <- setdiff(names(.data), names(new_row))
  extra_cols <- setdiff(names(new_row), names(.data))
  if (length(missing_cols) || length(extra_cols)) {
    stop(
      ".table_add_row(): new row's columns must exactly match `.data`'s. ",
      if (length(missing_cols)) paste0("Missing: ", paste(missing_cols, collapse = ", "), ". ") else "",
      if (length(extra_cols)) paste0("Unexpected: ", paste(extra_cols, collapse = ", "), ".") else ""
    )
  }
  new_row <- new_row[names(.data)]

  # rbind() otherwise silently upcasts an entire existing column's type to
  # accommodate one incompatible new value (e.g. one character value turns
  # a whole numeric column into character), with no warning at all -- catch
  # that here instead of letting it through silently. Integer/double are
  # treated as interchangeable, since combining them is ordinary and not
  # surprising to anyone.
  for (nm in names(.data)) {
    old_type <- typeof(.data[[nm]])
    new_type <- typeof(new_row[[nm]])
    both_numeric <- old_type %in% c("integer", "double") && new_type %in% c("integer", "double")
    if (!identical(old_type, new_type) && !both_numeric) {
      stop(
        ".table_add_row(): column `", nm, "` is `", old_type, "` in `.data` but `",
        new_type, "` in the new row; rbind() would silently coerce the whole ",
        "column to accommodate it. Convert the new value to `", old_type,
        "` first if this is intentional."
      )
    }
  }

  rbind(.data, new_row)
}
