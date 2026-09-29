## Ported from djnavarro/minis' minitable/tests/testthat/test-minitable.R.
## No source() call needed here (unlike upstream): .table_*() live in
## erglm's own R/minitable.R and are already part of the package's
## namespace when tests run. erglm's own code only actually calls
## .table_as_tibble() (see R/minitable.R's header comment for why
## .table_tibble()/.table_add_row() don't fit erglm's call sites) --
## the rest are tested here anyway for parity with upstream.

test_that(".table_tibble() builds a data.frame from name-value pairs", {
  out <- .table_tibble(a = 1:3, b = c("x", "y", "z"))
  expect_s3_class(out, "data.frame")
  expect_equal(out$a, 1:3)
  expect_equal(out$b, c("x", "y", "z"))
})

test_that(".table_tibble() supports cross-column references", {
  out <- .table_tibble(a = 1:3, b = a * 2, c = a + b)
  expect_equal(out$b, c(2, 4, 6))
  expect_equal(out$c, c(3, 6, 9))
})

test_that(".table_tibble() names unnamed columns after their expression", {
  out <- .table_tibble(1:2, letters[1:2])
  expect_equal(names(out), c("1:2", "letters[1:2]"))
})

test_that(".table_tibble() converts NULL/length-0 columns to NA", {
  out <- .table_tibble(a = 1, b = NULL)
  expect_true(is.na(out$b))
})

test_that(".table_as_tibble() coerces to a plain data.frame", {
  out <- .table_as_tibble(matrix(1:4, nrow = 2))
  expect_s3_class(out, "data.frame")
  expect_equal(dim(out), c(2, 2))
})

test_that(".table_rownames_to_column() moves custom row names into a column", {
  df <- data.frame(x = 1:2, row.names = c("r1", "r2"))
  out <- .table_rownames_to_column(df, var = "id")
  expect_equal(out$id, c("r1", "r2"))
  expect_equal(rownames(out), c("1", "2"))
})

test_that(".table_rownames_to_column() is a no-op for default sequential row names", {
  df <- data.frame(x = 1:2)
  out <- .table_rownames_to_column(df, var = "id")
  expect_false("id" %in% names(out))
  expect_equal(out, df)
})

test_that(".table_add_row() appends a row matched by column name", {
  df <- data.frame(x = 1:2, y = c("a", "b"), stringsAsFactors = FALSE)
  out <- .table_add_row(df, x = 3, y = "c")
  expect_equal(nrow(out), 3)
  expect_equal(out$x, c(1, 2, 3))
  expect_equal(out$y, c("a", "b", "c"))
})

test_that(".table_add_row() supports cross-column references in the new row", {
  df <- data.frame(x = 1:2, y = c(2, 4))
  out <- .table_add_row(df, x = 3, y = x * 2)
  expect_equal(out$x, c(1, 2, 3))
  expect_equal(out$y, c(2, 4, 6))
})

test_that(".table_add_row() errors clearly on a missing or unexpected column, instead of an rbind-level error", {
  df <- data.frame(x = 1:2, y = 3:4)
  expect_error(.table_add_row(df, x = 5), "Missing: y")
  expect_error(.table_add_row(df, x = 5, y = 6, z = 9), "Unexpected: z")
})

test_that(".table_add_row() works when named args are supplied out of order", {
  df <- data.frame(x = 1:2, y = 3:4)
  out <- .table_add_row(df, y = 6, x = 5)
  expect_equal(out$x, c(1, 2, 5))
  expect_equal(out$y, c(3, 4, 6))
})

test_that(".table_add_row() errors instead of silently upcasting an existing column's type", {
  df <- data.frame(x = 1:2, y = c(10, 20))
  expect_error(.table_add_row(df, x = "a", y = 30), "column `x`.*integer.*character")
})

test_that(".table_add_row() allows integer/double columns to mix without erroring", {
  df <- data.frame(x = 1:2)
  out <- .table_add_row(df, x = 3.5)
  expect_equal(out$x, c(1, 2, 3.5))
})
