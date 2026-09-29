## Ported from djnavarro/minis' minijoin/tests/testthat/test-minijoin.R.
## No source() call needed here (unlike upstream): .join_*() live in
## erglm's own R/minijoin.R and are already part of the package's
## namespace when tests run. erglm's own code only actually calls
## .join_left_join() -- the rest are tested here anyway for parity with
## upstream.

bands <- data.frame(
  band = c("Beatles", "Who", "Kinks"),
  founded = c(1960, 1964, 1963),
  stringsAsFactors = FALSE
)
albums <- data.frame(
  band = c("Beatles", "Pink Floyd"),
  album = c("Abbey Road", "The Wall"),
  stringsAsFactors = FALSE
)

test_that(".join_inner_join() keeps only rows matching on both sides", {
  out <- .join_inner_join(bands, albums, by = "band")
  expect_equal(nrow(out), 1)
  expect_equal(out$band, "Beatles")
  expect_equal(out$album, "Abbey Road")
})

test_that(".join_left_join() keeps all rows of x, filling unmatched with NA, in x's order", {
  out <- .join_left_join(bands, albums, by = "band")
  expect_equal(nrow(out), 3)
  expect_equal(out$band, bands$band)
  expect_true(is.na(out$album[out$band == "Who"]))
})

test_that(".join_right_join() keeps all rows of y, filling unmatched with NA", {
  out <- .join_right_join(bands, albums, by = "band")
  expect_equal(nrow(out), 2)
  expect_true(any(is.na(out$founded)))
})

test_that(".join_full_join() keeps all rows from both sides", {
  out <- .join_full_join(bands, albums, by = "band")
  expect_equal(nrow(out), 4)
})

test_that("joins support a named `by` for differently-named join columns", {
  albums2 <- data.frame(artist = c("Beatles", "Pink Floyd"), album = c("Abbey Road", "The Wall"))
  out <- .join_inner_join(bands, albums2, by = c("band" = "artist"))
  expect_equal(out$album, "Abbey Road")
})

test_that("keep = TRUE preserves both join columns, suffixed", {
  out <- .join_left_join(bands, albums, by = "band", keep = TRUE)
  expect_true(all(c("band.x", "band.y") %in% names(out)))
  # No redundant, unsuffixed third copy of the join column left behind.
  expect_false("band" %in% names(out))
})

test_that("keep = TRUE's y-side column is NA for rows with no actual match in y, not a copy of x's value", {
  out <- .join_left_join(bands, albums, by = "band", keep = TRUE)
  expect_true(is.na(out$band.y[out$band.x == "Who"]))
  expect_equal(out$band.y[out$band.x == "Beatles"], "Beatles")
})

test_that("keep = TRUE's x-side column is NA for a right-join row with no match in x", {
  out <- .join_right_join(bands, albums, by = "band", keep = TRUE)
  expect_true(is.na(out$band.x[out$band.y == "Pink Floyd"]))
  expect_equal(out$band.x[out$band.y == "Beatles"], "Beatles")
})

test_that("keep = TRUE works with a renamed `by` instead of crashing", {
  albums2 <- data.frame(artist = c("Beatles", "Pink Floyd"), album = c("Abbey Road", "The Wall"))
  out <- .join_left_join(bands, albums2, by = c("band" = "artist"), keep = TRUE)
  expect_true(all(c("band.x", "band.y") %in% names(out)))
  expect_equal(out$band.x, bands$band)
})

test_that("na_matches = 'never' excludes NA-keyed rows from matching", {
  x <- data.frame(id = c(1, NA), val = c("a", "b"))
  y <- data.frame(id = c(1, NA), val2 = c("A", "B"))
  out_na <- .join_inner_join(x, y, by = "id", na_matches = "na")
  out_never <- .join_inner_join(x, y, by = "id", na_matches = "never")
  expect_equal(nrow(out_na), 2)
  expect_equal(nrow(out_never), 1)
})
