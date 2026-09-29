## Ported from djnavarro/minis' miniverb/tests/testthat/test-miniverb.R.
## No source() call needed here (unlike upstream): .verb_*() live in
## erglm's own R/miniverb.R and are already part of the package's
## namespace when tests run. erglm's own code only actually calls
## .verb_mutate() -- the rest are tested here anyway for parity with
## upstream.

df <- data.frame(
  x = c(1, 2, 3, 4, NA),
  g = c("a", "a", "b", "b", "a"),
  stringsAsFactors = FALSE
)

## .verb_filter() --------------------------------------------------------

test_that(".verb_filter() keeps rows matching a single condition", {
  out <- .verb_filter(df, g == "a")
  expect_equal(out$x, c(1, 2, NA))
})

test_that(".verb_filter() combines multiple conditions with AND", {
  out <- .verb_filter(df, g == "a", x > 1)
  expect_equal(out$x, 2)
})

test_that(".verb_filter() drops rows where the condition is NA", {
  out <- .verb_filter(df, x > 1)
  expect_equal(out$x, c(2, 3, 4))
})

test_that(".verb_filter() with no conditions returns .data unchanged", {
  expect_equal(.verb_filter(df), df)
})

test_that(".verb_filter() evaluates conditions against .data's columns, not just the caller's", {
  threshold <- 2
  out <- .verb_filter(df, x > threshold)
  expect_equal(out$x, c(3, 4))
})

test_that(".verb_filter() with `.by` evaluates conditions per group", {
  gdf <- data.frame(g = c("a", "a", "b", "b"), x = c(1, 2, 3, 4))
  out <- .verb_filter(gdf, x > mean(x), .by = "g")
  expect_equal(out$x, c(2, 4))
})

test_that(".verb_filter() rejects a non-character `.by`", {
  expect_error(.verb_filter(df, x > 1, .by = 1), "character vector")
})

## .verb_select() ---------------------------------------------------------

sdf <- data.frame(a = 1:3, b = 4:6, c = 7:9)

test_that(".verb_select() keeps columns by bare name, in the order given", {
  out <- .verb_select(sdf, c, a)
  expect_equal(names(out), c("c", "a"))
})

test_that(".verb_select() renames with `new = old`", {
  out <- .verb_select(sdf, first = a)
  expect_equal(names(out), "first")
  expect_equal(out$first, sdf$a)
})

test_that(".verb_select() keeps columns by integer position", {
  out <- .verb_select(sdf, 1, 3)
  expect_equal(names(out), c("a", "c"))
})

test_that(".verb_select() drops columns with `-`", {
  out <- .verb_select(sdf, -b)
  expect_equal(names(out), c("a", "c"))
})

test_that(".verb_select() drops columns by negative position", {
  out <- .verb_select(sdf, -1)
  expect_equal(names(out), c("b", "c"))
})

test_that(".verb_select() errors when mixing inclusion and exclusion", {
  expect_error(.verb_select(sdf, a, -b), "cannot mix")
})

## .verb_mutate() ----------------------------------------------------------

test_that(".verb_mutate() adds a new column", {
  out <- .verb_mutate(sdf, d = a + b)
  expect_equal(out$d, sdf$a + sdf$b)
})

test_that(".verb_mutate() sees earlier columns added in the same call", {
  out <- .verb_mutate(sdf, d = a + b, e = d * 2)
  expect_equal(out$e, (sdf$a + sdf$b) * 2)
})

test_that(".verb_mutate() recycles a length-1 result", {
  out <- .verb_mutate(sdf, z = 1)
  expect_equal(out$z, rep(1, nrow(sdf)))
})

test_that(".verb_mutate() with `.by` evaluates per group", {
  gdf <- data.frame(g = c("a", "a", "b", "b"), x = c(1, 2, 3, 4))
  out <- .verb_mutate(gdf, centered = x - mean(x), .by = "g")
  expect_equal(out$centered, c(-0.5, 0.5, -0.5, 0.5))
})

test_that(".verb_mutate() with `.by` computes ifelse() correctly when a group's condition is not uniform (mixes TRUE/FALSE within the same group)", {
  # Contrast with `.make_erglm_data()`'s `weight` column (grouped `.by = "sex"`),
  # where the condition IS uniform within each group -- the exact pattern that
  # trips up base ifelse()'s short-circuit evaluation of its branches (see
  # AGENTS.md/HISTORY.md). Here the condition mixes TRUE/FALSE within every
  # group, so this exercises `.verb_mutate()`'s own per-group recycling/
  # assignment logic without that separate ifelse() hazard confounding it.
  gdf <- data.frame(g = rep(c("a", "b", "c"), each = 4), x = 1:12)
  out <- .verb_mutate(gdf, y = ifelse(x > mean(x), "hi", "lo"), .by = "g")
  expect_equal(
    out$y,
    rep(c("lo", "lo", "hi", "hi"), times = 3)
  )
})

test_that(".verb_mutate() requires named arguments", {
  expect_error(.verb_mutate(sdf, a + b), "must be named")
})

## .verb_arrange() ---------------------------------------------------------

adf <- data.frame(g = c("b", "a", "a"), x = c(1, 3, 2))

test_that(".verb_arrange() sorts ascending by default", {
  out <- .verb_arrange(adf, x)
  expect_equal(out$x, c(1, 2, 3))
})

test_that(".verb_arrange() sorts descending with .verb_desc()", {
  out <- .verb_arrange(adf, .verb_desc(x))
  expect_equal(out$x, c(3, 2, 1))
})

test_that(".verb_arrange() breaks ties with later arguments", {
  out <- .verb_arrange(adf, g, x)
  expect_equal(out$g, c("a", "a", "b"))
  expect_equal(out$x, c(2, 3, 1))
})

test_that(".verb_arrange() sorts NA last regardless of direction", {
  ndf <- data.frame(x = c(2, NA, 1))
  expect_equal(.verb_arrange(ndf, x)$x, c(1, 2, NA))
  expect_equal(.verb_arrange(ndf, .verb_desc(x))$x, c(2, 1, NA))
})

test_that(".verb_arrange() with no arguments returns .data unchanged", {
  expect_equal(.verb_arrange(adf), adf)
})

## .verb_summarise() --------------------------------------------------------

test_that(".verb_summarise() with no `.by` returns one overall row", {
  out <- .verb_summarise(sdf, total = sum(a))
  expect_equal(out$total, sum(sdf$a))
  expect_equal(nrow(out), 1L)
})

test_that(".verb_summarise() with `.by` returns one row per group", {
  gdf <- data.frame(g = c("a", "a", "b", "b"), x = c(1, 2, 3, 4))
  out <- .verb_summarise(gdf, mean_x = mean(x), .by = "g")
  expect_equal(out$g, c("a", "b"))
  expect_equal(out$mean_x, c(1.5, 3.5))
})

test_that(".verb_summarise()/.verb_mutate()'s `.by` groups rows in first-appearance order, not sorted key order", {
  gdf <- data.frame(g = c("b", "a", "b", "a"), x = c(1, 2, 3, 4))
  out_s <- .verb_summarise(gdf, s = sum(x), .by = "g")
  expect_equal(out_s$g, c("b", "a"))
  expect_equal(out_s$s, c(4, 6))

  out_m <- .verb_mutate(gdf, centered = x - mean(x), .by = "g")
  expect_equal(out_m$g, gdf$g)
  expect_equal(out_m$centered, c(-1, -1, 1, 1))
})

test_that(".verb_summarise() errors when a summary name collides with a `.by` grouping column", {
  gdf <- data.frame(g = c("a", "a", "b", "b"), x = c(1, 2, 3, 4))
  expect_error(.verb_summarise(gdf, g = sum(x), .by = "g"), "collide")
})

test_that(".verb_summarise() requires named arguments", {
  expect_error(.verb_summarise(sdf, sum(a)), "must be named")
})

test_that(".verb_summarise() errors when an expression isn't scalar per group", {
  expect_error(.verb_summarise(sdf, all_a = a), "single value per group")
})
