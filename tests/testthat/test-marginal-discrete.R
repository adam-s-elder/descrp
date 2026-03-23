test_that("marginal_discrete returns list with table and table_collapsed", {
  x <- sample(letters[1:5], 100, replace = TRUE)
  out <- marginal_discrete(x)
  expect_type(out, "list")
  expect_named(out, c("table", "table_collapsed"))
})

test_that("table_collapsed is NULL when <= 10 categories", {
  x <- sample(letters[1:5], 100, replace = TRUE)
  out <- marginal_discrete(x)
  expect_null(out$table_collapsed)
})

test_that("table_collapsed is NULL when <= 10 non-NA categories (exactly 10)", {
  # 10 non-NA categories + always-present NA row = 11 table rows, but
  # collapse threshold is based on non-NA category count only
  x <- sample(letters[1:10], 200, replace = TRUE)
  out <- marginal_discrete(x)
  expect_null(out$table_collapsed)
})

test_that("table_collapsed is produced when > 10 categories", {
  set.seed(1)
  x <- sample(letters, 500, replace = TRUE)
  out <- marginal_discrete(x)
  expect_s3_class(out$table, "knitr_kable")
  expect_s3_class(out$table_collapsed, "knitr_kable")
})

test_that("table_collapsed contains '< 5%' row", {
  set.seed(1)
  x <- sample(letters, 500, replace = TRUE)
  out <- marginal_discrete(x)
  tbl_text <- paste(out$table_collapsed, collapse = "\n")
  expect_match(tbl_text, "< 5%")
})

test_that("table_collapsed is NULL when no category is < 5% individually", {
  # All 26 letters with equal frequency -> each is ~3.8% so all collapse
  # Actually that would collapse. Make one dominant category.
  # Force: 11 categories each exactly at 5% or above -> none collapse
  x <- c(rep("a", 500), rep("b", 100), rep("c", 100), rep("d", 100),
         rep("e", 100), rep("f", 100), rep("g", 100), rep("h", 100),
         rep("i", 100), rep("j", 100), rep("k", 100))
  # each minority cat is 100/1500 = 6.7%, all >= 5%
  out <- marginal_discrete(x)
  expect_null(out$table_collapsed)
})

test_that("counts are comma-formatted for large numbers", {
  x <- c(rep("A", 12345), rep("B", 6789), rep("C", 1000),
         rep("D", 500), rep("E", 400), rep("F", 300), rep("G", 200),
         rep("H", 100), rep("I", 50), rep("J", 30), rep("K", 20))
  out <- marginal_discrete(x)
  tbl_text <- paste(out$table, collapse = "\n")
  expect_match(tbl_text, "12,345")
})

test_that("NA row is always present, even with zero missing values", {
  x <- c("a", "b", "c")
  out <- marginal_discrete(x)
  tbl_text <- paste(out$table, collapse = "\n")
  expect_match(tbl_text, "NA")
})

test_that("NA row shows correct count when NAs are present", {
  x <- c("a", "b", NA, NA, NA)
  out <- marginal_discrete(x)
  tbl_text <- paste(out$table, collapse = "\n")
  expect_match(tbl_text, "NA")
  expect_match(tbl_text, "60%")  # 3/5 = 60%
})

test_that("NA row is never collapsed into '< 5%'", {
  # Build > 10 categories with NAs that individually are < 5%
  set.seed(1)
  x <- c(sample(letters, 500, replace = TRUE), rep(NA, 5))
  out <- marginal_discrete(x)
  tbl_text <- paste(out$table_collapsed, collapse = "\n")
  # NA row should still appear explicitly, not be swallowed by "< 5%"
  expect_match(tbl_text, "\\| NA ")
})

test_that("var_name is used as the category column header", {
  x <- sample(c("yes", "no"), 50, replace = TRUE)
  out <- marginal_discrete(x, var_name = "Response")
  tbl_text <- paste(out$table, collapse = "\n")
  expect_match(tbl_text, "Response")
})

test_that("collapsed table only collapses categories individually < 5%", {
  # category "m" has exactly 5% — should NOT be collapsed
  set.seed(42)
  base <- sample(letters[1:20], 1900, replace = TRUE)
  # Add one category at exactly 5% of 2000 = 100 obs
  x <- c(base, rep("z_exactly5pct", 100))
  out <- marginal_discrete(x)
  tbl_text <- paste(out$table_collapsed, collapse = "\n")
  # "z_exactly5pct" at exactly 5% should remain as its own row
  expect_match(tbl_text, "z_exactly5pct")
})
