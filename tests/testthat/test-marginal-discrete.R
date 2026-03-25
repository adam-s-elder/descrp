test_that("marginal_discrete returns outputs/exclusions structure", {
  df <- data.frame(x = sample(letters[1:5], 100, replace = TRUE))
  out <- marginal_discrete(df, "x")
  expect_type(out, "list")
  expect_named(out, c("outputs", "exclusions", ".var_name", ".summary_type"))
  expect_named(out$outputs,    c("table", "table_collapsed"))
  expect_named(out$exclusions, c("table", "table_collapsed"))
})

test_that("table_collapsed is NULL when <= 10 categories", {
  df <- data.frame(x = sample(letters[1:5], 100, replace = TRUE))
  out <- marginal_discrete(df, "x")
  expect_null(out$outputs$table_collapsed)
})

test_that("table_collapsed is NULL when <= 10 non-NA categories (exactly 10)", {
  df <- data.frame(x = sample(letters[1:10], 200, replace = TRUE))
  out <- marginal_discrete(df, "x")
  expect_null(out$outputs$table_collapsed)
})

test_that("table_collapsed is produced when > 10 categories", {
  set.seed(1)
  df <- data.frame(x = sample(letters, 500, replace = TRUE))
  out <- marginal_discrete(df, "x")
  expect_s3_class(out$outputs$table,           "knitr_kable")
  expect_s3_class(out$outputs$table_collapsed, "knitr_kable")
})

test_that("table_collapsed contains '< 5%' row", {
  set.seed(1)
  df <- data.frame(x = sample(letters, 500, replace = TRUE))
  out <- marginal_discrete(df, "x")
  tbl_text <- paste(out$outputs$table_collapsed, collapse = "\n")
  expect_match(tbl_text, "< 5%")
})

test_that("table_collapsed is NULL when no category is < 5% individually", {
  x <- c(rep("a", 500), rep("b", 100), rep("c", 100), rep("d", 100),
         rep("e", 100), rep("f", 100), rep("g", 100), rep("h", 100),
         rep("i", 100), rep("j", 100), rep("k", 100))
  out <- marginal_discrete(data.frame(x = x), "x")
  expect_null(out$outputs$table_collapsed)
})

test_that("counts are comma-formatted for large numbers", {
  x <- c(rep("A", 12345), rep("B", 6789), rep("C", 1000),
         rep("D", 500), rep("E", 400), rep("F", 300), rep("G", 200),
         rep("H", 100), rep("I", 50), rep("J", 30), rep("K", 20))
  out <- marginal_discrete(data.frame(x = x), "x")
  tbl_text <- paste(out$outputs$table, collapse = "\n")
  expect_match(tbl_text, "12,345")
})

test_that("NA row is always present, even with zero missing values", {
  df <- data.frame(x = c("a", "b", "c"))
  out <- marginal_discrete(df, "x")
  tbl_text <- paste(out$outputs$table, collapse = "\n")
  expect_match(tbl_text, "NA")
})

test_that("NA row shows correct count when NAs are present", {
  df <- data.frame(x = c("a", "b", NA, NA, NA))
  out <- marginal_discrete(df, "x")
  tbl_text <- paste(out$outputs$table, collapse = "\n")
  expect_match(tbl_text, "NA")
  expect_match(tbl_text, "60%")  # 3/5 = 60%
})

test_that("NA row is never collapsed into '< 5%'", {
  set.seed(1)
  x <- c(sample(letters, 500, replace = TRUE), rep(NA, 5))
  out <- marginal_discrete(data.frame(x = x), "x")
  tbl_text <- paste(out$outputs$table_collapsed, collapse = "\n")
  expect_match(tbl_text, "\\| NA ")
})

test_that("var_name is used as the category column header", {
  df <- data.frame(response = sample(c("yes", "no"), 50, replace = TRUE))
  out <- marginal_discrete(df, "response")
  tbl_text <- paste(out$outputs$table, collapse = "\n")
  expect_match(tbl_text, "response")
})

test_that("collapsed table only collapses categories individually < 5%", {
  set.seed(42)
  base <- sample(letters[1:20], 1900, replace = TRUE)
  x <- c(base, rep("z_exactly5pct", 100))
  out <- marginal_discrete(data.frame(x = x), "x")
  tbl_text <- paste(out$outputs$table_collapsed, collapse = "\n")
  expect_match(tbl_text, "z_exactly5pct")
})

test_that(".summary_type is discrete_marginal", {
  df <- data.frame(x = c("a", "b", "c"))
  out <- marginal_discrete(df, "x")
  expect_equal(out$.summary_type, "discrete_marginal")
  expect_equal(out$.var_name, "x")
})

test_that("all exclusions are NULL for discrete marginal", {
  df <- data.frame(x = sample(letters, 500, replace = TRUE))
  out <- marginal_discrete(df, "x")
  expect_null(out$exclusions$table)
  expect_null(out$exclusions$table_collapsed)
})
