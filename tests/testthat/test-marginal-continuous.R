test_that("marginal_continuous returns outputs/exclusions structure", {
  set.seed(1)
  df <- data.frame(x = rnorm(200))
  out <- marginal_continuous(df, "x")
  expect_type(out, "list")
  expect_named(out, c("outputs", "exclusions", ".var_name", ".summary_type"))
  expect_named(out$outputs,    c("hist", "hist_trimmed", "hist_log"))
  expect_named(out$exclusions, c("hist", "hist_trimmed", "hist_log"))
  expect_s3_class(out$outputs$hist,         "ggplot")
  expect_s3_class(out$outputs$hist_trimmed, "ggplot")
  expect_s3_class(out$outputs$hist_log,     "ggplot")
})

test_that("hist_trimmed caption contains smallest and largest values", {
  set.seed(1)
  df <- data.frame(x = rnorm(100))
  out <- marginal_continuous(df, "x")
  caption <- out$outputs$hist_trimmed$labels$caption
  expect_match(caption, "smallest observations")
  expect_match(caption, "largest observations")
})

test_that("hist_trimmed uses 2 significant figures in caption", {
  x <- c(seq(0.001234, 0.01234, length.out = 10), rep(5, 80), seq(100.56, 110.56, length.out = 10))
  out <- marginal_continuous(data.frame(x = x), "x")
  caption <- out$outputs$hist_trimmed$labels$caption
  expect_match(caption, "0.0012")
})

test_that("hist_trimmed is NULL with a message when <= 20 observations (trim_count)", {
  expect_message(
    out <- marginal_continuous(data.frame(x = 1:15), "x"),
    "hist_trimmed"
  )
  expect_null(out$outputs$hist_trimmed)
  expect_null(out$exclusions$hist_trimmed)
})

test_that("hist_trimmed data excludes the 10 smallest and 10 largest (trim_count)", {
  df <- data.frame(x = 1:100)
  out <- marginal_continuous(df, "x")
  trimmed_data <- out$outputs$hist_trimmed$data$x
  expect_true(all(trimmed_data >= 11))
  expect_true(all(trimmed_data <= 90))
})

test_that("exclusions$hist_trimmed contains excluded values (trim_count)", {
  df <- data.frame(x = 1:100)
  out <- marginal_continuous(df, "x")
  expect_length(out$exclusions$hist_trimmed, 20)  # 10 smallest + 10 largest
  expect_true(all(sort(out$exclusions$hist_trimmed) == c(1:10, 91:100)))
})

test_that("trim_percentile trims by quantile", {
  set.seed(1)
  df <- data.frame(x = 1:200)
  out <- marginal_continuous(df, "x", trim = "trim_percentile", trim_pct = 0.05)
  caption <- out$outputs$hist_trimmed$labels$caption
  expect_match(caption, "Bottom 5")
  expect_match(caption, "top 5")
  expect_false(is.null(out$exclusions$hist_trimmed))
})

test_that("NA count and percentage appear in caption on hist and hist_trimmed", {
  df <- data.frame(x = c(1:100, NA, NA, NA))
  out <- marginal_continuous(df, "x")
  expect_match(out$outputs$hist$labels$caption,         "Missing: 3")
  expect_match(out$outputs$hist$labels$caption,         "%")
  expect_match(out$outputs$hist_trimmed$labels$caption, "Missing: 3")
})

test_that("caption shows 0 missing when no NAs", {
  out <- marginal_continuous(data.frame(x = 1:100), "x")
  expect_match(out$outputs$hist$labels$caption, "Missing: 0")
})

test_that("marginal_continuous errors on non-numeric, non-parseable input", {
  expect_error(marginal_continuous(data.frame(x = letters), "x"), "numeric")
})

test_that("var_name is used as x-axis label", {
  df <- data.frame(my_score = rnorm(50))
  out <- marginal_continuous(df, "my_score")
  expect_equal(out$outputs$hist$labels$x, "my_score")
})

test_that("hist_log uses log10 scale", {
  df <- data.frame(x = exp(rnorm(100)))
  out <- marginal_continuous(df, "x")
  expect_s3_class(out$outputs$hist_log, "ggplot")
  scale_names <- sapply(out$outputs$hist_log$scales$scales, function(s) s$trans$name)
  expect_true(any(grepl("log", scale_names, ignore.case = TRUE)))
})

test_that("hist_log is NULL with message when all values are non-positive", {
  df <- data.frame(x = c(-5, -3, -1, 0))
  expect_message(
    out <- marginal_continuous(df, "x"),
    "hist_log"
  )
  expect_null(out$outputs$hist_log)
})

test_that("hist_log caption notes excluded non-positive values", {
  df <- data.frame(x = c(-1, 0, 1:98))
  out <- marginal_continuous(df, "x")
  expect_match(out$outputs$hist_log$labels$caption, "non-positive")
})

test_that(".summary_type is continuous_marginal", {
  out <- marginal_continuous(data.frame(x = 1:50), "x")
  expect_equal(out$.summary_type, "continuous_marginal")
  expect_equal(out$.var_name, "x")
})

test_that("exclusions$hist and exclusions$hist_log are always NULL", {
  out <- marginal_continuous(data.frame(x = 1:100), "x")
  expect_null(out$exclusions$hist)
  expect_null(out$exclusions$hist_log)
})
