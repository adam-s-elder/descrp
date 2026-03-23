test_that("marginal_continuous returns a list with hist, hist_trimmed, hist_log", {
  set.seed(1)
  df <- data.frame(x = rnorm(200))
  out <- marginal_continuous(df, "x")
  expect_type(out, "list")
  expect_named(out, c("hist", "hist_trimmed", "hist_log", ".var_name", ".summary_type"))
  expect_s3_class(out$hist, "ggplot")
  expect_s3_class(out$hist_trimmed, "ggplot")
  expect_s3_class(out$hist_log, "ggplot")
})

test_that("hist_trimmed caption contains smallest and largest values", {
  set.seed(1)
  df <- data.frame(x = rnorm(100))
  out <- marginal_continuous(df, "x")
  caption <- out$hist_trimmed$labels$caption
  expect_match(caption, "ten smallest observations")
  expect_match(caption, "ten largest observations")
})

test_that("hist_trimmed uses 2 significant figures in caption", {
  x <- c(seq(0.001234, 0.01234, length.out = 10), rep(5, 80), seq(100.56, 110.56, length.out = 10))
  out <- marginal_continuous(data.frame(x = x), "x")
  caption <- out$hist_trimmed$labels$caption
  # signif(0.001234, 2) == 0.0012
  expect_match(caption, "0.0012")
})

test_that("hist_trimmed is NULL with a message when <= 20 observations", {
  expect_message(
    out <- marginal_continuous(data.frame(x = 1:15), "x"),
    "hist_trimmed"
  )
  expect_null(out$hist_trimmed)
})

test_that("hist_trimmed data excludes the 10 smallest and 10 largest", {
  df <- data.frame(x = 1:100)
  out <- marginal_continuous(df, "x")
  trimmed_data <- out$hist_trimmed$data$x
  expect_true(all(trimmed_data >= 11))
  expect_true(all(trimmed_data <= 90))
})

test_that("NA count and percentage appear in caption on hist and hist_trimmed", {
  df <- data.frame(x = c(1:100, NA, NA, NA))
  out <- marginal_continuous(df, "x")
  expect_match(out$hist$labels$caption, "Missing: 3")
  expect_match(out$hist$labels$caption, "%")
  expect_match(out$hist_trimmed$labels$caption, "Missing: 3")
})

test_that("caption shows 0 missing when no NAs", {
  out <- marginal_continuous(data.frame(x = 1:100), "x")
  expect_match(out$hist$labels$caption, "Missing: 0")
})

test_that("marginal_continuous errors on non-numeric, non-parseable input", {
  expect_error(marginal_continuous(data.frame(x = letters), "x"), "numeric")
})

test_that("var_name is used as x-axis label", {
  df <- data.frame(my_score = rnorm(50))
  out <- marginal_continuous(df, "my_score")
  expect_equal(out$hist$labels$x, "my_score")
})

test_that("hist_log uses log10 scale", {
  df <- data.frame(x = exp(rnorm(100)))
  out <- marginal_continuous(df, "x")
  expect_s3_class(out$hist_log, "ggplot")
  # scale_x_log10 creates a ScaleContinuousPosition with trans "log-10"
  scale_names <- sapply(out$hist_log$scales$scales, function(s) s$trans$name)
  expect_true(any(grepl("log", scale_names, ignore.case = TRUE)))
})

test_that("hist_log is NULL with message when all values are non-positive", {
  df <- data.frame(x = c(-5, -3, -1, 0))
  expect_message(
    out <- marginal_continuous(df, "x"),
    "hist_log"
  )
  expect_null(out$hist_log)
})

test_that("hist_log caption notes excluded non-positive values", {
  df <- data.frame(x = c(-1, 0, 1:98))
  out <- marginal_continuous(df, "x")
  expect_match(out$hist_log$labels$caption, "non-positive")
})

test_that(".summary_type is continuous_marginal", {
  out <- marginal_continuous(data.frame(x = 1:50), "x")
  expect_equal(out$.summary_type, "continuous_marginal")
  expect_equal(out$.var_name, "x")
})
