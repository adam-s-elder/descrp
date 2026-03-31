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

test_that("trim = NULL omits hist_trimmed", {
  df <- data.frame(x = 1:100)
  out <- marginal_continuous(df, "x", trim = NULL)
  expect_null(out$outputs$hist_trimmed)
  expect_null(out$exclusions$hist_trimmed)
})

test_that("trim_count(10) excludes 10 smallest and 10 largest", {
  df <- data.frame(x = 1:100)
  out <- marginal_continuous(df, "x", trim = trim_count(10))
  trimmed_data <- out$outputs$hist_trimmed$data$x
  expect_true(all(trimmed_data >= 11))
  expect_true(all(trimmed_data <= 90))
})

test_that("exclusions$hist_trimmed contains the trimmed values (trim_count)", {
  df <- data.frame(x = 1:100)
  out <- marginal_continuous(df, "x", trim = trim_count(10))
  expect_length(out$exclusions$hist_trimmed, 20)
  expect_true(all(sort(out$exclusions$hist_trimmed) == c(1:10, 91:100)))
})

test_that("hist_trimmed caption reports count trimmed and kept", {
  df <- data.frame(x = 1:100)
  out <- marginal_continuous(df, "x", trim = trim_count(10))
  caption <- out$outputs$hist_trimmed$labels$caption
  expect_match(caption, "20 observation")
  expect_match(caption, "80 kept")
})

test_that("trim_quantile trims by quantile", {
  df <- data.frame(x = 1:200)
  out <- marginal_continuous(df, "x", trim = trim_quantile(0.05))
  expect_s3_class(out$outputs$hist_trimmed, "ggplot")
  expect_false(is.null(out$exclusions$hist_trimmed))
  # Boundary values at positions k and n-k+1 are kept; k = floor(200*0.05) = 10
  # so 9 trimmed from each tail = 18 total
  expect_length(out$exclusions$hist_trimmed, 18)
})

test_that("hist_trimmed is NULL with a message when trimming removes all", {
  # All values are identical -> trim_count marks all as trim when n >= half
  expect_message(
    out <- marginal_continuous(data.frame(x = rep(1, 5)), "x",
                               trim = trim_count(10)),
    "trimming removed all"
  )
  expect_null(out$outputs$hist_trimmed)
  expect_null(out$exclusions$hist_trimmed)
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

test_that("trim_count functional returns a function", {
  f <- trim_count(5)
  expect_type(f, "closure")
  result <- f(1:20)
  expect_length(result, 20)
  expect_equal(sum(result), 10)  # 5 smallest + 5 largest
  expect_true(all(result[1:5] == 1L))
  expect_true(all(result[16:20] == 1L))
  expect_true(all(result[6:15] == 0L))
})

test_that("trim_quantile functional returns a function", {
  f <- trim_quantile(0.1)
  expect_type(f, "closure")
  result <- f(1:100)
  expect_length(result, 100)
  expect_equal(sum(result == 1L), 18)  # values < q10 or > q90
})

test_that("trim_count ignores NAs in ordering", {
  f <- trim_count(2)
  x <- c(NA, 1, 2, 3, 4, 5, NA)
  result <- f(x)
  expect_length(result, 7)
  expect_equal(result[is.na(x)], c(0L, 0L))  # NAs untouched
  # 2 smallest non-NA: positions 2,3; 2 largest: positions 5,6
  expect_equal(result[2], 1L)
  expect_equal(result[3], 1L)
  expect_equal(result[5], 1L)
  expect_equal(result[6], 1L)
})
