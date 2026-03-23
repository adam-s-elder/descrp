test_that("marginal_continuous returns a list with hist and hist_trimmed", {
  set.seed(1)
  out <- marginal_continuous(rnorm(200))
  expect_type(out, "list")
  expect_named(out, c("hist", "hist_trimmed"))
  expect_s3_class(out$hist, "ggplot")
  expect_s3_class(out$hist_trimmed, "ggplot")
})

test_that("hist_trimmed caption contains smallest and largest values", {
  set.seed(1)
  x <- rnorm(100)
  out <- marginal_continuous(x, var_name = "x")
  caption <- out$hist_trimmed$labels$caption
  expect_match(caption, "ten smallest observations")
  expect_match(caption, "ten largest observations")
})

test_that("hist_trimmed uses 2 significant figures in caption", {
  x <- c(seq(0.001234, 0.01234, length.out = 10), rep(5, 80), seq(100.56, 110.56, length.out = 10))
  out <- marginal_continuous(x)
  caption <- out$hist_trimmed$labels$caption
  # signif(0.001234, 2) == 0.0012
  expect_match(caption, "0.0012")
})

test_that("hist_trimmed is NULL with a message when <= 20 observations", {
  expect_message(
    out <- marginal_continuous(1:15),
    "hist_trimmed"
  )
  expect_null(out$hist_trimmed)
})

test_that("hist_trimmed data excludes the 10 smallest and 10 largest", {
  x <- 1:100
  out <- marginal_continuous(x)
  trimmed_data <- out$hist_trimmed$data$x
  expect_true(all(trimmed_data >= 11))
  expect_true(all(trimmed_data <= 90))
})

test_that("marginal_continuous warns on NA values", {
  x <- c(1:50, NA, NA)
  expect_warning(
    out <- marginal_continuous(x),
    "2 NA"
  )
  expect_s3_class(out$hist, "ggplot")
})

test_that("marginal_continuous errors on non-numeric input", {
  expect_error(marginal_continuous(letters), "numeric")
})

test_that("var_name is used as x-axis label", {
  out <- marginal_continuous(rnorm(50), var_name = "My Variable")
  expect_equal(out$hist$labels$x, "My Variable")
})
