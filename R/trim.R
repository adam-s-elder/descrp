#' Trim the n smallest and n largest observations
#'
#' Returns a trimming function that marks the `n` smallest and `n` largest
#' values in a numeric vector as trimmed. Designed to be passed to the `trim`
#' argument of continuous summary functions (e.g. [marginal_continuous()]).
#'
#' Like [ggplot2::scale_x_continuous()], calling `trim_count()` returns a
#' function; that function is what the summary functions invoke on the data.
#'
#' @param n Integer. Number of observations to remove from each tail. Default
#'   `10L`.
#'
#' @return A function that accepts a numeric vector and returns an integer
#'   vector of the same length with `0` for values to keep and `1` for values
#'   to trim.
#'
#' @examples
#' f <- trim_count(5)
#' f(1:20)  # positions 1-5 and 16-20 get 1
#'
#' df <- data.frame(x = rnorm(200))
#' out <- marginal_continuous(df, "x", trim = trim_count(10))
#'
#' @export
trim_count <- function(n = 10L) {
  n <- as.integer(n)
  function(x) {
    result <- integer(length(x))
    ord <- order(x, na.last = NA)   # indices of non-NA values in sorted order
    n_valid <- length(ord)
    if (n_valid > 0L && n > 0L) {
      trim_idx <- unique(c(
        ord[seq_len(min(n, n_valid))],
        ord[seq(max(1L, n_valid - n + 1L), n_valid)]
      ))
      result[trim_idx] <- 1L
    }
    result
  }
}

#' Trim values outside a quantile range
#'
#' Returns a trimming function that marks values below the `pct` quantile or
#' above the `1 - pct` quantile as trimmed. Designed to be passed to the
#' `trim` argument of continuous summary functions (e.g.
#' [marginal_continuous()]).
#'
#' Like [ggplot2::scale_x_continuous()], calling `trim_quantile()` returns a
#' function; that function is what the summary functions invoke on the data.
#'
#' @param pct Numeric in (0, 0.5). Tail fraction to remove from each end.
#'   Default `0.01` (1%).
#'
#' @return A function that accepts a numeric vector and returns an integer
#'   vector of the same length with `0` for values to keep and `1` for values
#'   to trim.
#'
#' @examples
#' f <- trim_quantile(0.05)
#' f(1:100)  # bottom 5 and top 5 get 1
#'
#' df <- data.frame(x = rnorm(200))
#' out <- marginal_continuous(df, "x", trim = trim_quantile(0.01))
#'
#' @export
trim_quantile <- function(pct = 0.01) {
  function(x) {
    result <- integer(length(x))
    x_valid <- x[!is.na(x)]
    n_valid <- length(x_valid)
    if (n_valid > 0L) {
      k <- floor(n_valid * pct)
      if (k >= 1L && k < n_valid) {
        sorted_valid <- sort(x_valid)
        low  <- sorted_valid[k]
        high <- sorted_valid[n_valid - k + 1L]
        result[!is.na(x) & (x < low | x > high)] <- 1L
      }
    }
    result
  }
}
