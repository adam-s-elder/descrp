#' Marginal distribution plots for a continuous variable
#'
#' Produces two histogram variants for exploratory analysis of a continuous
#' variable: a simple histogram of all observations, and a trimmed histogram
#' that excludes the 10 smallest and 10 largest observations.
#'
#' @param x A numeric vector.
#' @param var_name Character. Label used for the x-axis. Defaults to the
#'   deparsed name of `x`.
#'
#' @return A named list with two elements:
#' \describe{
#'   \item{`hist`}{A [ggplot2::ggplot()] object: simple histogram of all
#'     non-missing observations. The subtitle reports the count and percentage
#'     of missing values.}
#'   \item{`hist_trimmed`}{A [ggplot2::ggplot()] object: histogram with the 10
#'     smallest and 10 largest observations removed. The plot caption lists
#'     those removed values rounded to 2 significant figures; the subtitle
#'     reports missing values. `NULL` when `x` has 20 or fewer non-missing
#'     values.}
#' }
#'
#' @examples
#' out <- marginal_continuous(rnorm(200), var_name = "Score")
#' out$hist
#' out$hist_trimmed
#'
#' @export
marginal_continuous <- function(x, var_name = NULL) {
  if (is.null(var_name)) {
    var_name <- deparse(substitute(x))
  }

  if (!is.numeric(x)) {
    stop("`x` must be a numeric vector.")
  }

  n_total <- length(x)
  n_na    <- sum(is.na(x))
  na_subtitle <- sprintf("Missing: %s (%s%%)",
                         scales::comma(n_na),
                         round(n_na / n_total * 100, 1))

  x <- x[!is.na(x)]

  df <- data.frame(x = x)

  hist_plot <- ggplot2::ggplot(df, ggplot2::aes(x = x)) +
    ggplot2::geom_histogram() +
    ggplot2::labs(x = var_name, y = "Count", subtitle = na_subtitle)

  if (length(x) <= 20) {
    message("`hist_trimmed` requires more than 20 non-missing observations; returning NULL.")
    hist_trimmed <- NULL
  } else {
    sorted_x <- sort(x)
    n <- length(sorted_x)

    ten_smallest <- sorted_x[1:10]
    ten_largest  <- sorted_x[(n - 9):n]
    x_trimmed    <- sorted_x[11:(n - 10)]

    caption_text <- paste0(
      "ten smallest observations: ",
      paste(signif(ten_smallest, 2), collapse = ", "),
      "   ten largest observations: ",
      paste(signif(ten_largest, 2), collapse = ", ")
    )

    hist_trimmed <- ggplot2::ggplot(data.frame(x = x_trimmed), ggplot2::aes(x = x)) +
      ggplot2::geom_histogram() +
      ggplot2::labs(x = var_name, y = "Count", subtitle = na_subtitle, caption = caption_text)
  }

  list(hist = hist_plot, hist_trimmed = hist_trimmed)
}
