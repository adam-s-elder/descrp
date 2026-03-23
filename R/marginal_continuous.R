#' Marginal distribution plots for a continuous variable
#'
#' Produces two histogram variants for exploratory analysis of a continuous
#' variable: a simple histogram of all observations, and a trimmed histogram
#' that excludes the 10 smallest and 10 largest observations.
#'
#' @param x A numeric vector, or a character vector that can be coerced to
#'   numeric or parsed as dates (via lubridate).
#' @param var_name Character. Label used for the x-axis. Defaults to the
#'   deparsed name of `x`.
#'
#' @return A named list with two elements:
#' \describe{
#'   \item{`hist`}{A [ggplot2::ggplot()] object: simple histogram of all
#'     non-missing observations. The caption reports the count and percentage
#'     of missing values.}
#'   \item{`hist_trimmed`}{A [ggplot2::ggplot()] object: histogram with the 10
#'     smallest and 10 largest observations removed. The plot caption lists
#'     those removed values rounded to 2 significant figures and reports
#'     missing values. `NULL` when `x` has 20 or fewer non-missing values.}
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

  is_date <- FALSE

  if (is.character(x)) {
    orig_na <- is.na(x)
    non_na_count <- sum(!orig_na)

    # Try numeric first
    x_num <- suppressWarnings(as.numeric(x))
    newly_na_num <- sum(is.na(x_num) & !orig_na)

    if (non_na_count == 0 || newly_na_num / non_na_count <= 0.5) {
      x <- x_num
    } else {
      # Try date parsing with lubridate
      if (!requireNamespace("lubridate", quietly = TRUE)) {
        stop("`lubridate` is required for date parsing. Please install it.")
      }
      date_formats <- c("ymd", "mdy", "dmy", "Ymd", "Y/m/d", "m/d/Y", "d/m/Y",
                        "ymd HM", "ymd HMS", "mdy HM", "mdy HMS")
      x_date <- suppressWarnings(
        as.Date(lubridate::parse_date_time(x, orders = date_formats, quiet = TRUE))
      )
      newly_na_date <- sum(is.na(x_date) & !orig_na)

      if (non_na_count > 0 && newly_na_date / non_na_count <= 0.5) {
        x <- x_date
        is_date <- TRUE
      } else {
        stop("`x` must be numeric or a character vector parseable as numbers or dates.")
      }
    }
  }

  if (!is.numeric(x) && !inherits(x, "Date")) {
    stop("`x` must be a numeric vector.")
  }

  n_total <- length(x)
  n_na    <- sum(is.na(x))
  na_text <- sprintf("Missing: %s (%s%%)",
                     scales::comma(n_na),
                     round(n_na / n_total * 100, 1))

  x <- x[!is.na(x)]

  df <- data.frame(x = x)

  hist_plot <- ggplot2::ggplot(df, ggplot2::aes(x = x)) +
    ggplot2::geom_histogram() +
    ggplot2::labs(x = var_name, y = "Count", caption = na_text)

  if (is_date) {
    hist_plot <- hist_plot + ggplot2::scale_x_date()
  }

  if (length(x) <= 20) {
    message("`hist_trimmed` requires more than 20 non-missing observations; returning NULL.")
    hist_trimmed <- NULL
  } else {
    sorted_x <- sort(x)
    n <- length(sorted_x)

    ten_smallest <- sorted_x[1:10]
    ten_largest  <- sorted_x[(n - 9):n]
    x_trimmed    <- sorted_x[11:(n - 10)]

    if (is_date) {
      smallest_str <- format(ten_smallest)
      largest_str  <- format(ten_largest)
    } else {
      smallest_str <- signif(ten_smallest, 2)
      largest_str  <- signif(ten_largest, 2)
    }

    caption_text <- paste0(
      "ten smallest observations: ",
      paste(smallest_str, collapse = ", "),
      "\nten largest observations: ",
      paste(largest_str, collapse = ", "),
      "\n", na_text
    )

    hist_trimmed <- ggplot2::ggplot(data.frame(x = x_trimmed), ggplot2::aes(x = x)) +
      ggplot2::geom_histogram() +
      ggplot2::labs(x = var_name, y = "Count", caption = caption_text)

    if (is_date) {
      hist_trimmed <- hist_trimmed + ggplot2::scale_x_date()
    }
  }

  list(hist = hist_plot, hist_trimmed = hist_trimmed)
}
