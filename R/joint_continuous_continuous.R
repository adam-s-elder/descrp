#' Joint distribution plots for two continuous variables
#'
#' Produces two scatter plots exploring the relationship between two continuous
#' variables: a full scatter of all complete observations and a trimmed scatter
#' that removes the 5 smallest and 5 largest observations of each variable.
#' Both plots include a linear line of best fit via [ggplot2::geom_smooth()].
#'
#' @param data A data frame.
#' @param var1 Character. Name of the first continuous column (x-axis).
#' @param var2 Character. Name of the second continuous column (y-axis).
#'
#' @return A named list with the following elements:
#' \describe{
#'   \item{`scatter`}{A [ggplot2::ggplot()] scatter plot of all complete
#'     observations with a linear line of best fit. Caption reports rows
#'     excluded due to missingness in either variable.}
#'   \item{`scatter_trimmed`}{A [ggplot2::ggplot()] scatter plot with the 5
#'     smallest and 5 largest observations of each variable removed. The
#'     caption lists the removed values (2 significant figures) for both
#'     variables. `NULL` when fewer than 21 complete observations are
#'     available.}
#'   \item{`.var_name`}{Combined variable name used for file naming by
#'     [save_summaries()].}
#'   \item{`.summary_type`}{Character `"continuous_continuous_joint"`.}
#' }
#'
#' @examples
#' df <- data.frame(x = rnorm(200), y = rnorm(200))
#' out <- joint_continuous_continuous(df, "x", "y")
#' out$scatter
#' out$scatter_trimmed
#'
#' @export
joint_continuous_continuous <- function(data, var1, var2) {
  x <- data[[var1]]
  y <- data[[var2]]

  if (!is.numeric(x)) stop(sprintf("'%s' must be numeric.", var1))
  if (!is.numeric(y)) stop(sprintf("'%s' must be numeric.", var2))

  n_total   <- length(x)
  complete  <- !is.na(x) & !is.na(y)
  n_missing <- sum(!complete)
  x <- x[complete]
  y <- y[complete]

  na_text <- sprintf(
    "Rows missing in either variable: %s (%s%%)",
    scales::comma(n_missing),
    round(n_missing / n_total * 100, 1)
  )

  df <- data.frame(x = x, y = y)

  scatter <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x) +
    ggplot2::labs(x = var1, y = var2, caption = na_text)

  n <- nrow(df)
  if (n <= 20) {
    message("`scatter_trimmed` requires more than 20 complete observations; returning NULL.")
    scatter_trimmed <- NULL
  } else {
    ord_x <- order(x)
    ord_y <- order(y)

    five_small_x <- x[ord_x[1:5]]
    five_large_x <- x[ord_x[(n - 4):n]]
    five_small_y <- y[ord_y[1:5]]
    five_large_y <- y[ord_y[(n - 4):n]]

    # Remove rows where either variable is in the extreme 5 (by index)
    drop_idx <- unique(c(ord_x[1:5], ord_x[(n - 4):n],
                         ord_y[1:5], ord_y[(n - 4):n]))
    df_trimmed <- df[-drop_idx, ]

    caption_trimmed <- paste0(
      var1, " \u2014 5 smallest: ", paste(signif(five_small_x, 2), collapse = ", "),
      "; 5 largest: ", paste(signif(five_large_x, 2), collapse = ", "),
      "\n",
      var2, " \u2014 5 smallest: ", paste(signif(five_small_y, 2), collapse = ", "),
      "; 5 largest: ", paste(signif(five_large_y, 2), collapse = ", "),
      "\n", na_text
    )

    scatter_trimmed <- ggplot2::ggplot(df_trimmed, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_point(alpha = 0.5) +
      ggplot2::geom_smooth(method = "lm", formula = y ~ x) +
      ggplot2::labs(x = var1, y = var2, caption = caption_trimmed)
  }

  list(
    scatter         = scatter,
    scatter_trimmed = scatter_trimmed,
    .var_name       = paste(var1, var2, sep = "_"),
    .summary_type   = "continuous_continuous_joint"
  )
}
