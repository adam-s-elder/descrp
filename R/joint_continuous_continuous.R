#' Joint distribution plots for two continuous variables
#'
#' Produces two scatter plots exploring the relationship between two continuous
#' variables: a full scatter of all complete observations and a trimmed scatter
#' that removes extreme values from each variable.
#' Both plots include a linear line of best fit via [ggplot2::geom_smooth()].
#'
#' @param data A data frame.
#' @param var1 Character. Name of the first continuous column (x-axis).
#' @param var2 Character. Name of the second continuous column (y-axis).
#' @param trim Character. Trimming method for `scatter_trimmed`. One of
#'   `"trim_count"` (remove the `trim_n` smallest and `trim_n` largest
#'   observations of each variable) or `"trim_percentile"` (remove
#'   observations below the `trim_pct` quantile or above the `1 - trim_pct`
#'   quantile for either variable).
#' @param trim_n Integer. Number of observations to remove from each tail of
#'   each variable when `trim = "trim_count"`. Default `5`.
#' @param trim_pct Numeric in (0, 0.5). Tail fraction to remove when
#'   `trim = "trim_percentile"`. Default `0.01` (1%).
#'
#' @return A named list with the following elements:
#' \describe{
#'   \item{`outputs`}{A named list of [ggplot2::ggplot()] objects:
#'     \itemize{
#'       \item `scatter` — scatter plot of all complete observations with a
#'         linear line of best fit. Caption reports rows excluded due to
#'         missingness.
#'       \item `scatter_trimmed` — scatter plot after tail trimming of each
#'         variable. `NULL` when fewer than `2 * trim_n + 1` complete
#'         observations exist (for `trim_count`).
#'     }
#'   }
#'   \item{`exclusions`}{A named list mirroring `outputs`:
#'     \itemize{
#'       \item `scatter` — `NULL`.
#'       \item `scatter_trimmed` — data frame of excluded rows (columns
#'         named after `var1` and `var2`), or `NULL` when
#'         `scatter_trimmed` is `NULL`.
#'     }
#'   }
#'   \item{`.var_name`}{Combined variable name used for file naming by
#'     [save_summaries()].}
#'   \item{`.summary_type`}{Character `"continuous_continuous_joint"`.}
#' }
#'
#' @examples
#' df <- data.frame(x = rnorm(200), y = rnorm(200))
#' out <- joint_continuous_continuous(df, "x", "y")
#' out$outputs$scatter
#' out$outputs$scatter_trimmed
#'
#' @export
joint_continuous_continuous <- function(data, var1, var2,
                                        trim     = c("trim_count", "trim_percentile"),
                                        trim_n   = 5L,
                                        trim_pct = 0.01) {
  trim <- match.arg(trim)

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

  # --- Trimmed scatter -----------------------------------------------------
  scatter_trimmed      <- NULL
  scatter_trimmed_excl <- NULL

  n <- nrow(df)
  enough_obs <- if (trim == "trim_count") n > 2L * trim_n else n > 0L

  if (!enough_obs) {
    message(sprintf(
      "`scatter_trimmed` requires more than %d complete observations; returning NULL.",
      2L * trim_n
    ))
  } else {
    if (trim == "trim_count") {
      ord_x <- order(x)
      ord_y <- order(y)

      five_small_x <- x[ord_x[seq_len(trim_n)]]
      five_large_x <- x[ord_x[seq(n - trim_n + 1L, n)]]
      five_small_y <- y[ord_y[seq_len(trim_n)]]
      five_large_y <- y[ord_y[seq(n - trim_n + 1L, n)]]

      drop_idx   <- unique(c(ord_x[seq_len(trim_n)],
                             ord_x[seq(n - trim_n + 1L, n)],
                             ord_y[seq_len(trim_n)],
                             ord_y[seq(n - trim_n + 1L, n)]))
      df_trimmed <- df[-drop_idx, ]

      caption_trimmed <- paste0(
        var1, " \u2014 ", trim_n, " smallest: ",
        paste(signif(five_small_x, 2), collapse = ", "),
        "; ", trim_n, " largest: ",
        paste(signif(five_large_x, 2), collapse = ", "),
        "\n",
        var2, " \u2014 ", trim_n, " smallest: ",
        paste(signif(five_small_y, 2), collapse = ", "),
        "; ", trim_n, " largest: ",
        paste(signif(five_large_y, 2), collapse = ", "),
        "\n", na_text
      )
    } else {
      low_x <- quantile(x, trim_pct,     names = FALSE)
      hi_x  <- quantile(x, 1 - trim_pct, names = FALSE)
      low_y <- quantile(y, trim_pct,     names = FALSE)
      hi_y  <- quantile(y, 1 - trim_pct, names = FALSE)

      keep_mask  <- x >= low_x & x <= hi_x & y >= low_y & y <= hi_y
      df_trimmed <- df[keep_mask, ]

      caption_trimmed <- paste0(
        sprintf(
          "%s: bottom/top %.1f%% removed (< %s, > %s)",
          var1, trim_pct * 100, signif(low_x, 3), signif(hi_x, 3)
        ),
        "\n",
        sprintf(
          "%s: bottom/top %.1f%% removed (< %s, > %s)",
          var2, trim_pct * 100, signif(low_y, 3), signif(hi_y, 3)
        ),
        "\n", na_text
      )
      drop_idx <- which(!keep_mask)
    }

    if (nrow(df_trimmed) == 0L) {
      message("`scatter_trimmed`: trimming removed all observations; returning NULL.")
    } else {
      scatter_trimmed_excl        <- df[drop_idx, ]
      names(scatter_trimmed_excl) <- c(var1, var2)

      scatter_trimmed <- ggplot2::ggplot(df_trimmed, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_point(alpha = 0.5) +
        ggplot2::geom_smooth(method = "lm", formula = y ~ x) +
        ggplot2::labs(x = var1, y = var2, caption = caption_trimmed)
    }
  }

  list(
    outputs = list(
      scatter         = scatter,
      scatter_trimmed = scatter_trimmed
    ),
    exclusions = list(
      scatter         = NULL,
      scatter_trimmed = scatter_trimmed_excl
    ),
    .var_name     = paste(var1, var2, sep = "_"),
    .summary_type = "continuous_continuous_joint"
  )
}
