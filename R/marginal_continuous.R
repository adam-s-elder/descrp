#' Marginal distribution plots for a continuous variable
#'
#' Produces histogram variants for exploratory analysis of a continuous
#' variable: a simple histogram of all observations, a trimmed histogram,
#' and a log10 histogram.
#'
#' @param data A data frame.
#' @param var_name Character. Name of the column in `data` to summarise. The
#'   column may be numeric, or a character vector parseable as numbers or dates
#'   (via lubridate).
#' @param trim Character. Trimming method for `hist_trimmed`. One of
#'   `"trim_count"` (remove the `trim_n` smallest and `trim_n` largest
#'   observations) or `"trim_percentile"` (remove observations below the
#'   `trim_pct` quantile and above the `1 - trim_pct` quantile).
#' @param trim_n Integer. Number of observations to remove from each tail when
#'   `trim = "trim_count"`. Default `10`.
#' @param trim_pct Numeric in (0, 0.5). Tail fraction to remove when
#'   `trim = "trim_percentile"`. Default `0.01` (1%).
#'
#' @return A named list with the following elements:
#' \describe{
#'   \item{`outputs`}{A named list of [ggplot2::ggplot()] objects:
#'     \itemize{
#'       \item `hist` — histogram of all non-missing observations.
#'       \item `hist_trimmed` — histogram after tail trimming. `NULL` when
#'         fewer than `2 * trim_n + 1` non-missing observations exist (for
#'         `trim_count`) or when trimming would remove all observations.
#'       \item `hist_log` — histogram on a log10 x-axis using only positive
#'         values. `NULL` for date columns or when no positive values exist.
#'     }
#'   }
#'   \item{`exclusions`}{A named list mirroring `outputs`:
#'     \itemize{
#'       \item `hist` — `NULL` (no observations excluded).
#'       \item `hist_trimmed` — numeric vector of excluded values, or `NULL`
#'         when `hist_trimmed` is `NULL`.
#'       \item `hist_log` — `NULL`.
#'     }
#'   }
#'   \item{`.var_name`}{Character. The variable name used for file naming in
#'     [save_summaries()].}
#'   \item{`.summary_type`}{Character `"continuous_marginal"`.}
#' }
#'
#' @examples
#' df <- data.frame(score = rnorm(200))
#' out <- marginal_continuous(df, "score")
#' out$outputs$hist
#' out$outputs$hist_trimmed
#'
#' @export
marginal_continuous <- function(data, var_name,
                                trim     = c("trim_count", "trim_percentile"),
                                trim_n   = 10L,
                                trim_pct = 0.01) {
  trim <- match.arg(trim)

  x <- data[[var_name]]

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

  # --- Trimmed histogram ---------------------------------------------------
  hist_trimmed      <- NULL
  hist_trimmed_excl <- NULL

  enough_obs <- if (trim == "trim_count") length(x) > 2L * trim_n else length(x) > 0L

  if (!enough_obs) {
    message(sprintf(
      "`hist_trimmed` requires more than %d non-missing observations; returning NULL.",
      2L * trim_n
    ))
  } else {
    if (trim == "trim_count") {
      sorted_x      <- sort(x)
      n             <- length(sorted_x)
      excluded      <- c(sorted_x[seq_len(trim_n)], sorted_x[seq(n - trim_n + 1L, n)])
      x_trimmed     <- sorted_x[seq(trim_n + 1L, n - trim_n)]

      if (is_date) {
        small_str <- format(sorted_x[seq_len(trim_n)])
        large_str <- format(sorted_x[seq(n - trim_n + 1L, n)])
      } else {
        small_str <- signif(sorted_x[seq_len(trim_n)], 2)
        large_str <- signif(sorted_x[seq(n - trim_n + 1L, n)], 2)
      }

      caption_text <- paste0(
        trim_n, " smallest observations: ",
        paste(small_str, collapse = ", "),
        "\n", trim_n, " largest observations: ",
        paste(large_str, collapse = ", "),
        "\n", na_text
      )
    } else {
      low       <- quantile(x, trim_pct,       names = FALSE)
      high      <- quantile(x, 1 - trim_pct,   names = FALSE)
      keep_mask <- x >= low & x <= high
      excluded  <- x[!keep_mask]
      x_trimmed <- x[keep_mask]

      caption_text <- paste0(
        sprintf("Bottom %.1f%% (< %s) and top %.1f%% (> %s) removed.",
                trim_pct * 100, signif(low, 3),
                trim_pct * 100, signif(high, 3)),
        "\n", na_text
      )
    }

    if (length(x_trimmed) == 0L) {
      message("`hist_trimmed`: trimming removed all observations; returning NULL.")
    } else {
      hist_trimmed_excl <- excluded

      hist_trimmed <- ggplot2::ggplot(data.frame(x = x_trimmed), ggplot2::aes(x = x)) +
        ggplot2::geom_histogram() +
        ggplot2::labs(x = var_name, y = "Count", caption = caption_text)

      if (is_date) {
        hist_trimmed <- hist_trimmed + ggplot2::scale_x_date()
      }
    }
  }

  # --- Log-transformed histogram (non-date only) ---------------------------
  if (is_date) {
    hist_log <- NULL
  } else {
    x_pos    <- x[x > 0]
    n_nonpos <- length(x) - length(x_pos)

    if (length(x_pos) == 0L) {
      message("`hist_log` requires at least one positive value; returning NULL.")
      hist_log <- NULL
    } else {
      log_caption <- na_text
      if (n_nonpos > 0L) {
        log_caption <- paste0(
          "Excluded ", scales::comma(n_nonpos), " non-positive value(s) from log scale.\n",
          na_text
        )
      }
      hist_log <- ggplot2::ggplot(data.frame(x = x_pos), ggplot2::aes(x = x)) +
        ggplot2::geom_histogram() +
        ggplot2::scale_x_log10(labels = scales::comma) +
        ggplot2::labs(x = var_name, y = "Count", caption = log_caption)
    }
  }

  list(
    outputs = list(
      hist         = hist_plot,
      hist_trimmed = hist_trimmed,
      hist_log     = hist_log
    ),
    exclusions = list(
      hist         = NULL,
      hist_trimmed = hist_trimmed_excl,
      hist_log     = NULL
    ),
    .var_name     = var_name,
    .summary_type = "continuous_marginal"
  )
}
