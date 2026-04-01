#' Marginal distribution plots for a continuous variable
#'
#' Produces histogram variants for exploratory analysis of a continuous
#' variable: a simple histogram of all observations, optionally a trimmed
#' histogram, and a log10 histogram.
#'
#' @param data A data frame.
#' @param var_name Character. Name of the column in `data` to summarise. The
#'   column may be numeric, or a character vector parseable as numbers or dates
#'   (via lubridate).
#' @param trim A trimming function or `NULL`. If `NULL`, `hist_trimmed` is
#'   omitted. Otherwise, the function must accept a numeric vector and return
#'   an integer vector of the same length with `0` for values to keep and `1`
#'   for values to trim. Use [trim_count()] or [trim_quantile()] to create
#'   suitable functions. Default `trim_count()`.
#'
#' @return A named list with the following elements:
#' \describe{
#'   \item{`output`}{A named list of [ggplot2::ggplot()] objects:
#'     \itemize{
#'       \item `hist` — histogram of all non-missing observations.
#'       \item `hist_trimmed` — histogram after trimming. `NULL` when `trim`
#'         is `NULL` or trimming removes all observations.
#'       \item `hist_log` — histogram on a log10 x-axis using only positive
#'         values. `NULL` for date columns or when no positive values exist.
#'     }
#'   }
#'   \item{`excludes`}{A named list mirroring `output`:
#'     \itemize{
#'       \item `hist` — `NULL` (no observations excluded).
#'       \item `hist_trimmed` — numeric vector of excluded values, or `NULL`
#'         when `hist_trimmed` is `NULL`.
#'       \item `hist_log` — `NULL`.
#'     }
#'   }
#'   \item{`info`}{A named list of metadata:
#'     \itemize{
#'       \item `file_save_path` — Character. Variable name used for file naming
#'         in [save_summaries()].
#'       \item `summary_type` — Character `"continuous_marginal"`.
#'       \item `covariate` — Character. The variable name (`var_name`).
#'       \item `outcome` — `NULL` (marginal summary has no outcome variable).
#'     }
#'   }
#' }
#'
#' @examples
#' df <- data.frame(score = rnorm(200))
#' out <- marginal_continuous(df, "score")
#' out$output$hist
#' out$output$hist_trimmed
#'
#' # No trimming
#' out2 <- marginal_continuous(df, "score", trim = NULL)
#'
#' # Remove 5 from each tail
#' out3 <- marginal_continuous(df, "score", trim = trim_count(5))
#'
#' # Remove outer 2% of values
#' out4 <- marginal_continuous(df, "score", trim = trim_quantile(0.02))
#'
#' @export
marginal_continuous <- function(data, var_name, trim = trim_count()) {
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
      date_formats <- c(
        "ymd",
        "mdy",
        "dmy",
        "Ymd",
        "Y/m/d",
        "m/d/Y",
        "d/m/Y",
        "ymd HM",
        "ymd HMS",
        "mdy HM",
        "mdy HMS"
      )
      x_date <- suppressWarnings(
        as.Date(lubridate::parse_date_time(
          x,
          orders = date_formats,
          quiet = TRUE
        ))
      )
      newly_na_date <- sum(is.na(x_date) & !orig_na)

      if (non_na_count > 0 && newly_na_date / non_na_count <= 0.5) {
        x <- x_date
        is_date <- TRUE
      } else {
        stop(
          "`x` must be numeric or a character vector parseable as numbers or dates."
        )
      }
    }
  }

  if (!is.numeric(x) && !inherits(x, "Date")) {
    stop("`x` must be a numeric vector.")
  }

  n_total <- length(x)
  n_na <- sum(is.na(x))
  na_text <- sprintf(
    "Missing: %s (%s%%)",
    scales::comma(n_na),
    round(n_na / n_total * 100, 1)
  )

  x <- x[!is.na(x)]

  df <- data.frame(x = x)

  hist_plot <- ggplot2::ggplot(df, ggplot2::aes(x = x)) +
    ggplot2::geom_histogram() +
    ggplot2::labs(x = var_name, y = "Count", caption = na_text)

  if (is_date) {
    hist_plot <- hist_plot + ggplot2::scale_x_date()
  }

  # --- Trimmed histogram ---------------------------------------------------
  hist_trimmed <- NULL
  hist_trimmed_excl <- NULL

  if (!is.null(trim)) {
    trim_mask <- trim(x)
    x_trimmed <- x[trim_mask == 0L]
    excluded <- x[trim_mask == 1L]
    n_trimmed <- length(excluded)
    n_kept <- length(x_trimmed)

    if (n_kept == 0L) {
      message(
        "`hist_trimmed`: trimming removed all observations; returning NULL."
      )
    } else {
      hist_trimmed_excl <- excluded
      caption_text <- sprintf(
        "%d observation(s) trimmed; %d kept.\n%s",
        n_trimmed,
        n_kept,
        na_text
      )

      hist_trimmed <- ggplot2::ggplot(
        data.frame(x = x_trimmed),
        ggplot2::aes(x = x)
      ) +
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
    x_pos <- x[x > 0]
    n_nonpos <- length(x) - length(x_pos)

    if (length(x_pos) == 0L) {
      message(
        "`hist_log` requires at least one positive value; returning NULL."
      )
      hist_log <- NULL
    } else {
      log_caption <- na_text
      if (n_nonpos > 0L) {
        log_caption <- paste0(
          "Excluded ",
          scales::comma(n_nonpos),
          " non-positive value(s) from log scale.\n",
          na_text
        )
      }
      hist_log <- ggplot2::ggplot(data.frame(x = x_pos), ggplot2::aes(x = x)) +
        ggplot2::geom_histogram() +
        ggplot2::scale_x_log10(labels = scales::comma) +
        ggplot2::labs(x = var_name, y = "Count", caption = log_caption)
    }
  }

  info <- list(
    file_save_path = var_name,
    summary_type   = "continuous_marginal",
    covariate      = var_name,
    outcome        = NULL
  )
  return(list(
    hist = list(
      output  = hist_plot,
      exclude = NULL,
      info    = info
    ),
    hist_trimmed = list(
      output  = hist_trimmed,
      exclude = hist_trimmed_excl,
      info    = info
    ),
    hist_log = list(
      output  = hist_log,
      exclude = NULL,
      info    = info
    )
  ))
}
