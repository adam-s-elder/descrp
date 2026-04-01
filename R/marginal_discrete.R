#' Marginal distribution table for a discrete variable
#'
#' Produces a frequency table for a discrete (categorical) variable showing
#' counts and percentages. When there are more than 10 categories, a second
#' collapsed table is also returned in which the smallest categories — those
#' that each individually represent less than 5% of observations — are
#' combined into a single `"< 5%"` row.
#'
#' The collapsed table uses the greatest number of categories for which every
#' individual collapsed category is below 5%. Categories are evaluated in
#' ascending order of frequency; collapsing stops as soon as a category
#' reaches or exceeds 5%.
#'
#' @param data A data frame.
#' @param var_name Character. Name of the column in `data` to summarise. Used
#'   as the header of the category column. The column may be any type
#'   (character, factor, logical, etc.). `NA` values are always included as
#'   their own category. The `NA` row is never collapsed into `"< 5%"`.
#'
#' @return A named list with the following elements:
#' \describe{
#'   \item{`output`}{A named list of data frames:
#'     \itemize{
#'       \item `table` — frequency table with columns `Category`,
#'         `n` (comma-formatted), and `%` (1 decimal place). Rows sorted
#'         descending by count; `NA` row always last.
#'       \item `table_collapsed` — same format with small categories collapsed
#'         into `"< 5%"`. `NULL` when there are 10 or fewer non-NA categories,
#'         or when no category individually falls below 5%.
#'     }
#'   }
#'   \item{`excludes`}{Named list with `table = NULL` and
#'     `table_collapsed = NULL` (no observations are excluded).}
#'   \item{`info`}{A named list of metadata:
#'     \itemize{
#'       \item `file_save_path` — Character. Variable name used for file naming
#'         in [save_summaries()].
#'       \item `summary_type` — Character `"discrete_marginal"`.
#'       \item `covariate` — Character. The variable name (`var_name`).
#'       \item `outcome` — `NULL` (marginal summary has no outcome variable).
#'     }
#'   }
#' }
#'
#' @examples
#' # Few categories — no collapsed table
#' df <- data.frame(letter = sample(letters[1:5], 100, replace = TRUE))
#' out <- marginal_discrete(df, "letter")
#' out$output$table
#' out$output$table_collapsed  # NULL
#'
#' # Many categories — collapsed table produced
#' df2 <- data.frame(letter = sample(letters, 500, replace = TRUE))
#' out2 <- marginal_discrete(df2, "letter")
#' out2$output$table
#' out2$output$table_collapsed
#'
#' @export
marginal_discrete <- function(data, var_name) {
  total <- nrow(data)

  # Count non-NA categories, sorted descending by frequency
  tbl_non_na <- data |>
    dplyr::transmute(Category = as.character(.data[[var_name]])) |>
    dplyr::filter(!is.na(Category)) |>
    dplyr::count(Category, name = "n") |>
    dplyr::arrange(dplyr::desc(n)) |>
    dplyr::mutate(pct = n / total)

  # NA row — always included, kept as real NA internally
  n_na   <- sum(is.na(data[[var_name]]))
  na_row <- data.frame(Category = NA_character_, n = n_na, pct = n_na / total)

  tbl <- dplyr::bind_rows(tbl_non_na, na_row)

  md_table <- .format_freq_table(tbl, var_name)

  # Collapse logic only considers non-NA categories; trigger when > 10 non-NA cats
  n_cats <- nrow(tbl_non_na)

  if (n_cats <= 10) {
    return(list(
      output = list(
        table           = md_table,
        table_collapsed = NULL
      ),
      excludes = list(
        table           = NULL,
        table_collapsed = NULL
      ),
      info = list(
        file_save_path = var_name,
        summary_type   = "discrete_marginal",
        covariate      = var_name,
        outcome        = NULL
      )
    ))
  }

  # --- Collapse algorithm (non-NA categories only) ---
  tbl_asc <- dplyr::arrange(tbl_non_na, pct)

  # Find maximum K: consecutive categories from smallest that are each < 5%
  k <- 0L
  for (i in seq_len(nrow(tbl_asc))) {
    if (tbl_asc$pct[i] < 0.05) {
      k <- i
    } else {
      break
    }
  }

  if (k == 0L) {
    return(list(
      output = list(
        table           = md_table,
        table_collapsed = NULL
      ),
      excludes = list(
        table           = NULL,
        table_collapsed = NULL
      ),
      info = list(
        file_save_path = var_name,
        summary_type   = "discrete_marginal",
        covariate      = var_name,
        outcome        = NULL
      )
    ))
  }

  small <- tbl_asc[seq_len(k), ]
  large <- tbl_asc[seq(k + 1L, nrow(tbl_asc)), ]

  collapsed_row <- data.frame(
    Category = "< 5%",
    n        = sum(small$n),
    pct      = sum(small$pct)
  )

  # Remaining non-NA categories descending, then collapsed row, then NA row
  large         <- dplyr::arrange(large, dplyr::desc(n))
  tbl_collapsed <- dplyr::bind_rows(large, collapsed_row, na_row)

  md_table_collapsed <- .format_freq_table(tbl_collapsed, var_name)

  return(list(
    output = list(
      table           = md_table,
      table_collapsed = md_table_collapsed
    ),
    excludes = list(
      table           = NULL,
      table_collapsed = NULL
    ),
    info = list(
      file_save_path = var_name,
      summary_type   = "discrete_marginal",
      covariate      = var_name,
      outcome        = NULL
    )
  ))
}

# Internal helper: format a frequency data frame for display.
# Expects columns: Category (NA = missing), n (integer), pct (proportion 0-1).
.format_freq_table <- function(tbl, var_name) {
  display <- tbl |>
    dplyr::transmute(
      Category = dplyr::if_else(is.na(Category), "NA", Category),
      n        = scales::comma(n),
      pct      = paste0(round(pct * 100, 1), "%")
    )
  names(display) <- c(var_name, "n", "%")
  knitr::kable(display, format = "markdown", row.names = FALSE)
}
