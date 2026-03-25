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
#'   \item{`outputs`}{A named list of `knitr_kable` markdown tables:
#'     \itemize{
#'       \item `table` — frequency table with columns `Category`,
#'         `n` (comma-formatted), and `%` (1 decimal place). Rows sorted
#'         descending by count; `NA` row always last.
#'       \item `table_collapsed` — same format with small categories collapsed
#'         into `"< 5%"`. `NULL` when there are 10 or fewer non-NA categories,
#'         or when no category individually falls below 5%.
#'     }
#'   }
#'   \item{`exclusions`}{Named list with `table = NULL` and
#'     `table_collapsed = NULL` (no observations are excluded).}
#'   \item{`.var_name`}{Character. The variable name used for file naming in
#'     [save_summaries()].}
#'   \item{`.summary_type`}{Character `"discrete_marginal"`. Used by
#'     [save_summaries()] to dispatch saving logic.}
#' }
#'
#' @examples
#' # Few categories — no collapsed table
#' df <- data.frame(letter = sample(letters[1:5], 100, replace = TRUE))
#' out <- marginal_discrete(df, "letter")
#' out$outputs$table
#' out$outputs$table_collapsed  # NULL
#'
#' # Many categories — collapsed table produced
#' df2 <- data.frame(letter = sample(letters, 500, replace = TRUE))
#' out2 <- marginal_discrete(df2, "letter")
#' out2$outputs$table
#' out2$outputs$table_collapsed
#'
#' @export
marginal_discrete <- function(data, var_name) {
  x <- data[[var_name]]

  total <- length(x)

  # Build frequency table — always include NA row
  tbl <- as.data.frame(table(x, useNA = "always"), stringsAsFactors = FALSE)
  names(tbl) <- c("Category", "n")
  tbl$Category <- as.character(tbl$Category)
  tbl$Category[is.na(tbl$Category)] <- "NA"
  tbl$pct <- tbl$n / total

  # Separate NA row; sort non-NA rows descending by count
  na_row  <- tbl[tbl$Category == "NA", , drop = FALSE]
  tbl_non_na <- tbl[tbl$Category != "NA", , drop = FALSE]
  tbl_non_na <- tbl_non_na[order(-tbl_non_na$n), ]

  # Reassemble with NA row always at the bottom
  tbl <- rbind(tbl_non_na, na_row)

  md_table <- .format_freq_table(tbl, var_name)

  # Collapse logic only considers non-NA categories; trigger when > 10 non-NA cats
  n_cats <- nrow(tbl_non_na)

  if (n_cats <= 10) {
    return(list(
      outputs = list(
        table           = md_table,
        table_collapsed = NULL
      ),
      exclusions = list(
        table           = NULL,
        table_collapsed = NULL
      ),
      .var_name     = var_name,
      .summary_type = "discrete_marginal"
    ))
  }

  # --- Collapse algorithm (non-NA categories only) ---
  tbl_asc <- tbl_non_na[order(tbl_non_na$pct), ]

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
      outputs = list(
        table           = md_table,
        table_collapsed = NULL
      ),
      exclusions = list(
        table           = NULL,
        table_collapsed = NULL
      ),
      .var_name     = var_name,
      .summary_type = "discrete_marginal"
    ))
  }

  small <- tbl_asc[seq_len(k), ]
  large <- tbl_asc[seq(k + 1L, nrow(tbl_asc)), ]

  collapsed_row <- data.frame(
    Category = "< 5%",
    n        = sum(small$n),
    pct      = sum(small$pct),
    stringsAsFactors = FALSE
  )

  # Remaining non-NA categories descending, then collapsed row, then NA row
  large <- large[order(-large$n), ]
  tbl_collapsed <- rbind(large, collapsed_row, na_row)

  md_table_collapsed <- .format_freq_table(tbl_collapsed, var_name)

  list(
    outputs = list(
      table           = md_table,
      table_collapsed = md_table_collapsed
    ),
    exclusions = list(
      table           = NULL,
      table_collapsed = NULL
    ),
    .var_name     = var_name,
    .summary_type = "discrete_marginal"
  )
}

# Internal helper: format a frequency data frame as a markdown kable.
# Expects columns: Category, n (integer), pct (proportion 0-1).
.format_freq_table <- function(tbl, var_name) {
  display <- data.frame(
    Category = tbl$Category,
    n        = scales::comma(tbl$n),
    pct      = paste0(round(tbl$pct * 100, 1), "%"),
    stringsAsFactors = FALSE
  )
  names(display) <- c(var_name, "n", "%")
  knitr::kable(display, format = "markdown", row.names = FALSE)
}
