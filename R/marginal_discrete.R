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
#' @param x A vector (character, factor, logical, etc.) of categorical values.
#'   `NA` values are included as their own category.
#' @param var_name Character. Used as the header of the category column.
#'   Defaults to the deparsed name of `x`.
#'
#' @return A named list with two elements:
#' \describe{
#'   \item{`table`}{A `knitr_kable` markdown table with columns
#'     `Category`, `n` (comma-formatted), and `%` (1 decimal place).
#'     Rows are sorted descending by count.}
#'   \item{`table_collapsed`}{A `knitr_kable` markdown table in the same
#'     format, with small categories collapsed into `"< 5%"`. `NULL` when
#'     there are 10 or fewer categories, or when no category individually
#'     falls below 5%.}
#' }
#'
#' @examples
#' # Few categories — no collapsed table
#' out <- marginal_discrete(sample(letters[1:5], 100, replace = TRUE))
#' out$table
#' out$table_collapsed  # NULL
#'
#' # Many categories — collapsed table produced
#' out2 <- marginal_discrete(sample(letters, 500, replace = TRUE))
#' out2$table
#' out2$table_collapsed
#'
#' @export
marginal_discrete <- function(x, var_name = NULL) {
  if (is.null(var_name)) {
    var_name <- deparse(substitute(x))
  }

  total <- length(x)

  # Build frequency table including NAs
  tbl <- as.data.frame(table(x, useNA = "ifany"), stringsAsFactors = FALSE)
  names(tbl) <- c("Category", "n")
  tbl$Category <- as.character(tbl$Category)
  tbl$Category[is.na(tbl$Category)] <- "NA"
  tbl$pct <- tbl$n / total

  # Sort descending by count
  tbl <- tbl[order(-tbl$n), ]

  md_table <- .format_freq_table(tbl, var_name)

  n_cats <- nrow(tbl)

  if (n_cats <= 10) {
    return(list(table = md_table, table_collapsed = NULL))
  }

  # --- Collapse algorithm ---
  # Sort ascending to identify smallest categories
  tbl_asc <- tbl[order(tbl$pct), ]

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
    return(list(table = md_table, table_collapsed = NULL))
  }

  small <- tbl_asc[seq_len(k), ]
  large <- tbl_asc[seq(k + 1L, nrow(tbl_asc)), ]

  collapsed_row <- data.frame(
    Category = "< 5%",
    n        = sum(small$n),
    pct      = sum(small$pct),
    stringsAsFactors = FALSE
  )

  # Remaining categories sorted descending, collapsed row at bottom
  large <- large[order(-large$n), ]
  tbl_collapsed <- rbind(large, collapsed_row)

  md_table_collapsed <- .format_freq_table(tbl_collapsed, var_name)

  list(table = md_table, table_collapsed = md_table_collapsed)
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
