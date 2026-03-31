#' Cross-tabulation of two discrete variables
#'
#' Produces two cross-tabulation tables showing the joint frequency
#' distribution of two categorical variables. Each cell contains the count and
#' the row percentage. `NA` values in either variable are included as their own
#' level.
#'
#' @param data A data frame.
#' @param var1 Character. Name of the first discrete column.
#' @param var2 Character. Name of the second discrete column.
#'
#' @return A named list with the following elements:
#' \describe{
#'   \item{`outputs`}{A named list of data frames:
#'     \itemize{
#'       \item `crosstab` — `var1` levels as rows, `var2` levels as columns,
#'         with a Total row and column. Each cell shows `count (row%)`.
#'       \item `crosstab_transposed` — the same table with `var2` as rows and
#'         `var1` as columns.
#'     }
#'   }
#'   \item{`exclusions`}{Named list with `crosstab = NULL` and
#'     `crosstab_transposed = NULL` (no observations are excluded).}
#'   \item{`.var_name`}{Combined variable name for file naming by
#'     [save_summaries()].}
#'   \item{`.summary_type`}{Character `"discrete_discrete_joint"`.}
#' }
#'
#' @examples
#' df <- data.frame(
#'   sex    = sample(c("M", "F"), 200, replace = TRUE),
#'   status = sample(c("Active", "Inactive", "Unknown"), 200, replace = TRUE)
#' )
#' out <- joint_discrete_discrete(df, "sex", "status")
#' out$outputs$crosstab
#' out$outputs$crosstab_transposed
#'
#' @export
joint_discrete_discrete <- function(data, var1, var2) {
  x1 <- as.character(data[[var1]])
  x2 <- as.character(data[[var2]])

  x1[is.na(x1)] <- "NA"
  x2[is.na(x2)] <- "NA"

  return(list(
    outputs = list(
      crosstab            = .make_crosstab(x1, x2, var1, var2),
      crosstab_transposed = .make_crosstab(x2, x1, var2, var1)
    ),
    exclusions = list(
      crosstab            = NULL,
      crosstab_transposed = NULL
    ),
    .var_name     = paste(var1, var2, sep = "_"),
    .summary_type = "discrete_discrete_joint"
  ))
}

# Build a cross-tab data frame: rows = row_vals levels, cols = col_vals levels.
# Each interior cell: "count (row%)". Totals row and column show counts only.
.make_crosstab <- function(row_vals, col_vals, row_var, col_var) {
  tab     <- table(row_vals, col_vals)
  row_pct <- prop.table(tab, margin = 1L)

  # Interior cells
  cells <- matrix(
    sprintf("%d (%.1f%%)", as.integer(tab), row_pct * 100),
    nrow     = nrow(tab),
    dimnames = dimnames(tab)
  )

  # Append Total column (row sums, 100% by definition)
  row_totals <- rowSums(tab)
  cells <- cbind(cells, Total = sprintf("%d (100%%)", row_totals))

  # Append Total row (column sums; row % not meaningful here)
  col_totals  <- colSums(tab)
  grand_total <- sum(tab)
  total_row   <- c(sprintf("%d", col_totals), sprintf("%d", grand_total))
  cells <- rbind(cells, Total = total_row)

  # Convert to data frame for kable, prepending the row-variable column
  df_out <- as.data.frame(cells, stringsAsFactors = FALSE)
  df_out <- cbind(setNames(data.frame(rownames(df_out), stringsAsFactors = FALSE),
                            row_var),
                  df_out)
  rownames(df_out) <- NULL

  # Rename column header over the col_var levels
  names(df_out)[-1L] <- c(colnames(tab), "Total")

  return(df_out)
}
