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
#'   \item{`output`}{A named list of data frames:
#'     \itemize{
#'       \item `crosstab` — `var1` levels as rows, `var2` levels as columns,
#'         with a Total row and column. Each cell shows `count (row%)`.
#'       \item `crosstab_transposed` — the same table with `var2` as rows and
#'         `var1` as columns.
#'     }
#'   }
#'   \item{`excludes`}{Named list with `crosstab = NULL` and
#'     `crosstab_transposed = NULL` (no observations are excluded).}
#'   \item{`info`}{A named list of metadata:
#'     \itemize{
#'       \item `file_save_path` — Combined variable name for file naming by
#'         [save_summaries()].
#'       \item `summary_type` — Character `"discrete_discrete_joint"`.
#'       \item `covariate` — Character. The first variable (`var1`).
#'       \item `outcome` — Character. The second variable (`var2`).
#'     }
#'   }
#' }
#'
#' @examples
#' df <- data.frame(
#'   sex    = sample(c("M", "F"), 200, replace = TRUE),
#'   status = sample(c("Active", "Inactive", "Unknown"), 200, replace = TRUE)
#' )
#' out <- joint_discrete_discrete(df, "sex", "status")
#' out$output$crosstab
#' out$output$crosstab_transposed
#'
#' @export
joint_discrete_discrete <- function(data, var1, var2) {
  x1 <- as.character(data[[var1]])
  x2 <- as.character(data[[var2]])

  x1[is.na(x1)] <- "NA"
  x2[is.na(x2)] <- "NA"

  info <- list(
    file_save_path = paste(var1, var2, sep = "_"),
    summary_type   = "discrete_discrete_joint",
    covariate      = var1,
    outcome        = var2
  )
  return(list(
    crosstab = list(
      output  = .make_crosstab(x1, x2, var1, var2),
      exclude = NULL,
      info    = info
    ),
    crosstab_transposed = list(
      output  = .make_crosstab(x2, x1, var2, var1),
      exclude = NULL,
      info    = info
    ),
    bar_stacked = list(
      output  = .make_stacked_bar(x1, x2, var1, var2),
      exclude = NULL,
      info    = info
    ),
    bar_stacked_transposed = list(
      output  = .make_stacked_bar(x2, x1, var2, var1),
      exclude = NULL,
      info    = info
    )
  ))
}

# Build a stacked bar chart: x = row_vals levels, fill = col_vals levels.
# Fill levels are sorted by overall frequency and then reversed so the most
# frequent category sits at the top of each bar. Each segment is labelled with
# its count and row percentage.
.make_stacked_bar <- function(row_vals, col_vals, row_var, col_var) {
  df <- data.frame(
    row_val = row_vals,
    col_val = col_vals,
    stringsAsFactors = FALSE
  )

  counts <- df |>
    dplyr::count(row_val, col_val) |>
    dplyr::group_by(row_val) |>
    dplyr::mutate(
      total = sum(n),
      pct   = n / total * 100
    ) |>
    dplyr::ungroup()

  # Sort fill levels by overall frequency; reverse so the most frequent
  # category is rendered on top of each bar (flip from default bottom-first).
  fill_levels <- counts |>
    dplyr::count(col_val, wt = n, sort = TRUE) |>
    dplyr::pull(col_val)
  counts$col_val <- factor(counts$col_val, levels = rev(fill_levels))

  ggplot2::ggplot(counts, ggplot2::aes(x = row_val, y = n, fill = col_val)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::geom_text(
      ggplot2::aes(
        label = paste0(scales::comma(n), "\n(", round(pct, 1), "%)"),
        group = col_val
      ),
      position = ggplot2::position_stack(vjust = 0.5),
      size  = 2.8,
      colour = "white"
    ) +
    ggplot2::labs(x = row_var, y = "Count", fill = col_var) +
    cowplot::theme_minimal_grid()
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
