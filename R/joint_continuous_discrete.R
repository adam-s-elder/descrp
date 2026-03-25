#' Joint distribution of a continuous and a discrete variable
#'
#' Produces a faceted histogram showing the distribution of a continuous
#' variable separately for each level of a discrete variable, and a per-level
#' summary table.
#'
#' Each histogram panel includes dashed vertical lines at the median (blue) and
#' mode (red) of the continuous variable within that level, annotated with
#' their values. The faceted plot is only produced when the discrete variable
#' has 10 or fewer levels; the recommended export height is increased
#' automatically when there are 5 or more levels.
#'
#' NA values in the discrete variable are treated as their own level (`"NA"`).
#' NA values in the continuous variable are excluded from all calculations; a
#' message reports how many were removed.
#'
#' @param data A data frame.
#' @param cont_var Character. Name of the continuous (numeric) column.
#' @param disc_var Character. Name of the discrete (categorical) column.
#'
#' @return A named list with the following elements:
#' \describe{
#'   \item{`outputs`}{A named list:
#'     \itemize{
#'       \item `hist_faceted` — [ggplot2::ggplot()] faceted histogram, one
#'         panel per level of `disc_var`. `NULL` when `disc_var` has more than
#'         10 levels.
#'       \item `summary_table` — `knitr_kable` markdown table with columns for
#'         level, n, %, mean, median, and SD of `cont_var`. Sorted descending
#'         by count with the `NA` level last.
#'     }
#'   }
#'   \item{`exclusions`}{Named list with `hist_faceted = NULL` and
#'     `summary_table = NULL` (no observations are excluded).}
#'   \item{`.var_name`}{Combined variable name for file naming by
#'     [save_summaries()].}
#'   \item{`.summary_type`}{Character `"continuous_discrete_joint"`.}
#'   \item{`.plot_height`}{Recommended export height in inches for
#'     `hist_faceted`. Used by [save_summaries()].}
#' }
#'
#' @examples
#' df <- data.frame(
#'   score = rnorm(300),
#'   group = sample(c("A", "B", "C"), 300, replace = TRUE)
#' )
#' out <- joint_continuous_discrete(df, "score", "group")
#' out$outputs$hist_faceted
#' out$outputs$summary_table
#'
#' @export
joint_continuous_discrete <- function(data, cont_var, disc_var) {
  cont <- data[[cont_var]]
  disc <- data[[disc_var]]

  if (!is.numeric(cont)) {
    stop(sprintf("'%s' must be numeric.", cont_var))
  }

  # Represent NA levels explicitly
  disc_char <- as.character(disc)
  disc_char[is.na(disc_char)] <- "NA"

  # Remove rows with NA in the continuous variable
  n_cont_na <- sum(is.na(cont))
  if (n_cont_na > 0) {
    message(sprintf(
      "%d NA value(s) in '%s' excluded from analysis.",
      n_cont_na,
      cont_var
    ))
  }
  keep <- !is.na(cont)
  cont <- cont[keep]
  disc_char <- disc_char[keep]

  # Sorted levels: descending count, NA last
  level_counts <- sort(table(disc_char), decreasing = TRUE)
  levels_sorted <- names(level_counts)
  if ("NA" %in% levels_sorted && tail(levels_sorted, 1) != "NA") {
    levels_sorted <- c(setdiff(levels_sorted, "NA"), "NA")
  }

  n_levels <- length(levels_sorted)
  n_total <- length(cont)

  # --- Per-level stats -------------------------------------------------
  stats_list <- lapply(levels_sorted, function(lv) {
    vals <- cont[disc_char == lv]
    data.frame(
      level = lv,
      n = length(vals),
      pct = length(vals) / n_total * 100,
      mean_val = mean(vals),
      median_val = stats::median(vals),
      mode_val = .mode_val(vals),
      sd_val = stats::sd(vals),
      stringsAsFactors = FALSE
    )
  })
  stats_df <- do.call(rbind, stats_list)

  # --- Summary table ---------------------------------------------------
  tbl_display <- data.frame(
    level = stats_df$level,
    n = scales::comma(stats_df$n),
    pct = paste0(round(stats_df$pct, 1), "%"),
    mean = round(stats_df$mean_val, 2),
    median = round(stats_df$median_val, 2),
    sd = round(stats_df$sd_val, 2),
    stringsAsFactors = FALSE
  )
  names(tbl_display) <- c(disc_var, "n", "%", "Mean", "Median", "SD")
  summary_table <- knitr::kable(
    tbl_display,
    format = "markdown",
    row.names = FALSE
  )

  # --- Faceted histogram -----------------------------------------------
  plot_height <- if (n_levels >= 5) max(3 * n_levels, 12) else 6

  if (n_levels > 10) {
    message(sprintf(
      "'%s' has %d levels (> 10); faceted histogram not produced.",
      disc_var,
      n_levels
    ))
    hist_faceted <- NULL
    plot_height <- 6
  } else {
    # Build plot data with fixed column name for continuous axis
    plot_df <- data.frame(
      cont_val = cont,
      stringsAsFactors = FALSE
    )
    plot_df[[disc_var]] <- factor(disc_char, levels = levels_sorted)

    # Per-facet vline data in long format: one row per level x measure
    vline_wide <- data.frame(
      median_val = stats_df$median_val,
      mode_val = stats_df$mode_val,
      stringsAsFactors = FALSE
    )
    vline_wide[[disc_var]] <- factor(stats_df$level, levels = levels_sorted)

    vline_df <- rbind(
      data.frame(
        xintercept = vline_wide$median_val,
        measure = "Median",
        vjust_text = 1.4,
        stringsAsFactors = FALSE
      ),
      data.frame(
        xintercept = vline_wide$mode_val,
        measure = "Mode",
        vjust_text = 2.8,
        stringsAsFactors = FALSE
      )
    )
    vline_df[[disc_var]] <- factor(
      rep(stats_df$level, times = 2),
      levels = levels_sorted
    )
    vline_df$measure <- factor(vline_df$measure, levels = c("Median", "Mode"))

    measure_colours <- c(Median = "steelblue", Mode = "firebrick")
    measure_linetypes <- c(Median = "dashed", Mode = "dotted")

    ncols <- if (n_levels >= 5) 1L else 2L
    disc_var_name_formula <- ifelse(
      grepl(" ", disc_var),
      paste0("`", disc_var, "`"),
      disc_var
    )

    hist_faceted <- ggplot2::ggplot(plot_df) +
      ggplot2::geom_histogram(ggplot2::aes(x = cont_val), bins = 30) +
      ggplot2::geom_vline(
        data = vline_df,
        ggplot2::aes(
          xintercept = xintercept,
          colour = measure,
          linetype = measure
        ),
        linewidth = 0.8
      ) +
      ggplot2::geom_text(
        data = vline_df,
        ggplot2::aes(
          x = xintercept,
          y = Inf,
          label = paste0(measure, ": ", round(xintercept, 2)),
          colour = measure,
          vjust = vjust_text
        ),
        hjust = -0.05,
        size = 2.8
      ) +
      ggplot2::scale_colour_manual(values = measure_colours) +
      ggplot2::scale_linetype_manual(values = measure_linetypes) +
      ggplot2::guides(colour = "none", linetype = "none") +
      ggplot2::facet_wrap(
        stats::as.formula(paste("~", disc_var_name_formula)),
        ncol = ncols,
        scales = "free_y"
      ) +
      ggplot2::labs(x = cont_var, y = "Count")
  }

  list(
    outputs = list(
      hist_faceted  = hist_faceted,
      summary_table = summary_table
    ),
    exclusions = list(
      hist_faceted  = NULL,
      summary_table = NULL
    ),
    .var_name     = paste(cont_var, disc_var, sep = "_"),
    .summary_type = "continuous_discrete_joint",
    .plot_height  = plot_height
  )
}

# First mode of a numeric/character vector (NA values ignored).
.mode_val <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0L) {
    return(NA_real_)
  }
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
