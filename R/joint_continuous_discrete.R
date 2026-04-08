#' Joint distribution of a continuous and a discrete variable
#'
#' Produces a faceted histogram showing the distribution of a continuous
#' variable separately for each level of a discrete variable, optionally a
#' trimmed version of the same plot, and a per-level summary table.
#'
#' Each histogram panel includes dashed vertical lines at the median (blue) and
#' mean (red) of the continuous variable within that level, annotated with
#' their values. The faceted plots are only produced when the discrete variable
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
#' @param trim A trimming function or `NULL`. If `NULL`, `hist_faceted_trimmed`
#'   is omitted. Otherwise, the function is applied to the continuous variable;
#'   rows flagged as trimmed are excluded from the trimmed plot. Use
#'   [trim_count()] or [trim_quantile()] to create suitable functions. Default
#'   `trim_count()`.
#'
#' @return A named list with the following elements:
#' \describe{
#'   \item{`output`}{A named list:
#'     \itemize{
#'       \item `hist_faceted` — [ggplot2::ggplot()] faceted histogram, one
#'         panel per level of `disc_var`. `NULL` when `disc_var` has more than
#'         10 levels.
#'       \item `hist_faceted_trimmed` — faceted histogram on the trimmed
#'         continuous values. `NULL` when `trim` is `NULL`, `disc_var` has
#'         more than 10 levels, or trimming removes all observations.
#'       \item `summary_table` — data frame with columns for level, n, %,
#'         mean, median, and SD of `cont_var`. Sorted descending by count with
#'         the `NA` level last.
#'     }
#'   }
#'   \item{`excludes`}{Named list:
#'     \itemize{
#'       \item `hist_faceted` — `NULL`.
#'       \item `hist_faceted_trimmed` — data frame of excluded rows (columns
#'         named after `cont_var` and `disc_var`), or `NULL` when
#'         `hist_faceted_trimmed` is `NULL`.
#'       \item `summary_table` — `NULL`.
#'     }
#'   }
#'   \item{`info`}{A named list of metadata:
#'     \itemize{
#'       \item `file_save_path` — Combined variable name for file naming by
#'         [save_summaries()].
#'       \item `summary_type` — Character `"continuous_discrete_joint"`.
#'       \item `covariate` — Character. The continuous variable (`cont_var`).
#'       \item `outcome` — Character. The discrete variable (`disc_var`).
#'       \item `plot_height` — Recommended export height in inches for
#'         `hist_faceted`. Used by [save_summaries()].
#'     }
#'   }
#' }
#'
#' @examples
#' df <- data.frame(
#'   score = rnorm(300),
#'   group = sample(c("A", "B", "C"), 300, replace = TRUE)
#' )
#' out <- joint_continuous_discrete(df, "score", "group")
#' out$output$hist_faceted
#' out$output$summary_table
#'
#' # No trimming
#' out2 <- joint_continuous_discrete(df, "score", "group", trim = NULL)
#'
#' @export
joint_continuous_discrete <- function(
  data,
  cont_var,
  disc_var,
  trim = trim_count()
) {
  cont <- data[[cont_var]]
  disc <- data[[disc_var]]

  if (!is.numeric(cont)) {
    stop(sprintf("'%s' must be numeric.", cont_var))
  }

  # Represent NA levels explicitly; keep NAs as real NA internally
  disc_char <- as.character(disc)

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
  work_df <- data.frame(cont = cont, disc = disc_char)
  level_counts <- dplyr::count(work_df, disc, sort = TRUE)
  levels_sorted <- level_counts$disc
  if (anyNA(levels_sorted) && !is.na(tail(levels_sorted, 1))) {
    levels_sorted <- c(levels_sorted[!is.na(levels_sorted)], NA_character_)
  }

  n_levels <- length(levels_sorted)
  n_total <- nrow(work_df)

  # --- Per-level stats -------------------------------------------------
  stats_df <- work_df |>
    dplyr::group_by(disc) |>
    dplyr::summarize(
      n = dplyr::n(),
      pct = dplyr::n() / n_total * 100,
      mean_val = mean(cont),
      median_val = stats::median(cont),
      sd_val = stats::sd(cont),
      .groups = "drop"
    ) |>
    dplyr::rename(level = disc) |>
    dplyr::mutate(
      level = addNA(factor(
        level,
        levels = levels_sorted[!is.na(levels_sorted)]
      ))
    ) |>
    dplyr::arrange(level) |>
    dplyr::mutate(level = as.character(level))

  # --- Summary table ---------------------------------------------------
  tbl_display <- stats_df |>
    dplyr::transmute(
      level = level,
      n = scales::comma(n),
      pct = paste0(round(pct, 1), "%"),
      Mean = round(mean_val, 2),
      Median = round(median_val, 2),
      SD = round(sd_val, 2)
    )
  names(tbl_display) <- c(disc_var, "n", "%", "Mean", "Median", "SD")
  summary_table <- tbl_display

  # --- Faceted histogram helper ----------------------------------------
  .make_faceted_hist <- function(cont_vals, disc_vals, lvls, caption_text) {
    inner_df <- data.frame(cont_val = cont_vals, disc_val = disc_vals)
    stats_sub <- inner_df |>
      dplyr::group_by(level = disc_val) |>
      dplyr::summarize(
        median_val = stats::median(cont_val),
        mean_val = mean(cont_val),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        level = addNA(factor(level, levels = lvls[!is.na(lvls)]))
      ) |>
      dplyr::arrange(level) |>
      dplyr::mutate(level = as.character(level))

    plot_df <- data.frame(cont_val = cont_vals)
    plot_df[[disc_var]] <- addNA(factor(disc_vals, levels = lvls[!is.na(lvls)]))

    vline_wide <- data.frame(
      median_val = stats_sub$median_val,
      mean_val = stats_sub$mean_val
    )
    vline_wide[[disc_var]] <- addNA(factor(
      stats_sub$level,
      levels = lvls[!is.na(lvls)]
    ))

    vline_df <- dplyr::bind_rows(
      data.frame(
        xintercept = vline_wide$median_val,
        measure = "Median",
        vjust_text = 1.4
      ),
      data.frame(
        xintercept = vline_wide$mean_val,
        measure = "Mean",
        vjust_text = 2.8
      )
    )
    vline_df[[disc_var]] <- addNA(factor(
      rep(stats_sub$level, times = 2),
      levels = lvls[!is.na(lvls)]
    ))
    vline_df$measure <- factor(vline_df$measure, levels = c("Median", "Mean"))

    measure_colours <- c(Median = "steelblue", Mean = "firebrick")
    measure_linetypes <- c(Median = "dashed", Mean = "dotted")

    ncols <- if (length(lvls) >= 5) 1L else 2L
    disc_var_name_formula <- ifelse(
      grepl(" ", disc_var),
      paste0("`", disc_var, "`"),
      disc_var
    )

    p <- ggplot2::ggplot(plot_df) +
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
      ggplot2::labs(x = cont_var, y = "Count", caption = caption_text) +
      cowplot::theme_minimal_grid()
    p
  }

  # --- Faceted histogram (full data) -----------------------------------
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
    hist_faceted <- .make_faceted_hist(
      cont,
      disc_char,
      levels_sorted,
      caption_text = NULL
    )
  }

  # --- Trimmed faceted histogram ---------------------------------------
  hist_faceted_trimmed <- NULL
  hist_faceted_trimmed_excl <- NULL

  if (!is.null(trim) && n_levels <= 10) {
    trim_mask <- trim(cont)
    cont_kept <- cont[trim_mask == 0L]
    disc_kept <- disc_char[trim_mask == 0L]
    n_trimmed <- sum(trim_mask == 1L)
    n_kept <- length(cont_kept)

    if (n_kept == 0L) {
      message(
        "`hist_faceted_trimmed`: trimming removed all observations; returning NULL."
      )
    } else {
      excl_df <- data.frame(
        cont[trim_mask == 1L],
        disc_char[trim_mask == 1L],
        stringsAsFactors = FALSE
      )
      names(excl_df) <- c(cont_var, disc_var)
      hist_faceted_trimmed_excl <- excl_df

      caption_text <- sprintf(
        "%d observation(s) trimmed; %d kept.",
        n_trimmed,
        n_kept
      )
      hist_faceted_trimmed <- .make_faceted_hist(
        cont_kept,
        disc_kept,
        levels_sorted,
        caption_text
      )
    }
  }

  info <- list(
    file_save_path = paste(cont_var, disc_var, sep = "_"),
    summary_type   = "continuous_discrete_joint",
    covariate      = disc_var,
    outcome        = cont_var,
    plot_height    = plot_height
  )
  return(list(
    hist_faceted = list(
      output  = hist_faceted,
      exclude = NULL,
      info    = info
    ),
    hist_faceted_trimmed = list(
      output  = hist_faceted_trimmed,
      exclude = hist_faceted_trimmed_excl,
      info    = info
    ),
    summary_table = list(
      output  = summary_table,
      exclude = NULL,
      info    = info
    )
  ))
}
