#' Save all summary objects produced by descrp functions
#'
#' Writes every non-`NULL` plot or table from the `output` element of one or
#' more summary lists (as returned by any descrp summary function) to files in
#' `output_dir`. File names encode both the variable name(s) and the kind of
#' summary, e.g. `score_marginal_hist.png` or `group_table_collapsed.md`.
#'
#' ggplot2 objects are saved as PNG; data frame tables are converted to markdown
#' via `knitr::kable()` and saved as `.md`.
#' `NULL` outputs are silently skipped.
#'
#' @param summaries A single summary list returned by a descrp function, **or**
#'   a plain list whose elements are such summary lists.
#' @param output_dir Character. Directory in which to write files. Created
#'   (recursively) if it does not already exist. Defaults to `"."`.
#' @param width,height Numeric. Width and height in inches passed to
#'   [ggplot2::ggsave()] for PNG files. Defaults: `8` and `6`. For
#'   `continuous_discrete_joint` summaries the height is overridden by the
#'   `info$plot_height` field embedded in the summary object when it is larger than
#'   `height`.
#' @param dpi Numeric. Resolution passed to [ggplot2::ggsave()]. Default `150`.
#'
#' @return Invisibly returns a character vector of the file paths written.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   score  = rnorm(300),
#'   group  = sample(c("A", "B", "C"), 300, replace = TRUE),
#'   score2 = rnorm(300)
#' )
#' summaries <- list(
#'   marginal_continuous(df, "score"),
#'   marginal_discrete(df, "group"),
#'   joint_continuous_continuous(df, "score", "score2"),
#'   joint_continuous_discrete(df, "score", "group"),
#'   joint_discrete_discrete(df, "group", "group")
#' )
#' save_summaries(summaries, output_dir = "output")
#' }
#'
#' @export
save_summaries <- function(
  summaries,
  output_dir = ".",
  width = 8,
  height = 6,
  dpi = 150
) {
  # Accept a single summary object as well as a list of them
  if (!is.null(summaries$info)) {
    summaries <- list(summaries)
  }

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  output_dir <- normalizePath(output_dir)

  saved <- character(0)

  for (s in summaries) {
    type <- s$info$summary_type
    if (is.null(type)) {
      warning(
        "Skipping a list element with no `info$summary_type`; was it produced by a descrp function?"
      )
      next
    }

    stem <- .sanitize_name(s$info$file_save_path)

    if (type == "continuous_marginal") {
      plots <- list(
        marginal_hist = s$output$hist,
        marginal_hist_trimmed = s$output$hist_trimmed,
        marginal_hist_log = s$output$hist_log
      )
      saved <- c(
        saved,
        .save_plots(plots, stem, output_dir, width, height, dpi)
      )
    } else if (type == "discrete_marginal") {
      tables <- list(
        table = s$output$table,
        table_collapsed = s$output$table_collapsed
      )
      saved <- c(saved, .save_tables(tables, stem, output_dir))
    } else if (type == "continuous_continuous_joint") {
      plots <- list(
        scatter = s$output$scatter,
        scatter_trimmed = s$output$scatter_trimmed
      )
      saved <- c(
        saved,
        .save_plots(plots, stem, output_dir, width, height, dpi)
      )
    } else if (type == "continuous_discrete_joint") {
      # Use embedded recommended height when it exceeds the caller's default
      h <- max(height, s$info$plot_height %||% height)
      plots <- list(
        hist_faceted = s$output$hist_faceted,
        hist_faceted_trimmed = s$output$hist_faceted_trimmed
      )
      saved <- c(saved, .save_plots(plots, stem, output_dir, width, h, dpi))
      tables <- list(summary_table = s$output$summary_table)
      saved <- c(saved, .save_tables(tables, stem, output_dir))
    } else if (type == "discrete_discrete_joint") {
      tables <- list(
        crosstab = s$output$crosstab,
        crosstab_transposed = s$output$crosstab_transposed
      )
      saved <- c(saved, .save_tables(tables, stem, output_dir))
    } else if (type == "spatial_summary") {
      plots <- list(
        map = s$output$map,
        scatter = s$output$scatter
      )
      saved <- c(
        saved,
        .save_plots(plots, stem, output_dir, width, height, dpi)
      )
    } else {
      warning(sprintf("Unknown `info$summary_type` '%s'; skipping.", type))
    }
  }

  if (length(saved) > 0) {
    message(sprintf("Saved %d file(s) to: %s", length(saved), output_dir))
  } else {
    message("No files were saved.")
  }
  invisible(saved)
}

# ---------------------------------------------------------------------------
# Internal helpers

# Replace runs of non-alphanumeric characters with underscores, strip
# leading/trailing underscores, and lowercase.
.sanitize_name <- function(x) {
  x <- tolower(trimws(x))
  x <- gsub("[^[:alnum:]]+", "_", x)
  gsub("^_|_$", "", x)
}

# Save a named list of ggplot objects as PNGs; return paths written.
.save_plots <- function(plots, stem, output_dir, width, height, dpi) {
  saved <- character(0)
  for (key in names(plots)) {
    obj <- plots[[key]]
    if (is.null(obj)) {
      next
    }
    path <- file.path(output_dir, paste0(stem, "_", key, ".png"))
    ggplot2::ggsave(path, obj, width = width, height = height, dpi = dpi)
    saved <- c(saved, path)
  }
  saved
}

# Save a named list of data frame tables as .md files; return paths written.
.save_tables <- function(tables, stem, output_dir) {
  saved <- character(0)
  for (key in names(tables)) {
    obj <- tables[[key]]
    if (is.null(obj)) {
      next
    }
    path <- file.path(output_dir, paste0(stem, "_", key, ".md"))
    writeLines(as.character(obj), path)
    saved <- c(saved, path)
  }
  saved
}

# Minimal null-coalescing operator (avoids rlang dependency).
`%||%` <- function(x, y) if (is.null(x)) y else x
