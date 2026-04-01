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
  # Accept a single summary object as well as a list of them.
  # A single summary's top-level elements each contain $output.
  if ("output" %in% names(summaries[[1]])) {
    summaries <- list(summaries)
  }

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  output_dir <- normalizePath(output_dir)

  saved <- character(0)

  for (s in summaries) {
    if (is.null(s[[1]]$info$summary_type)) {
      warning(
        "Skipping a list element with no `info$summary_type`; was it produced by a descrp function?"
      )
      next
    }

    for (nm in names(s)) {
      entry <- s[[nm]]
      if (is.null(entry$output)) next

      stem <- .sanitize_name(entry$info$file_save_path)

      if (inherits(entry$output, "gg")) {
        h <- max(height, entry$info$plot_height %||% height)
        path <- file.path(output_dir, paste0(stem, "_", nm, ".png"))
        ggplot2::ggsave(path, entry$output, width = width, height = h, dpi = dpi)
        saved <- c(saved, path)
      } else {
        path <- file.path(output_dir, paste0(stem, "_", nm, ".md"))
        writeLines(as.character(entry$output), path)
        saved <- c(saved, path)
      }
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

# Minimal null-coalescing operator (avoids rlang dependency).
`%||%` <- function(x, y) if (is.null(x)) y else x
