#' Joint distribution plots for two continuous variables
#'
#' Produces two scatter plots exploring the relationship between two continuous
#' variables: a full scatter of all complete observations and, optionally, a
#' trimmed scatter that removes extreme values from each variable.
#' Both plots include a linear line of best fit via [ggplot2::geom_smooth()].
#'
#' @param data A data frame.
#' @param var1 Character. Name of the first continuous column (x-axis).
#' @param var2 Character. Name of the second continuous column (y-axis).
#' @param trim A trimming function or `NULL`. If `NULL`, `scatter_trimmed` is
#'   omitted. Otherwise, the function is applied independently to each
#'   variable; any row flagged as trimmed in either variable is excluded. Use
#'   [trim_count()] or [trim_quantile()] to create suitable functions. Default
#'   `trim_count(n = 5L)`.
#'
#' @return A named list with the following elements:
#' \describe{
#'   \item{`output`}{A named list of [ggplot2::ggplot()] objects:
#'     \itemize{
#'       \item `scatter` — scatter plot of all complete observations with a
#'         linear line of best fit. Caption reports rows excluded due to
#'         missingness.
#'       \item `scatter_trimmed` — scatter plot after trimming. `NULL` when
#'         `trim` is `NULL` or trimming removes all observations.
#'     }
#'   }
#'   \item{`excludes`}{A named list mirroring `output`:
#'     \itemize{
#'       \item `scatter` — `NULL`.
#'       \item `scatter_trimmed` — data frame of excluded rows (columns named
#'         after `var1` and `var2`), or `NULL` when `scatter_trimmed` is
#'         `NULL`.
#'     }
#'   }
#'   \item{`info`}{A named list of metadata:
#'     \itemize{
#'       \item `file_save_path` — Combined variable name used for file naming
#'         by [save_summaries()].
#'       \item `summary_type` — Character `"continuous_continuous_joint"`.
#'       \item `covariate` — Character. The first variable (`var1`).
#'       \item `outcome` — Character. The second variable (`var2`).
#'     }
#'   }
#' }
#'
#' @examples
#' df <- data.frame(x = rnorm(200), y = rnorm(200))
#' out <- joint_continuous_continuous(df, "x", "y")
#' out$output$scatter
#' out$output$scatter_trimmed
#'
#' # No trimming
#' out2 <- joint_continuous_continuous(df, "x", "y", trim = NULL)
#'
#' # Remove outer 1% from each variable
#' out3 <- joint_continuous_continuous(df, "x", "y", trim = trim_quantile(0.01))
#'
#' @export
joint_continuous_continuous <- function(data, var1, var2,
                                        trim = trim_count(n = 5L)) {
  x <- data[[var1]]
  y <- data[[var2]]

  if (!is.numeric(x)) stop(sprintf("'%s' must be numeric.", var1))
  if (!is.numeric(y)) stop(sprintf("'%s' must be numeric.", var2))

  n_total   <- length(x)
  complete  <- !is.na(x) & !is.na(y)
  n_missing <- sum(!complete)
  x <- x[complete]
  y <- y[complete]

  na_text <- sprintf(
    "Rows missing in either variable: %s (%s%%)",
    scales::comma(n_missing),
    round(n_missing / n_total * 100, 1)
  )

  df <- data.frame(x = x, y = y)

  scatter <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x) +
    ggplot2::labs(x = var1, y = var2, caption = na_text)

  # --- Trimmed scatter -----------------------------------------------------
  scatter_trimmed      <- NULL
  scatter_trimmed_excl <- NULL

  if (!is.null(trim)) {
    trim_x    <- trim(x)
    trim_y    <- trim(y)
    drop_mask <- (trim_x == 1L) | (trim_y == 1L)
    df_trimmed <- df[!drop_mask, ]
    n_dropped  <- sum(drop_mask)
    n_kept     <- sum(!drop_mask)

    if (n_kept == 0L) {
      message("`scatter_trimmed`: trimming removed all observations; returning NULL.")
    } else {
      scatter_trimmed_excl        <- df[drop_mask, ]
      names(scatter_trimmed_excl) <- c(var1, var2)

      caption_trimmed <- sprintf(
        "%d observation(s) trimmed; %d kept.\n%s",
        n_dropped, n_kept, na_text
      )

      scatter_trimmed <- ggplot2::ggplot(df_trimmed, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_point(alpha = 0.5) +
        ggplot2::geom_smooth(method = "lm", formula = y ~ x) +
        ggplot2::labs(x = var1, y = var2, caption = caption_trimmed)
    }
  }

  info <- list(
    file_save_path = paste(var1, var2, sep = "_"),
    summary_type   = "continuous_continuous_joint",
    covariate      = var1,
    outcome        = var2
  )
  return(list(
    scatter = list(
      output  = scatter,
      exclude = NULL,
      info    = info
    ),
    scatter_trimmed = list(
      output  = scatter_trimmed,
      exclude = scatter_trimmed_excl,
      info    = info
    )
  ))
}
