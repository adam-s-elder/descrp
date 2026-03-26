#' Choropleth map and scatter of observations or a continuous variable across
#' spatial units
#'
#' Aggregates a dataset by a spatial covariate (US county FIPS codes or ZIP
#' codes) and returns a ggplot2 choropleth map plus a scatter plot of the value
#' against observation count. Only areas present in the summary dataset appear in the
#' map (left-join from data). The scatter plot is restricted to Washington
#' state. For county-level maps, each county is labelled with its value.
#'
#' Shapefiles are fetched via the \pkg{tigris} package (cached after the first
#' download). Plotting requires \pkg{sf}.
#'
#' @param data A data frame.
#' @param spatial_var Character. Name of the column containing county FIPS
#'   codes (5-digit, e.g. `"06037"`) or ZIP codes.
#' @param summary_var Character or `NULL`. Name of a numeric column to
#'   summarise within each area. When `NULL` (default) the map shows
#'   observation counts.
#' @param spatial_type Character. One of `"county"` or `"zipcode"`.
#' @param metric Character. Summary function to apply to `summary_var`.
#'   One of `"mean"`, `"median"`, `"sum"`, `"sd"`, `"min"`, or `"max"`.
#'   Ignored when `summary_var` is `NULL`.
#'
#' @return A named list with:
#' \describe{
#'   \item{`outputs`}{A named list:
#'     \itemize{
#'       \item `map` — ggplot2 choropleth. For county maps, each county is
#'         labelled with its value (3 significant figures).
#'       \item `scatter` — ggplot2 scatter of value (y) vs number of
#'         observations (x), labelled by county name or ZIP code, filtered to
#'         Washington state only.
#'     }
#'   }
#'   \item{`exclusions`}{Named list with `map = NULL` and `scatter = NULL`.}
#'   \item{`.var_name`}{Character string used for file naming by
#'     [save_summaries()].}
#'   \item{`.summary_type`}{`"spatial_summary"`.}
#' }
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   fips   = sample(c("06037", "36061", "17031", "48201"), 200, replace = TRUE),
#'   income = rnorm(200, 50000, 10000)
#' )
#' # Count map
#' out1 <- spatial_summary(df, "fips", spatial_type = "county")
#' out1$outputs$map
#'
#' # Mean income map
#' out2 <- spatial_summary(df, "fips", summary_var = "income",
#'                         spatial_type = "county", metric = "mean")
#' out2$outputs$map
#' out2$outputs$scatter
#' }
#'
#' @export
spatial_summary <- function(
  data,
  spatial_var,
  summary_var = NULL,
  spatial_type = c("county", "zipcode"),
  metric = "mean"
) {
  spatial_type <- match.arg(spatial_type)

  # --- input validation ---------------------------------------------------

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }
  if (!spatial_var %in% names(data)) {
    stop(sprintf("Column '%s' not found in `data`.", spatial_var))
  }
  if (!is.null(summary_var)) {
    if (!summary_var %in% names(data)) {
      stop(sprintf("Column '%s' not found in `data`.", summary_var))
    }
    if (!is.numeric(data[[summary_var]])) {
      stop(sprintf("`summary_var` column '%s' must be numeric.", summary_var))
    }
  }

  valid_metrics <- c("mean", "median", "sum", "sd", "min", "max")
  if (!metric %in% valid_metrics) {
    stop(sprintf(
      "`metric` must be one of: %s.",
      paste(valid_metrics, collapse = ", ")
    ))
  }

  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required. Install it with install.packages('sf').")
  }
  if (!requireNamespace("tigris", quietly = TRUE)) {
    stop(
      "Package 'tigris' is required. Install it with install.packages('tigris')."
    )
  }

  # --- aggregate data by spatial unit -------------------------------------

  # Pad to 5 characters with leading zeros (standard FIPS / ZIP format)
  work_df <- dplyr::mutate(
    data,
    geo_id = formatC(as.character(.data[[spatial_var]]), width = 5, flag = "0")
  )

  if (is.null(summary_var)) {
    agg_df <- dplyr::count(work_df, geo_id, name = "value")
    agg_df$n <- agg_df$value
    fill_label <- "Count"
  } else {
    agg_df <- work_df |>
      dplyr::group_by(geo_id) |>
      dplyr::summarize(
        n = dplyr::n(),
        value = switch(
          metric,
          mean = mean(.data[[summary_var]], na.rm = TRUE),
          median = median(.data[[summary_var]], na.rm = TRUE),
          sum = sum(.data[[summary_var]], na.rm = TRUE),
          sd = sd(.data[[summary_var]], na.rm = TRUE),
          min = min(.data[[summary_var]], na.rm = TRUE),
          max = max(.data[[summary_var]], na.rm = TRUE)
        ),
        .groups = "drop"
      )
    fill_label <- paste0(
      toupper(substring(metric, 1, 1)),
      substring(metric, 2),
      "\n",
      summary_var
    )
  }

  # --- fetch shapefile ----------------------------------------------------

  options(tigris_use_cache = TRUE)

  if (spatial_type == "county") {
    shp <- tigris::counties(cb = TRUE, resolution = "20m", year = 2024)
    shp_key <- "GEOID"
    label_col <- "NAME"
  } else {
    shp <- tigris::zctas(cb = TRUE, year = 2024)
    shp_key <- "ZCTA5CE20"
    label_col <- "ZCTA5CE20"
  }

  shp <- sf::st_as_sf(shp)
  shp[[shp_key]] <- as.character(shp[[shp_key]])

  # For county joins, sanitize the key in both datasets so they match
  # regardless of case, leading/trailing whitespace, or internal spaces.
  if (spatial_type == "county") {
    .sanitize_join_key <- function(x) gsub(" ", "_", trimws(tolower(x)))
    shp[[shp_key]] <- .sanitize_join_key(shp[[shp_key]])
    agg_df$geo_id <- .sanitize_join_key(agg_df$geo_id)
  }

  # --- left-join: only areas present in summary dataset -------------------
  # dplyr::inner_join preserves sf class when the left table is an sf object
  by_spec <- stats::setNames("geo_id", shp_key)
  shp_joined <- dplyr::inner_join(shp, agg_df, by = by_spec)

  # --- build caption ------------------------------------------------------

  n_total <- nrow(data)
  n_areas <- nrow(agg_df)

  if (is.null(summary_var)) {
    caption <- sprintf(
      "%s observations across %s %s(s).",
      format(n_total, big.mark = ","),
      format(n_areas, big.mark = ","),
      spatial_type
    )
    title <- sprintf("Count of observations per %s", spatial_type)
  } else {
    n_missing_sv <- sum(is.na(data[[summary_var]]))
    caption <- sprintf(
      "%s observations (%s missing %s); %s %s(s) with data.",
      format(n_total, big.mark = ","),
      format(n_missing_sv, big.mark = ","),
      summary_var,
      format(n_areas, big.mark = ","),
      spatial_type
    )
    title <- sprintf("%s of %s per %s", metric, summary_var, spatial_type)
  }

  # --- build choropleth map -----------------------------------------------

  gg_map <- ggplot2::ggplot(shp_joined) +
    ggplot2::geom_sf(ggplot2::aes(fill = value), colour = NA) +
    ggplot2::scale_fill_viridis_c(
      name = fill_label,
      option = "plasma"
    ) +
    ggplot2::labs(title = title, caption = caption) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 13, face = "bold"),
      plot.caption = ggplot2::element_text(
        hjust = 0,
        size = 8,
        colour = "grey40"
      ),
      legend.position = "right"
    )

  # For county maps, label each area with its value (3 sig figs)
  if (spatial_type == "county") {
    gg_map <- gg_map +
      ggplot2::geom_sf_text(
        ggplot2::aes(label = signif(value, 3)),
        size = 2,
        colour = "white"
      )
  }

  # --- build WA scatter plot (value vs land area) -------------------------

  if (spatial_type == "county") {
    # STATEFP "53" = Washington
    shp_wa <- dplyr::filter(shp_joined, STATEFP == "53")
    scatter_label_col <- "NAME"
  } else {
    # Filter ZCTAs spatially to Washington state
    wa_boundary <- tigris::states(cb = TRUE, year = 2020)
    wa_boundary <- sf::st_as_sf(wa_boundary)
    wa_boundary <- dplyr::filter(wa_boundary, STUSPS == "WA")
    wa_boundary <- sf::st_transform(wa_boundary, sf::st_crs(shp_joined))
    shp_wa <- sf::st_filter(shp_joined, wa_boundary)
    scatter_label_col <- shp_key
  }

  if (nrow(shp_wa) == 0L) {
    gg_scatter <- NULL
  } else {
    scatter_df <- sf::st_drop_geometry(shp_wa) |>
      dplyr::transmute(
        n     = n,
        value = value,
        label = as.character(.data[[scatter_label_col]])
      ) |>
      dplyr::filter(!is.na(n))

    scatter_y_label <- if (is.null(summary_var)) {
      "Count"
    } else {
      paste0(
        toupper(substring(metric, 1, 1)),
        substring(metric, 2),
        " of ",
        summary_var
      )
    }

    gg_scatter <- ggplot2::ggplot(
      scatter_df,
      ggplot2::aes(x = n, y = value, label = label)
    ) +
      ggplot2::geom_point(alpha = 0.7) +
      ggplot2::geom_text(vjust = -0.5, size = 2.5, check_overlap = TRUE) +
      ggplot2::scale_x_log10(labels = scales::comma) +
      ggplot2::labs(
        x = "Number of observations",
        y = scatter_y_label,
        title = paste0(
          scatter_y_label,
          " vs observation count \u2014 Washington state"
        ),
        caption = sprintf(
          "Washington state only. %s areas shown.",
          nrow(scatter_df)
        )
      ) +
      ggplot2::theme_bw()
  }

  # --- build .var_name ----------------------------------------------------

  if (is.null(summary_var)) {
    .var_name <- spatial_var
  } else {
    .var_name <- paste0(spatial_var, "_", summary_var)
  }

  list(
    outputs = list(
      map = gg_map,
      scatter = gg_scatter
    ),
    exclusions = list(
      map = NULL,
      scatter = NULL
    ),
    .var_name = .var_name,
    .summary_type = "spatial_summary"
  )
}
