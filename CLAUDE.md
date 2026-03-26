# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

`descrp` is an R package providing utilities for computing and visualizing marginal and joint distributions of continuous and discrete variables during exploratory data analysis. It also supports choropleth maps for spatial variables. It uses dplyr syntax whenever subsetting and summarizing data. It also follows the tenants of Hadley Whickhams "R packages" book.

## Common Commands

```r
# Install dependencies and load package during development
devtools::load_all()

# Run all tests
devtools::test()

# Run a single test file
testthat::test_file("tests/testthat/test-marginal-continuous.R")

# Rebuild documentation (roxygen2)
devtools::document()

# Check the package
devtools::check()
```

## Architecture

All exported functions follow the same pattern: accept a `data` frame plus variable name(s), and return a named list with this structure:

```r
list(
  outputs    = list(<named plots/tables>),
  exclusions = list(<same names as outputs, NULL or excluded data for trimmed outputs>),
  .var_name     = "...",   # used by save_summaries() for file names
  .summary_type = "..."    # dispatched on by save_summaries()
  # .plot_height only present for continuous_discrete_joint
)
```

`exclusions` mirrors the names in `outputs`. For non-trimmed outputs the value is `NULL`; for trimmed outputs it contains the excluded observations (a numeric vector for `hist_trimmed`, a data frame for `scatter_trimmed`).

### Exported functions and their `.summary_type`

| Function | `.summary_type` | `outputs` keys |
|---|---|---|
| `marginal_continuous(data, var_name, trim)` | `"continuous_marginal"` | `hist`, `hist_trimmed`, `hist_log` |
| `marginal_discrete(data, var_name)` | `"discrete_marginal"` | `table`, `table_collapsed` |
| `joint_continuous_continuous(data, var1, var2, trim)` | `"continuous_continuous_joint"` | `scatter`, `scatter_trimmed` |
| `joint_continuous_discrete(data, cont_var, disc_var, trim)` | `"continuous_discrete_joint"` | `hist_faceted`, `hist_faceted_trimmed`, `summary_table` |
| `joint_discrete_discrete(data, var1, var2)` | `"discrete_discrete_joint"` | `crosstab`, `crosstab_transposed` |
| `spatial_summary(data, spatial_var, summary_var, spatial_type, metric)` | `"spatial_summary"` | `map`, `scatter` |

### Trim argument (`marginal_continuous`, `joint_continuous_continuous`, `joint_continuous_discrete`)

The `trim` argument accepts a trimming function or `NULL`. If `NULL`, no trimmed output is produced.

A trimming function takes a numeric vector and returns an integer vector of the same length: `0` = keep, `1` = trim. The trimmed summary is built from the kept subset; the excluded values are stored in `exclusions`.

Two trimming functionals are exported (like ggplot2 scale functions — they return a function):

- `trim_count(n = 10L)`: returns a function that marks the `n` smallest and `n` largest observations as trimmed. Default `n = 10` for marginal functions, `n = 5` for joint.
- `trim_quantile(pct = 0.01)`: returns a function that marks values outside `[quantile(x, pct), quantile(x, 1-pct)]` as trimmed.

### `save_summaries()`

Accepts a single summary list or a list of summary lists. Dispatches on `.summary_type`, accesses `s$outputs$<key>` to save:
- ggplot2 objects → PNG via `ggplot2::ggsave()`
- kable objects → `.md` files via `writeLines()`

File names are `<sanitized_var_name>_<key>.png` or `<sanitized_var_name>_<key>.md`.

### Key behaviors to be aware of

- `marginal_continuous` accepts numeric columns or character columns parseable as numbers or dates (via `lubridate`). Date columns skip `hist_log`.
- `marginal_discrete` collapses categories individually below 5% into a `"< 5%"` row when there are more than 10 non-NA categories.
- `joint_continuous_discrete` returns `hist_faceted = NULL` when the discrete variable has more than 10 levels. It also returns `.plot_height` which `save_summaries()` uses to override the default export height.
- `spatial_summary` uses `dplyr::inner_join()` so only areas present in the data appear in the map (no grey "no data" areas). The `scatter` output shows value vs land area (`ALAND`/`ALAND20`) filtered to Washington state only (county STATEFP `"53"` or ZCTA spatial filter via `sf::st_filter`). County maps include `geom_sf_text` labels with values rounded to 3 significant figures.
- `spatial_summary` requires `sf` and `tigris` (in `Suggests`); shapefiles are cached via `options(tigris_use_cache = TRUE)`.
- Internal helpers are prefixed with `.` (e.g., `.format_freq_table`, `.make_crosstab`, `.sanitize_name`, `.save_plots`, `.save_tables`).

## Dependencies

**Imports** (always available): `dplyr`, `ggplot2`, `knitr`, `scales`

**Suggests** (checked at runtime with `requireNamespace`): `lubridate` (date parsing in `marginal_continuous`), `sf` and `tigris` (spatial maps), `testthat` (testing)
