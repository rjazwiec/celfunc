# celfunc

Internal R package with utility functions for PK/PD data wrangling and formatting.

## Install

```r
devtools::install_github("rjazwiec/celfunc")

renv::install("rjazwiec/celfunc")
```

## Functions

### Formatting

| Function | Description |
|---|---|
| `pct_print(val, all, ...)` | Divides `val` by `all` and returns a formatted percentage string. |
| `pct_print2(pct, ...)` | Formats a pre-computed proportion or percentage, replacing `NA` with a configurable token. |
| `prt(val, digits)` | Rounds a number and returns a trimmed character string suitable for table cells. |
| `pv_prt(val, digits, pref)` | Formats a p-value, using `"< threshold"` notation for values below the detection limit. |

### Data wrangling

| Function | Description |
|---|---|
| `rotate_df(df, rowname_col)` | Transposes a data frame, promoting the first row to column names. |
| `merge_suffix_columns(df)` | Coalesces `.x`/`.y` column pairs produced by a `dplyr` join, preferring `.x`. |
| `get_list_names(x)` | Recursively enumerates all element paths in a nested list. |
| `date_string(dt, fmt)` | Formats a datetime as a compact timestamp string (default: `"YYMMDD_HHMM"`). |

### Statistical / PK

| Function | Description |
|---|---|
| `geom_mean(x)` | Returns the geometric mean, or `NA` if any non-missing value is non-positive. |
| `geom_mean_sup(x, sup_val)` | Geometric mean with zeros replaced by `sup_val` before log-transformation. |
| `geom_mean_paired(x, y)` | Geometric mean of `x` restricted to subjects with complete `(x, y)` pairs. |
| `means_long(values, group, ...)` | Geometric or arithmetic mean for use inside `dplyr::summarise()`, with optional complete-case restriction. |
| `paired_median(values, group, ...)` | Median with the same complete-case restriction logic as `means_long()`. |
| `geom_mean_r(x, y)` | Geometric mean ratio GM(y)/GM(x) via Welch's t-test, returning point estimate, 95% CI, and p-value. |
| `calc_auc(conc, time)` | Area under the curve by the linear trapezoidal rule; preserves `units`-class attributes. |

### Units utilities

| Function | Description |
|---|---|
| `save_units_in_df(df)` | Snapshots the unit strings of all `units`-class columns into a two-column data frame. |
| `apply_units_from_df(df, units_spec)` | Restores `units`-class attributes from a snapshot produced by `save_units_in_df()`. |

Helper functions for PK/PD reports
