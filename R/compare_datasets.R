#' Compare datasets
#'
#' Compare new datasets from source with current datasets in WDI/DCS.
#'
#' The main usage of `compare_datasets()` is to compare the individual
#' country-year values in the new source dataset with the current values in
#' WDI/DCS.
#'
#' Comparison is done by merging the two datasets (left join on `new`),
#' calculating the absolute difference between the two `value` columns, and then
#' running outlier detection on `diff` column.
#'
#' Users should look for **both** large differences in values (`diff`) and large
#' p-values (`p_values`) to identify outliers or other possible unwanted changes
#' in the data.
#'
#' In the case where a few values for a specific country are substantially
#' different from the current dataset in WDI/DCS they should pop out as outliers
#' with large p-values. On the other hand it might be the case that most or all
#' values for a specific country have changed. In that case it is unlikely to be
#' any outliers, but changes can be found by inspecting the `diff` and
#' `n_diff` columns.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param new data.frame: A `pddcs` formatted data frame.
#' @param current data.frame: A `pddcs` formatted data frame.
#' @inheritParams detect_outliers
#'
#' @return A tibble with the following columns added to `new`:
#'
#' * `current_value`: Value in `current` dataset.
#' * `current_source`: Source in `current` dataset.
#' * `diff`: Absolute difference between `value` and `current_value`.
#' * `outlier`: TRUE if the `diff` value is an outlier.
#' * `p_value`: p-value for the `diff` value.
#' * `n_diff`: Sum of `diff` by country.
#' * `n_outlier`: Sum of `outlier` by country.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' # Fetch indicator from source
#' df <- fetch_indicator("SH.MED.NUMW.P3", "who")
#'
#' # Compare with WDI
#' dl <- compare_with_wdi(df)
#'
#' # Compare new (source) and current (WDI) datasets
#' res <- compare_datasets(new = dl$source, current = dl$wdi)
#' }
#'
compare_datasets <- function(new, current, alpha = 0.05)  {

  # Merge datasets (left join on 'new')
  current$current_value <- current$value
  current$current_source <- current$source
  df <-
    merge(new, current[c('iso3c', 'year', 'indicator',
                         'current_value', 'current_source')],
          by = c('iso3c', 'year', 'indicator'), all.x  = TRUE)

  # Value diff
  df$diff <- abs(df$value - df$current_value)
  df$diff <- round(df$diff, getOption("digits"))

  # Outlier detection
  df <- detect_outliers(df, var = "diff", alpha = alpha)

  # Number of inconsistencies and outliers by country
  df$inconsistent <- dplyr::if_else(df$diff > 0, TRUE, FALSE)
  df <- data.table::setDT(df)
  df[, n_diff := sum(inconsistent, na.rm = TRUE), by = iso3c]
  df[, n_outlier := sum(outlier, na.rm = TRUE), by = iso3c]
  df$inconsistent <- NULL

  # Convert to tibble
  df <- tibble::as_tibble(df)


  return(df)

}
