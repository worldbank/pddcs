#' Fetch indicator
#'
#' Fetch data for a specified indicator from available sources.
#'
#' `r lifecycle::badge("stable")`
#'
#' See `?indicatorlist` for an overview of available indicators.
#'
#' @param indicator character: A CETS type indicator code. See details.
#' @param source character: Data source.
#' @return tibble
#' @examples
#' \dontrun{
#' # Fetch population data from Eurostat
#' df <- fetch_indicator('SP.POP.TOTL', source = 'eurostat')
#'
#' # Fetch population data from UNPD
#' df <- fetch_indicator('SP.POP.TOTL', source = 'unpd')
#'
#' # Fetch bed nets data from UNICEF
#' df <- fetch_indicator('SH.MLR.NETS.ZS', source = 'unicef')
#'
#' # Fetch multiple indicators from the same source
#' df <- fetch_indicator(
#'  indicator = c('SH.STA.ANV4.ZS', 'SN.ITK.VITA.ZS'),
#'  source = 'unicef')
#'
#' # Fetch multiple indicators from different sources
#' df <- fetch_indicator(
#'  indicator = c('SH.STA.ANV4.ZS', 'SP.POP.TOTL'),
#'  source = c('unicef', 'eurostat'))
#' }
#' @export
fetch_indicator <- function(indicator, source) {

  # Repeat source if length is 1
  if (length(source) == 1)
    source <- rep(source, length(indicator))

  # Check that lengths match
  if (length(indicator) != length(source)) {
    rlang::abort(
      '`source` must either be of length 1 or have the same length as `indicator`.')
  }

  # Fetch data
  dl <- purrr::map2(indicator, source, .f = fetch_indicator_single)

  # Combine to data frame
  df <- data.table::rbindlist(dl)

  # Convert to tibble
  df <- dplyr::tibble(df)

  return(df)

}

#' Fetch indicator (single)
#'
#' @inheritParams fetch_indicator
#' @keywords internal
fetch_indicator_single <- function(indicator,
                                   source = c('eurostat', 'unicef',
                                              'unpd', 'who')) {
  # Match argument
  source <- match.arg(source)

  # Check inputs
  check_inputs_fetch_indicator(indicator, source)

  # Fetch data
  df <- fetch_data(indicator, source)

  return(df)

}

#' fetch_data
#' @inheritParams fetch_indicator
#' @return data.frame
#' @noRd
fetch_data <- function(indicator, source) {

  # Fetch data, based on source
  if (source == 'eurostat') {
    df <- fetch_eurostat(indicator)
  }
  if (source == 'unpd') {
    df <- fetch_unpd(indicator)
  }
  if (source == 'unicef') {
    df <- fetch_unicef(indicator)
  }
  if (source == 'who') {
    df <- fetch_who(indicator)
  }

  # Add source column
  df$source <- source

  return(df)

}

#' check_inputs_fetch_indicator
#' @inheritParams fetch_indicator
#' @return logical
#' @noRd
check_inputs_fetch_indicator <- function(indicator, source) {

  if (!source %in% c('eurostat', 'unpd', 'who', 'unicef')) {
    rlang::abort(sprintf('`%s` is not a valid source.', source))
  }

  if (source == 'eurostat') {
    if (!indicator %in% eurostat_indicators)
      rlang::abort(sprintf('`%s` is not a valid indicator for Eurostat.', indicator))
  }

  if (source == 'unpd') {
    if (!indicator %in% unpd_indicators)
      rlang::abort(sprintf('`%s` is not a valid indicator for UN Population Division.', indicator))
  }

  if (source == 'who') {
    if (!indicator %in% who_indicators)
      rlang::abort(sprintf('`%s` is not a valid indicator for WHO GHO.', indicator))
  }

  if (source == 'unicef') {
    if (!indicator %in% unicef_indicators)
      rlang::abort(sprintf('`%s` is not a valid indicator for UNICEF.', indicator))
  }
}


