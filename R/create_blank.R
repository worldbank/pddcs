#' Create blank file
#'
#' Create a file with empty data or footnotes to blank pre-existing values in
#' DCS.
#'
#' @param indicator character: A CETS type indicator code.
#' @param iso3c character: ISO3 code. Defaults to 'all'.
#' @param year numeric: Year. Defaults to 'all'.
#' @inheritParams format_dcs
#'
#' @return data.frame
#' @examples
#' # A specific indicator / country /year
#' df <- create_blank('SP.POP.TOTL',
#'   iso3c = 'ALB', year = 2014)
#'
#' # Multiple indicators / countries / years
#' df <- create_blank(
#'   indicator = c('SP.POP.TOTL', 'SH.MLR.NETS.ZS'),
#'   iso3c = c('ALB', 'AGO'),
#'   year = 2014:2015)
#'
#' # All countries and years (data)
#' df <- create_blank('SP.POP.TOTL',
#'   type = 'data')
#'
#' # All countries and years (metadata)
#' df <- create_blank('SP.POP.TOTL',
#'   type = 'meta')
#' @export
create_blank <- function(indicator,
                         iso3c = 'all',
                         year = 'all',
                         type = c('data', 'meta')) {

  # Match argument
  type <- match.arg(type)

  # Expand country / year if 'all' is specified
  if (length(iso3c) == 1)
    if (iso3c == 'all')
      iso3c <- wdi_country_codes
  if (length(year) == 1)
    if (year == 'all')
      year <- wdi_years

  # Create data frame
  df <- tidyr::expand_grid(
    iso3c, year, indicator,
    value = NA, note = as.character(NA),
    source = NA)

  # Format to DCS
  if (type == 'data') {
    df <- pddcs::format_dcs(df, type = 'data')
  }
  if (type == 'meta') {
    df <- pddcs::format_dcs(df, type = 'meta')
  }

  return(df)

}
