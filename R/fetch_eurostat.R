#' Fetch data from Eurostat
#'
#' Fetch data from Eurostat API.
#'
#' @param indicator character: A CETS type indicator code.
#' @return data.frame
#' @keywords internal
fetch_eurostat <- function(indicator) {

  # Send query to Eurostat API
  df <- send_eurostat_query(indicator)

  # Standardize format
  df <- standardize_eurostat(df, indicator)

  return(df)
}

#' send_eurostat_query
#' @inheritParams fetch_eurostat
#' @return data.frame
#' @noRd
send_eurostat_query <- function(indicator) {

  if (!eurostat::check_access_to_data())
    rlang::abort(c('Couldn\'t connect to Eurostat API.',
                 i = 'Check your Internet connection.'))

  # 27 EU countries + UK, NOR & ISL (not Cyprus and Liechtenstein?)
  countries <- c('AT', 'BE', 'BG', 'CZ', 'DK', 'DE', 'EE', 'IE', # CY,
                 'EL', 'ES', 'FR', 'HR', 'IT', 'LV', 'LT', 'LU',
                 'HU', 'MT', 'NL', 'PL', 'PT', 'RO', 'SI', 'SK',
                 'FI', 'SE', 'UK', 'IS', 'NO')

  # Total population
  if (indicator == 'SP.POP.TOTL') {
    id <- 'demo_gind'
    filter <- list(indic_de = 'AVG', geo = countries,
                   time_format = 'num')
  }

  # Crude birth rate
  if (indicator == 'SP.DYN.CBRT.IN') {
    id <- 'demo_gind'
    filter <- list(indic_de = 'GBIRTHRT', geo = countries,
                   time_format = 'num')
  }

  df <- eurostat::get_eurostat_json(id, filters = filter)

  return(df)

}

#' standardize_eurostat
#' @param df data.frame: Output from [eurostat::get_eurostat_json()].
#' @inheritParams fetch_eurostat
#' @return data.frame
#' @noRd
standardize_eurostat <- function(df, indicator) {

  # Add ISO3 country codes
  df$iso3c <- countrycode::countrycode(df$geo, 'eurostat', 'iso3c')

  # Add WDI code
  df$indicator <- indicator

  # Select and rename columns
  df <- df[c('iso3c', 'time', 'indicator', 'values')]
  names(df) <- c('iso3c', 'year', 'indicator', 'value')

  # Add note column
  df$note <- 'Data source: Eurostat'

  # General standardization
  df <- standardize_all(df)

  return(df)
}


