#' Fetch data from UNICEF
#'
#' Fetch data from UNICEF API.
#'
#' @param indicator character: A CETS type indicator code.
#' @return data.frame
#' @keywords internal
fetch_unicef <- function(indicator) {

  # Create URL
  u <- create_url_unicef(indicator)

  # Fetch data
  df <- utils::read.csv(u, fileEncoding = 'UTF-8')

  # Standardize data
  df <- standardize_unicef(df, indicator)

  return(df)
}

#' standardize_unicef
#' @param df data.frame: A data frame with data from the UNICEF API.
#' @inheritParams fetch_unicef
#' @return data.frame
#' @noRd
standardize_unicef <- function(df, indicator) {

  # Add ISO3 country codes
  df$iso3c <- suppressWarnings(
    countrycode::countrycode(df$Areas, 'country.name', 'iso3c'))

  # Select rows with country data
  df <- df[!is.na(df$iso3c), ]

  # Add WDI code
  df$indicator <- indicator

  # Select and rename columns
  df <- df[c('iso3c', 'TIME_PERIOD', 'indicator', 'OBS_VALUE', 'CDDATASOURCE')]
  names(df) <- c('iso3c', 'year', 'indicator', 'value', 'note')
  df$note <- ifelse(is.na(df$note), '', df$note)

  # Remove anything after hyphen in year
  # Note: This can lead to duplicated country-years
  # that need to be removed with filter_unicef()
  df$year <- sub('-.*', '', df$year)

  # General standardization
  df <- standardize_all(df)

  return(df)
}


#' create_url_who
#' @inheritParams fetch_unicef
#' @return character
#' @noRd
create_url_unicef <- function(indicator) {

  # Base URL
  base_url <- 'https://sdmx.data.unicef.org/ws/public/sdmxapi/rest/data/CD2030,CDCOV,1.0/'

  # Check access
  if (httr::http_error('https://sdmx.data.unicef.org/'))
    rlang::abort(c('Couldn\'t connect to UNICEF API.',
                   i = 'Check your Internet connection.'))

  # Create main URL
  unicef_code <- recode_unicef_codes(indicator)
  the_url <- sprintf('%s%s...?format=csv', base_url, unicef_code)

  return(the_url)
}

#' recode_unicef_codes
#' @inheritParams fetch_unicef
#' @return character
#' @noRd
recode_unicef_codes <- function(indicator) {

  dplyr::recode(indicator,
                # Pregnant women receiving prenatal care of at least four visits
                'SH.STA.ANV4.ZS' = 'A2',
                # Use of insecticide-treated bed nets
                'SH.MLR.NETS.ZS' = 'A16',
                # Diarrhea treatment (ORS packet)
                'SH.STA.ORTH' = 'A19',
                # Vitamin A supplementation coverage rate
                'SN.ITK.VITA.ZS' = 'A29',
                )
}
