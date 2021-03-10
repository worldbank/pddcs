#' Fetch data from WDI
#'
#' Fetch data from WDI API.
#'
#' @param indicator character: A WDI indicator code.
#' @return data.frame
#' @keywords internal
fetch_wdi <- function(indicator) {

  # Create URL
  u <- create_url_wdi(indicator)

  # Fetch data
  df <- jsonlite::fromJSON(u, flatten = TRUE)[[2]]

  # Standardize format
  df <- standardize_wdi(df)

  return(df)
}

#' create_url_wdi
#' @inheritParams fetch_wdi
#' @return character
#' @noRd
create_url_wdi <- function(indicator) {

  # base_url <- paste0('https://api.worldbank.org/v2/country/all/indicator/%s',
  #                    '?source=%s&format=json&per_page=%s&page=1')
  # the_url <- sprintf(base_url,
  #                    paste(indicator, collapse = ';'),
  #                    length(indicator),
  #                    32500)

  base_url <- paste0('https://api.worldbank.org/v2/country/all/indicator/%s',
                     '?format=json&per_page=%s&page=1')
  the_url <- sprintf(base_url,indicator, 32500)

  return(the_url)
}

#' standardize_wdi
#' @param df data.frame: A data frame with data from the WDI API.
#' @return data.frame
#' @noRd
standardize_wdi <- function(df) {

  # Select columns
  df <- df[c('countryiso3code', 'date', 'indicator.id', 'value')]
  names(df) <- c('iso3c', 'year', 'indicator', 'value')

  # Select rows with country data
  df <- df[!df$iso3c %in% wb_aggreate_codes, ]

  # Add note column
  # df$note <- ''

  # Add source column
  df$source <- 'wdi'

  # General standardization
  df <- standardize_all(df)

  return(df)
}




