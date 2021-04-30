#' Prepare poverty MDIM
#'
#' Prepare MDIM poverty data for DCS upload.
#'
#' @inheritParams prepare_data
#' @examples
#' \dontrun{
#' # Prepare data
#' df <- prepare_poverty_mdim('wdi_latest.xltm')
#' }
#' @keywords internal
prepare_poverty_mdim <- function(path) {

  # Read in dataset
  df <- readxl::read_excel(path, sheet = 2)

  # Recode column names
  names(df) <- c('country', 'indicator', 'year', 'note', 'value')
  df$source <- 'internal'

  # Add country codes
  df$iso3c <-
    countrycode::countrycode(
      df$country,
      origin = 'country.name', 'iso3c')
  df$iso3c <- ifelse(df$country == 'Columbia', 'COL', df$iso3c)

  # Rearrange columns
  df <- df[c('iso3c', 'year', 'indicator',
             'value', 'note', 'source')]

  # Convert to tibble
  df <- dplyr::as_tibble(df)

  return(df)

}
