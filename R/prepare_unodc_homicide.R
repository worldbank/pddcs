#' Prepare UNODC homicide
#'
#' Prepare UNODC homicide data for DCS upload.
#'
#' @inheritParams prepare_data
#' @examples
#' \dontrun{
#' # Prepare data
#' df <- prepare_unodc_homicide('homicide_all.xls')
#' }
#' @keywords internal
prepare_unodc_homicide <- function(path) {

  # Read data from file
  df <- readxl::read_excel(path)

  # Select rows w/ country observations
  df <- df[df$Level == 'Country', ]

  # Select relevant indicators
  df <- df[df$Indicator %in%
             c('Homicide rate', 'Female homicide rate',
               'Male homicide rate'), ]

  # Add country codes
  df$iso3c <- countrycode::countrycode(
    df$Territory, 'country.name', destination = 'iso3c'
  )
  df$iso3c <- ifelse(
    df$Territory == 'Kosovo under UNSCR 1244',
    'XKX', df$iso3c)
  df$iso3c <- ifelse(
    df$Territory == 'Channel Islands',
    'CHI', df$iso3c)

  # Add source and note cols
  df$note <- df$Source
  df$source <- 'unodc'

  # Select columns and recode names
  df <- df[c('iso3c', 'Year', 'Indicator',
             'Value', 'note', 'source')]
  names(df) <- tolower(names(df))
  df$note <- as.character(df$note)
  df$value <- as.numeric(df$value)

  # Recode indicator codes
  df$indicator <-
    ifelse(df$indicator == 'Homicide rate',
           'VC.IHR.PSRC.P5', df$indicator)
  df$indicator <-
    ifelse(df$indicator == 'Female homicide rate',
           'VC.IHR.PSRC.FE.P5', df$indicator)
  df$indicator <-
    ifelse(df$indicator == 'Male homicide rate',
           'VC.IHR.PSRC.MA.P5', df$indicator)

  # Convert to tibble
  df <- dplyr::as_tibble(df)

  return(df)
}
