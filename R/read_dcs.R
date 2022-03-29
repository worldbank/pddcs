#' Read DCS
#'
#' Read data from DCS.
#'
#' `r lifecycle::badge("experimental")`
#'
#' Data from DCS needs to be download with Export > Custom > OK. See example
#' dataset for the accepted format.
#'
#' @param path character: Path to file.
#' @return tibble
#' @examples
#' path <- system.file("extdata",
#'   "Data Nurses and Midwives in DCS.xlsx",
#'    package = "pddcs")
#' df <- read_dcs(path)
#' @export
read_dcs <- function(path) {

  # Read data
  df <- readxl::read_xlsx(path)

  # Standardize format
  df <- standardize_dcs(df)

  return(df)
}

#' standardize_dcs
#' @param df data.frame: A data frame with data from the WDI API.
#' @return data.frame
#' @noRd
standardize_dcs <- function(df) {

  # Select columns
  df <- df[c("Country", "Time", "Series", "Data")]
  names(df) <- c("iso3c", "year", "indicator", "value")
  df$year <- sub("YR", "", df$year)

  # Add source column
  df$source <- "dcs"

  # General standardization
  df <- standardize_all(df)

  # Convert to tibble
  df <- dplyr::tibble(df)

  return(df)
}
