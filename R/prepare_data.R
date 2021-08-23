#' Prepare data
#'
#' Prepare internal data for DCS upload.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param path character: Path to file.
#' @param source character: Data source.
#' @return tibble
#' @examples
#' # Prepare UNODC homicide data
#' path <- system.file("extdata",
#'   "homicide_all.xlsx",
#'   package = "pddcs"
#' )
#' df <- prepare_data(path, source = "homicide")
#' @export
prepare_data <- function(path, source) {
  if (source == "homicide") {
    df <- prepare_unodc_homicide(path)
  }
  if (source == "mdim") {
    df <- prepare_poverty_mdim(path)
  }
  if (source == "rmt") {
    df <- prepare_poverty_rmt(path)
  }

  # Convert to tibble
  df <- dplyr::as_tibble(df)

  return(df)
}
