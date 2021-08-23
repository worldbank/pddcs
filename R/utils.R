#' standardize_all
#' @param df data.frame: A data frame to standardize.
#' @return data.frame
#' @noRd
standardize_all <- function(df) {

  # Order by country-year (increasing)
  df <- df[order(df$iso3c, df$year), ]

  # Make sure value is numeric
  df$value <- as.numeric(as.character(df$value))

  # Make sure year is numeric
  df$year <- as.numeric(as.character(df$year))

  # Convert to data frame
  df <- as.data.frame(df)

  return(df)
}
