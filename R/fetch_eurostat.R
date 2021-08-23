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
  if (!eurostat::check_access_to_data()) {
    rlang::abort(c("Couldn't connect to Eurostat API.",
      i = "Check your Internet connection."
    ))
  }

  # EU countries (excluding Cyprus) + UK, NOR & ISL
  countries <- c(
    "AT", "BE", "BG", "CZ", "DK", "DE", "EE", "IE", # CY,
    "EL", "ES", "FR", "HR", "IT", "LV", "LT", "LU",
    "HU", "MT", "NL", "PL", "PT", "RO", "SI", "SK",
    "FI", "SE", "UK", "IS", "NO"
  )

  # Total population
  if (indicator == "SP.POP.TOTL") {
    id <- "demo_gind"
    df <- suppressMessages(
      eurostat::get_eurostat(id = id, keepFlags = TRUE, time_format = "num")
    )
    df <- df[df$indic_de == "AVG", ]
  }

  # Crude birth rate
  if (indicator == "SP.DYN.CBRT.IN") {
    id <- "demo_gind"
    df <- suppressMessages(
      eurostat::get_eurostat(id = id, keepFlags = TRUE, time_format = "num")
    )
    df <- df[df$indic_de == "GBIRTHRT", ]
  }

  df <- df[df$geo %in% countries, ]

  return(df)
}

#' standardize_eurostat
#' @param df data.frame: Output from [eurostat::get_eurostat_json()].
#' @inheritParams fetch_eurostat
#' @return data.frame
#' @noRd
standardize_eurostat <- function(df, indicator) {

  # Add ISO3 country codes
  df$iso3c <- countrycode::countrycode(df$geo, "eurostat", "iso3c")

  # Add WDI code
  df$indicator <- indicator

  # Add note column
  df$note <- recode_eurostat_footnotes(df$flags)

  # Select and rename columns
  df <- df[c("iso3c", "time", "indicator", "values", "note")]
  names(df) <- c("iso3c", "year", "indicator", "value", "note")

  # General standardization
  df <- standardize_all(df)

  return(df)
}

#' recode_eurostat_footnotes
#' @param x character: A vector with Eurostat flags.
#' @return character
#' @noRd
recode_eurostat_footnotes <- function(x) {
  dplyr::recode(
    x,
    "b" = "Break in series.",
    "e" = "Estimated by the government.",
    "p" = "Preliminary.",
    "ep" = "Estimated by the government. Preliminary.",
    "be" = "Break in series. Estimated by the government.",
    "bp" = "Break in series. Preliminary.",
    "bep" = "Break in series. Estimated by the government. Preliminary."
  )
}
