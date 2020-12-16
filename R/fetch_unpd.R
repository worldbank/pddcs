#' @importFrom magrittr %>%
NULL

#' Fetch data from UN World Population Prospects
#'
#' Fetch data from the UN Population Division's World Population Prospects.
#'
#' @param indicator character: A CETS type indicator code.
#' @return data.frame
#' @keywords internal
fetch_unpd <- function(indicator) {

  # Create URL
  u <- create_url_wpp(indicator)

  # Read file into memory
  httr::GET(u, httr::write_disk(tmp <- tempfile(fileext = '.xlsx')))

  # Read data from Excel file
  df_estimates <- readxl::read_excel(tmp, 'ESTIMATES', skip = 16)
  df_medium <- readxl::read_excel(tmp, 'MEDIUM VARIANT', skip = 16)

  # Standardize format
  df <- standardize_wpp(df_estimates, df_medium, indicator)

  return(df)
}

#' create_url_wpp
#' @inheritParams fetch_unpd_wpp
#' @return character
#' @noRd
create_url_wpp <- function(indicator) {

  # Base URL
  base_url <- 'https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/'

  # Check access
  if (httr::http_error('https://population.un.org/'))
    rlang::abort(c('Couldn\'t connect to UNPD website.',
                   i = 'Check your Internet connection.'))

  if (indicator == 'SP.POP.TOTL')
    file_url <- '1_Population/WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.xlsx'
  if (indicator == 'SP.DYN.TFRT.IN')
    file_url <- '5_Interpolated/WPP2019_INT_F01_ANNUAL_DEMOGRAPHIC_INDICATORS.xlsx'
  if (indicator == 'SP.POP.AG00.MA.IN')
    file_url <- '5_Interpolated/WPP2019_INT_F03_2_POPULATION_BY_AGE_ANNUAL_MALE.xlsx'

  # Create URL
  the_url <- paste0(base_url, file_url)

  return(the_url)
}

#' standardize_wpp
#' @param estimates data.frame: A data frame from a WPP Excel ESTIMATES sheet.
#' @param medium data.frame: A data frame from a WPP Excel MEDIUM VARIANT
#'   sheet.
#' @inheritParams fetch_unpd_wpp
#' @return data.frame
#' @noRd
standardize_wpp <- function(estimates, medium, indicator) {

  # Merge vars
  vars <- c('Variant', 'Country code', 'Notes', 'Type')

  # Total population
  if (indicator == 'SP.POP.TOTL') {
    df <- standardize_wpp_sp_pop_totl(estimates, medium, vars)
  }

  # Total fertility
  if (indicator == 'SP.DYN.TFRT.IN') {
    df <- standardize_wpp_sp_dyn_tfrt(estimates, medium)
  }

  # Male population (ages 0 - 25)
  if (indicator == 'SP.POP.AG00.MA.IN') {
    df <- standardize_wpp_sp_pop_ag_ma(estimates, medium, vars)
  }

  # Select rows with country data
  df <- df[df$Type == 'Country/Area', ]
  df <- df[!is.na(df$`Country code`), ]

  # Add ISO3 column
  df$iso3c <- suppressWarnings(
    countrycode::countrycode(df$`Country code`, 'un', 'iso3c')
  )
  # Fix ISO3 codes for Taiwan
  df$iso3c <- ifelse(df$`Country code` == 158, 'TWN', df$iso3c)
  # Fix ISO3 codes for Channel Islands
  df$iso3c <- ifelse(df$`Country code` == 830, 'CHI', df$iso3c)

  # Select columns
  df <- df[c('iso3c', 'year', 'indicator', 'value')]

  # Add note column
  df$note <- ''

  # General standardization
  df <- standardize_all(df)

  return(df)

}

#' standardize_wpp_sp_pop_totl
#' @param estimates data.frame: A data frame from a WPP Excel ESTIMATES sheet.
#' @param medium data.frame: A data frame from a WPP Excel MEDIUM VARIANT
#'   sheet.
#' @param vars character: A vector with column names to be used when merging the
#'   and transforming the Excel sheets.
#' @return data.frame
#' @noRd
standardize_wpp_sp_pop_totl <- function(estimates, medium, vars) {

  # Remove current year column from estimates (e.g. 2020)
  # estimates$`2020` <- NULL
  estimates[format(Sys.Date(), '%Y')] <- NULL

  # Remove unnecessary columns
  estimates <-
    estimates[c(vars, grep('\\d', names(estimates), value = TRUE))]
  medium <-
    medium[c(vars, grep('\\d', names(medium), value = TRUE))]

  # Merge datasets
  df <- merge(estimates, medium, all = TRUE, by = vars)

  # Reshape to long format (by Year)
  df <- df %>%
    data.table::as.data.table() %>%
    data.table::melt(id.vars = vars,
                     measure.vars = grep('\\d', names(df), value = TRUE),
                     variable.name  = 'year',
                     value.name = 'value') %>%
    as.data.frame()

  # Remove NA rows
  df <- df[!is.na(df$value), ]
  # df <- df[!is.na(df$`Country code`), ]

  # Set '...' values in value to NA
  df[df$value == '...', ] <- NA

  # Convert value to WDI format (Thousands)
  df$value <- as.numeric(df$value) * 1000

  # Add indicator column
  df$indicator <- 'SP.POP.TOTL'

  return(df)

}

#' standardize_wpp_sp_dyn_tfrt
#' @param estimates data.frame: A data frame from a WPP Excel ESTIMATES sheet.
#' @param medium data.frame: A data frame from a WPP Excel MEDIUM VARIANT
#'   sheet.
#' @return data.frame
#' @noRd
standardize_wpp_sp_dyn_tfrt <- function(estimates, medium) {

  # Bind data frames together
  df <- rbind(estimates, medium)

  # Set value column
  df$value <- df$`Total fertility (live births per woman)`

  # Set year column
  df$year <- df$`Reference date (1 January - 31 December)`

  # Add indicator column
  df$indicator <- 'SP.DYN.TFRT.IN'

  return(df)
}

#' standardize_wpp_sp_pop_ag
#' @param estimates data.frame: A data frame from a WPP Excel ESTIMATES sheet.
#' @param medium data.frame: A data frame from a WPP Excel MEDIUM VARIANT
#'   sheet.
#' @param vars character: A vector with column names to be used when merging the
#'   and transforming the Excel sheets.
#' @return data.frame
#' @noRd
standardize_wpp_sp_pop_ag_ma <- function(estimates, medium, vars) {

  # Bind data frames together
  df <- rbind(estimates, medium)

  # Set Time column
  df$year <- df$`Reference date (as of 1 July)`

  # Select age vars 0 - 25
  df <- df[c(vars, 'year', 0:25)]

  # Reshape to long format (by Age)
  df <- df %>%
    data.table::as.data.table() %>%
    data.table::melt(id.vars = c(vars, 'year'),
                     measure.vars = grep('\\d', names(df), value = TRUE),
                     variable.name = 'age',
                     value.name = 'value'
    ) %>%
    as.data.frame()

  # Add indicator column
  df$indicator <- ifelse(nchar(as.character(df$age)) == 1,
                      sprintf('SP.POP.AG0%s.MA.IN', df$age),
                      sprintf('SP.POP.AG%s.MA.IN', df$age))

  df$age <- NULL

  return(df)
}

