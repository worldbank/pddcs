#' Prepare poverty RMT
#'
#' Prepare SI.RMT.COST data for DCS upload.
#'
#' @inheritParams prepare_data
#' @examples
#' \dontrun{
#' # Prepare data
#' df <- prepare_poverty_rmt('UN SDG - 10.c_inquiry response.xlsx')
#' }
#' @keywords internal
prepare_poverty_rmt <- function(path) {

  df <- readxl::read_xlsx(path, sheet = 'SmaRT Corridors', skip = 3)
  names(df) <- sub('Q3 ', 'X', names(df))
  df$To <- sub('.* to ', '', df$Period)
  df$From <- sub(' to .*', '', df$Period)

  # SI.RMT.COST.OB.ZS
  df_ob <- df %>%
    dplyr::group_by(From) %>%
    dplyr::summarise(
      X2016 = mean(X2016, na.rm = TRUE),
      X2017 = mean(X2017, na.rm = TRUE),
      X2018 = mean(X2018, na.rm = TRUE))
  df_ob$iso3c <-
    countrycode::countrycode(df_ob$From, 'country.name', 'iso3c')
  df_ob <- df_ob %>%
    data.table::setDT() %>%
    data.table::melt(
      id.vars = 'iso3c',
      measure.vars = paste0('X', 2016:2018),
      value.name = 'value',
      variable.name = 'year')
  df_ob$indicator <- 'SI.RMT.COST.OB.ZS'

  # SI.RMT.COST.IB.ZS
  df_ib <- df %>%
    dplyr::group_by(To) %>%
    dplyr::summarise(
      X2016 = mean(X2016, na.rm = TRUE),
      X2017 = mean(X2017, na.rm = TRUE),
      X2018 = mean(X2018, na.rm = TRUE))
  df_ib$iso3c <-
    countrycode::countrycode(df_ib$To, 'country.name', 'iso3c')
  df_ib$iso3c <- ifelse(df_ib$To == 'Kosovo', 'XKX', df_ib$iso3c)
  df_ib <- df_ib %>%
    data.table::setDT() %>%
    data.table::melt(
      id.vars = 'iso3c',
      measure.vars = paste0('X', 2016:2018),
      value.name = 'value',
      variable.name = 'year')
  df_ib$indicator <- 'SI.RMT.COST.IB.ZS'

  # Combine data
  df_out <- rbind(df_ib, df_ob)
  df_out$value <- round(df_out$value, 2)
  df_out$year <- gsub('X', '', df_out$year) %>%
    as.numeric()
  df_out$note <- ''
  df_out$source <- 'internal'

  # Rearrange columns
  df_out <- df_out[c('iso3c', 'year', 'indicator',
                     'value', 'note', 'source')]

  # Convert to tibble
  df_out <- dplyr::as_tibble(df_out)

  return(df_out)


}
