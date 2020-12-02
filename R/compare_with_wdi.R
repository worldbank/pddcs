#' Compare with WDI
#'
#' Compare a dataset retrieved with [fetch_indicator()] with WDI.
#'
#' `compare_with_wdi()` returns a list of three data frames; the original dataset
#' (`source`), the data retrieved from WDI (`wdi`) and the rows in the source
#' dataset that are not present in WDI (`not_in_wdi`).
#'
#' @param df data.frame: A `pddcs` formatted data frame. Output of
#'   `fetch_indicator()`.
#' @return list
#' @examples
#' # Compare SH.MLR.NETS.ZS data from UNICEF with WDI
#' data('bednets')
#' dl <- compare_with_wdi(bednets)
#' str(dl)
#' @export
compare_with_wdi <- function(df) {

  # Fetch data from WDI
  df_wdi <- fetch_wdi(unique(df$indicator))

  # Compare datasets (left excluding join)
  df_diff <-  dplyr::anti_join(df, df_wdi,
                               by = c('iso3c', 'year',
                                      'indicator', 'value'))

  # Create list
  out <- list(source = df, wdi = df_wdi, not_in_wdi = df_diff)

  return(out)

}
