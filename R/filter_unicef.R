#' @importFrom magrittr %>%
NULL

# Add global variables to avoid NSE notes in R CMD check
if (getRversion() >= '2.15.1')
  utils::globalVariables(
    c('data_keep', 'indicator', 'iso3c', 'year', 'n')
  )

#' Filter UNICEF
#'
#' Remove duplicated country-year rows in UNICEF datasets, based on a specified
#' priority order.
#'
#' You can use the abbreviation of survey sources in the `note` column to
#' prioritize between different surveys if there are duplicated country-years.
#' Typical use cases are MIS, DHS, MICS and AIDS. The surveys are prioritized in
#' the order they are listed, e.g. if `priority = c('X', 'Y', 'Z')`, then X will
#' be chosen before Y, which again will be chosen before Z.
#'
#' @inheritParams compare_with_wdi
#' @param priority character: A vector with survey sources to prioritize
#'   between. See details.
#' @return data.frame
#' @examples
#' # Select MIS, then DHS, then MICS for duplicated country-years
#' data("bednets")
#' df <- filter_unicef(bednets, priority = c('MIS', 'DHS', 'MICS'))
#'
#' @export
filter_unicef <- function(df, priority) {

  if (length(priority) > 4)
    rlang::abort('You can\'t have more then four priority items.')

  # Add count column for duplicated rows
  df <- df %>%
    dplyr::group_by(iso3c, year, indicator, source) %>%
    dplyr::mutate(n = dplyr::n())

  # Add priority columns
  for (i in seq_along(priority)) {
    df[paste0('has_pri_', i)] <- grepl(priority[i], df$note)
  }

  # Select rows to keep
  df <- tidyfast::dt_nest(df, iso3c, year, indicator, source, n)
  df$data_keep <- purrr::map(df$data, select_unicef_rows)
  df$data <- NULL
  df <- tidyfast::dt_unnest(df, data_keep)

  # Convert to data frame
  df <- as.data.frame(df)

  # Select columns
  df <- df[c('iso3c', 'year', 'indicator', 'value', 'note', 'source')]

  return(df)

}

#' select_unicef_rows
#' @param df data.frame: A data frame.
#' @return data.frame
#' @noRd
select_unicef_rows <- function(df) {

  # Return as is if only one row
  if (nrow(df) == 1) return(df)

  # Select rows to keep
  if (any(df$has_pri_1)) {
    df$keep <- ifelse(df$has_pri_1 == TRUE, TRUE, FALSE)
  } else if (any(df$has_pri_2)) {
    df$keep <- ifelse(df$has_pri_2 == TRUE, TRUE, FALSE)
  } else if (any(df$has_pri_3)) {
    df$keep <- ifelse(df$has_pri_3 == TRUE, TRUE, FALSE)
  } else {
    df$keep <- ifelse(df$has_pri_4 == TRUE, TRUE, FALSE)
  }
  df <- df[df$keep == TRUE, ]
  df$keep <- NULL

  return(df)
}

