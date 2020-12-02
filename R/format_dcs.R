#' Convert to DCS format
#'
#' Convert a dataset retrieved with [fetch_indicator()] to DCS format.
#'
#' @inheritParams compare_with_wdi
#' @param type character: Type, either 'data' or 'meta'.
#' @return data.frame
#' @examples
#' # Load example data
#' data("bednets")
#'
#' # Convert to 'data' format
#' format_dcs(bednets, type = 'data')
#'
#' # Convert to 'metadata' format
#' format_dcs(bednets, type = 'meta')
#' @export
format_dcs <- function(df, type = c('data', 'meta')) {

  # Match argument
  type <- match.arg(type)

  # Check inputs
  check_inputs_format_dcs(df)

  # Remove rows with missing data
  df <- df[!is.na(df$value), ]

  # Rearrange data
  if (type == 'data') {
    # Add scale column
    df$scale <- 0
    # Select and reorder columns
    df <- df[c('year', 'iso3c', 'indicator', 'scale', 'value')]
    # Rename columns
    names(df) <- c('Time', 'Country', 'Series', 'Scale', 'Data')
  } else if (type == 'meta') {
    # Select and reorder columns
    df <- df[c('iso3c', 'indicator', 'year', 'note')]
    # Rename columns
    names(df) <- c('Country', 'Series', 'Time', 'Footnote')
  }

  # Reformat Time column
  df$Time <- paste0('YR', df$Time)

  return(df)

}

#' check_inputs_format_dcs
#' @inheritParams format_dcs
#' @return logical
#' @noRd
check_inputs_format_dcs <- function(df) {

  # Check for correct columns
  cols <- c('iso3c', 'year', 'indicator' , 'value', 'note', 'source')
  if (!isTRUE(all.equal(names(df), cols)))
    rlang::abort('`df` contains invalid columns.')

}
