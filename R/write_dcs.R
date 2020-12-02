#' Write DCS
#'
#' Write a DCS formatted dataset to a file that can be uploaded to the Data
#' Collection System.
#'
#' @param df data.frame: A DCS formatted data frame. Output of `format_dcs()`.
#' @param path character: A file name to write to.
#' @param type character: Type, either 'data' or 'meta'.
#'
#' @return data.frame
#' @examples
#' \dontrun{
#' # Load example data
#' data("bednets")
#'
#' # Write DCS 'data' format
#' df <- format_dcs(bednets, type = 'data')
#' write_dcs(df, path = 'data-bednets.xlsx', type = 'data')
#'
#' # Write DCS 'metadata' format
#' df <- format_dcs(bednets, type = 'meta')
#' write_dcs(df, path = 'meta-bednets.xlsx', type = 'meta')
#' }
#' @export
write_dcs <- function(df, path, type = c('data', 'meta')) {

  # Match argument
  type <- match.arg(type)

  # Check input
  check_inputs_write_dcs(df, path, type)

  # Data
  if (type == 'data') {
    writexl::write_xlsx(x = list('Sheet1' = df),
                        path = path,
                        format_headers = FALSE)
  }

  # Metadata
  if (type == 'meta') {
    writexl::write_xlsx(x = list('Country-Series-Time_Table' = df),
                        path = path,
                        format_headers = FALSE)
  }

  rlang::inform(sprintf('File saved to %s.', path))
}

#' check_inputs_write_dcs
#' @inheritParams write_dcs
#' @return logical
#' @noRd
check_inputs_write_dcs <- function(df, path, type) {

  # Check file extension
  filext <- tools::file_ext(path)
  if (filext != 'xlsx') {
    rlang::abort('`path` must have a \'.xlsx\' file extension.')
  }

  # Check for correct columns in data
  if (type == 'data') {
    cols <- c('Time', 'Country', 'Series' , 'Scale', 'Data')
    if (!isTRUE(all.equal(names(df), cols)))
      rlang::abort('`df` contains invalid columns.')
  }

  # Check for correct columns in metadata
  if (type == 'meta') {
    cols <- c('Country', 'Series', 'Time', 'Footnote')
    if (!isTRUE(all.equal(names(df), cols)))
      rlang::abort('`df` contains invalid columns.')
  }
}
