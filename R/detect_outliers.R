#' Detect outliers
#'
#' Detect outlier values by country.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @inheritParams compare_with_wdi
#' @param var character: Column to use for outlier detection. Defaults to "value".
#' @param alpha numeric: Significance level for a two-tailed test. Defaults to 0.05.
#' @return tibble
#' @examples
#' # Fetch source data
#' df <- fetch_indicator('SH.MED.PHYS.ZS', source = 'who')
#'
#' # Detect outliers (alpha = 0.05)
#' df2 <- detect_outliers(df)
#' df2[df2$outlier,]
#'
#' # Detect outliers (alpha = 0.01)
#' df3 <- detect_outliers(df, alpha = 0.01)
#' df3[df3$outlier,]
#'
#' @export
detect_outliers <- function(df, var = "value", alpha = .05) {

  assertthat::assert_that(
    any(names(df) != 'tmp'),
    msg = "`df` can't contain a column called `tmp`.")

  # Add tmp column
  df$tmp <- df[[var]]

  # Check for NA values
  check <- anyNA(df$tmp)
  if (check) {
    df_na <- df[is.na(df$tmp),]
    df <- df[!is.na(df$tmp),]
  }

  # Detect outliers by country
  df <- data.table::setDT(df)
  df <- df[, c('outlier', 'p_value') := get_outliers(tmp, alpha), by = iso3c]

  # Convert to tibble
  df <- dplyr::tibble(df)

  if (check) {
    df_na$outlier <- FALSE
    df_na$p_value <- NA_real_
    df <- rbind(df, df_na)
  }

  # Delete tmp
  df$tmp <- NULL

  # Order rows
  df <- df[order(df$iso3c, df$year),]

  return(df)
}


#' Get outliers
#'
#' Calculate t-scores and p-values for a numeric vector, and compare against a
#' given significance level threshold.
#'
#' @param x numeric: A vector of values.
#' @inheritParams detect_outliers
#'
#' @return data.frame
#' @keywords internal
get_outliers <- function(x, alpha) {

  if (length(x) > 1) {

    # z-scores
    z_scores <- abs((x - mean(x)) / sd(x))

    # p-values
    p_values <- pnorm(z_scores)

    # Two-tailed test
    threshold <- qnorm(alpha/2, lower.tail = FALSE)

    # Check scores against threshold
    check <- z_scores > threshold

    out <- data.frame(outlier = check, p_value = p_values)

  } else {
    out <- data.frame(outlier = FALSE, p_value = NA_real_)
  }

  return(out)
}


