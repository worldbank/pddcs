test_that("create_blank() works correctly", {

  # Multiple indicators / countries / years (data)
  df <- create_blank(
    indicator = c('SP.POP.TOTL', 'SH.MLR.NETS.ZS'),
    iso3c = c('ALB', 'AGO'),
    year = 2014:2015)
  expect_identical(
    names(df), c('Time', 'Country', 'Series', 'Scale', 'Data'))
  expect_true(
    nrow(df) == 8
  )
  expect_identical(
    unique(df$Country), c('ALB', 'AGO')
  )
  expect_identical(
    unique(df$Time), paste0('YR', 2014:2015))
  expect_true(
    all(is.na(df$Data))
  )

  # All countries and years (metadata)
  df <- create_blank(
    'SP.POP.TOTL', type = 'meta')
  expect_identical(
    names(df), c('Country', 'Series', 'Time', 'Footnote'))
  expect_identical(
    unique(df$Country), wdi_country_codes
  )
  expect_identical(
    unique(df$Time), paste0('YR', 1960:2020))
  expect_true(
    all(is.na(df$Footnote))
  )

})
