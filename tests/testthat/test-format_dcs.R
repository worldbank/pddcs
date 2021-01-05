data('bednets')
bednets_data <- readRDS('../testdata/bednets_data.RDS')
bednets_meta <- readRDS('../testdata/bednets_meta.RDS')

# Tests
test_that('format_dcs() works correctly', {

  # Data format
  df <- format_dcs(bednets, type = 'data')
  expect_identical(class(df), 'data.frame')
  expect_identical(names(df), c('Time', 'Country', 'Series', 'Scale', 'Data'))
  expect_identical(class(df$Time), 'character')
  expect_identical(class(df$Country), 'character')
  expect_identical(class(df$Series), 'character')
  expect_identical(class(df$Scale), 'numeric')
  expect_identical(class(df$Data), 'numeric')
  expect_identical(df, bednets_data)

  # Metadata format
  df <- format_dcs(bednets, type = 'meta')
  expect_identical(class(df), 'data.frame')
  expect_identical(names(df), c('Country', 'Series', 'Time', 'Footnote'))
  expect_identical(class(df$Time), 'character')
  expect_identical(class(df$Country), 'character')
  expect_identical(class(df$Series), 'character')
  expect_identical(class(df$Footnote), 'character')
  expect_identical(df, bednets_meta)

})
