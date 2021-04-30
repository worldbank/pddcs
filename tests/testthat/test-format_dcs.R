data('bednets')
bednets_data <- readRDS('../testdata/bednets_data.RDS')
bednets_meta <- readRDS('../testdata/bednets_meta.RDS')

# Tests
test_that('format_dcs() works correctly', {

  # Data format
  df <- format_dcs(bednets, type = 'data')
  expect_identical(class(df), c('tbl_df', 'tbl', 'data.frame'))
  expect_identical(names(df), c('Time', 'Country', 'Series', 'Scale', 'Data'))
  expect_identical(class(df$Time), 'character')
  expect_identical(class(df$Country), 'character')
  expect_identical(class(df$Series), 'character')
  expect_identical(class(df$Scale), 'numeric')
  expect_identical(class(df$Data), 'numeric')
  expect_identical(df$Time, bednets_data$Time)
  expect_identical(df$Country, bednets_data$Country)
  expect_identical(df$Series, bednets_data$Series)
  expect_identical(df$Scale, bednets_data$Scale)
  expect_identical(df$Data, bednets_data$Data)


  # Metadata format
  df <- format_dcs(bednets, type = 'meta')
  expect_identical(class(df), c('tbl_df', 'tbl', 'data.frame'))
  expect_identical(names(df), c('Country', 'Series', 'Time', 'Footnote'))
  expect_identical(class(df$Time), 'character')
  expect_identical(class(df$Country), 'character')
  expect_identical(class(df$Series), 'character')
  expect_identical(class(df$Footnote), 'character')
  expect_identical(df$Time, bednets_meta$Time)
  expect_identical(df$Country, bednets_meta$Country)
  expect_identical(df$Series, bednets_meta$Series)
  expect_identical(df$Footnote, bednets_meta$Footnote)

})
