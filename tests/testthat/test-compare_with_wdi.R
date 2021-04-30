data('bednets')
bednets_compare <- readRDS('../testdata/bednets_compare.RDS')

# Tests
test_that('compare_with_wdi() works correctly', {

  dl <- compare_with_wdi(bednets)

  # Names and classes
  expect_identical(names(dl), c('source', 'wdi', 'not_in_wdi'))
  expect_identical(names(dl$source),
                   c('iso3c', 'year', 'indicator',
                     'value', 'note', 'source'))
  expect_identical(names(dl$wdi),
                   c('iso3c', 'year', 'indicator',
                     'value', 'source'))
  expect_identical(names(dl$not_in_wdi),
                   c('iso3c', 'year', 'indicator',
                     'value', 'note', 'source'))

  # Source
  expect_identical(dl$source$iso3c, bednets_compare$source$iso3c)
  expect_identical(dl$source$year, bednets_compare$source$year)
  expect_identical(dl$source$indicator, bednets_compare$source$indicator)
  expect_identical(dl$source$note, bednets_compare$source$note)
  expect_identical(dl$source$source, bednets_compare$source$source)
  expect_equal(dl$source$value, bednets_compare$source$value)

  # WDI
  expect_identical(dl$wdi$iso3c, bednets_compare$wdi$iso3c)
  expect_identical(dl$wdi$year, bednets_compare$wdi$year)
  expect_identical(dl$wdi$indicator, bednets_compare$wdi$indicator)
  expect_identical(dl$wdi$note, bednets_compare$wdi$note)
  expect_identical(dl$wdi$source, bednets_compare$wdi$source)
  expect_equal(dl$wdi$value, bednets_compare$wdi$value)

  # Comparison (antijoin)
  expect_identical(dl$not_in_wdi$iso3c, bednets_compare$not_in_wdi$iso3c)
  expect_identical(dl$not_in_wdi$year, bednets_compare$not_in_wdi$year)
  expect_identical(dl$not_in_wdi$indicator, bednets_compare$not_in_wdi$indicator)
  expect_identical(dl$not_in_wdi$note, bednets_compare$not_in_wdi$note)
  expect_identical(dl$not_in_wdi$source, bednets_compare$not_in_wdi$source)
  expect_equal(dl$not_in_wdi$value, bednets_compare$not_in_wdi$value)

})
