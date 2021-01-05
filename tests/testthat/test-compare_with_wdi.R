data('bednets')
bednets_compare <- readRDS('../testdata/bednets_compare.RDS')

# Tests
test_that('compare_with_wdi() works correctly', {

  dl <- compare_with_wdi(bednets)
  expect_identical(names(dl), c('source', 'wdi', 'not_in_wdi'))
  expect_identical(names(dl$source), c('iso3c', 'year', 'indicator', 'value', 'note', 'source'))
  expect_identical(names(dl$wdi), c('iso3c', 'year', 'indicator', 'value', 'source'))
  expect_identical(names(dl$not_in_wdi), c('iso3c', 'year', 'indicator', 'value', 'note', 'source'))
  expect_identical(dl, bednets_compare)

})
