files <- c(
  '../testdata/UN SDG - 10.c_inquiry response.xlsx',
  '../testdata/homicide_all.xls',
  '../testdata/wdi_latest.xltm'
)
sources <- c('rmt', 'homicide', 'mdim')

test_that("prepare_data() returns the correct type of output", {

  purrr::map2(
    files, sources,
    function(file, source) {
      skip_if(!file.exists(file))
      df <- suppressWarnings(
        prepare_data(path = file, source = source))
      expect_identical(
        names(df), c('iso3c', 'year', 'indicator',
                     'value', 'note', 'source'))
      expect_identical(class(df$iso3c), 'character')
      expect_identical(class(df$year), 'numeric')
      expect_identical(class(df$indicator), 'character')
      expect_identical(class(df$value), 'numeric')
      expect_identical(class(df$note), 'character')
      expect_identical(class(df$source), 'character')
    }
  ) %>% invisible()

})
