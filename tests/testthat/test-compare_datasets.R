dl <- readRDS('../testdata/nurses.RDS')

test_that("compare_datasets() works", {

  res <- compare_datasets(new = dl$source, current = dl$wdi)

  expect_identical(class(res), c("tbl_df", "tbl", "data.frame"))
  expect_identical(
    names(res),
    c('iso3c','year','indicator','value','note','source','current_value',
      'current_source','diff','outlier','p_value','n_diff','n_outlier')
  )
  expect_equal(nrow(res), nrow(dl$source))

  # AUS
  tmp <- res[res$iso3c == "AUS" & res$diff > 0 & !is.na(res$diff), ]
  expect_equal(nrow(tmp), 4)
  expect_equal(unique(tmp$n_diff), sum(tmp$diff > 0))
  expect_equal(unique(tmp$n_outlier), sum(tmp$outlier))

  # BLZ
  tmp <- res[res$iso3c == "BLZ", ]
  expect_equal(nrow(tmp), 9)
  expect_false(all(tmp$outlier))
  expect_equal(unique(tmp$n_diff), sum(tmp$diff > 0))
  expect_equal(unique(tmp$n_outlier), sum(tmp$outlier))

})
