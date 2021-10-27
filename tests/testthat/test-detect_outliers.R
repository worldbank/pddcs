data("bednets")
physicians <- readRDS('../testdata/physicians.RDS')

test_that("detect_outliers() works", {

  # Bednets data
  res <- detect_outliers(bednets, alpha = 0.2)
  expect_equal(sum(res$outlier), 30)
  res <- detect_outliers(bednets, alpha = 0.1)
  expect_equal(sum(res$outlier), 2)
  res <- detect_outliers(bednets, alpha = 0.05)
  expect_equal(sum(res$outlier), 0)

  # Physicians data
  res <- detect_outliers(physicians, alpha = 0.05)
  expect_equal(sum(res$outlier), 70)
  res <- detect_outliers(physicians, alpha = 0.01)
  expect_equal(sum(res$outlier), 18)
  res <- detect_outliers(physicians, alpha = 0.005)
  expect_equal(sum(res$outlier), 16)
  expect_equal(
    res[res$iso3c == "BEL" & res$year == 2019,]$p_value,
    0.99999244)
  expect_equal(
    res[res$iso3c == "NOR" & res$year == 1993,]$p_value,
    0.99801487)

})

test_that("get_outliers() works", {

  # alpha 0.1
  res <- get_outliers(x = c(1:15, 20, 22), alpha = 0.1)
  expect_false(all(res$outlier[1:15]))
  expect_true(all(res$outlier[16:17]))
  expect_equal(res$p_value[16], 0.95895776)
  expect_equal(res$p_value[17], 0.98081275)

  # alpha 0.05
  res <- get_outliers(x = c(1:15, 20, 22), alpha = 0.05)
  expect_false(all(res$outlier[1:16]))
  expect_true(res$outlier[17])
  expect_equal(res$p_value[17], 0.98081275)

  # alpha 0.01
  res <- get_outliers(x = c(1:15, 20, 22), alpha = 0.01)
  expect_false(all(res$outlier))
  expect_equal(res$p_value[17], 0.98081275)

  # Vectors of length one
  res <- get_outliers(x = 1, alpha = 0.1)
  expect_equal(nrow(res), 1)
  expect_equal(res$p_value, NA_real_)
  expect_false(res$outlier)

})

