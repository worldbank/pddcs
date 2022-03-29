path <- system.file("extdata",  "data-nurses-and-midwives-in-dcs.xlsx", package = "pddcs")

test_that("read_dcs() returns the correct format", {
  df <- read_dcs(path)
  expect_true(dplyr::is.tbl(df))
  expect_identical(names(df), c("iso3c", "year", "indicator", "value", "source"))
  expect_identical(class(df$iso3c), "character")
  expect_identical(class(df$year), "numeric")
  expect_identical(class(df$indicator), "character")
  expect_identical(class(df$value), "numeric")
  expect_identical(class(df$source), "character")
})

test_that("read_dcs() returns the correct output", {
  df <- read_dcs(path)
  df2 <- readxl::read_xlsx(path)
  expect_equal(dim(df), dim(df2))
})
