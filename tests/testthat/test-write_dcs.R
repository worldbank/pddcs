bednets_data <- readRDS('../testdata/bednets_data.RDS')
bednets_meta <- readRDS('../testdata/bednets_meta.RDS')

# Tests
test_that('write_dcs() works correctly', {

  # Data format
  tmp <- write_dcs(bednets_data, type = 'data', path = tempfile(fileext = ".xlsx"))
  expect_identical(readxl::excel_sheets(tmp), 'Sheet1')
  out <- readxl::read_xlsx(tmp)
  expect_identical(as.data.frame(out), bednets_data)

  # Metadata format
  tmp <- write_dcs(bednets_meta, type = 'meta', path = tempfile(fileext = ".xlsx"))
  expect_identical(readxl::excel_sheets(tmp), 'Country-Series-Time_Table')
  out <- readxl::read_xlsx(tmp)
  expect_identical(as.data.frame(out), bednets_meta)

})
