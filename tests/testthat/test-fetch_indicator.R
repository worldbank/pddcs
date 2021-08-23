# data('indicatorlist')
# indicators <- indicatorlist$indicator
# sources <- indicatorlist$source

# Constants
indicators <- c(
  "SP.DYN.CBRT.IN", "SP.POP.TOTL", "SP.DYN.TFRT.IN", "SP.POP.AG00.MA.IN", "SP.POP.TOTL",
  "SH.MLR.NETS.ZS", "SH.STA.BRTW.ZS", "SH.STA.ORCF.ZS", "SP.REG.BRTH.ZS", "SH.HIV.KNOW.FE.ZS",
  "SH.DYN.NCOM.ZS", "SH.ALC.PCAP.LI", "SH.STA.POIS.P5", "SH.STA.TRAF.P5", "SH.PRV.SMOK"
)
sources <- c(rep("eurostat", 2), rep("unpd", 3), rep("unicef", 5), rep("who", 5))

# Tests
test_that("fetch_indicator() works for a sample of available indicators", {
  skip_if_offline()
  purrr::map2(indicators, sources, .f = function(x, y) {
    df <- fetch_indicator(x, source = y)
    expect_identical(class(df), c("tbl_df", "tbl", "data.frame"))
    expect_identical(names(df), c(
      "iso3c", "year", "indicator", "value",
      "note", "source"
    ))
    expect_identical(class(df$iso3c), "character")
    expect_identical(class(df$year), "numeric")
    expect_identical(class(df$indicator), "character")
    expect_identical(class(df$value), "numeric")
    expect_identical(class(df$note), "character")
    expect_identical(class(df$source), "character")
  })
})
