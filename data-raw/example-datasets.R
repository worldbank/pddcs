# Fetch datasets
population <- fetch_indicator('SP.POP.TOTL', source = 'eurostat')
bednets <- fetch_indicator('SH.MLR.NETS.ZS', source = 'unicef')

# Save files
usethis::use_data(
  population,
  bednets,
  overwrite = TRUE,
  compress = 'bzip2',
  version = 3
  )
