# Create vectors with WDI indicator codes
eurostat <- pddcs:::eurostat_indicators
unpd <- pddcs:::unpd_indicators
who <- pddcs:::who_indicators
unicef <- pddcs:::unicef_indicators

# Create 'indicatorlist' data frame
indicatorlist <-
  data.frame(
    indicator = c(eurostat, unpd, who, unicef),
    source_code = c('AVG', 'GBIRTHRT', rep(NA, length(unpd)),
                 pddcs:::recode_who_codes(who),
                 pddcs:::recode_unicef_codes(unicef)),
    source = c(
      rep('eurostat', length(eurostat)),
      rep('unpd', length(unpd)),
      rep('who', length(who)),
      rep('unicef', length(unicef))),
    source_link = c(
      rep('https://ec.europa.eu/eurostat/web/json-and-unicode-web-services',
          length(eurostat)),
      rep('https://population.un.org/wpp/', length(unpd)),
      rep('https://www.who.int/data/gho/info/gho-odata-api', length(who)),
      rep('https://sdmx.data.unicef.org/webservice/data.html', length(unicef)))
  )

# Save file
usethis::use_data(indicatorlist,
                  overwrite = TRUE,
                  compress = 'bzip2',
                  version = 3)
