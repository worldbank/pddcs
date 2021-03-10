
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pddcs

<!-- badges: start -->
<!-- badges: end -->

`pddcs` fetches raw data from different sources and prepares datasets
for upload to DCS. Current supported sources are UNICEF, WHO, Eurostat
and UN’s Population Division.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github('Aeilert/pddcs')
```

``` r
library(pddcs)
```

## Examples

**Fetch a single indicator:**

``` r
# Fetch population data from Eurostat
df <- fetch_indicator('SP.POP.TOTL', source = 'eurostat')
```

**Fetch multiple indicators:**

``` r
# Fetch multiple indicators from the same source
df <- fetch_indicator(
  indicator = c('SH.STA.ANV4.ZS', 'SN.ITK.VITA.ZS'),
  source = 'unicef')

# Fetch multiple indicators from different sources
df <- fetch_indicator(
  indicator = c('SH.STA.ANV4.ZS', 'SP.POP.TOTL'),
  source = c('unicef', 'eurostat'))
```

**Compare with WDI:**

``` r
# Compare with WDI
dl <- compare_with_wdi(df)
str(dl)
```

**Format data:**

``` r
# Convert to DCS 'data' format 
df <- format_dcs(df, type = 'data')
```

**Write data:**

``` r
# Write to a DCS formatted file  
write_dcs(df, path = 'data-SP.POP.TOTL-eurostat.xlsx', type = 'data')
```
