
# pddcs 0.0.0.9400

## New features

* Add a new function `detect_outliers()`, which detects potential outliers in a single dataset by calculating z-scores/p-values for each `value` by country.
* Add a new function `compare_datasets()`, which compares a dataset from source with a dataset in WDI/DCS and identifies differences and potential outliers.
# pddcs 0.0.0.9300
## Breaking changes

* `format_dcs()` no longer removes missing values.

## New features

* Add a new function `create_blank()`, which creates DCS formatted data or metadata datasets with empty values.
* Add a new function `prepare_data()` to prepare internal or manually downloaded datasets for DCS upload.
* Add pipe operator (`%>%`) to exports.

## Enhancements

* Update unit tests to testthat's 3rd edition.  
* `fetch_indicator()` now returns a `tibble` instead of a `data.frame`.  

# pddcs 0.0.0.9200

* First stable development version of the package.
* `fetch_indicator()` now accepts multiple indicators in a single query.
* Fix bug in `fetch_wdi()` to ensure that `compare_with_wdi()` works for multiple indicators.
* Added data source vignettes.
* Added pkgdown website.
* Added a `NEWS.md` file to track changes to the package.
