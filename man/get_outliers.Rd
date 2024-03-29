% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/detect_outliers.R
\name{get_outliers}
\alias{get_outliers}
\title{Get outliers}
\usage{
get_outliers(x, alpha)
}
\arguments{
\item{x}{numeric: A vector of values.}

\item{alpha}{numeric: Significance level for a two-tailed test. Defaults to 0.05.}
}
\value{
data.frame
}
\description{
Calculate t-scores and p-values for a numeric vector, and compare against a
given significance level threshold.
}
\keyword{internal}
