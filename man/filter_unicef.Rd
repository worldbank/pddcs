% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter_unicef.R
\name{filter_unicef}
\alias{filter_unicef}
\title{Filter UNICEF}
\usage{
filter_unicef(df, priority)
}
\arguments{
\item{df}{data.frame: A \code{pddcs} formatted data frame. Output of
\code{fetch_indicator()}.}

\item{priority}{character: A vector with survey sources to prioritize
between. See details.}
}
\value{
data.frame
}
\description{
Remove duplicated country-year rows in UNICEF datasets, based on a specified
priority order.
}
\details{
The abbreviation of survey sources in the \code{note} column is used to
prioritize between different surveys if there are duplicated country-years.
Typical use cases are MIS, DHS, MICS and AIS. The surveys are prioritized in
the order they are listed, e.g. if \code{priority = c('X', 'Y', 'Z')}, then X will
be chosen before Y, which again will be chosen before Z.
}
\keyword{internal}
