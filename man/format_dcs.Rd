% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format_dcs.R
\name{format_dcs}
\alias{format_dcs}
\title{Convert to DCS format}
\usage{
format_dcs(df, type = c("data", "meta"))
}
\arguments{
\item{df}{data.frame: A \code{pddcs} formatted data frame. Output of
\code{fetch_indicator()}.}

\item{type}{character: Type, either 'data' or 'meta'.}
}
\value{
tibble
}
\description{
Convert a dataset retrieved with \code{\link[=fetch_indicator]{fetch_indicator()}} to DCS format.
}
\details{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}
}
\examples{
# Load example data
data("bednets")

# Convert to 'data' format
format_dcs(bednets, type = "data")

# Convert to 'metadata' format
format_dcs(bednets, type = "meta")
}
