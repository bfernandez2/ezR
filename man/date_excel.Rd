% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/R ez.R
\name{date_excel}
\alias{date_excel}
\title{Convert dates from Excel}
\usage{
date_excel(x)
}
\arguments{
\item{x}{numerical vector}
}
\description{
This function returns a vector of dates with excel origin, which is "1899-12-30"
}
\examples{
x = c(44197,44198,44198)
date_excel(x)
}
