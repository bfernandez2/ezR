% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/R ez.R
\name{fix_rut}
\alias{fix_rut}
\title{Fix Rut}
\usage{
fix_rut(x)
}
\arguments{
\item{x}{Vector of numeric type or character}
}
\description{
This function fixes the rut
}
\examples{
x = c("012345678-9","0123456789 "," 0.12345678 9")
fix_rut(x)
}
