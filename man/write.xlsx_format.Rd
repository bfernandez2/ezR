% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/R ez.R
\name{write.xlsx_format}
\alias{write.xlsx_format}
\title{Style write.xlsx_format xlsx}
\usage{
style_elegant_xlsx(sheet, name, data)
}
\arguments{
\item{sheet}{Sheet name}

\item{name}{file name}

\item{data}{Database name}
}
\description{
Write an excel file with format.
}
\examples{
write.xlsx_format("example","iris.xlsx",iris)
}
