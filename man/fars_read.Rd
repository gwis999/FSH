% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_read}
\alias{fars_read}
\title{Read a fars data set}
\usage{
fars_read(filename)
}
\arguments{
\item{filename}{A character string with the name of the fars
data set file. May include path information.}
}
\value{
A tibble
}
\description{
Read the named fars data set and return a tibble.
Returns an error if the specified fars data set is not found.
}
\seealso{
Documentation for tibble package
}
