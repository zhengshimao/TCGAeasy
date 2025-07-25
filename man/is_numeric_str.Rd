% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{is_numeric_str}
\alias{is_numeric_str}
\title{Check if a string is numeric-like}
\usage{
is_numeric_str(x)
}
\arguments{
\item{x}{A character string to test.}
}
\value{
A logical value. TRUE if the string looks like a number, FALSE otherwise.
}
\description{
This function checks whether the input string represents a numeric value,
allowing optional leading minus sign and decimal point.
Note: This is a simple check and does not guarantee strict numeric formatting.
}
\examples{
is_numeric_str("123")      # TRUE
is_numeric_str("-45.6")    # TRUE
is_numeric_str("12.3.4")   # TRUE (not strictly valid number)
is_numeric_str("abc")      # FALSE
}
