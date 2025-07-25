% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{get_numeric}
\alias{get_numeric}
\title{Extract Numeric from XML Node}
\usage{
get_numeric(doc, xpath, return_all = TRUE)
}
\arguments{
\item{doc}{An XML document parsed by \code{xml2::read_xml()}.}

\item{xpath}{A character string representing the XPath query.}

\item{return_all}{return all result or not. if TRUE, return all results. if FALSE, return unique result.}
}
\value{
An numeric vector of node values. Returns \code{NA} if the node is not found.
}
\description{
A helper function to extract numeric values from XML nodes using XPath.
}
\keyword{internal}
