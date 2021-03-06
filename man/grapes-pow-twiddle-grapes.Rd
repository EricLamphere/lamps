% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{\%^~\%}
\alias{\%^~\%}
\title{Anti-Pattern Matching}
\usage{
x \%^~\% reg
}
\arguments{
\item{x}{Character vector}

\item{reg}{Character string containing a regular expression to be anti-matched in the given character vector.}
}
\value{
See \code{grepl} documentation for more information.
}
\description{
Search for anti-matches to an argument pattern within each element of a character vector. Uses perl style regex and ignores case. See \code{grepl} documentation for more information.
}
