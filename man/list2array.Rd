% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{list2array}
\alias{list2array}
\title{list2array}
\usage{
list2array(x)
}
\description{
If a given list (of length l >= 0) of numeric vectors has all vectors of the
same length k >= 0 this function returns a numeric matrix (k, l):
each column of the matrix corresponds to one element from the list.
If lengths of vectors in given list are not the same, a function returns
this list.
}
\details{
ARGUMENTS:
x - a list (of length l >= 0) of numeric vectors

RETURN VALUE:
a matrix or a list
}
\seealso{
Other fungroup: \code{\link{freq_value}};
  \code{\link{perms}}
}

