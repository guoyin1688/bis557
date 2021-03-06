% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sparse-matrix.R
\name{sparse_transpose}
\alias{sparse_transpose}
\title{Transpose a sparse matrix}
\usage{
sparse_transpose(a)
}
\arguments{
\item{a}{a sparse.matrix object}
}
\value{
a sparse.matrix object
}
\description{
This function transposes a sparse matrix.
}
\examples{
a <- sparse.matrix(i = c(1, 2), j = c(1, 3), x = c(3, 1))
sparse_transpose(a)
}
