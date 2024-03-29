% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/isplit.R
\name{isplit}
\alias{isplit}
\title{Split a \code{mids} Object into List of \code{mids}}
\usage{
isplit(mids, chunk_size = 1L)
}
\arguments{
\item{mids}{A \code{mids} object, as created by \code{mice::mice()}}

\item{chunk_size}{The number of to allocate to each smaller \code{mids} object in
the resulting \code{list}. If \code{mids$m \%\% chunk_size != 0}, a smaller
\code{chunk_size} will be used.}
}
\value{
A \code{list} of \code{mids} objects
}
\description{
\code{isplit()} is the inverse of \code{\link[futuremice:ibindlist]{ibindlist()}}
and converts a single \code{mids} object into a list of \code{mids} objects containing
the largest number of chains possible while
\enumerate{
\item distributing chains evenly among \code{mids} objects and
\item keeping the number of chains per object <= \code{chunk_size}.
The resulting number of objects is specified implicitly through \code{mids$m} and
\code{chunk_size}.
}
}
\examples{
# Create `mids` object
mids <- mice::mice(mice::nhanes, m = 2L, maxit = 1L)

# Split into two - 1 imputation per object
mids_list <- isplit(mids)
mids_list

}
