\name{is.seq.mat}
\alias{is.seq.mat}
\title{Is Matrix Element in Sequence by Row}
\usage{
is.seq.mat(from = 1, to = 1, dim = c(1, 1))
}
\arguments{
  \item{from}{vector of starting positions}

  \item{to}{vector of finishing positions}

  \item{dim}{length 2 numeric vector defining number of
  rows and columns of comparison matrix}
}
\value{
logical matrix with dimensions dim
}
\description{
Is Matrix Element in Sequence by Row
}
\examples{
is.seq.mat(from = 1:3, to = 4:6, dim = c(3, 6))
}

