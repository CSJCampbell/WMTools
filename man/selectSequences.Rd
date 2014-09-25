\name{selectSequences}
\alias{selectSequences}
\title{Select Rows from Matrix by Sequences}
\usage{
selectSequences(mat, from = 1, to = 1, pad = NA)
}
\arguments{
  \item{mat}{matrix from which to select}

  \item{from}{vector of starting positions}

  \item{to}{vector of finishing positions}

  \item{pad}{value with which to pad short rows (default
  NA)}
}
\description{
Select Rows from Matrix by Sequences
}
\examples{
ma <- matrix(letters[1:18], nrow = 3, ncol = 6, byrow = TRUE)
    selectSequences(mat = ma, from = 1:3, to = 4:6)
    selectSequences(mat = ma, from = 1:3, to = 4)
}

