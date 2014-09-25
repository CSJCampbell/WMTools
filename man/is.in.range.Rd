\name{is.in.range}
\alias{is.in.range}
\title{is a Ranged Weapon in Range (including scatter tagging)?}
\usage{
is.in.range(weapon, dist = 6)
}
\arguments{
  \item{weapon}{list with element stats containing a named
  numeric vector with elements 'AOE' and 'RNG'}

  \item{dist}{single numeric distance between bases
  (default 6)}
}
\value{
single logical
}
\description{
is a Ranged Weapon in Range (including scatter tagging)?
}
\examples{
is.in.range(list(stats = c(AOE = 10, RNG = 1)), dist = 10)
}

