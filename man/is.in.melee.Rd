\name{is.in.melee}
\alias{is.in.melee}
\title{is a Melee Weapon in Range}
\usage{
is.in.melee(weapon, dist = 6)
}
\arguments{
  \item{weapon}{list with element stats containing a named
  numeric vector with element 'RNG'}

  \item{dist}{single numeric distance between bases
  (default 6)}
}
\value{
single logical
}
\description{
is a Melee Weapon in Range
}
\examples{
is.in.melee(list(stats = c(RNG = 2)), dist = 1)
is.in.melee(list(stats = c(RNG = 0.5)), dist = 1)
}

