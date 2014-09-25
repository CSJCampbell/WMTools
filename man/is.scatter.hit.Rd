\name{is.scatter.hit}
\alias{is.scatter.hit}
\title{is target hit by scatter?}
\usage{
is.scatter.hit(weapon, short = 0, base = 30, max = 6, dice = sample(1:6,
  size = 2, replace = TRUE))
}
\arguments{
  \item{weapon}{length 2 list with elements stats and
  special.  stats is a length three vector with named
  elements RNG, POW and AOE special is a character vector}

  \item{short}{single numeric with value 0 if the target
  was in range or a positive number indicating inches out
  of range (default 0)}

  \item{base}{single numeric indicating size of target base
  in mm (default 30)}

  \item{max}{single numeric indicating the maximum scatter
  of the shot (default 6)}

  \item{dice}{length 2 numeric vector specifing (distance,
  direction)}
}
\value{
single logical
}
\description{
is target hit by scatter?
}
\examples{
is.scatter.hit(weapon = list(stats = c(RNG = 14, POW = 14, AOE = 3),
       special = c("arcing")))
}

