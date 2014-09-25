\name{is.engaged}
\alias{is.engaged}
\title{Is Attacker Engaged/Engaging?}
\usage{
is.engaged(warjack, target, dist, kd = FALSE)
}
\arguments{
  \item{warjack}{list attacker warjack object with elements
  melee}

  \item{target}{list target warjack object with elements
  melee}

  \item{dist}{numeric vector distance between bases}

  \item{kd}{logical vector length 1 or length dist is the
  target knocked down? (default \code{FALSE}) NB Knocked
  down models never engage}
}
\value{
single logical
}
\description{
Is Attacker Engaged/Engaging?
}
\examples{
blueleader <- list(stats = c(SPD = 5, MAT = 7, RAT = 5),
    range = list(),
    melee = list('quake hammer' = list(stats = c(RNG = 2, PAS = 18),
            special = c("crit knockdown")),
        'open fist' = list(stats = c(RNG = 0.5, PAS = 14), special = character(0))))
is.engaged(warjack = blueleader, target = redbandit, dist = 1)
is.engaged(warjack = blueleader, target = redbandit, dist = 0)
is.engaged(warjack = blueleader, target = redbandit, dist = 0, kd = TRUE)
is.engaged(warjack = blueleader, target = redbandit, dist = 0:2, kd = TRUE)
is.engaged(warjack = list(), target = list(), dist = 0:2, kd = TRUE)
}

