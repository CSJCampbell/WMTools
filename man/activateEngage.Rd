\name{activateEngage}
\alias{activateEngage}
\title{Perform activation by engaging}
\usage{
activateEngage(warjack, target = list(stats = c(DEF = 12, ARM = 18, BASE =
  50)), boost_hit = TRUE, boost_damage = TRUE, foc = 3, kd = FALSE,
  dist = 6, dice = sample(1:6, size = 30, replace = TRUE))
}
\arguments{
  \item{warjack}{list active warjack object with elements
  stats, range, melee and special}

  \item{target}{list target warjack object with elements
  stats and special stats has elements \enumerate{ \item
  DEF single numeric NB should include effect of being
  knocked down (no adjustment here due to possible effect
  of other modifiers) \item ARM single numeric armour value
  \item BASE single numeric diameter of base (mm) }}

  \item{boost_hit}{single logical boost attack roll?}

  \item{boost_damage}{single logical boost damage roll?}

  \item{foc}{single numeric}

  \item{kd}{single logical is target knocked down?}

  \item{dist}{single numeric distance between bases
  (default 6)}

  \item{dice}{numeric vector}
}
\value{
single numeric
}
\description{
Perform activation by engaging
}
\examples{
blueleader <- list(stats = c(SPD = 5, MAT = 7, RAT = 5),
        range = list(),
        melee = list('quake hammer' = list(stats = c(RNG = 2, PAS = 18),
                special = c("crit knockdown")),
            'open fist' = list(stats = c(RNG = 0.5, PAS = 14), special = character(0))))
    WMTools:::activateEngage(blueleader, target = list(stats = list(DEF = 13, ARM = 13, BASE = 30)),
        boost_hit = TRUE, boost_damage = TRUE, foc = 3,
        dice = c(1, 5, 4, 1, 1, 2))
}

