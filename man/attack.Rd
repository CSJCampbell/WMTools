\name{attack}
\alias{attack}
\title{Perform a Melee Attack}
\usage{
attack(warjack, which = 1L, target = list(stats = c(DEF = 12, ARM = 18, BASE
  = 50)), charge = FALSE, boost_hit = TRUE, boost_damage = TRUE,
  foc = 0, kd = FALSE, dist = 0, dice = sample(1:6, size = 20, replace =
  TRUE), pos = 1)
}
\arguments{
  \item{warjack}{list warjack object with elements stats,
  range and melee}

  \item{which}{single integer or single character 'power'
  index of warjack weapon to use, or identify as a power
  attack}

  \item{target}{list target warjack object with elements
  stats and special stats has elements \enumerate{ \item
  DEF single numeric NB should include effect of being
  knocked down (no adjustment here due to possible effect
  of other modifiers) \item ARM single numeric armour value
  \item BASE single numeric diameter of base (mm) }}

  \item{charge}{single logical}

  \item{boost_hit}{single logical}

  \item{boost_damage}{single logical}

  \item{foc}{single numeric number of focus}

  \item{kd}{single logical is the target knocked down?
  (default \code{FALSE})}

  \item{dist}{single numeric distance between bases
  (default 0)}

  \item{dice}{numeric vector (default \code{sample(1:6)})}

  \item{pos}{single numeric (default 1)}
}
\value{
named vector with elements \enumerate{ \item damage amount
of damage inflicted \item focus remaining focus following
attack \item knocked down is the target knocked down? 0:
No, 1: Yes \item position which die to use next \item hit
was the target hit? 0: No, 1: Yes }
}
\description{
Perform a Melee Attack
}
\examples{
blueleader <- list(stats = c(SPD = 5, MAT = 7, RAT = 5),
        range = list(),
        melee = list('quake hammer' = list(stats = c(RNG = 2, PAS = 18),
                special = c("crit knockdown")),
            'open fist' = list(stats = c(RNG = 0.5, PAS = 14), special = character(0))))
    attack(blueleader, which = 1, target = list(stats = c(DEF = 13, ARM = 13, BASE = 30)),
        boost_hit = TRUE, boost_damage = TRUE, foc = 3,
        dice = c(1, 5, 4, 1, 1, 2))
}

