\name{melee}
\alias{melee}
\title{Perform Melee attacks}
\usage{
melee(warjack, target = list(stats = c(DEF = 12, ARM = 18, BASE = 50)),
  boost_hit = TRUE, boost_damage = TRUE, foc = 3, kd = FALSE,
  dice = sample(1:6, size = 30, replace = TRUE))
}
\arguments{
  \item{warjack}{list attacker warjack object with elements
  stats, range and melee and special}

  \item{target}{list target warjack object with elements
  stats and special stats has elements \enumerate{ \item
  DEF single numeric NB should include effect of being
  knocked down (no adjustment here due to possible effect
  of other modifiers) \item ARM single numeric armour value
  \item BASE single numeric diameter of base (mm) }}

  \item{boost_hit}{single logical}

  \item{boost_damage}{single logical}

  \item{foc}{single numeric}

  \item{kd}{single logical}

  \item{dice}{numeric vector}
}
\value{
single numeric
}
\description{
Perform Melee attacks
}
\examples{
blueleader <- list(stats = c(SPD = 5, MAT = 7, RAT = 5),
    range = list(),
    melee = list('quake hammer' = list(stats = c(RNG = 2, PAS = 18),
            special = c("crit knockdown")),
        'open fist' = list(stats = c(RNG = 0.5, PAS = 14), special = character(0))))
melee(blueleader, target = list(stats = c(DEF = 12, ARM = 18)),
   boost_hit = TRUE, boost_damage = TRUE,
   foc = 3, dice = rep(6, 9))
melee(blueleader, target = list(stats = c(DEF = 12, ARM = 18)),
    kd = TRUE,
    boost_hit = FALSE, boost_damage = FALSE, foc = 3, dice = rep(1, 11))
}

