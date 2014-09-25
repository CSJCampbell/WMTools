\name{ranged}
\alias{ranged}
\title{Perform Ranged Attacks}
\usage{
ranged(warjack, target = list(stats = c(DEF = 12, ARM = 18, BASE = 50)),
  boost_hit = TRUE, boost_damage = TRUE, foc = 3, kd = FALSE,
  dist = 0, dice = sample(1:6, size = 30, replace = TRUE))
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
Perform Ranged Attacks
}
\examples{
redbandit <- list(stats = c(SPD = 4, MAT = 6, RAT = 4, DEF = 10, ARM = 20, BASE = 50),
        range = list(bombard = list(
                stats = c(RNG = 14, ROF = 1, AOE = 3, POW = 14),
                special = "arcing")
        ),
        melee = list(axe = list(stats = c(RNG = 0.5, PAS = 12),
                special = "critical amputation")),
        special = character(0))
ranged(redbandit, target = list(stats = c(DEF = 10, ARM = 14, BASE = 40)),
    boost_hit = TRUE, boost_damage = TRUE,
    foc = 2, dist = 2, dice = rep(6, 6))
ranged(redbandit,
    target = list(stats = c(DEF = 13, ARM = 13, BASE = 30)), boost_hit = FALSE, boost_damage = TRUE,
    foc = 3, dist = 0.1, dice = rep(5, 10))
}

