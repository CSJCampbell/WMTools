\name{shot}
\alias{shot}
\title{Perform a Ranged Attack}
\usage{
shot(warjack, which = 1, target = list(stats = c(DEF = 12, ARM = 18, BASE =
  50)), boost_hit = TRUE, boost_damage = TRUE, foc = 0, kd = FALSE,
  dist = 6, dice = sample(1:6, size = 20, replace = TRUE), pos = 1,
  recycle = FALSE)
}
\arguments{
  \item{warjack}{list attacker warjack object with elements
  stats, range and melee and special}

  \item{which}{single integer specify ranged weapon to use}

  \item{target}{list target warjack object with elements
  stats and special stats has elements \enumerate{ \item
  DEF single numeric NB should include effect of being
  knocked down (no adjustment here due to possible effect
  of other modifiers) \item ARM single numeric armour value
  \item BASE single numeric diameter of base (mm) }}

  \item{boost_hit}{single logical}

  \item{boost_damage}{single logical}

  \item{foc}{single numeric number of focus}

  \item{kd}{single logical is the target knocked down?
  (default \code{FALSE})}

  \item{dist}{numeric vector distance between bases
  (default 6)}

  \item{dice}{numeric vector (default \code{sample(1:6)})}

  \item{pos}{single numeric}

  \item{recycle}{single logical should dice be recycled for
  each value of dist? (default \code{FALSE})}
}
\value{
named vector with elements \enumerate{ \item damage amount
of damage inflicted \item focus remaining focus following
attack \item knocked down is the target knocked down? 0:
No, 1: Yes \item position which die to use next \item hit
was the target hit? 0: No, 1: Yes }
}
\description{
Perform a Ranged Attack
}
\examples{
redbandit <- list(stats = c(SPD = 4, MAT = 6, RAT = 4, DEF = 10, ARM = 20, BASE = 50),
        range = list(bombard = list(
            stats = c(RNG = 14, POW = 14, AOE = 3),
            special = c("arcing"))),
        melee = list(axe = list(stats = c(RNG = 0.5, PAS = 12),
                special = c("crit amp"))))
    shot(redbandit, which = 1, target = redbandit,
        boost_hit = TRUE, boost_damage = TRUE, foc = 3, dist = 10,
        dice = c(1, 5, 4, 1, 1, 2))
}

