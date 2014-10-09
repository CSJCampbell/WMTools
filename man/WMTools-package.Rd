\docType{package}
\name{WMTools-package}
\alias{WMTools}
\alias{WMTools-package}
\title{Tools for simulating activations in Warmachine(R)}
\description{
Simulate ranged and melee attacks in the game of
Warmachine(R)
}
\details{
\tabular{ll}{ Package: \tab WMTools \cr Type: \tab Package
\cr Version: \tab 0.1 \cr Date: \tab 2014-10-09 \cr
Lazyload: \tab yes \cr }
}
\section{Special abilities recognized}{
  warjack \enumerate{ \item gunfighter (\code{shot}) }
}

\section{Special abilities recognized}{
  range \enumerate{ \item free boost hit (\code{shot})
  \item free boost damage (\code{shot}) \item ammo
  type:quake (\code{shot}) \item critical knockdown
  (\code{shot}) \item critical devastation (\code{shot})
  \item linked guns (\code{ranged}) \item rapid fire
  (\code{ranged}) }
}

\section{Special abilities recognized}{
  melee \enumerate{ \item chain attack bloodbath
  (\code{melee}) \item powerful charge (\code{attack})
  \item crit knockdown (\code{attack}) }
}

\section{Special abilities recognized}{
  target \enumerate{ \item stealth (\code{shot}) }
}
\examples{
blueleader <- list(stats = c(SPD = 5, MAT = 7, RAT = 5),
        range = list(),
        melee = list('quake hammer' = list(stats = c(RNG = 2, PAS = 18),
                special = c("crit knockdown")),
            'open fist' = list(stats = c(RNG = 0.5, PAS = 14), special = character(0))))
    activation(blueleader, which = 1, target = list(stats = list(DEF = 13, ARM = 13, BASE = 30)),
        strategy = "aim", boost_hit = TRUE, boost_damage = TRUE, foc = 3,
        dice = c(1, 5, 4, 1, 1, 2))
    activation(blueleader, which = 1, target = list(stats = list(DEF = 13, ARM = 13, BASE = 30)),
        strategy = "charge", boost_hit = TRUE, boost_damage = TRUE, foc = 3,
        dice = c(1, 5, 4, 1, 1, 2))
}
\keyword{game}
\keyword{package}
\keyword{simulation}

