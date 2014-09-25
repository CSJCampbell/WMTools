WMTools
=======

An [R package](http://www.r-project.org/) to simulating activations in Warmachine(R). 
See [analytical gaming](http://lacerto1.wordpress.com/) for more information.

how to use this code
--------

```R
# install devtools for devtools::install_github
install.packages("devtools")
require(devtools)
# install visualTest
install_github("CSJCampbell/WMTools")
```

This package also includes tests in **testthat** format. From R run the call `test_package("WMTools")`.
   
```R
require(WMTools)
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
```
