
#' @title Perform Melee attacks
#' @param warjack list attacker warjack object with elements stats, range and melee and special
#' @param target list target warjack object with elements stats and special
#'     stats has elements \enumerate{
#'     \item DEF single numeric NB should include effect of being knocked down 
#'     (no adjustment here due to possible effect of other modifiers)
#'     \item ARM single numeric armour value
#'     \item BASE single numeric diameter of base (mm)
#' }
#' @param boost_hit single logical
#' @param boost_damage single logical
#' @param foc single numeric
#' @param kd single logical
#' @param dice numeric vector
#' @return single numeric
#' @export
#' @examples
#' blueleader <- list(stats = c(SPD = 5, MAT = 7, RAT = 5), 
#'     range = list(),
#'     melee = list('quake hammer' = list(stats = c(RNG = 2, PAS = 18), 
#'             special = c("crit knockdown")), 
#'         'open fist' = list(stats = c(RNG = 0.5, PAS = 14), special = character(0))))
#' melee(blueleader, target = list(stats = c(DEF = 12, ARM = 18)), 
#'    boost_hit = TRUE, boost_damage = TRUE, 
#'    foc = 3, dice = rep(6, 9))
#' melee(blueleader, target = list(stats = c(DEF = 12, ARM = 18)), 
#'     kd = TRUE,
#'     boost_hit = FALSE, boost_damage = FALSE, foc = 3, dice = rep(1, 11))

melee <- function(warjack, target = list(stats = c(DEF = 12, ARM = 18, BASE = 50)), 
    boost_hit = TRUE, boost_damage = TRUE, foc = 3, kd = FALSE, dice = sample(1:6, size = 30, replace = TRUE)) {
    if (foc < 1) { 
        warning("warjack is unable to charge into combat")
        return(as.numeric(0)) }
    num_cc <- length(warjack$melee)
    tot <- 0
    pos <- 1
    chain <- -1

    for (i in seq_len(num_cc)) {
        bd <- boost_damage
        chg <- FALSE
        if (i == 1) { chg <- TRUE }
        out <- attack(warjack = warjack, which = i, target = target, charge = chg,
            boost_hit = boost_hit, boost_damage = boost_damage, foc = foc, kd = kd, dice = dice, pos = pos)
        tot <- tot + unname(out['damage'])
        foc <- unname(out['focus'])
        kd <- unname(out['knocked down'])
        pos <- unname(out['position'])
        if ("chain attack bloodbath" %in% warjack$melee[[i]]$special & out['hit']) { 
            chain <- chain + 1 }
    }
    if (chain != 1) { chain <- 0 }
    for (i in seq_len(foc + chain)) {
        if (foc > 0 | chain == 1) {
            if (chain == 1) { chain <- 0 
            } else {foc <- foc - 1 }
            out <- attack(warjack = warjack, which = 1, target = target, 
                boost_hit = boost_hit, boost_damage = boost_damage, foc = foc, kd = kd, dice = dice, pos = pos)
            tot <- tot + unname(out['damage'])
            foc <- unname(out['focus'])
            kd <- unname(out['knocked down'])
            pos <- unname(out['position'])
        }
    }
    return(tot)
}
