#' Simulate ranged and melee attacks in the game of Warmachine(R)
#' 
#' \tabular{ll}{
#'  Package:  \tab  WMTools \cr
#'  Type:  \tab  Package \cr
#'  Version:  \tab  0.1 \cr
#'  Date:  \tab  2014-09-26 \cr
#'  Lazyload:  \tab  yes \cr
#' }
#' @name WMTools-package
#' @aliases WMTools
#' @docType package
#' @title Tools for simulating activations in Warmachine(R)
#' @keywords package game simulation
NULL

#' @title Perform activation following a named strategy
#' @param warjack list active warjack object with elements stats, range, melee and special
#' @param target list target warjack object with elements stats and special
#'     stats has elements \enumerate{
#'     \item DEF single numeric NB should include effect of being knocked down 
#'     (no adjustment here due to possible effect of other modifiers)
#'     \item ARM single numeric armour value
#'     \item BASE single numeric diameter of base (mm)
#' }
#' @param strategy single character attack mode, from list 'aim', 'assault' (walk and shoot), 'engage' (walk and melee), 'charge', 'trample', 'slam', 'headbutt'
#' @param boost_hit single logical boost attack roll?
#' @param boost_damage single logical boost damage roll?
#' @param foc single numeric
#' @param kd single logical is target knocked down?
#' @param dist single numeric distance between bases (default 6)
#' @param dice numeric vector
#' @return single numeric
#' @export
#' @examples
#'     blueleader <- list(stats = c(SPD = 5, MAT = 7, RAT = 5), 
#'         range = list(),
#'         melee = list('quake hammer' = list(stats = c(RNG = 2, PAS = 18), 
#'                 special = c("crit knockdown")), 
#'             'open fist' = list(stats = c(RNG = 0.5, PAS = 14), special = character(0))))
#'     activation(blueleader, which = 1, target = list(stats = list(DEF = 13, ARM = 13, BASE = 30)), 
#'         strategy = "aim", boost_hit = TRUE, boost_damage = TRUE, foc = 3, 
#'         dice = c(1, 5, 4, 1, 1, 2))
#'     activation(blueleader, which = 1, target = list(stats = list(DEF = 13, ARM = 13, BASE = 30)), 
#'         strategy = "charge", boost_hit = TRUE, boost_damage = TRUE, foc = 3, 
#'         dice = c(1, 5, 4, 1, 1, 2))

activation <- function(warjack, target = list(stats = c(DEF = 12, ARM = 18, BASE = 50)), 
    strategy = "aim", boost_hit = TRUE, boost_damage = TRUE, 
    foc = 3, kd = FALSE, dist = 6,
    dice = sample(1:6, size = 30, replace = TRUE)) {
    
    out <- switch(strategy, 
        aim = activateAim(warjack = warjack, target = target, 
            boost_hit = boost_hit, boost_damage = boost_damage, 
            foc = foc, kd = kd, dist = dist, dice = dice),
        assault = activateAssault(warjack = warjack, target = target, 
            boost_hit = boost_hit, boost_damage = boost_damage, 
            foc = foc, kd = kd, dist = dist, dice = dice),
        engage = activateEngage(warjack = warjack, target = target, 
            boost_hit = boost_hit, boost_damage = boost_damage, 
            foc = foc, kd = kd, dist = dist, dice = dice),
        charge = activateCharge(warjack = warjack, target = target, 
            boost_hit = boost_hit, boost_damage = boost_damage, 
            foc = foc, kd = kd, dist = dist, dice = dice),
        trample = activateTrample(warjack = warjack, target = target, 
            boost_hit = boost_hit, boost_damage = boost_damage, 
            foc = foc, kd = kd, dist = dist, dice = dice),
        slam = activateSlam(warjack = warjack, target = target, 
            boost_hit = boost_hit, boost_damage = boost_damage, 
            foc = foc, kd = kd, dist = dist, dice = dice),
        headbutt = activateHeadbutt(warjack = warjack, target = target, 
            boost_hit = boost_hit, boost_damage = boost_damage, 
            foc = foc, kd = kd, dist = dist, dice = dice), 
        stop("not implemented"))
        
    return(out)
}


#' @title Perform activation by aiming
#' @param warjack list active warjack object with elements stats, range, melee and special
#' @param target list target warjack object with elements stats and special
#'     stats has elements \enumerate{
#'     \item DEF single numeric NB should include effect of being knocked down 
#'     (no adjustment here due to possible effect of other modifiers)
#'     \item ARM single numeric armour value
#'     \item BASE single numeric diameter of base (mm)
#' }
#' @param strategy single character attack mode, from list 'aim', 'assault' (walk and shoot), 'engage' (walk and melee), 'charge', 'trample', 'slam', 'headbut'
#' @param boost_hit single logical boost attack roll?
#' @param boost_damage single logical boost damage roll?
#' @param foc single numeric
#' @param kd single logical is target knocked down?
#' @param dist single numeric distance between bases (default 6)
#' @param dice numeric vector
#' @return single numeric
#' @examples
#'     blueleader <- list(stats = c(SPD = 5, MAT = 7, RAT = 5), 
#'         range = list(),
#'         melee = list('quake hammer' = list(stats = c(RNG = 2, PAS = 18), 
#'                 special = c("crit knockdown")), 
#'             'open fist' = list(stats = c(RNG = 0.5, PAS = 14), special = character(0))))
#'     WMTools:::activateAim(blueleader, target = list(stats = list(DEF = 13, ARM = 13, BASE = 30)), 
#'         boost_hit = TRUE, boost_damage = TRUE, foc = 3, 
#'         dice = c(1, 5, 4, 1, 1, 2))

activateAim <- function(warjack, target = list(stats = c(DEF = 12, ARM = 18, BASE = 50)), 
    boost_hit = TRUE, boost_damage = TRUE, 
    foc = 3, kd = FALSE, dist = 6,
    dice = sample(1:6, size = 30, replace = TRUE)) {
    
    if (!all(is.element(c("stats", "range"), names(warjack)))) {
        stop("missing elements in warjack object") }
    
    damage <- 0
    
    if (length(warjack$range) > 0) {
        
        checkRange <- sapply(warjack$range, is.in.range, dist = dist)
        
        if (any(checkRange)) { 
            
            warjack$stats["RAT"] <- warjack$stats["RAT"] + 2
            
            damage <- ranged(warjack = warjack, target = target, 
                boost_hit = boost_hit, boost_damage = boost_damage, 
                foc = foc, kd = kd, dist = dist, dice = dice)
        }
    }
    
    return(damage)
}


#' @title Perform activation by assaulting
#' @param warjack list active warjack object with elements stats, range, melee and special
#' @param target list target warjack object with elements stats and special
#'     stats has elements \enumerate{
#'     \item DEF single numeric NB should include effect of being knocked down 
#'     (no adjustment here due to possible effect of other modifiers)
#'     \item ARM single numeric armour value
#'     \item BASE single numeric diameter of base (mm)
#' }
#' @param boost_hit single logical boost attack roll?
#' @param boost_damage single logical boost damage roll?
#' @param foc single numeric
#' @param kd single logical is target knocked down?
#' @param dist single numeric distance between bases (default 6)
#' @param dice numeric vector
#' @return single numeric
#' @examples
#'     blueleader <- list(stats = c(SPD = 5, MAT = 7, RAT = 5), 
#'         range = list(),
#'         melee = list('quake hammer' = list(stats = c(RNG = 2, PAS = 18), 
#'                 special = c("crit knockdown")), 
#'             'open fist' = list(stats = c(RNG = 0.5, PAS = 14), special = character(0))))
#'     WMTools:::activateAssault(blueleader, 
#'         target = list(stats = list(DEF = 13, ARM = 13, BASE = 30)), 
#'         boost_hit = TRUE, boost_damage = TRUE, foc = 3, 
#'         dice = c(1, 5, 4, 1, 1, 2))

activateAssault <- function(warjack, target = list(stats = c(DEF = 12, ARM = 18, BASE = 50)), 
    boost_hit = TRUE, boost_damage = TRUE, 
    foc = 3, kd = FALSE, dist = 6,
    dice = sample(1:6, size = 30, replace = TRUE)) {
    
    if (!all(is.element(c("stats", "range"), names(warjack)))) {
        stop("missing elements in warjack object") }
    
    dist <- dist - warjack$stats["SPD"]
    
    if (dist < 0) { dist <- 0 }
    
    damage <- 0
    
    if (length(warjack$range) > 0) {
        
        checkRange <- sapply(warjack$range, is.in.range, dist = dist)
        
        if (any(checkRange)) {
            
            damage <- ranged(warjack = warjack, target = target, 
                boost_hit = boost_hit, boost_damage = boost_damage, 
                foc = foc, kd = kd, dist = dist, dice = dice)
        }
    }
    
    return(damage)
}


#' @title Perform activation by engaging
#' @param warjack list active warjack object with elements stats, range, melee and special
#' @param target list target warjack object with elements stats and special
#'     stats has elements \enumerate{
#'     \item DEF single numeric NB should include effect of being knocked down 
#'     (no adjustment here due to possible effect of other modifiers)
#'     \item ARM single numeric armour value
#'     \item BASE single numeric diameter of base (mm)
#' }
#' @param boost_hit single logical boost attack roll?
#' @param boost_damage single logical boost damage roll?
#' @param foc single numeric
#' @param kd single logical is target knocked down?
#' @param dist single numeric distance between bases (default 6)
#' @param dice numeric vector
#' @return single numeric
#' @examples
#'     blueleader <- list(stats = c(SPD = 5, MAT = 7, RAT = 5), 
#'         range = list(),
#'         melee = list('quake hammer' = list(stats = c(RNG = 2, PAS = 18), 
#'                 special = c("crit knockdown")), 
#'             'open fist' = list(stats = c(RNG = 0.5, PAS = 14), special = character(0))))
#'     WMTools:::activateEngage(blueleader, target = list(stats = list(DEF = 13, ARM = 13, BASE = 30)), 
#'         boost_hit = TRUE, boost_damage = TRUE, foc = 3, 
#'         dice = c(1, 5, 4, 1, 1, 2))

activateEngage <- function(warjack, target = list(stats = c(DEF = 12, ARM = 18, BASE = 50)), 
    boost_hit = TRUE, boost_damage = TRUE, 
    foc = 3, kd = FALSE, dist = 6,
    dice = sample(1:6, size = 30, replace = TRUE)) {
    
    if (!all(is.element(c("stats", "range"), names(warjack)))) {
        stop("missing elements in warjack object") }
    
    dist <- dist - warjack$stats["SPD"]
    
    if (dist < 0) { dist <- 0 }
    
    damage <- 0
    
    if (length(warjack$melee) > 0) {
        
        checkRange <- sapply(warjack$melee, is.in.melee, dist = dist)
        
        if (any(checkRange)) {
            
            damage <- melee(warjack = warjack, target = target, 
                boost_hit = boost_hit, boost_damage = boost_damage, 
                foc = foc, kd = kd, dice = dice)
        }
    }
    
    return(damage)
}



#' @title Perform activation by charging
#' @param warjack list active warjack object with elements stats, range, melee and special
#' @param target list target warjack object with elements stats and special
#'     stats has elements \enumerate{
#'     \item DEF single numeric NB should include effect of being knocked down 
#'     (no adjustment here due to possible effect of other modifiers)
#'     \item ARM single numeric armour value
#'     \item BASE single numeric diameter of base (mm)
#' }
#' @param boost_hit single logical boost attack roll?
#' @param boost_damage single logical boost damage roll?
#' @param foc single numeric
#' @param kd single logical is target knocked down?
#' @param dist single numeric distance between bases (default 6)
#' @param dice numeric vector
#' @return single numeric
#' @examples
#'     blueleader <- list(stats = c(SPD = 5, MAT = 7, RAT = 5), 
#'         range = list(),
#'         melee = list('quake hammer' = list(stats = c(RNG = 2, PAS = 18), 
#'                 special = c("crit knockdown")), 
#'             'open fist' = list(stats = c(RNG = 0.5, PAS = 14), special = character(0))))
#'     WMTools:::activateCharge(blueleader, target = list(stats = list(DEF = 13, ARM = 13, BASE = 30)), 
#'         boost_hit = TRUE, boost_damage = TRUE, foc = 3, 
#'         dice = c(1, 5, 4, 1, 1, 2))

activateCharge <- function(warjack, target = list(stats = c(DEF = 12, ARM = 18, BASE = 50)), 
    boost_hit = TRUE, boost_damage = TRUE, 
    foc = 3, kd = FALSE, dist = 6,
    dice = sample(1:6, size = 30, replace = TRUE)) {
    
    if (!all(is.element(c("stats", "range"), names(warjack)))) {
        stop("missing elements in warjack object") }
    
    dist <- dist - warjack$stats["SPD"]
    
    if (dist < 0) { dist <- 0 }
    
    damage <- 0
    
    if (length(warjack$melee) > 0) {
        
        checkRange <- sapply(warjack$melee, is.in.melee, dist = dist)
        
        if (any(checkRange)) {
            
            damage <- melee(warjack = warjack, target = target, 
                boost_hit = boost_hit, boost_damage = boost_damage, 
                foc = foc, kd = kd, dice = dice)
        }
    }
    
    return(damage)
}



#' @title Perform activation by trampling
#' @param warjack list active warjack object with elements stats, range, melee and special
#' @param target list target warjack object with elements stats and special
#'     stats has elements \enumerate{
#'     \item DEF single numeric NB should include effect of being knocked down 
#'     (no adjustment here due to possible effect of other modifiers)
#'     \item ARM single numeric armour value
#'     \item BASE single numeric diameter of base (mm)
#' }
#' @param boost_hit single logical boost attack roll?
#' @param boost_damage single logical boost damage roll?
#' @param foc single numeric
#' @param kd single logical is target knocked down?
#' @param dist single numeric distance between bases (default 6)
#' @param dice numeric vector
#' @return single numeric
#' @examples
#'     blueleader <- list(stats = c(SPD = 5, MAT = 7, RAT = 5), 
#'         range = list(),
#'         melee = list('quake hammer' = list(stats = c(RNG = 2, PAS = 18), 
#'                 special = c("crit knockdown")), 
#'             'open fist' = list(stats = c(RNG = 0.5, PAS = 14), special = character(0))))
#'     WMTools:::activateTrample(blueleader, target = list(stats = list(DEF = 13, ARM = 13, BASE = 30)), 
#'         boost_hit = TRUE, boost_damage = TRUE, foc = 3, 
#'         dice = c(1, 5, 4, 1, 1, 2))

activateTrample <- function(warjack, target = list(stats = c(DEF = 12, ARM = 18, BASE = 50)), 
    boost_hit = TRUE, boost_damage = TRUE, 
    foc = 3, kd = FALSE, dist = 6,
    dice = sample(1:6, size = 30, replace = TRUE)) {
    
    if (!all(is.element(c("stats", "range"), names(warjack)))) {
        stop("missing elements in warjack object") }
    
    dist <- dist - warjack$stats["SPD"]
    
    if (dist < 0) { dist <- 0 }
    
    damage <- 0
    
    if (length(warjack$melee) > 0) {
        
        checkRange <- sapply(warjack$melee, is.in.melee, dist = dist)
        
        if (any(checkRange)) {
            
            damage <- melee(warjack = warjack, target = target, 
                boost_hit = boost_hit, boost_damage = boost_damage, 
                foc = foc, kd = kd, dice = dice)
        }
    }
    
    return(damage)
}




#' @title Perform activation by slamming
#' @param warjack list active warjack object with elements stats, range, melee and special
#' @param target list target warjack object with elements stats and special
#'     stats has elements \enumerate{
#'     \item DEF single numeric NB should include effect of being knocked down 
#'     (no adjustment here due to possible effect of other modifiers)
#'     \item ARM single numeric armour value
#'     \item BASE single numeric diameter of base (mm)
#' }
#' @param boost_hit single logical boost attack roll?
#' @param boost_damage single logical boost damage roll?
#' @param foc single numeric
#' @param kd single logical is target knocked down?
#' @param dist single numeric distance between bases (default 6)
#' @param dice numeric vector
#' @return single numeric
#' @examples
#'     blueleader <- list(stats = c(SPD = 5, MAT = 7, RAT = 5), 
#'         range = list(),
#'         melee = list('quake hammer' = list(stats = c(RNG = 2, PAS = 18), 
#'                 special = c("crit knockdown")), 
#'             'open fist' = list(stats = c(RNG = 0.5, PAS = 14), special = character(0))))
#'     WMTools:::activateSlam(blueleader, target = list(stats = list(DEF = 13, ARM = 13, BASE = 30)), 
#'         boost_hit = TRUE, boost_damage = TRUE, foc = 3, 
#'         dice = c(1, 5, 4, 1, 1, 2))

activateSlam <- function(warjack, target = list(stats = c(DEF = 12, ARM = 18, BASE = 50)), 
    boost_hit = TRUE, boost_damage = TRUE, 
    foc = 3, kd = FALSE, dist = 6,
    dice = sample(1:6, size = 30, replace = TRUE)) {
    
    if (!all(is.element(c("stats", "range"), names(warjack)))) {
        stop("missing elements in warjack object") }
    
    dist <- dist - warjack$stats["SPD"]
    
    if (dist < 0) { dist <- 0 }
    
    damage <- 0
    
    if (length(warjack$melee) > 0) {
        
        checkRange <- sapply(warjack$melee, is.in.melee, dist = dist)
        
        if (any(checkRange)) {
            
            damage <- melee(warjack = warjack, target = target, 
                boost_hit = boost_hit, boost_damage = boost_damage, 
                foc = foc, kd = kd, dice = dice)
        }
    }
    
    return(damage)
}




#' @title Perform activation by headbutting
#' @param warjack list active warjack object with elements stats, range, melee and special
#' @param target list target warjack object with elements stats and special
#'     stats has elements \enumerate{
#'     \item DEF single numeric NB should include effect of being knocked down 
#'     (no adjustment here due to possible effect of other modifiers)
#'     \item ARM single numeric armour value
#'     \item BASE single numeric diameter of base (mm)
#' }
#' @param boost_hit single logical boost attack roll?
#' @param boost_damage single logical boost damage roll?
#' @param foc single numeric
#' @param kd single logical is target knocked down?
#' @param dist single numeric distance between bases (default 6)
#' @param dice numeric vector
#' @return single numeric
#' @examples
#'     blueleader <- list(stats = c(SPD = 5, MAT = 7, RAT = 5), 
#'         range = list(),
#'         melee = list('quake hammer' = list(stats = c(RNG = 2, PAS = 18), 
#'                 special = c("crit knockdown")), 
#'             'open fist' = list(stats = c(RNG = 0.5, PAS = 14), special = character(0))))
#'     WMTools:::activateHeadbutt(blueleader, target = list(stats = list(DEF = 13, ARM = 13, BASE = 30)), 
#'         boost_hit = TRUE, boost_damage = TRUE, foc = 3, 
#'         dice = c(1, 5, 4, 1, 1, 2))

activateHeadbutt <- function(warjack, target = list(stats = c(DEF = 12, ARM = 18, BASE = 50)), 
    boost_hit = TRUE, boost_damage = TRUE, 
    foc = 3, kd = FALSE, dist = 6,
    dice = sample(1:6, size = 30, replace = TRUE)) {
    
    if (!all(is.element(c("stats", "range"), names(warjack)))) {
        stop("missing elements in warjack object") }
    
    dist <- dist - warjack$stats["SPD"]
    
    if (dist < 0) { dist <- 0 }
    
    damage <- 0
    
    if (length(warjack$melee) > 0) {
        
        checkRange <- sapply(warjack$melee, is.in.melee, dist = dist)
        
        if (any(checkRange)) {
            
            damage <- melee(warjack = warjack, target = target, 
                boost_hit = boost_hit, boost_damage = boost_damage, 
                foc = foc, kd = kd, dice = dice)
        }
    }
    
    return(damage)
}

