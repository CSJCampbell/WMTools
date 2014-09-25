
#' @title is a Ranged Weapon in Range (including scatter tagging)?
#' @param weapon list with element stats containing a named numeric vector with elements 'AOE' and 'RNG'
#' @param dist single numeric distance between bases (default 6)
#' @return single logical
#' @export
#' @examples
#' is.in.range(list(stats = c(AOE = 10, RNG = 1)), dist = 10)

is.in.range <- function(weapon, dist = 6) {

    rad <- 0.5 * weapon$stats["AOE"]
    
    if (is.na(rad)) { rad <- 0 }
    
    extra <- 0
    
    if (rad > 0) { extra <- 6 + rad } 
    
    check <- weapon$stats["RNG"] + extra >= dist
    
    return(check)
}



#' @title is a Melee Weapon in Range
#' @param weapon list with element stats containing a named numeric vector with element 'RNG'
#' @param dist single numeric distance between bases (default 6)
#' @return single logical
#' @export
#' @examples
#' is.in.melee(list(stats = c(RNG = 2)), dist = 1)
#' is.in.melee(list(stats = c(RNG = 0.5)), dist = 1)

is.in.melee <- function(weapon, dist = 6) {

    check <- weapon$stats["RNG"] >= dist
    
    return(unname(check))
}



#' @title Is Attacker Engaged/Engaging?
#' @param warjack list attacker warjack object with elements melee
#' @param target list target warjack object with elements melee
#' @param dist numeric vector distance between bases
#' @param kd logical vector length 1 or length dist is the target knocked down? (default \code{FALSE}) 
#' NB Knocked down models never engage
#' @return single logical
#' @export
#' @examples
#' blueleader <- list(stats = c(SPD = 5, MAT = 7, RAT = 5), 
#'     range = list(),
#'     melee = list('quake hammer' = list(stats = c(RNG = 2, PAS = 18), 
#'             special = c("crit knockdown")), 
#'         'open fist' = list(stats = c(RNG = 0.5, PAS = 14), special = character(0))))
#' is.engaged(warjack = blueleader, target = redbandit, dist = 1)
#' is.engaged(warjack = blueleader, target = redbandit, dist = 0)
#' is.engaged(warjack = blueleader, target = redbandit, dist = 0, kd = TRUE)
#' is.engaged(warjack = blueleader, target = redbandit, dist = 0:2, kd = TRUE)
#' is.engaged(warjack = list(), target = list(), dist = 0:2, kd = TRUE)

is.engaged <- function(warjack, target, dist, kd = FALSE) {
    
    if (any(is.na(dist))) { stop("dist is NA") }
    
    if (any(dist < 0)) { stop("dist is less than zero") }
    
    nd <- length(dist)
    
    if (length(kd) != nd) { 
        if (length(kd) != 1) { warning("length kd does not equal length dist") }
        kd <- c(matrix(kd, nrow = 1, ncol = nd))
    }
    
    isEng <- rep(FALSE, times = nd)
    
    if (is.element("melee", set = names(warjack)) && length(warjack$melee) > 0) {
        
        wm <- max(sapply(warjack$melee, function(x) { unname(x$stats["RNG"]) }))
        
    } else { wm <- -1 }
    
    if (is.element("melee", set = names(target)) && length(target$melee) > 0) {
        
        tm <- max(sapply(target$melee, function(x) { unname(x$stats["RNG"]) }))
        
        tmv <- rep(tm, times = nd)
        
        tmv[kd] <- -1
        
    } else { tmv <- -1 }
    
    maxv <- apply(X = cbind(wm, tmv), MARGIN = 1, max)
    
    isEng <- maxv >= dist
    
    isEng[kd] <- FALSE
    
    return(isEng)
}
