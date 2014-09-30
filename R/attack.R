
#' @title Perform a Melee Attack
#' @param warjack list warjack object with elements stats, range and melee
#' @param which single integer or single character 'power'
#' index of warjack weapon to use, or identify as a power attack
#' @param target list target warjack object with elements stats and special
#'     stats has elements \enumerate{
#'     \item DEF single numeric NB should include effect of being knocked down 
#'     (no adjustment here due to possible effect of other modifiers)
#'     \item ARM single numeric armour value
#'     \item BASE single numeric diameter of base (mm)
#' }
#' @param charge single logical
#' @param boost_hit single logical
#' @param boost_damage single logical
#' @param foc single numeric number of focus
#' @param kd single logical is the target knocked down? (default \code{FALSE})
#' @param dist single numeric distance between bases (default 0)
#' @param dice numeric vector (default \code{sample(1:6)})
#' @param pos single numeric (default 1)
#' @return named vector with elements \enumerate{
#'     \item damage amount of damage inflicted
#'     \item focus remaining focus following attack
#'     \item knocked down is the target knocked down? 0: No, 1: Yes
#'     \item position which die to use next
#'     \item hit was the target hit? 0: No, 1: Yes
#' }
#' @export
#' @examples 
#'     blueleader <- list(stats = c(SPD = 5, MAT = 7, RAT = 5), 
#'         range = list(),
#'         melee = list('quake hammer' = list(stats = c(RNG = 2, PAS = 18), 
#'                 special = c("crit knockdown")), 
#'             'open fist' = list(stats = c(RNG = 0.5, PAS = 14), special = character(0))))
#'     attack(blueleader, which = 1, target = list(stats = c(DEF = 13, ARM = 13, BASE = 30)), 
#'         boost_hit = TRUE, boost_damage = TRUE, foc = 3, 
#'         dice = c(1, 5, 4, 1, 1, 2))

attack <- function(warjack, which = 1L, target = list(stats = c(DEF = 12, ARM = 18, BASE = 50)), 
    charge = FALSE, boost_hit = TRUE, boost_damage = TRUE, foc = 0, kd = FALSE, dist = 0,
    dice = sample(1:6, size = 20, replace = TRUE), pos = 1) {
    
    if (!is.element("stats", names(warjack)) | !is.element("melee", names(warjack))) {
        stop("missing elements in warjack object") }
    
    if (is.na(which)) { stop("which is missing") }
    
    num_dice_hit <- 2
    
    num_dice_dam <- 2
    
    damage <- 0
    
    hit_roll <- FALSE
    
    hit <- FALSE
    
    miss <- FALSE
    
    if (!is.numeric(which)) {
        
        if (which == "power") {
            
            wjs <- which
            
            wjp <- warjack$stats["STR"]
            
            wjr <- 0.5
            
            if (warjack$stats["BASE"] > 50) { wjr <- 2 }
            
        } else { stop("which must be integer or 'power'") }
        
    } else {
        
        wjs <- warjack$melee[[which]]$special
        
        wjp <- warjack$melee[[which]]$stats["PAS"]
        
        wjr <- warjack$melee[[which]]$stats["RNG"]
    }
    
    if (wjr < dist) { miss <- TRUE }

    # charge attack always spend focus first
    if (charge) { 
        if (foc < 1) { stop("at least 1 focus required for charge attack") }
        foc <- foc - 1
        num_dice_dam <- num_dice_dam + 1
        boost_damage <- FALSE
        
        if ("powerful charge" %in% wjs) { warjack$stats["MAT"] <- warjack$stats["MAT"] + 2 }
    }

    # boost hit if able when not knocked down
    if (!kd & !miss) { 
        if (boost_hit & foc > 0) { 
            num_dice_hit <- num_dice_hit + 1
            foc <- foc - 1 }
        # hit
        hit_roll <- dice[seq.int(from = pos, to = pos + num_dice_hit - 1)]
        if (any(is.na(hit_roll))) { stop("insufficient dice for hit_roll") }
        pos <- pos + num_dice_hit
        
        hit <- unname((sum(hit_roll, warjack$stats["MAT"]) >= target$stats["DEF"] & !all(hit_roll < 2)) | all(hit_roll > 5))
    }
    
    if (!miss & kd) { hit <- TRUE }
    
    # cause damage when hit
    if (hit) { 

        # only boost damage roll when hit
        if (boost_damage & foc > 0) { 
            num_dice_dam <- num_dice_dam + 1
            foc <- foc - 1 
        }

        # check for critical effect
        if (!is.logical(hit_roll) & sum(duplicated(hit_roll)) > 0) { 
            if ("crit knockdown" %in% wjs) { kd <- TRUE }
        }

        damage_roll <- dice[seq.int(from = pos, to = pos + num_dice_dam - 1)]
        if (any(is.na(damage_roll))) { stop("insufficient dice for damage_roll") }
        pos <- pos + num_dice_dam
        damage <- unname(sum(wjp, damage_roll) - target$stats["ARM"])
    }
    if (damage < 0) { damage <- 0 }

    return(c('damage' = damage, 'focus' = foc, 'knocked down' = kd, 'position' = pos, 'hit' = hit))
}
