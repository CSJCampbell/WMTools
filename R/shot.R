
#' @title Perform a Ranged Attack
#' @param warjack list attacker warjack object with elements stats, range and melee and special
#' @param which single integer specify ranged weapon to use
#' @param target list target warjack object with elements stats and special
#'     stats has elements \enumerate{
#'     \item DEF single numeric NB should include effect of being knocked down 
#'     (no adjustment here due to possible effect of other modifiers)
#'     \item ARM single numeric armour value
#'     \item BASE single numeric diameter of base (mm)
#' }
#' @param boost_hit single logical
#' @param boost_damage single logical
#' @param foc single numeric number of focus
#' @param kd single logical is the target knocked down? (default \code{FALSE})
#' @param dist numeric vector distance between bases (default 6)
#' @param dice numeric vector (default \code{sample(1:6)})
#' @param pos single numeric
#' @param recycle single logical should dice be recycled for each value of dist? (default \code{FALSE})
#' @return named vector with elements \enumerate{
#'     \item damage amount of damage inflicted
#'     \item focus remaining focus following attack
#'     \item knocked down is the target knocked down? 0: No, 1: Yes
#'     \item position which die to use next
#'     \item hit was the target hit? 0: No, 1: Yes
#' }
#' @export
#' @examples 
#'     redbandit <- list(stats = c(SPD = 4, MAT = 6, RAT = 4, DEF = 10, ARM = 20, BASE = 50), 
#'         range = list(bombard = list(
#'             stats = c(RNG = 14, POW = 14, AOE = 3), 
#'             special = c("arcing"))),
#'         melee = list(axe = list(stats = c(RNG = 0.5, PAS = 12), 
#'                 special = c("crit amp"))))
#'     shot(redbandit, which = 1, target = redbandit, 
#'         boost_hit = TRUE, boost_damage = TRUE, foc = 3, dist = 10,
#'         dice = c(1, 5, 4, 1, 1, 2))

shot <- function(warjack, which = 1, target = list(stats = c(DEF = 12, ARM = 18, BASE = 50)),
    boost_hit = TRUE, boost_damage = TRUE, foc = 0, kd = FALSE, dist = 6, 
    dice = sample(1:6, size = 20, replace = TRUE), pos = 1, recycle = FALSE) {
    
    # check input
    
    if (!all(is.element(c("stats", "range"), names(warjack)))) {
        stop("missing elements in warjack object") }
    
    if (!is.element(c("melee"), names(warjack))) {
        warjack$melee <- list() }
    
    if (!is.element(c("special"), names(warjack))) {
        warjack$special <- character() }
    
    if (!is.element(c("melee"), names(target))) {
        target$melee <- list() }
        
    if (!is.element(c("special"), names(target))) {
        target$special <- character() }
    
    if (!is.element(c("modify"), names(target))) {
        target$modify <- character() }
    
    # vectorized outputs
    
    nd <- length(dist)
    
    damage <- rep(0, nd)
    
    short <- rep(0, nd)
    
    hit_roll <- FALSE
    
    hit <- rep(FALSE, nd)
    
    miss <- rep(FALSE, nd)
    
    if (length(pos) != 1) {
        
        if (length(pos) != nd) { stop("pos should be length one or length dist") }
        
    } else { pos <- rep(pos, times = nd) }
    
    if (length(foc) != 1) {
        
        if (length(foc) != nd) { stop("foc should be length one or length dist") }
        
    } else { foc <- rep(foc, times = nd) }

    if (length(kd) != 1) {
        
        if (length(kd) != nd) { stop("kd should be length one or length dist") }
        
    } else { kd <- rep(kd, times = nd) }
    
    wjs <- warjack$range[[which]]$special
    
    wjp <- warjack$range[[which]]$stats["POW"]
    
    wjr <- warjack$range[[which]]$stats["RNG"]
    
    # handle direct and indirect hit independently
    
    miss[wjr < dist] <- TRUE
    
    short[wjr < dist] <- c(dist - wjr)[wjr < dist]
    
    # special rules affecting hit
    
    miss["stealth" %in% target$special & dist > 5] <- TRUE
    
    # engagement
    
    isEng <- is.engaged(warjack, target, dist = dist, kd = kd)
    
    isEng["gunfighter" %in% wjs & dist <= 0.5] <- FALSE
    
    # loop for distances provided
    
    for (ds in seq_len(nd)) {
        
        num_dice_hit <- 2
        num_dice_dam <- 2
        
        if (!isEng[ds]) {
            
            if (!miss[ds]) {
                
                # boost hit if able
                
                if ((boost_hit & foc[ds] > 0) | 
                        "free boost hit" %in% wjs) {
                    
                    num_dice_hit <- num_dice_hit + 1
                    
                    if (!"free boost hit" %in% wjs) { foc[ds] <- foc[ds] - 1 }
                }
                
                # roll dice
                
                hit_roll <- dice[seq.int(from = pos[ds], to = pos[ds] + num_dice_hit - 1)]
                
                if (any(is.na(hit_roll))) { stop("insufficient dice for hit_roll") }
                
                pos[ds] <- pos[ds] + num_dice_hit
                
                # knocked down sets DEF to 5
                
                def <- target$stats["DEF"]
                
                mod <- 0
                
                if (!is.na(target$modify["DEF"])) { mod <- target$modify["DEF"] }
                
                if (kd[ds]) { def <- 5 + mod }
                
                # see if hit
                
                hit[ds] <- unname(((sum(hit_roll, 
                    warjack$stats["RAT"]) >= def & 
                        !all(hit_roll < 2)) | all(hit_roll > 5)))
                
                if ("ammo type:quake" %in% wjs) {
                    
                    # KD on direct hit
                    
                    kd[ds][hit[ds] & !miss[ds]] <- TRUE
                }
            }
            
            # if miss, scatter if is AOE
            
            if (!hit[ds] && !is.na(warjack$range[[which]]$stats["AOE"])) {
                
                scatter_roll <- dice[seq.int(from = pos[ds], to = pos[ds] + 1)]
                if (any(is.na(scatter_roll))) { stop("insufficient dice for scatter_roll") }
                
                hit[ds] <- is.scatter.hit(warjack$range[[which]], 
                    short = short[ds], 
                    base = target$stats["BASE"], 
                    max = min(wjr, dist[ds]) / 2, 
                    dice = scatter_roll)
                
                pos[ds] <- pos[ds] + 2
                
                miss[ds] <- TRUE
            }
            
            # cause damage when hit
            
            if (hit[ds]) { 
                
                # only boost damage roll when hit
                
                if ((boost_damage & foc[ds] > 0) | 
                        "free boost damage" %in% wjs) {
                    
                    num_dice_dam <- num_dice_dam + 1
                    
                    if (!"free boost damage" %in% wjs) {
                        foc[ds] <- foc[ds] - 1
                    }
                }
                
                # check for critical effect
                
                if (!is.logical(hit_roll) & sum(duplicated(hit_roll)) > 0) { 
                    if ("critical knockdown" %in% wjs) { kd[ds] <- TRUE } 
                    
                    if ("critical devastation" %in% wjs) {
                        if (is.null(target$stats["BASE"]) || is.na(target$stats["BASE"])) {
                            warning("assuming 30 mm base for critical devastation movement")
                            target$stats["BASE"] <- 30 }
                        if (target$stats["BASE"] < 120) { 
                            dist[ds] <- dist[ds] + dice[pos[ds]]
                            pos[ds] <- pos[ds] + 1
                            kd[ds] <- TRUE
                        }
                    }
                }
                
                damage_roll <- dice[seq.int(from = pos[ds], to = pos[ds] + num_dice_dam - 1)]
                
                if (any(is.na(damage_roll))) { stop("insufficient dice for damage_roll") }
                
                pos[ds] <- pos[ds] + num_dice_dam
                
                if (!recycle & ds < nd) { pos[ds + 1] <- pos[ds] }
                
                pow <- wjp
                
                # if not direct hit, divide POW by 2, rounding up
                # TODO support for modify for warjack to allow for bonuses
                
                if (miss[ds]) { pow <- ceiling(wjp / 2) }
                
                # modify
                
                arm <- target$stats["ARM"]
                
                mod <- 0
                
                if (!is.na(target$modify["ARM"])) { mod <- target$modify["ARM"] }
                
                damage[ds] <- unname(pow + 
                    sum(damage_roll) - (arm + mod))
            }
            
        }
    }
    
    damage[damage < 0] <- 0
    
    return(cbind('damage' = damage, 'focus' = foc, 'knocked down' = kd, 
            'position' = pos, 'hit' = hit, 'dist' = dist))
}

