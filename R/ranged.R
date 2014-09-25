
#' @title Perform Ranged Attacks
#' @param warjack list  attacker warjack object with elements stats, range and melee and special
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
#' redbandit <- list(stats = c(SPD = 4, MAT = 6, RAT = 4, DEF = 10, ARM = 20, BASE = 50), 
#'         range = list(bombard = list(
#'                 stats = c(RNG = 14, ROF = 1, AOE = 3, POW = 14), 
#'                 special = "arcing")
#'         ),
#'         melee = list(axe = list(stats = c(RNG = 0.5, PAS = 12), 
#'                 special = "critical amputation")),
#'         special = character(0))
#' ranged(redbandit, target = list(stats = c(DEF = 10, ARM = 14, BASE = 40)), 
#'     boost_hit = TRUE, boost_damage = TRUE, 
#'     foc = 2, dist = 2, dice = rep(6, 6))
#' ranged(redbandit, 
#'     target = list(stats = c(DEF = 13, ARM = 13, BASE = 30)), boost_hit = FALSE, boost_damage = TRUE, 
#'     foc = 3, dist = 0.1, dice = rep(5, 10))

ranged <- function(warjack, target = list(stats = c(DEF = 12, ARM = 18, BASE = 50)), 
    boost_hit = TRUE, boost_damage = TRUE, foc = 3, kd = FALSE, dist = 0, 
    dice = sample(1:6, size = 30, replace = TRUE)) {
    
    num_rng <- length(warjack$range)
    
    shots <- rep(0, times = num_rng)
    
    rofs <- sapply(warjack$range, function(x) { x$stats["ROF"] })
    
    tot <- 0
    
    meta <- matrix(NA, nrow = length(dist), ncol = 4)
    
    colnames(meta) <- c('focus', 'knocked down', 'position', 'dist')
    
    meta[, 'focus'] <- foc
    
    meta[, 'knocked down'] <- kd
    
    meta[, 'position'] <- 1
    
    meta[, 'dist'] <- dist
    
    use <- 1
    
    for (i in seq_len(num_rng)) {
        
        out <- shot(warjack = warjack, which = i, target = target, 
            boost_hit = boost_hit, boost_damage = boost_damage, foc = meta[, 'focus'], 
            kd = as.logical(meta[, 'knocked down']), dist = meta[, 'dist'], dice = dice, pos = meta[, 'position'])
        
        tot <- tot + unname(out[, 'damage'])
        
        meta[, c('focus', 'knocked down', 'position', 'dist')] <- out[, c('focus', 'knocked down', 'position', 'dist')]
        
        shots[i] <- shots[i] + 1
        
        additional_shots <- 0
        
        wjs <- warjack$range[[i]]$special
        
        if ("linked guns" %in% wjs) { additional_shots <- 1 }
        
        if ("rapid fire" %in% wjs) {
            
            # D3
            
            rapid_fire <- dice[meta[, 'position']]
            
            if (any(is.na(rapid_fire))) { stop("insufficient dice for rapid fire roll") }
            
            meta[, 'position'] <- meta[, 'position'] + 1
            
            rapid_fire <- (rapid_fire + 1) %/% 2
            
            # take additional shots
            
            if (rapid_fire > 1) { additional_shots <- rapid_fire - 1 }
        }
        
        for (addsht in seq_len(additional_shots)) {
            
            out <- shot(warjack = warjack, which = i, target = target, 
                boost_hit = boost_hit, boost_damage = boost_damage, foc = meta[, 'focus'], 
                kd = meta[, 'knocked down'], dist = meta[, 'dist'], dice = dice, pos = meta[, 'position'])
            
            tot <- tot + unname(out[, 'damage'])
            
            meta[, c('focus', 'knocked down', 'position', 'dist')] <- out[, c('focus', 'knocked down', 'position', 'dist')]
        }
    }
    
    # buy ranged attacks provided rof has not been exceeded
    # remaining focus may be used to buy or boost
    available <- rofs > shots
    
    if (any(available)) {
        
        for (i in seq_len(max(meta[, 'focus']))) {
            
            these <- meta[, 'focus'] > 0
            
            if (any(these)) {
                
                if (rofs[use] <= shots[use]) { use <- which(available)[1] }
                
                meta[these, 'focus'] <- meta[these, 'focus'] - 1
                
                out <- shot(warjack = warjack, which = use, target = target, 
                    boost_hit = boost_hit, boost_damage = boost_damage, foc = meta[these, 'focus'], 
                    kd = meta[these, 'knocked down'], dist = meta[these, 'dist'], dice = dice, pos = meta[these, 'position'])
                
                tot[these] <- tot[these] + unname(out[these, 'damage'])
                
                meta[these, c('focus', 'knocked down', 'position', 'dist')] <- out[these, c('focus', 'knocked down', 'position', 'dist')]
                
                shots[use] <- shots[use] + 1
                
                available <- rofs > shots
                
                if (!any(available)) break
            }
        }
    }
    return(tot)
}
