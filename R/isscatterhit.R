
#' @title is target hit by scatter?
#' @param weapon length 2 list with elements stats and special. 
#'     stats is a length three vector with named elements RNG, POW and AOE
#'     special is a character vector
#' @param short single numeric with value 0 if the target was in range 
#'     or a positive number indicating inches out of range (default 0)
#' @param base single numeric indicating size of target base in mm (default 30)
#' @param max single numeric indicating the maximum scatter of the shot (default 6)
#' @param dice length 2 numeric vector specifing (distance, direction)
#' @return single logical
#' @export
#' @examples is.scatter.hit(weapon = list(stats = c(RNG = 14, POW = 14, AOE = 3), 
#'        special = c("arcing")))

is.scatter.hit <- function(weapon, short = 0, base = 30, max = 6, dice = sample(1:6, size = 2, replace = TRUE)) {

    rad <- 0.5 * weapon$stats["AOE"]
    
    if (is.null(base) || is.na(base) || length(base) != 1) { stop("base should be length 1") }
    
    radBase <- 0.5 * base / 25.4
    
    isHit <- FALSE
    
    if (!is.na(rad)) {
        
        if (length(dice) < 2) { stop("insufficient dice provided, but two required") }
        if (!all(dice[1:2] %in% 1:6)) { stop("expecting D6 result, but instead: ", paste(dice, collapse = ", ")) }
        
        moves <- dice[1]
        
        if (moves > max) { moves <- max }
        
        x <- cos(pi/6)
        y <- sin(pi/6)
        
        # system is symmetrical, so only solve for one half of x
        
        dir <- switch(dice[2], 
                c(0, 1),  # straight forward
                c(x, y),  # forward
                c(x, -y), # backward
                c(0, -1), # straight backward
                c(x, -y), # backward
                c(x, y))  # forward
        
        sep <- rep(0, times = length(short))
        
        sep[short != 0] <- short[short != 0] + radBase
        
        dmove <- sqrt((sep - moves * dir[2])^2 + (moves * dir[1])^2)
        
        isHit <- unname(abs(dmove) < radBase + rad)
    }
    return(isHit)
}
    
    
