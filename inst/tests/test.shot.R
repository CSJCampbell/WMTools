
context("take a shot")

test_that("shot", {
    
    redbandit <- list(stats = c(SPD = 4, MAT = 6, RAT = 4, DEF = 10, ARM = 20, BASE = 50), 
        range = list(bombard = list(
                stats = c(RNG = 14, ROF = 1, AOE = 3, POW = 14), 
                special = "arcing")
        ),
        melee = list(axe = list(stats = c(RNG = 0.5, PAS = 12), 
                special = "critical amputation")),
        special = character(0))

    colnm <- c("damage", "focus", "knocked down", "position", "hit", "dist")
    # TEST 1: no focus, all sixes
    test1 <- shot(redbandit, which = 1, target = list(stats = c(DEF = 20, ARM = 0)), 
        boost_hit = FALSE, boost_damage = TRUE, foc = 0, kd = FALSE, dice = rep(6, 4))
    expect_equal(object = test1, expected = matrix(c(26, 0, 0, 5, 1, 6), ncol = 6, dimnames = list(NULL, colnm)))

    # TEST 2: no focus DEF 16
    test2 <- shot(redbandit, which = 1, target = list(stats = c(DEF = 20, ARM = 0, BASE = 10)), dist = 14, 
        boost_hit = FALSE, boost_damage = TRUE, foc = 0, kd = FALSE, dice = c(6, 5, 6, 5))
    expect_equal(object = test2, expected = matrix(c(0, 0, 0, 5, 0, 14), ncol = 6, dimnames = list(NULL, colnm)))

    # TEST 3: no focus DEF 16, lucky scatter
    test3 <- shot(redbandit, which = 1, target = list(stats = c(DEF = 20, ARM = 0, BASE = 50)), dist = 6,
        boost_hit = FALSE, boost_damage = TRUE, foc = 0, kd = FALSE, dice = c(1, 3, 1, 1, 1, 1))
    expect_equal(object = test3, expected = matrix(c(9, 0, 0, 7, 1, 6), ncol = 6, dimnames = list(NULL, colnm)))

    # TEST 4: no focus short, lucky scatter
    test4 <- shot(redbandit, which = 1, target = list(stats = c(DEF = 0, ARM = 0, BASE = 50)), dist = 14.5, 
        boost_hit = TRUE, boost_damage = FALSE, foc = 3, kd = FALSE, dice = c(1, 5, 1, 1))
    expect_equal(object = test4, expected = matrix(c(9, 3, 0, 5, 1, 14.5), ncol = 6, dimnames = list(NULL, colnm)))

    # TEST 5: no focus short, long lucky scatter (6" to 1)
    test5 <- shot(redbandit, which = 1, target = list(stats = c(DEF = 0, ARM = 0, BASE = 50)), dist = 21, 
        boost_hit = FALSE, boost_damage = TRUE, foc = 0, kd = FALSE, dice = c(6, 1, 1, 1))
    expect_equal(object = test5, expected = matrix(c(9, 0, 0, 5, 1, 21), ncol = 6, dimnames = list(NULL, colnm)))

    # TEST 6: boost blast damage
    test6 <- shot(redbandit, which = 1, target = list(stats = c(DEF = 0, ARM = 13, BASE = 30)), dist = 14.5, 
        boost_hit = TRUE, boost_damage = TRUE, foc = 1, kd = FALSE, dice = c(1, 2, 1, 2, 4))
    expect_equal(object = test6, expected = matrix(c(1, 0, 0, 6, 1, 14.5), ncol = 6, dimnames = list(NULL, colnm)))

    # TEST 7: stealth
    test7 <- shot(redbandit, which = 1, target = list(stats = c(DEF = 0, ARM = 13, BASE = 30), special = "stealth"), dist = 6, 
        boost_hit = TRUE, boost_damage = TRUE, foc = 3, kd = FALSE, dice = c(1, 2, 1, 2, 4))
    expect_equal(object = test7, expected = matrix(c(1, 2, 0, 6, 1, 6), ncol = 6, dimnames = list(NULL, colnm)))

    # TEST 8: in melee
    test8 <- shot(redbandit, which = 1, target = list(stats = c(DEF = 0, ARM = 13, BASE = 30)), dist = 0.5, 
        boost_hit = TRUE, boost_damage = TRUE, foc = 3, kd = FALSE, dice = c())
    expect_equal(object = test8, expected = matrix(c(0, 3, 0, 1, 0, 0.5), ncol = 6, dimnames = list(NULL, colnm)))

    redleader <- list(stats = c(SPD = 4, MAT = 6, RAT = 4, DEF = 10, ARM = 20, BASE = 50), 
        range = list(bombard1 = list(
                stats = c(RNG = 14, ROF = 1, AOE = 3, POW = 14), 
                special = "arcing"),
            bombard2 = list(
                stats = c(RNG = 14, ROF = 1, AOE = 3, POW = 14), 
                special = "arcing")),
        melee = list(fist = list(stats = c(RNG = 0.5, PAS = 12), 
                special = "open", "arm piercing")), 
        special = c("subcortex"))

    # TEST 9: in melee engaging
    test9 <- shot(redleader, which = 2, 
        target = list(stats = c(DEF = 0, ARM = 13, BASE = 30), melee = list('doom hammer' = list(stats = c('RNG' = 2)))), 
        dist = 2, 
        boost_hit = TRUE, boost_damage = TRUE, foc = 3, kd = FALSE, dice = c())
    expect_equal(object = test9, expected = matrix(c(0, 3, 0, 1, 0, 2), ncol = 6, dimnames = list(NULL, colnm)))

    redking <- list(stats = c(SPD = 4, MAT = 6, RAT = 4, DEF = 7, ARM = 20, BASE = 120),
        range = list('main guns' = list(
                stats = c(RNG = 15, ROF = 1, AOE = 4, POW = 15), 
                special = "critical devastation"),
            'secondary battery' = list(
                stats = c(RNG = 12, ROF = 1, AOE = 3, POW = 12), 
                special = c("left", "linked guns")),
            'secondary battery' = list(
                stats = c(RNG = 12, ROF = 1, AOE = 3, POW = 12), 
                special = c("right", "linked guns"))),
        melee = list('fist' = list(stats = c(RNG = 2, PAS = 22), 
                special = c("left", "open")),
            'fist' = list(stats = c(RNG = 2, PAS = 22), 
                special = c("right", "open"))), 
        special = character(0))

    # TEST 10: knocked down
    test10 <- shot(redking, which = 3, target = list(stats = c(DEF = 12, ARM = 18, BASE = 50)), dist = 0.5, 
        boost_hit = TRUE, boost_damage = FALSE, foc = 3, kd = TRUE, dice = rep(6, 5))
    expect_equal(object = test10, expected = matrix(c(6, 2, 1, 6, 1, 0.5), ncol = 6, dimnames = list(NULL, colnm)))

    # TEST 11: no focus, all sixes range 3:5
    test11 <- shot(redbandit, which = 1, target = list(stats = c(DEF = 20, ARM = 0)), 
        boost_hit = FALSE, boost_damage = TRUE, foc = 0, dist = 3:5, kd = FALSE, dice = rep(6, 4 * 3))
    m11 <- cbind(26, 0, 0, c(5, 9, 13), 1, 3:5)
    colnames(m11) <- colnm
    expect_equal(object = test11, expected = m11)

    # TEST 12: no focus DEF 7, stealth, lucky scatter, range 4:6
    test12 <- shot(redbandit, which = 1, target = list(stats = c(DEF = 7, ARM = 0, BASE = 30), special = "stealth"), dist = 4:6,
        boost_hit = TRUE, boost_damage = FALSE, foc = 0, kd = FALSE, dice = c(1, 3, 1, 1, 1, 1), recycle = TRUE)
    m12 <- cbind(c(16, 16, 9), 0, 0, 5, 1, 4:6)
    colnames(m12) <- colnm
    expect_equal(object = test12, expected = m12)

    # TEST 13: 3 focus, all fives range c(1, 6, 11, 16)
    test13 <- shot(redbandit, which = 1, target = list(stats = c(DEF = 13, ARM = 13, BASE = 30), melee = list(Halberd = list(stats = c(RNG = 2)))), 
        boost_hit = TRUE, boost_damage = TRUE, foc = 3, dist = c(1, 6, 11, 16), kd = FALSE, dice = rep(5, 4 * 6), recycle = TRUE)
    m13 <- cbind(c(0, 16, 16, 0), 
            focus = c(3, 1, 1, 3), 0, 
            position = c(1, 7, 7, 3), 
            hit = c(0, 1, 1, 0), c(1, 6, 11, 16))
    colnames(m13) <- colnm
    expect_equal(object = test13, expected = m13)
    
    blueleader <- list(stats = c(SPD = 5, MAT = 7, RAT = 5, DEF = 12, ARM = 18, BASE = 50), 
        range = list(),
        melee = list('quake hammer' = list(stats = c(RNG = 0.5, PAS = 18), 
                special = c("crit knockdown")), 
            'open fist' = list(stats = c(RNG = 0.5, PAS = 14), special = "open")), 
        special = character(0))

    # TEST 14:  no focus short, long lucky scatter (6" to 1)
    test14 <- shot(redbandit, which = 1, target = blueleader, dist = 21,
        boost_hit = TRUE, boost_damage = TRUE, foc = 3, kd = FALSE, dice = c(6, 1, 6, 6, 6))
    expect_equal(object = test14, expected = matrix(c(7, 2, 0, 6, 1, 21), ncol = 6, dimnames = list(NULL, colnm)))

    # TEST 15: short and long range hits 
    test15 <- shot(redbandit, which = 1, target = blueleader, 
            boost_hit = TRUE, boost_damage = TRUE, foc = 3, 
            dist = c(1, 20), kd = FALSE, dice = c(4, 3, 2, 3, 4, 4, 6, 1, 6, 6, 6), recycle = FALSE)
    m15 <- cbind(7, 1:2, 0, c(7, 12), 1, c(1, 20))
    colnames(m15) <- colnm
    expect_equal(object = test15, expected = m15)
    
    robot <- list(stats = c(RAT = 1), 
        range = list(gun = list(
                stats = c(RNG = 6, ROF = 1, AOE = NA, POW = 1), 
                special = "free boost hit")))
    
    # TEST 16: free boost hit
    test16 <- shot(robot, target = list(stats = c(DEF = 5, ARM = 1)), 
                boost_hit = FALSE, boost_damage = FALSE, foc = 0, dice = c(1, 1, 2, 6, 6))
    expect_equal(object = unname(c(test16)), expected = c(12, 0, 0, 6, 1, 6))
    
    robot$range$gun$special <- "free boost damage"
    
    # TEST 17: free boost damage
    test17 <- shot(robot, target = list(stats = c(DEF = 5, ARM = 1)), 
                boost_hit = FALSE, boost_damage = FALSE, foc = 0, dice = c(1, 3, 2, 4, 6))
    expect_equal(object = unname(c(test17)), expected = c(12, 0, 0, 6, 1, 6))
    
    robot$range$gun$special <- "ammo type:quake"
    
    # TEST 18: ammo type:quake
    test18 <- shot(robot, target = list(stats = c(DEF = 5, ARM = 1)), 
                boost_hit = FALSE, boost_damage = FALSE, foc = 0, dice = c(1, 3, 6, 6))
    expect_equal(object = unname(c(test18)), expected = c(12, 0, 1, 5, 1, 6))
    
    robot$range$gun$special <- "critical knockdown"
    
    # TEST 19: critical knockdown
    test19 <- shot(robot, target = list(stats = c(DEF = 5, ARM = 1)), 
                boost_hit = FALSE, boost_damage = FALSE, foc = 0, dice = c(2, 2, 6, 6))
    expect_equal(object = unname(c(test19)), expected = c(12, 0, 1, 5, 1, 6))
    
    # TEST 20: critical knockdown no crit
    test20 <- shot(robot, target = list(stats = c(DEF = 5, ARM = 1)), 
                boost_hit = FALSE, boost_damage = FALSE, foc = 0, dice = c(1,3, 6, 6))
    expect_equal(object = unname(c(test20)), expected = c(12, 0, 0, 5, 1, 6))
    
    robot$range$gun$special <- "critical devastation"
    
    # TEST 21: critical devastation
    test21 <- shot(robot, target = list(stats = c(DEF = 5, ARM = 1, BASE = 50)), 
                boost_hit = FALSE, boost_damage = FALSE, foc = 0, dice = c(2, 2, 1, 6, 6))
    expect_equal(object = unname(c(test21)), expected = c(12, 0, 1, 6, 1, 7))
    
    # TEST 22: critical devastation no crit
    test22 <- shot(robot, target = list(stats = c(DEF = 5, ARM = 1, BASE = 50)), 
                boost_hit = FALSE, boost_damage = FALSE, foc = 0, dice = c(3, 1, 6, 6))
    expect_equal(object = unname(c(test22)), expected = c(12, 0, 0, 5, 1, 6))
    
    # TEST 23: miss with non-AOE 6" to 1
    test23 <- shot(robot, target = list(stats = c(DEF = 5, ARM = 1, BASE = 30)), 
                boost_hit = FALSE, boost_damage = FALSE, foc = 0, dice = c(1, 1))
    expect_equal(object = unname(c(test23)), expected = c(0, 0, 0, 3, 0, 6))
})
