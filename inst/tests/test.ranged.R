
context("make a ranged attack")

test_that("ranged", {

    redbandit <- list(stats = c(SPD = 4, MAT = 6, RAT = 4, DEF = 10, ARM = 20, BASE = 50), 
        range = list(bombard = list(
                stats = c(RNG = 14, ROF = 1, AOE = 3, POW = 14), 
                special = "arcing")
        ),
        melee = list(axe = list(stats = c(RNG = 0.5, PAS = 12), 
                special = "critical amputation")),
        special = character(0))

    # TEST 1: all sixes, boost all 
    test1 <- ranged(redbandit, target = list(stats = c(DEF = 10, ARM = 14, BASE = 40)), boost_hit = TRUE, boost_damage = TRUE, 
        foc = 2, dist = 2, dice = rep(6, 6))
    expect_equal(object = test1, expected = 6 * 3)

    # TEST 2: all sixes, boost all, 3 FOC
    test2 <- ranged(redbandit, target = list(stats = c(DEF = 10, ARM = 14, BASE = 30)), boost_hit = TRUE, boost_damage = TRUE, 
        foc = 3, dist = 2, dice = rep(6, 6))
    expect_equal(object = test2, expected = 6 * 3)

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
    
    # TEST 3: sixes, 3 FOC
    test3 <- ranged(redking, target = list(stats = c(DEF = 10, ARM = 20, BASE = 50)), boost_hit = FALSE, boost_damage = FALSE, 
        foc = 3, dist = 12, dice = c(5, rep(6, 19)))
    expect_equal(object = test3, expected = 7 + 4 * 4)

    # TEST 4: all sixes, 3 FOC, crit devastation
    test4 <- ranged(redking, target = list(stats = c(DEF = 10, ARM = 20, BASE = 30)), boost_hit = FALSE, boost_damage = FALSE, 
        foc = 3, dist = 12, dice = rep(6, 13))
    expect_equal(object = test4, expected = 7)

    # TEST 5: all sixes, 3 FOC, crit devastation, lucky scatter
    test5 <- ranged(redking, target = list(stats = c(DEF = 20, ARM = 14, BASE = 30)), boost_hit = FALSE, boost_damage = FALSE, 
        foc = 3, dist = 12, dice = c(rep(6, 5), 6, 1, 4, 5, 6, 1, 5, 4, 6, 1, 6, 3, 5, 1, 3, 6))
    expect_equal(object = test5, expected = 17)

    # TEST 6: all sixes, 3 FOC, crit devastation, lucky scatter
    test6 <- ranged(redking, target = list(stats = c(DEF = 20, ARM = 14, BASE = 30)), boost_hit = FALSE, boost_damage = TRUE, 
        foc = 3, dist = 12, dice = c(rep(6, 6), 6, 1, 4, 5, 1, 6, 1, 5, 4, 1, 6, 1, 6, 3, 5, 1, 3, 6))
    expect_equal(object = test6, expected = 25)

    # TEST 7: all sixes, 3 FOC, ROF 3
    test7 <- ranged(list(stats = c(RAT = 6), range = list(gun = list(stats = c(RNG = 10, ROF = 3, AOE = NA, POW = 10), special = ""))), 
        target = list(stats = c(DEF = 13, ARM = 13, BASE = 30)), boost_hit = FALSE, boost_damage = FALSE, 
        foc = 3, dist = 10, dice = rep(6, 12))
    expect_equal(object = test7, expected = 3 * 9)

    # TEST 8: all fives, 3 FOC, ROF 3, boost damage
    test8 <- ranged(list(stats = c(RAT = 6), range = list(gun = list(stats = c(RNG = 10, ROF = 3, AOE = NA, POW = 10), special = ""))), 
        target = list(stats = c(DEF = 13, ARM = 13, BASE = 30)), boost_hit = FALSE, boost_damage = TRUE, 
        foc = 3, dist = 10, dice = rep(5, 10))
    expect_equal(object = test8, expected = 2 * 12)

    redknave <- list(stats = c(SPD = 4, MAT = 6, RAT = 4, DEF = 10, ARM = 20, BASE = 50), 
        range = list('grenade launcher' = list(stats = c(RNG = 10, ROF = 1, AOE = 3, POW = 10),
                special = "arcing"), 
            'grenade launcher' = list(stats = c(RNG = 10, ROF = 1, AOE = 3, POW = 10),
                special = "arcing")
        ),
        melee = list('war lance' = list(stats = c(RNG = 2, PAS = 18), special = "powerful charge"),
            'assault shield' = list(stats = c(RNG = 0.5, PAS = 14), special = character(0))), 
        special = "bulldoze")
    
    # TEST 9: engaged
    test9 <- ranged(redknave, 
        target = list(stats = c(DEF = 13, ARM = 13, BASE = 30)), boost_hit = FALSE, boost_damage = TRUE, 
        foc = 3, dist = 0.1, dice = rep(5, 10))
    expect_equal(object = test9, expected = 0)

    # TEST 10: near to far
    test10 <- ranged(redknave, 
        target = list(stats = c(DEF = 13, ARM = 13, BASE = 30)), boost_hit = TRUE, boost_damage = TRUE, 
        foc = 3, dist = seq(0, 20, length = 30), dice = rep(5, 10 * 30))
    expect_equal(object = test10, expected = c(rep(0, 3), rep(19, 12), rep(0, 15)))
    
    robot <- list(stats = c(RAT = 1), 
        range = list(gun = list(
                stats = c(RNG = 6, ROF = 1, AOE = NA, POW = 1), 
                special = "linked guns")))
    
    # TEST 11: linked guns
    test11 <- ranged(robot, target = list(stats = c(DEF = 5, ARM = 1)), 
                boost_hit = FALSE, boost_damage = FALSE, foc = 0, dice = c(1, 3, 6, 6, 3, 1, 6, 6))
    expect_equal(object = test11, expected = 24)
    
    # TEST 12: linked guns miss
    test12 <- ranged(robot, target = list(stats = c(DEF = 5, ARM = 1)), 
                boost_hit = FALSE, boost_damage = FALSE, foc = 0, dice = c(1, 2, 1, 1))
    expect_equal(object = test12, expected = 0)
    
    robot$range$gun$special <- "rapid fire"
    
    # TEST 13: rapid fire (D3 rolled after initial shot)
    test13 <- ranged(robot, target = list(stats = c(DEF = 5, ARM = 1)), 
                boost_hit = FALSE, boost_damage = FALSE, foc = 0, dice = c(1, 3, 6, 6, 5, 3, 1, 5, 5, 2, 2, 4, 4))
    expect_equal(object = test13, expected = 30)
    
    # TEST 14: rapid fire miss twice (D3 rolled after initial shot)
    test14 <- ranged(robot, target = list(stats = c(DEF = 5, ARM = 1)), 
                boost_hit = FALSE, boost_damage = FALSE, foc = 0, dice = c(1, 2, 5, 2, 1, 2, 2, 1, 1))
    expect_equal(object = test14, expected = 2)
    
})
