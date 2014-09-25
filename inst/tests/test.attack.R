
context("make an attack")

test_that("attack", {
    
    blueleader <- list(stats = c(SPD = 5, MAT = 7, RAT = 5, DEF = 12, ARM = 18, BASE = 50), 
        range = list(),
        melee = list('quake hammer' = list(stats = c(RNG = 0.5, PAS = 18), 
                special = c("crit knockdown")), 
            'open fist' = list(stats = c(RNG = 0.5, PAS = 14), special = "open")), 
        special = character(0))

    # TEST 1: no focus, all sixes
    test1 <- attack(blueleader, which = 1, target = list(stats = c(DEF = 0, ARM = 0)), 
        boost_hit = FALSE, boost_damage = TRUE, foc = 0, kd = FALSE, dice = rep(6, 4))
    expect_equal(object = test1, expected = c(damage = 30, focus = 0, "knocked down" = 1, position = 5, hit = 1))

    # TEST 2: two focus, all ones
    test2 <- attack(blueleader, which = 2, target = list(stats = c(DEF = 0, ARM = 0)), 
        boost_hit = TRUE, boost_damage = TRUE, foc = 2, kd = FALSE, dice = rep(1, 3))
    expect_equal(object = test2, expected = c(damage = 0, focus = 1, "knocked down" = 0, position = 4, hit = 0))

    # TEST 3: foc > 2, pos 4, kd
    test3 <- attack(blueleader, which = 1, target = list(stats = c(DEF = 0, ARM = 0)), 
        boost_hit = TRUE, boost_damage = TRUE, foc = 4, kd = FALSE, 
        dice = c(1, 1, 1, 5, 5, 1, 1, 2, 3), pos = 4)
    expect_equal(object = test3, expected = c(damage = 24, focus = 2, "knocked down" = 1, position = 10, hit = 1))

    # TEST 4: DEF 100
    test4 <- attack(blueleader, which = 1, target = list(stats = c(DEF = 100, ARM = 100)), 
        boost_hit = TRUE, boost_damage = TRUE, foc = 4, kd = TRUE, 
        dice = c(1, 1, 1, 5, 5, 1, 1, 2, 3), pos = 4)
    expect_equal(object = test4, expected = c(damage = 0, focus = 3, "knocked down" = 1, position = 7, hit = 1))

    # TEST 5: DEF 100
    test5 <- attack(blueleader, which = 1, target = list(stats = c(DEF = 100, ARM = 100)), 
        boost_hit = TRUE, boost_damage = FALSE, foc = 1, kd = FALSE, 
        dice = c(5, 5, 3), pos = 1)
    expect_equal(object = test5, expected = c(damage = 0, focus = 0, "knocked down" = 0, position = 4, hit = 0))

    # TEST 6: charge blueleader
    test6 <- attack(blueleader, which = 1, target = list(stats = c(DEF = 12, ARM = 18)), charge = TRUE, 
        boost_hit = TRUE, boost_damage = TRUE, foc = 1, kd = FALSE, 
        dice = c(5, 4, 1, 1, 1), pos = 1)
    expect_equal(object = test6, expected = c(damage = 3, focus = 0, "knocked down" = 0, position = 6, hit = 1))

    # TEST 7: charge blueleader, 0.6" away
    test7 <- attack(blueleader, which = 1, target = list(stats = c(DEF = 12, ARM = 18)), charge = TRUE, 
        boost_hit = TRUE, boost_damage = TRUE, foc = 1, kd = FALSE, dist = 0.6,
        dice = c(5, 4, 1, 1, 1), pos = 1)
    expect_equal(object = test7, expected = c(damage = 0, focus = 0, "knocked down" = 0, position = 1, hit = 0))
    
    redknave <- list(stats = c(SPD = 4, MAT = 6, RAT = 4, DEF = 10, ARM = 20, BASE = 50), 
        range = list('grenade launcher' = list(stats = c(RNG = 10, ROF = 1, AOE = 3, POW = 10),
                special = "arcing"), 
            'grenade launcher' = list(stats = c(RNG = 10, ROF = 1, AOE = 3, POW = 10),
                special = "arcing")
        ),
        melee = list('war lance' = list(stats = c(RNG = 2, PAS = 18), special = "powerful charge"),
            'assault shield' = list(stats = c(RNG = 0.5, PAS = 14), special = character(0))), 
        special = "bulldoze")
    
    # TEST 8: charge with redknave charge attack
    test8 <- attack(redknave, which = 1, target = list(stats = c(DEF = 12, ARM = 18)), charge = TRUE, 
        boost_hit = TRUE, boost_damage = TRUE, foc = 1, kd = FALSE, 
        dice = c(2, 2, 1, 1, 1), pos = 1)
    expect_equal(object = test8, expected = c(damage = 3, focus = 0, "knocked down" = 0, position = 6, hit = 1))

    # TEST 9: charge with redknave, 0.6" away
    test9 <- attack(redknave, which = 1, target = list(stats = c(DEF = 12, ARM = 18)), charge = TRUE, 
        boost_hit = TRUE, boost_damage = TRUE, foc = 1, kd = FALSE, dist = 0.6,
        dice = c(6, 6, 1, 1, 1), pos = 1)
    expect_equal(object = test9, expected = c(damage = 3, focus = 0, "knocked down" = 0, position = 6, hit = 1))

    # TEST 10: buy attack with redknave
    test10 <- attack(redknave, which = 1, target = list(stats = c(DEF = 12, ARM = 18)), charge = FALSE, 
        boost_hit = FALSE, boost_damage = TRUE, foc = 1, kd = FALSE, 
        dice = c(2, 2, 1, 1, 1), pos = 1)
    expect_equal(object = test10, expected = c(damage = 0, focus = 1, "knocked down" = 0, position = 3, hit = 0))

})
