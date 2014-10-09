

context("activation")

test_that("activation aim", {
    
    blueleader <- list(stats = c(SPD = 5, MAT = 7, RAT = 5, DEF = 12, ARM = 18, BASE = 50), 
        range = list(),
        melee = list('quake hammer' = list(stats = c(RNG = 0.5, PAS = 18), 
                special = c("crit knockdown")), 
            'open fist' = list(stats = c(RNG = 0.5, PAS = 14), special = "open")), 
        special = character(0))
    
    # TEST 1: aim no ranged 
    test1 <- activation(blueleader, target = list(stats = c(DEF = 12, ARM = 18)), 
        strategy = "aim",
        boost_hit = TRUE, boost_damage = TRUE, 
        foc = 3, dice = numeric(0))
    expect_equal(object = test1, expected = 0)
    
    redbandit <- list(stats = c(SPD = 4, MAT = 6, RAT = 4, DEF = 10, ARM = 20, BASE = 50), 
        range = list(bombard = list(
                stats = c(RNG = 14, ROF = 1, AOE = 3, POW = 14), 
                special = "arcing")
        ),
        melee = list(axe = list(stats = c(RNG = 0.5, PAS = 12), 
                special = "critical amputation")),
        special = character(0))

    # TEST 2: aim all sixes, boost 
    test2 <- activation(redbandit, 
        target = list(stats = c(DEF = 4 + 2 + 3 * 6, ARM = 18)), 
        strategy = "aim",
        boost_hit = TRUE, boost_damage = TRUE, 
        foc = 3, dice = rep(6, 6))
    expect_equal(object = test2, expected = 14)

    # TEST 3: aim 5, 6, 6 vs DEF 24
    test3 <- activation(redbandit, 
        target = list(stats = c(DEF = 4 + 2 + 3 * 6, ARM = 18, BASE = 30)), 
        strategy = "aim",
        boost_hit = TRUE, boost_damage = TRUE, 
        foc = 3, dice = c(5, 6, 6, 4, 6))
    expect_equal(object = test3, expected = 0)

    # TEST 4: aim all sixes, 3 FOC, ROF 3
    test4 <- activation(list(stats = c(RAT = 6), 
            range = list(gun = list(stats = c(RNG = 10, ROF = 3, AOE = NA, POW = 10), 
            special = ""))), 
        target = list(stats = c(DEF = 20, ARM = 21, BASE = 30)), 
        strategy = "aim",
        boost_hit = FALSE, boost_damage = FALSE, 
        foc = 3, dist = 10, dice = rep(6, 12))
    expect_equal(object = test4, expected = 3)

    # TEST 5: aim all sixes, 3 FOC, ROF 3 out of range
    test5 <- activation(list(stats = c(RAT = 6), 
            range = list(gun = list(stats = c(RNG = 10, ROF = 3, AOE = NA, POW = 10), 
            special = ""))), 
        target = list(stats = c(DEF = 20, ARM = 21, BASE = 30)), 
        strategy = "aim",
        boost_hit = FALSE, boost_damage = FALSE, 
        foc = 3, dist = 11, dice = rep(6, 12))
    expect_equal(object = test5, expected = 0)

})



test_that("activation assault", {
    
    blueleader <- list(stats = c(SPD = 5, MAT = 7, RAT = 5, DEF = 12, ARM = 18, BASE = 50), 
        range = list(),
        melee = list('quake hammer' = list(stats = c(RNG = 0.5, PAS = 18), 
                special = c("crit knockdown")), 
            'open fist' = list(stats = c(RNG = 0.5, PAS = 14), special = "open")), 
        special = character(0))
    
    # TEST 1: assault no ranged 
    test1 <- activation(blueleader, target = list(stats = c(DEF = 12, ARM = 18)), 
        strategy = "assault",
        boost_hit = TRUE, boost_damage = TRUE, 
        foc = 3, dice = numeric(0))
    expect_equal(object = test1, expected = 0)
    
    redbandit <- list(stats = c(SPD = 4, MAT = 6, RAT = 4, DEF = 10, ARM = 20, BASE = 50), 
        range = list(bombard = list(
                stats = c(RNG = 14, ROF = 1, AOE = 3, POW = 14), 
                special = "arcing")
        ),
        melee = list(axe = list(stats = c(RNG = 0.5, PAS = 12), 
                special = "critical amputation")),
        special = character(0))

    # TEST 2: assault all sixes, boost 
    test2 <- activation(redbandit, 
        target = list(stats = c(DEF = 4 + 3 * 6, ARM = 18)), 
        strategy = "assault",
        boost_hit = TRUE, boost_damage = TRUE, 
        foc = 3, dist = 18, dice = rep(6, 6))
    expect_equal(object = test2, expected = 14)
    
    # TEST 3: assault 5, 6, 6 vs DEF 22
    test3 <- activation(redbandit, 
        target = list(stats = c(DEF = 4 + 3 * 6, ARM = 18, BASE = 30)), 
        strategy = "assault",
        boost_hit = TRUE, boost_damage = TRUE, 
        foc = 3, dist = 18, dice = c(5, 6, 6, 4, 6))
    expect_equal(object = test3, expected = 0)

    # TEST 4: assault all sixes, 3 FOC, ROF 3
    test4 <- activation(list(stats = c(SPD = 4, RAT = 6), 
            range = list(gun = list(stats = c(RNG = 10, ROF = 3, AOE = NA, POW = 10), 
            special = ""))), 
        target = list(stats = c(DEF = 40, ARM = 21, BASE = 30)), 
        strategy = "assault",
        boost_hit = FALSE, boost_damage = FALSE, 
        foc = 3, dist = 10, dice = rep(6, 12))
    expect_equal(object = test4, expected = 3)


    # TEST 5: assault all sixes, 3 FOC, ROF 3 out of range
    test5 <- activation(list(stats = c(SPD = 4, RAT = 6), 
            range = list(gun = list(stats = c(RNG = 10, ROF = 3, AOE = NA, POW = 10), 
            special = ""))), 
        target = list(stats = c(DEF = 20, ARM = 21, BASE = 30)), 
        strategy = "assault",
        boost_hit = FALSE, boost_damage = FALSE, 
        foc = 3, dist = 14.1, dice = rep(6, 12))
    expect_equal(object = test5, expected = 0)

})

