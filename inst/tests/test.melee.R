
context("make a melee attack")

test_that("melee", {

    blueleader <- list(stats = c(SPD = 5, MAT = 7, RAT = 5, DEF = 12, ARM = 18, BASE = 50), 
        range = list(),
        melee = list('quake hammer' = list(stats = c(RNG = 0.5, PAS = 18), 
                special = c("crit knockdown")), 
            'open fist' = list(stats = c(RNG = 0.5, PAS = 14), special = "open")), 
        special = character(0))

    wo <- getOption("warn")

    # TEST 1: all sixes, boost all 
    test1 <- melee(blueleader, target = list(stats = c(DEF = 12, ARM = 18)), boost_hit = TRUE, boost_damage = TRUE, 
        foc = 3, dice = rep(6, 9))
    expect_equal(object = test1, expected = 32)

    # TEST 2: all sixes, no boost
    test2 <- melee(blueleader, target = list(stats = c(DEF = 12, ARM = 18)), boost_hit = FALSE, boost_damage = FALSE, 
        foc = 3, dice = rep(6, 11))
    expect_equal(object = test2, expected = 50)

    # TEST 3: all ones, no boost, knocked down
    test3 <- melee(blueleader, target = list(stats = c(DEF = 12, ARM = 18)), boost_hit = FALSE, boost_damage = FALSE, 
        foc = 3, kd = TRUE, dice = rep(1, 9))
    expect_equal(object = test3, expected = 7)

    # TEST 4: all ones, no boosted
    test4 <- melee(blueleader, target = list(stats = c(DEF = 12, ARM = 18)), boost_hit = FALSE, boost_damage = FALSE, 
        foc = 3, dice = rep(1, 10))
    expect_equal(object = test4, expected = 0)

    # TEST 5: max (17) dice, min damage
    d5 <- c(1, 4, 1, 1, 1, 2, 3, 1, 1, 3, 2, 1, 1, 4, 2, 1, 1)
    test5 <- melee(blueleader, target = list(stats = c(DEF = 12, ARM = 18)), 
        boost_hit = FALSE, boost_damage = FALSE, foc = 3, dice = d5)
    expect_equal(object = test5, expected = 7)

    # TEST 6: 
    d6 <- c(1, 1, 6, 2, 2, 1, 2, 6, 3, 1, 2, 1, 2, 2, 6, 2, 6)
    test6 <- melee(blueleader, target = list(stats = c(DEF = 12, ARM = 18)), 
        boost_hit = FALSE, boost_damage = FALSE, foc = 3, dice = d6)
    expect_equal(object = test6, expected = 4)

    # TEST 7: 
    d7 <- c(6, 1, 3, 4, 2, 2, 5, 5, 5, 3, 1, 2, 2, 5, 4, 5, 2)
    test7 <- melee(blueleader, target = list(stats = c(DEF = 12, ARM = 18)), 
        boost_hit = TRUE, boost_damage = FALSE, foc = 3, dice = d7)
    expect_equal(object = test7, expected = 8)

    # TEST 8: no charge
    options(warn = -1)
    test8 <- melee(blueleader, target = list(stats = c(DEF = 12, ARM = 18)), 
        boost_hit = TRUE, boost_damage = FALSE, foc = 0, dice = d6)
    options(warn = wo)
    expect_equal(object = test8, expected = 0)

    greycharlatan <- list(stats = c(SPD = 6, MAT = 6, RAT = 4, DEF = 12, ARM = 18, BASE = 50),
        range = list(),
        melee = list('scythe' = list(stats = c(RNG = 0.5, PAS = 17), special = c("chain attack bloodbath")), 
            'scythe' = list(stats = c(RNG = 0.5, PAS = 17), special = c("chain attack bloodbath"))), 
        special = character(0))
    
    # TEST 9: sythean 6s
    test9 <- melee(greycharlatan, target = list(stats = c(DEF = 12, ARM = 18)), 
        boost_hit = FALSE, boost_damage = FALSE, foc = 3, dice = rep(6, 21))
    expect_equal(object = test9, expected = 6 * 11 - 5)

    # TEST 10: sythean 1s
    test10 <- melee(greycharlatan, target = list(stats = c(DEF = 12, ARM = 18)), 
        boost_hit = FALSE, boost_damage = FALSE, foc = 3, dice = rep(1, 8))
    expect_equal(object = test10, expected = 0)

    # TEST 11: sythean 1s, kd
    test11 <- melee(greycharlatan, target = list(stats = c(DEF = 12, ARM = 18)), kd = TRUE,
        boost_hit = FALSE, boost_damage = FALSE, foc = 3, dice = rep(1, 11))
    expect_equal(object = test11, expected = 6)

})
