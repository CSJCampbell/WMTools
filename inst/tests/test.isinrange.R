
context("is attacker engaged")

test_that("is.engaged", {


    redbandit <- list(stats = c(SPD = 4, MAT = 6, RAT = 4, DEF = 10, ARM = 20, BASE = 50), 
        range = list(bombard = list(
                stats = c(RNG = 14, ROF = 1, AOE = 3, POW = 14), 
                special = "arcing")
        ),
        melee = list(axe = list(stats = c(RNG = 0.5, PAS = 12), 
                special = "critical amputation")),
        special = character(0))
    
    redknave <- list(stats = c(SPD = 4, MAT = 6, RAT = 4, DEF = 10, ARM = 20, BASE = 50), 
        range = list('grenade launcher' = list(stats = c(RNG = 10, ROF = 1, AOE = 3, POW = 10),
                special = "arcing"), 
            'grenade launcher' = list(stats = c(RNG = 10, ROF = 1, AOE = 3, POW = 10),
                special = "arcing")
        ),
        melee = list('war lance' = list(stats = c(RNG = 2, PAS = 18), special = "powerful charge"),
            'assault shield' = list(stats = c(RNG = 0.5, PAS = 14), special = character(0))), 
        special = "bulldoze")
    

    # TEST 1: redbandit dist 6 kd
    expect_false(object = is.engaged(redbandit, redbandit, dist = 6, kd = TRUE))

    # TEST 2: redbandit dist 2
    expect_false(object = is.engaged(redbandit, redbandit, dist = 2, kd = FALSE))

    # TEST 3: redbandit dist 0.5
    expect_true(object = is.engaged(redbandit, redbandit, dist = 0.5, kd = FALSE))

    # TEST 4: redknave dist 2
    expect_true(object = is.engaged(redknave, redbandit, dist = 2, kd = FALSE))

    # TEST 5: redbandit redknave dist 0.5
    expect_true(object = is.engaged(redbandit, redknave, dist = 0.5))

    # TEST 6: redbandit redknave dist 2
    expect_true(object = is.engaged(redbandit, redknave, dist = 2))

    # TEST 7: redbandit redknave dist -2
    expect_error(object = is.engaged(redbandit, redknave, dist = -2), regexp = "dist is less than zero")

    # TEST 8: redbandit redknave dist -2
    expect_error(object = is.engaged(redbandit, redknave, dist = NA), regexp = "dist is NA")

    # TEST 9: redbandit redknave dist 0:3
    expect_equal(object = is.engaged(redbandit, redknave, dist = 0:3), expected = c(TRUE, TRUE, TRUE, FALSE))

    # TEST 10: redbandit redknave dist 0:3 kd rep(c(TRUE, FALSE), each = 2)
    expect_equal(object = is.engaged(redbandit, redknave, dist = 0:3, kd = rep(c(TRUE, FALSE), each = 2)), 
        expected = c(FALSE, FALSE, TRUE, FALSE))

    # TEST 11: knocked down redknave does not engage at 2
    expect_false(object = is.engaged(redbandit, redknave, dist = 2, kd = TRUE))

    # TEST 12: dist 0
    expect_equal(is.engaged(warjack = redbandit, target = redbandit, dist = c(0, 0), kd = c(TRUE, FALSE)), 
        expected = c(FALSE, TRUE))

    # TEST 13: vectorized calculation
    expect_equal(object = is.engaged(warjack = redbandit, target = redbandit, dist = 0:2, kd = TRUE),
        expected = c(FALSE, FALSE, FALSE))

    # TEST 14: warjack with no melee weapons target kd
    expect_equal(object = is.engaged(warjack = list(), target = redbandit, dist = 0:2, kd = TRUE),
        expected = c(FALSE, FALSE, FALSE))

    # TEST 15: warjack with no melee weapons
    expect_equal(object = is.engaged(warjack = list(), target = redbandit, dist = 0:2),
        expected = c(TRUE, FALSE, FALSE))
    
    # TEST 16: target with no melee weapons
    expect_equal(object = is.engaged(warjack = redbandit, target = list(), dist = 0:2, kd = FALSE),
        expected = c(TRUE, FALSE, FALSE))
})



test_that("is.in.range", {

    # TEST 1: RNG 10, dist 10
    expect_true(object = is.in.range(list(stats = c(AOE = 3, RNG = 10)), dist = 10))

    # TEST 2: AOE NA RNG 10, dist 10
    expect_true(object = is.in.range(list(stats = c(AOE = NA, RNG = 10)), dist = 10))

    # TEST 3: AOE 1 RNG 1, dist 10
    expect_false(object = is.in.range(list(stats = c(AOE = 1, RNG = 1)), dist = 10))

    # TEST 4: AOE 0 RNG 100, dist 1
    expect_true(object = is.in.range(list(stats = c(AOE = 0, RNG = 100)), dist = 1))

    # TEST 5: AOE 10 RNG 1, dist 10
    expect_true(object = is.in.range(list(stats = c(AOE = 10, RNG = 1)), dist = 10))

})

