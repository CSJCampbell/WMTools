

context("is scattering shot a hit")

test_that("is.scatter.hit", {

    # TEST 1: 1" scatter following miss is hit
    expect_true(object = is.scatter.hit(list(stats = c(RNG = 14, POW = 14, AOE = 3), 
        special = c("arcing")), dice = c(1, 1)))

    # TEST 2: 1" scatter following 1" short is hit
    expect_true(object = is.scatter.hit(list(stats = c(RNG = 14, POW = 14, AOE = 3), 
        special = c("arcing")), short = 1 + 0.5 * 30 / 25.4, dice = c(1, 1)))

    # TEST 3: 1" scatter following 2.5" short is miss
    expect_false(object = is.scatter.hit(list(stats = c(RNG = 14, POW = 14, AOE = 3), 
        special = c("arcing")), short = 2.5 + 0.5 * 30 / 25.4, dice = c(1, 1)))

    # TEST 4: 6" scatter following 2.5" short is miss
    expect_false(object = is.scatter.hit(list(stats = c(RNG = 14, POW = 14, AOE = 3), 
        special = c("arcing")), short = 2.5 + 0.5 * 30 / 25.4, dice = c(6, 1)))

    # TEST 5: 5" scatter following 2.5" short is hit
    expect_true(object = is.scatter.hit(list(stats = c(RNG = 14, POW = 14, AOE = 3), 
        special = c("arcing")), short = 2.5 + 0.5 * 30 / 25.4, dice = c(5, 1)))

    # TEST 6: 6" scatter following 2.5" short to 4 is hit
    expect_false(object = is.scatter.hit(list(stats = c(RNG = 14, POW = 14, AOE = 3), 
        special = c("arcing")), short = 2.5 + 0.5 * 30 / 25.4, dice = c(6, 4)))

    # TEST 7: 1" scatter following miss to 2 is hit
    expect_true(object = is.scatter.hit(list(stats = c(RNG = 14, POW = 14, AOE = 3), 
        special = c("arcing")), dice = c(1, 2)))

    # TEST 8: 2" scatter following miss to 3 is hit
    expect_true(object = is.scatter.hit(list(stats = c(RNG = 14, POW = 14, AOE = 3), 
        special = c("arcing")), dice = c(2, 3)))

    # TEST 9: 3" scatter following miss to 5 is miss
    expect_false(object = is.scatter.hit(list(stats = c(RNG = 14, POW = 14, AOE = 3), 
        special = c("arcing")), dice = c(3, 5)))

    # TEST 10: 4" scatter following miss to 6 is miss
    expect_false(object = is.scatter.hit(list(stats = c(RNG = 14, POW = 14, AOE = 3), 
        special = c("arcing")), dice = c(4, 6)))

    # TEST 11: 4" scatter following miss to 6 from 2" range is hit
    expect_true(object = is.scatter.hit(list(stats = c(RNG = 14, POW = 14, AOE = 3), 
        special = c("arcing")), max = 1, dice = c(4, 6)))

    # TEST 12: 3" scatter following AOE 5 miss to 6 is hit
    expect_true(object = is.scatter.hit(list(stats = c(AOE = 5)), 
        dice = c(3, 6)))

    # TEST 13: NA AOE is miss
    expect_false(object = is.scatter.hit(list(stats = c(RNG = 14, POW = 14, AOE = NA)), 
        max = 1, dice = 1))

    # TEST 14: insufficient dice is error
    expect_error(object = is.scatter.hit(list(stats = c(RNG = 14, POW = 14, AOE = 14)), 
        max = 14, dice = 1), regexp = "insufficient dice provided, but two required")

    # TEST 15: length 4 short
    expect_equal(object = is.scatter.hit(list(stats = c(AOE = 3)), 
        short = 0:3, base = 30, dice = c(1, 1)), expected = c(TRUE, TRUE, TRUE, FALSE))

})


