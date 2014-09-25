
context("utilities")

test_that("is.seq.mat", {

    # TEST 1: from = 1:3, to = 4:6, dim = c(3, 6)
    test1 <- is.seq.mat(from = 1:3, to = 4:6, dim = c(3, 6))
    expect_equal(object = test1, expected = rbind(1:6 %in% 1:4, 1:6 %in% 2:5, 1:6 %in% 3:6))

    # TEST 2: length 1 to
    test2 <- is.seq.mat(from = 1:3, to = 4, dim = c(3, 6))
    expect_equal(object = test2, expected = rbind(1:6 %in% 1:4, 1:6 %in% 2:4, 1:6 %in% 3:4))

    # TEST 3: length 1
    test3 <- is.seq.mat(from = 1, to = 1, dim = c(1, 1))
    expect_true(test3)

    # TEST 4: length 2
    expect_error(is.seq.mat(from = 1, to = 2, dim = c(1, 1)), regex = "from and to should be between 1 and 1")

})


test_that("selectSequences", {

    # TEST 1: letters
    ma <- matrix(letters[1:18], nrow = 3, ncol = 6, byrow = TRUE)
    test1 <- selectSequences(mat = ma, from = 1:3, to = 4:6)
    expect_equal(object = test1, expected = rbind(letters[1:4], letters[8:11], letters[15:18]))

    # TEST 2: letters NA
    test2 <- selectSequences(mat = ma, from = 1:3, to = 4)
    expect_equal(object = test2, expected = rbind(letters[1:4], c(letters[8:10], NA), c(letters[15:16], NA, NA)))

    # TEST 3: letters z
    test3 <- selectSequences(mat = ma, from = 1:3, to = 4, pad = "z")
    expect_equal(object = test3, expected = rbind(letters[1:4], letters[c(8:10, 26)], letters[c(15, 16, 26, 26)]))

})
