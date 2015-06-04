context("cue_validity")

# cueValidity

expect_equal(1   , cueValidity(c(5,4), c(1,0)))
expect_equal(   0, cueValidity(c(5,4), c(0,1)))
expect_equal(0.5, cueValidity(c(5,4), c(0,0)))
expect_equal(0.5, cueValidity(c(5,4), c(1,1)))
expect_equal(0.5, cueValidity(c(4,5), c(1,1)))
expect_equal(0   , cueValidity(c(4,5), c(1,0)))

expect_equal(0.5, cueValidity(c(5,4,3), c(0,0,0)))
expect_equal(   0, cueValidity(c(5,4,3), c(0,0,1)))
expect_equal(0.5, cueValidity(c(5,4,3), c(0,1,0)))
expect_equal(   0, cueValidity(c(5,4,3), c(0,1,1)))
expect_equal(   1, cueValidity(c(5,4,3), c(1,0,0)))
expect_equal(0.5, cueValidity(c(5,4,3), c(1,0,1)))
expect_equal(   1, cueValidity(c(5,4,3), c(1,1,0)))
expect_equal(0.5, cueValidity(c(5,4,3), c(1,1,1)))

expect_equal(0.667, cueValidity(c(5,4,3,2), c(1,1,0,1)), 0.001)
expect_equal(0.75,  cueValidity(c(5,4,3,2,1), c(1,1,1,0,1)))

expect_equal(-10, cueValidity(c(5,4,3), c(1,1,1), replaceNanWith=-10))
expect_equal(0.75,  cueValidity(c(5,4,3,2,1), c(1,1,1,0,1), replaceNanWith=-10))

# matrixCueValidity

expect_equal(c(1), matrixCueValidity(matrix(c(5,4,1,0), 2, 2), 1, c(2)) )
expect_equal(c(1), matrixCueValidity(matrix(c(5,4,1,0,0,1), 2, 2), 1, c(2)) )
expect_equal(c(1,0), matrixCueValidity(matrix(c(5,4,1,0,0,1), 2, 3), 1, c(2,3)) )
expect_equal(c(1,0), matrixCueValidity(matrix(c(1,0,5,4,0,1), 2, 3), 2, c(1,3)) )
expect_equal(c(1,0), matrixCueValidity(matrix(c(5,4,0,1), 2, 2), 1, c(1, 2)) )

test_that("matrixCueValidity 3x3 names shifted criterion", {
  df <- data.frame(matrix(c(99, 99, 99, 5,4,3,1,0,1), 3, 3))
  names(df) <- c('Garbage', 'Criterion', 'Cue')
  cv <- matrixCueValidity(df, 2, c(3)) 
  expect_equal(c(0.5), unname(cv))
  expect_equal(c('Cue'), names(cv))
})

