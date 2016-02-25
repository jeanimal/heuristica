context("cue_validity")

# cueValidity

expect_equal(1   , cueValidity(c(5,4), c(1,0)),tolerance=1)
expect_equal(   0, cueValidity(c(5,4), c(0,1)),tolerance=1)
expect_equal(0.5, cueValidity(c(5,4), c(0,0)),tolerance=0.1)
expect_equal(0.5, cueValidity(c(5,4), c(1,1)),tolerance=0.1)
expect_equal(0.5, cueValidity(c(4,5), c(1,1)),tolerance=0.1)
expect_equal(0   , cueValidity(c(4,5), c(1,0)),tolerance=1)

expect_equal(0.5, cueValidity(c(5,4,3), c(0,0,0)),tolerance=0.1)
expect_equal(   0, cueValidity(c(5,4,3), c(0,0,1)),tolerance=1)
expect_equal(0.5, cueValidity(c(5,4,3), c(0,1,0)),tolerance=0.1)
expect_equal(   0, cueValidity(c(5,4,3), c(0,1,1)),tolerance=1)
expect_equal(   1, cueValidity(c(5,4,3), c(1,0,0)),tolerance=1)
expect_equal(0.5, cueValidity(c(5,4,3), c(1,0,1)),tolerance=0.1)
expect_equal(   1, cueValidity(c(5,4,3), c(1,1,0)),tolerance=1)
expect_equal(0.5, cueValidity(c(5,4,3), c(1,1,1)),tolerance=0.1)

expect_equal(0.667, cueValidity(c(5,4,3,2), c(1,1,0,1)), tolerance=0.001)
expect_equal(0.75,  cueValidity(c(5,4,3,2,1), c(1,1,1,0,1)),tolerance=0.01)

# Real values.
expect_equal(0.667, cueValidity(c(397,385,327), c(99,100,85)),
  tolerance=0.001)

expect_equal(-10, cueValidity(c(5,4,3), c(1,1,1), replaceNanWith=-10),tolerance=1)
expect_equal(0.75,  cueValidity(c(5,4,3,2,1), c(1,1,1,0,1), replaceNanWith=-10),tolerance=0.01)

# matrixCueValidity

expect_equal(c(1), matrixCueValidity(matrix(c(5,4,1,0), 2, 2), 1, c(2)) ,tolerance=1)
expect_equal(c(1), matrixCueValidity(matrix(c(5,4,1,0,0,1), 2, 2), 1, c(2)) ,tolerance=1)
expect_equal(c(1,0), matrixCueValidity(matrix(c(5,4,1,0,0,1), 2, 3), 1, c(2,3)) ,tolerance=1)
expect_equal(c(1,0), matrixCueValidity(matrix(c(1,0,5,4,0,1), 2, 3), 2, c(1,3)), tolerance=1)
expect_equal(c(1,0), matrixCueValidity(matrix(c(5,4,0,1), 2, 2), 1, c(1, 2)),tolerance=1 )

test_that("matrixCueValidity 5x4 names", {
  df <- data.frame(Criterion=c(5,4,3,2,1), a=c(1,0,0,0,1),
                   b=c(1,1,1,0,0), c=c(1,1,1,0,1))
  cv <- matrixCueValidity(df, 1, c(2,3,4)) 
  expect_equal(c(a=0.5, b=1.0, c=0.75), cv)
})

test_that("matrixCueValidity 3x3 names shifted criterion", {
  df <- data.frame(Garbage=c(99, 99, 99), Criterion=c(5,4,3),
                   Cue=c(1,0,1))
  names(df) <- c('Garbage', 'Criterion', 'Cue')
  cv <- matrixCueValidity(df, 2, c(3)) 
  expect_equal(c(0.5), unname(cv),tolerance=0.1)
  expect_equal(c('Cue'), names(cv),tolernace=1)
})

