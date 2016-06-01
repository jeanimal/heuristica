context("performance")

# require('testthat')

# confusionMatrixRequiredCategories

test_that("confusionMatrixRequiredCategories match all factors in data", {
  data1 <- c(0,1,1)
  data2 <- c(0,1,1)
  out <- confusionMatrixRequiredCategories(data1, data2, c(0,1))
  expected <- cbind(c(1,0), c(0,2))
  expect_equal(expected, matrix(out, 2, 2))
  expect_equal(c("0", "1"), dimnames(out)[[1]])
  expect_equal(c("0", "1"), dimnames(out)[[2]])
})

test_that("confusionMatrixRequiredCategories non-match all factors in data", {
  data1 <- c(0,0,1,1)
  data2 <- c(0,1,0,1)
  out <- confusionMatrixRequiredCategories(data1, data2, c(0,1))
  expected <- cbind(c(1,1), c(1,1))
  expect_equal(expected, matrix(out, 2, 2))
  expect_equal(c("0", "1"), dimnames(out)[[1]])
  expect_equal(c("0", "1"), dimnames(out)[[2]])
})

test_that("confusionMatrixRequiredCategories non-match missing -1 factor in data", {
  data1 <- c(0,0,1,1)
  data2 <- c(0,1,0,1)
  out <- confusionMatrixRequiredCategories(data1, data2, c(-1,0,1))
  expected <- cbind(c(0,0,0), c(0,1,1), c(0,1,1))
  expect_equal(expected, matrix(out, 3, 3))
  expect_equal(c("-1", "0", "1"), dimnames(out)[[1]])
  expect_equal(c("-1", "0", "1"), dimnames(out)[[2]])
})

test_that("confusionMatrixRequiredCategories non-match missing 2 factor in data", {
  data1 <- c(0,0,1,1)
  data2 <- c(0,1,0,1)
  out <- confusionMatrixRequiredCategories(data1, data2, c(0,1,2))
  expected <- cbind(c(1,1,0), c(1,1,0), c(0,0,0))
  expect_equal(expected, matrix(out, 3, 3))
  expect_equal(c("0", "1", "2"), dimnames(out)[[1]])
  expect_equal(c("0", "1", "2"), dimnames(out)[[2]])
})

test_that("confusionMatrixRequiredCategories missing 1 factor in data1", {
  data1 <- c(0,0,0)
  data2 <- c(0,1,1)
  out <- confusionMatrixRequiredCategories(data1, data2, c(0,1))
  expected <- cbind(c(1,0), c(2,0))
  expect_equal(expected, matrix(out, 2, 2))
  expect_equal(c("0", "1"), dimnames(out)[[1]])
  expect_equal(c("0", "1"), dimnames(out)[[2]])
})

test_that("confusionMatrixRequiredCategories missing 1 factor in data2", {
  data1 <- c(0,1,1)
  data2 <- c(0,0,0)
  out <- confusionMatrixRequiredCategories(data1, data2, c(0,1))
  expected <- cbind(c(1,2), c(0,0))
  expect_equal(expected, matrix(out, 2, 2))
  expect_equal(c("0", "1"), dimnames(out)[[1]])
  expect_equal(c("0", "1"), dimnames(out)[[2]])
})


# confusionMatrixPredictPair
# (Most tests of this are via the more generic (but non-public)
# confusionMatrix function.)

test_that("confusionMatrixPredictPair missing -1 factor in data", {
  # Below, the correct outcome is always 1, so only the last row of the
  # confusion matrix has non-zero counts.  But the predictor makes a few
  # mistakes, so some non-zero counts are off the diagonal.
  out <- confusionMatrixPredictPair(c(1,1,1), c(1,0,0))
  expected <- cbind(c(0,0,0), c(0,0,2), c(0,0,1))
  expect_equal(expected, matrix(out, 3, 3))
  expect_equal(c("-1", "0", "1"), dimnames(out)[[1]])
  expect_equal(c("-1", "0", "1"), dimnames(out)[[2]])
})

# accuracyFromConfusionMatrix

test_that("accuracyFromConfusionMatrix 1", {
  # Below accuracy is 1 (100% correct) because 4 -1's were correctly predicted,
  # and 2 1's were correctly predicted.  (On-diagonal elements are correct
   # predictions.)
  expect_equal(1, accuracyFromConfusionMatrix(
    cbind("-1"=c(4,0,0), "0"=c(0,0,0), "1"=c(0,0,2))))
})

test_that("accuracyFromConfusionMatrix 0", {
  # 3 wrong and 3 more wrong for 0 accuracy.
  expect_equal(0, accuracyFromConfusionMatrix(
    cbind(c(0,0,3), c(0,0,0), c(3,0,0))))
})

test_that("accuracyFromConfusionMatrix 0.9", {
  # Below is 4 + 5 correct, 1 incorrect, for 9/10 = 0.9 accuracy.
  expect_equal(0.9, accuracyFromConfusionMatrix(
    cbind(c(4,0,1), c(0,0,0), c(0,0,5))))
})

test_that("accuracyFromConfusionMatrix guess 0.5", {
  # Below has 3+1=4 guesses, and 0.5 are assigned correct.
  expect_equal(0.5, accuracyFromConfusionMatrix(
    cbind(c(0,0,0), c(3,0,1), c(0,0,0))))
})

# TODO(jeanw): What about this?  The 2 is on-diagonal, and that guess
# is treated as automatically correct.  Should it be?
# > accuracyFromConfusionMatrix(cbind(c(0,0,0), c(3,2,1), c(0,0,0)))
# [1] 0.6666667

test_that("guessExpectedValue", {
  m <- rbind(c("-1"=2, "0"=2, "1"=2), c("-1"=4, "0"=4, "1"=4),
             c("-1"=6, "0"=6, "1"=6))
  rownames(m) <- c("-1", "0", "1")
  out <- guessExpectedValue(m)
  # In cases when correct value was -1, it guessed.
  expect_equal(c("-1"=3, "0"=0, "1"=3), out["-1",])
  # There were no cases when correct value was 0.
  expect_equal(c("-1"=6, "0"=0, "1"=6), out["0",])
  # In cases when correct value was 1, it guessed.
  expect_equal(c("-1"=9, "0"=0, "1"=9), out["1",])
  
  # Check dimensions-- now there are 3.
  expect_equal(3, nrow(out))
  expect_equal(3, ncol(out))
})