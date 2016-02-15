context("row_pair_data")


test_that("logRegData simple", {
  train_data <- cbind(y=c(5,4), x1=c(1,0), x2=c(1,1))
  out <- logRegData(train_data, 1, c(2,3), rowDiff)
  # Check criterion
  expect_equal(c(1,0), out[,1])
  # Check predictors
  expect_equal(c(1,-1), out[,2])
  expect_equal(c(0,0), out[,3])
  # Check column names
  expect_equal(c("y", "x1", "x2"), colnames(out))
})

test_that("logRegData data.frame", {
  train_data <- data.frame(y=c(5,4), x1=c(1,0), x2=c(1,1))
  out <- logRegData(train_data, 1, c(2,3), rowDiff)
  # Check criterion
  expect_equal(c(1,0), out[,1])
  # Check predictors
  expect_equal(c(1,-1), out[,2])
  expect_equal(c(0,0), out[,3])
  # Check column names
  expect_equal(c("y", "x1", "x2"), colnames(out))
})

