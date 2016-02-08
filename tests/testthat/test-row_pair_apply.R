context("row_pair_apply")

# require('testthat')

#
# bindFunctionToRowPairs
#

test_that("bindFunctionToRowPairs easy sum", {
  data <- cbind(y=c(5,4), x1=c(1,0))
  fn1 <- bindFunctionToRowPairs(data, sum)
  expected_1_2 <- sum(oneRow(data, 1), oneRow(data, 2))
  expect_equal(expected_1_2, fn1(c(1, 2)))
  expected_1_1 <- sum(oneRow(data, 1), oneRow(data, 1))
  expect_equal(expected_1_1, fn1(c(1, 1)))
  expected_2_2 <- sum(oneRow(data, 1), oneRow(data, 2))
  expect_equal(4+4, fn1(c(2, 2)))
})

# TODO: Test row index out of range.  Should that live in oneRow?

#
# applyFunctionToRowPairs
#

test_that("applyFunctionToRowPairs diff", {
  data <- cbind(y=c(5,4,4), x1=c(10,0,0))
  diff <- function(row1, row2) row1 - row2
  out <- applyFunctionToRowPairs(data, diff)
  expect_equal(3, nrow(out))
  expect_equal(cbind(c(1), c(10)), out[1,])  # Row 1 - Row 2
  expect_equal(cbind(c(1), c(10)), out[2,])  # Row 1 - Row 3
  expect_equal(cbind(c(0),  c(0)), out[3,])  # Row 2 - Row 2
})

test_that("applyFunctionToRowPairs sum has one-column output", {
  data <- cbind(y=c(5,4,4), x1=c(10,0,0))
  out <- applyFunctionToRowPairs(data, sum)
  expect_equal(3, nrow(out))
  expect_equal(c(19), out[1,])  # 5 + 10 + 4 + 0
  expect_equal(c(19), out[2,])  # 5 + 10 + 4 + 0
  expect_equal( c(8), out[3,])  # 4 +  4 + 0+ 0
})

#
#  allRowPairApply
#

#TODO: Do this test with a dumber function than ttb.
test_that("allRowPairApply dimension test: matrix, 2 rows = 1 pair", {
  train_matrix <- cbind(y=c(5,4), x1=c(1,0), x2=c(0,1))
  model <- ttbModel(train_matrix, 1, c(2,3))
  out1 <- allRowPairApply(train_matrix, heuristics(model))
  expect_equal(1, nrow(out1))
  expect_equal(1, ncol(out1))
  out2 <- allRowPairApply(train_matrix, heuristics(model, model))
  expect_equal(1, nrow(out2))
  expect_equal(2, ncol(out2))
})

test_that("allRowPairApply dimension test: matrix, 3 rows = 3 pairs", {
  train_df <- data.frame(y=c(5,4,3), x1=c(1,0,0), x2=c(0,1,0))
  model <- ttbModel(train_df, 1, c(2,3))
  out1 <- allRowPairApply(train_df, heuristics(model))
  expect_equal(3, nrow(out1))
  expect_equal(1, ncol(out1))
  out2 <- allRowPairApply(train_df, heuristics(model, model))
  expect_equal(3, nrow(out2))
  expect_equal(2, ncol(out2))
})

test_that("allRowPairApply dimension test: data.frame", {
  train_df <- data.frame(y=c(5,4,3), x1=c(1,0,0), x2=c(0,1,0))
  model <- ttbModel(train_df, 1, c(2,3))
  out1 <- allRowPairApply(train_df, heuristics(model))
  expect_equal(3, nrow(out1))
  expect_equal(1, ncol(out1))
  out2 <- allRowPairApply(train_df, heuristics(model, model))
  expect_equal(3, nrow(out2))
  expect_equal(2, ncol(out2))
})

test_that("allRowPairApply dimension test: heuristics, heuristics", {
  train_df <- data.frame(y=c(5,4,3), x1=c(1,0,0), x2=c(0,1,0))
  model <- ttbModel(train_df, 1, c(2,3))
  out1 <- allRowPairApply(train_df, heuristics(model))
  expect_equal(3, nrow(out1))
  expect_equal(1, ncol(out1))
  out2 <- allRowPairApply(train_df, heuristics(model), heuristics(model))
  expect_equal(3, nrow(out2))
  expect_equal(2, ncol(out2))
})

test_that("allRowPairApply criterion function", {
  train_df <- data.frame(y=c(5,4,3), x1=c(1,0,0), x2=c(0,1,0))
  model <- ttbModel(train_df, 1, c(2,3))
  out1 <- allRowPairApply(train_df, criterion(1))
  expect_equal(3, nrow(out1))
  expect_equal(1, ncol(out1))
  out2 <- allRowPairApply(train_df, criterion(1), criterion(1))
  expect_equal(3, nrow(out2))
  expect_equal(2, ncol(out2))
})

test_that("allRowPairApply rowIndexes function", {
  train_df <- data.frame(y=c(5,4,3), x1=c(1,0,0), x2=c(0,1,0))
  model <- ttbModel(train_df, 1, c(2,3))
  out1 <- allRowPairApply(train_df, rowIndexes())
  expect_equal(3, nrow(out1))
  expect_equal(2, ncol(out1))
  out2 <- allRowPairApply(train_df, rowIndexes(), rowIndexes())
  expect_equal(3, nrow(out2))
  expect_equal(4, ncol(out2))
})

test_that("allRowPairApply colPairValues function numeric", {
  train_df <- data.frame(y=c(5,4,3), x1=c(1,0,0), x2=c(0,1,0))
  model <- ttbModel(train_df, 1, c(2,3))
  # colPairValues outputs 2 columns
  out1 <- allRowPairApply(train_df, colPairValues(1, "a"))
  expect_equal(3, nrow(out1))
  expect_equal(2, ncol(out1))
  out2 <- allRowPairApply(train_df, colPairValues(1, "a"),
                          colPairValues(1, "b"))
  expect_equal(3, nrow(out2))
  expect_equal(4, ncol(out2))
})

