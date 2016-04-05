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
  expect_equal(4+4, fn1(c(2, 2)))
})

test_that("bindFunctionToRowPairs diff preserve column names", {
  data <- cbind(y=c(5,4), x1=c(1,0))
  my_diff <- function(row1, row2) row1 - row2
  fn1 <- bindFunctionToRowPairs(data, my_diff)
  expected_1_2 <- my_diff(oneRow(data, 1), oneRow(data, 2))
  out_1_2 <- fn1(c(1, 2))
  expect_equal(expected_1_2, out_1_2)
  expect_equal(colnames(data), colnames(out_1_2))
})

# TODO: Test row index out of range.  Should that live in oneRow?

test_that("combineIntoOneFn identity once and twice", {
  expect_equal(111, identity(111)) # A sanity check
  fn_all <- combineIntoOneFn(list(identity))
  expect_equal(cbind(out=222), fn_all(222))
  fn_all <- combineIntoOneFn(list(identity, identity))
  expect_equal(cbind(333, 333), unname(fn_all(333)))
})

test_that("combineIntoOneFn plus functions", {
  plusOne <- function(a) return(a+1)
  plusTwo <- function(a) return(a+2)
  fn_all <- combineIntoOneFn(list(identity, plusOne, plusTwo))
  expect_equal(cbind(100, 101, 102), unname(fn_all(100)))
})

test_that("combineIntoOneFn two column functions", {
  plusOneAndPlusTwo <- function(a) return(cbind(a+1, a+2))
  fn_all <- combineIntoOneFn(list(identity, plusOneAndPlusTwo, identity))
  expect_equal(cbind(100, 101, 102, 100), unname(fn_all(100)))
})

test_that("combineIntoOneFn two column function with header", {
  plusOneAndPlusTwoWithHeader <- function(a) {
    return(cbind(x1=a+1, x2=a+2))
  }
  # sanity check
  expect_equal(cbind(x1=101, x2=102), plusOneAndPlusTwoWithHeader(100))
  fn_all <- combineIntoOneFn(list(plusOneAndPlusTwoWithHeader))
  expect_equal(cbind(x1=101, x2=102), fn_all(100))
})

#
# applyFunctionToRowPairs
#

test_that("applyFunctionToRowPairs diff", {
  data <- cbind(y=c(5,4,4), x1=c(10,0,0))
  diff <- function(row1, row2) row1 - row2
  out <- applyFunctionToRowPairs(data, diff)
  expect_equal(3, nrow(out))
  # applyFunctionToRowPairs should not lose the column names.
  expect_equal(c("y", "x1"), colnames(out))
  # Row 1 - Row 2
  expect_equal(cbind(y=c(1), x1=c(10)), oneRow(out, 1))
  # Row 1 - Row 3
  expect_equal(cbind(y=c(1), x1=c(10)), oneRow(out, 2))
  # Row 2 - Row 2
  expect_equal(cbind(y=c(0), x1=c(0)), oneRow(out, 3))
})

test_that("applyFunctionToRowPairs diff data.frame", {
  data <- data.frame(y=c(5,4,4), x1=c(10,0,0))
  diff <- function(row1, row2) row1 - row2
  out <- applyFunctionToRowPairs(data, diff)
  expect_equal(3, nrow(out))
  # applyFunctionToRowPairs should not lose the column names.
  expect_equal(c("y", "x1"), colnames(out))
  # Row 1 - Row 2
  expect_equal(cbind(y=c(1), x1=c(10)), oneRow(out, 1))
  # Row 1 - Row 3
  expect_equal(cbind(y=c(1), x1=c(10)), oneRow(out, 2))
  # Row 2 - Row 2
  expect_equal(cbind(y=c(0), x1=c(0)), oneRow(out, 3))
})

test_that("applyFunctionToRowPairs sum has one-column output", {
  data <- cbind(y=c(5,4,4), x1=c(10,0,0))
  out <- applyFunctionToRowPairs(data, sum)
  expect_equal(3, nrow(out))
  expect_equal(c(19), out[1,])  # 5 + 10 + 4 + 0
  expect_equal(c(19), out[2,])  # 5 + 10 + 4 + 0
  expect_equal( c(8), out[3,])  # 4 +  4 + 0 + 0
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

test_that("allRowPairApply heuristics with fn", {
  train_matrix <- cbind(y=c(5,4), x1=c(1,1))
  model <- ttbModel(train_matrix, 1, c(2))
  out <- allRowPairApply(train_matrix, heuristics(model, fn=predictRoot),
                         heuristics(model, fn=predictPairInternal))
  # predictRoot guesses wtih 0.5.  predictPairInternal guesses with 0.
  expect_equal(cbind(0.5, 0), unname(out))
})

## Testing new functions not yet integrated with the rest of the package.

test_that("simpleRowPairApplyList and heuristicWrapperFn", {
  m <- cbind(y=c(3:1), x1=c(1,0,0), x2=c(1,0,1))
  ttb <- ttbModel(m, 1, c(2:3))
  dawes <- dawesModel(m, 1, c(2:3))
  #out <- simpleRowPairApplyList(m, list(heuristicWrapperFn(ttb),
  #                                      createProbGreaterFn(1)))
  #expect_equal(c(1,1,0), out[,"ttbModel"])
  #expect_equal(c(1,1,1), out[,"ProbGreater"])
})

# simpleRowPairApply(city_population, heuristicWrapperFn2(city_population, ttb)))
# head(simpleRowPairApply(city_population, createHeuristicWrapperFn2(ttb)))

test_that("simpleRowPairApply and createHeuristicWrapperFn2", {
  m <- cbind(y=c(3:1), x1=c(1,0,0), x2=c(1,0,1))
  ttb <- ttbModel(m, 1, c(2:3))
  out <- simpleRowPairApply(m, createHeuristicWrapperFn2(ttb))
  expect_equal(c(1,1,0), out[,"ttbModel"])
})

test_that("simpleRowPairApply and createrRowIndexPairFn", {
  m <- cbind(y=c(3:1), x1=c(1,0,0), x2=c(1,0,1))
  out <- simpleRowPairApply(m, createrRowIndexPairFn())
  expect_equal(cbind(Row1=1, Row2=2), oneRow(out, 1))
  expect_equal(cbind(Row1=1, Row2=3), oneRow(out, 2))
  expect_equal(cbind(Row1=2, Row2=3), oneRow(out, 3))
})

test_that("simpleRowPairApplyList createHeuristicWrapperFn2 createrRowIndexPairFn", {
  m <- cbind(y=c(3:1), x1=c(1,0,0), x2=c(1,0,1))
  ttb <- ttbModel(m, 1, c(2:3))
  out <- simpleRowPairApplyList(m, list(createrRowIndexPairFn(),
                                        createHeuristicWrapperFn2(ttb)))
  expect_equal(cbind(Row1=1, Row2=2, ttbModel=1), oneRow(out, 1))
  expect_equal(cbind(Row1=1, Row2=3, ttbModel=1), oneRow(out, 2))
  expect_equal(cbind(Row1=2, Row2=3, ttbModel=0), oneRow(out, 3))
})
