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

# pairMatrix

test_that("pairMatrix one column one pair", {
  one_row <- matrix(c(1), 1, 1)
  out <- pairMatrix(2, function(x) { one_row })
  expect_equal(one_row, out)
})

test_that("pairMatrix two columns 3 pairs", {
  one_row <- matrix(c(10, 11), 1, 2)
  out <- pairMatrix(3, function(x) { one_row })
  # 3 * 2 / 2 = 3 rows.
  expect_equal(matrix(rep(c(10,11), 3), 3, 2, byrow = TRUE), out)
})

test_that("pairMatrix two columns 3 pairs return row_pair", {
  out <- pairMatrix(3, function(row_pair) { matrix(row_pair, 1, 2) })
  # 3 * 2 / 2 = 3 rows.
  expected_matrix <- rbind(c(1,2), c(1,3), c(2,3))
  expect_equal(rbind(c(1,2), c(1,3), c(2,3)), out)
})

test_that("pairMatrix return row_pair", {
  out <- pairMatrix(3, function(row_pair) { matrix(row_pair, 1, 2) })
  # 3 * 2 / 2 = 3 rows.
  expect_equal(rbind(c(1,2), c(1,3), c(2,3)), out)
})

test_that("pairMatrix return row_pair also_reverse_row_pairs", {
  out <- pairMatrix(3, function(row_pair) { matrix(row_pair, 1, 2) },
                    also_reverse_row_pairs=TRUE)
  # 3 rows in usual order + 3 rows in reverse order.
  # Ideally this test would not care about the order of the rows.
  expect_equal(
    rbind(c(1,2), c(1,3), c(2,3), c(3,2), c(3,1), c(2,1)), out)
})

test_that("pairMatrix error when not row", {
  expect_error(pairMatrix(2, identity),
               "pair evaluator function did not return rows")
})

# combineIntoOneFn

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
#  rowPairApplyList
#
test_that("rowPairApplyList rowIndexes ttb", {
  train_matrix <- matrix(c(5,4,1,0,0,1), 2, 3)
  ttb <- ttbModel(train_matrix, 1, c(2,3), fit_name="ttb1")
  out <- rowPairApplyList(train_matrix, list(rowIndexes(), heuristics(ttb)))
  expect_equal(cbind(Row1=c(1), Row2=c(2), ttb1=c(1)), out)
})

test_that("rowPairApplyList rowIndexes ttb also_reverse_row_pairs", {
  train_matrix <- matrix(c(5,4,1,0,0,1), 2, 3)
  ttb <- ttbModel(train_matrix, 1, c(2,3), fit_name="ttb1")
  out <- rowPairApplyList(train_matrix, list(rowIndexes(), heuristics(ttb)),
                          also_reverse_row_pairs=TRUE)
  expect_equal(cbind(Row1=c(1,2), Row2=c(2,1), ttb1=c(1,-1)), out)
})

#
#  rowPairApply
#

test_that("rowPairApply ttb test: matrix, 2 rows = 1 pair", {
  train_matrix <- matrix(c(5,4,1,0,0,1), 2, 3)
  ttb <- ttbModel(train_matrix, 1, c(2,3))
  
  # heuristics
  
  out_1 <- rowPairApply(train_matrix, heuristics(ttb))
  # output should look like
  #      ttbModel
  # [1,]        1
  expect_equal(cbind(ttbModel=c(1)), out_1)
  
  out_2 <- rowPairApply(train_matrix, heuristics(ttb, ttb))
  expected_out2 <- matrix(c(1,1), 1, 2, dimnames=list(NULL, c("ttbModel", "ttbModel")))
  #           ttbModel ttbModel
  # [1,]        1        1
  expect_equal(expected_out2, out_2)
  
  # heuristicsProb
  
  out_p1 <- rowPairApply(train_matrix, heuristicsProb(ttb))
  # output should look like
  #      ttbModel
  # [1,]        1
  expect_equal(cbind(ttbModel=c(1)), out_p1)
  
  out_p2 <- rowPairApply(train_matrix, heuristicsProb(ttb, ttb))
  expected_out2 <- matrix(c(1,1), 1, 2, dimnames=list(NULL, c("ttbModel", "ttbModel")))
  #           ttbModel ttbModel
  # [1,]        1        1
  expect_equal(expected_out2, out_p2)
})

#TODO: Do this test with a dumber function than ttb.
test_that("rowPairApply dimension test: matrix, 2 rows = 1 pair", {
  train_matrix <- cbind(y=c(5,4), x1=c(1,0), x2=c(0,1))
  model <- ttbModel(train_matrix, 1, c(2,3))
  out1 <- rowPairApply(train_matrix, heuristicsProb(model))
  expect_equal(1, nrow(out1))
  expect_equal(1, ncol(out1))
  out2 <- rowPairApply(train_matrix, heuristicsProb(model, model))
  expect_equal(1, nrow(out2))
  expect_equal(2, ncol(out2))
})

test_that("rowPairApply dimension test: matrix, 3 rows = 3 pairs", {
  train_df <- data.frame(y=c(5,4,3), x1=c(1,0,0), x2=c(0,1,0))
  model <- ttbModel(train_df, 1, c(2,3))
  out1 <- rowPairApply(train_df, heuristicsProb(model))
  expect_equal(3, nrow(out1))
  expect_equal(1, ncol(out1))
  out2 <- rowPairApply(train_df, heuristicsProb(model, model))
  expect_equal(3, nrow(out2))
  expect_equal(2, ncol(out2))
})

test_that("rowPairApply dimension test: data.frame", {
  train_df <- data.frame(y=c(5,4,3), x1=c(1,0,0), x2=c(0,1,0))
  model <- ttbModel(train_df, 1, c(2,3))
  out1 <- rowPairApply(train_df, heuristicsProb(model))
  expect_equal(3, nrow(out1))
  expect_equal(1, ncol(out1))
  out2 <- rowPairApply(train_df, heuristicsProb(model, model))
  expect_equal(3, nrow(out2))
  expect_equal(2, ncol(out2))
})

test_that("rowPairApply dimension test: heuristics, heuristics", {
  train_df <- data.frame(y=c(5,4,3), x1=c(1,0,0), x2=c(0,1,0))
  model <- ttbModel(train_df, 1, c(2,3))
  out1 <- rowPairApply(train_df, heuristicsProb(model))
  expect_equal(3, nrow(out1))
  expect_equal(1, ncol(out1))
  out2 <- rowPairApply(train_df, heuristicsProb(model), heuristicsProb(model))
  expect_equal(3, nrow(out2))
  expect_equal(2, ncol(out2))
})

test_that("rowPairApply probGreater function dimensions", {
  train_df <- data.frame(y=c(5,4,3), x1=c(1,0,0), x2=c(0,1,0))
  out1 <- rowPairApply(train_df, probGreater(1))
  expect_equal(3, nrow(out1))
  expect_equal(1, ncol(out1))
  out2 <- rowPairApply(train_df, probGreater(1), probGreater(1))
  expect_equal(3, nrow(out2))
  expect_equal(2, ncol(out2))
})

test_that("rowPairApply probGreater greater", {
  train_df <- data.frame(y=c(9,5))
  out <- rowPairApply(train_df, probGreater(1))
  expect_equal(c(ProbGreater=1), out[1,1])
})

test_that("rowPairApply probGreater same", {
  train_df <- data.frame(y=c(5,5))
  out <- rowPairApply(train_df, probGreater(1))
  expect_equal(c(ProbGreater=0.5), out[1,1])
})

test_that("rowPairApply probGreater less", {
  train_df <- data.frame(y=c(5,9))
  out <- rowPairApply(train_df, probGreater(1))
  expect_equal(c(ProbGreater=0), out[1,1])
})

test_that("rowPairApply correctGreater function dimensions", {
  train_df <- data.frame(y=c(5,4,3), x1=c(1,0,0), x2=c(0,1,0))
  out1 <- rowPairApply(train_df, correctGreater(1))
  expect_equal(3, nrow(out1))
  expect_equal(1, ncol(out1))
  out2 <- rowPairApply(train_df, correctGreater(1), correctGreater(1))
  expect_equal(3, nrow(out2))
  expect_equal(2, ncol(out2))
})

test_that("rowPairApply correctGreater greater", {
  train_df <- data.frame(y=c(9,5))
  out <- rowPairApply(train_df, correctGreater(1))
  expect_equal(c(CorrectGreater=1), out[1,1])
})

test_that("rowPairApply correctGreater same", {
  train_df <- data.frame(y=c(5,5))
  out <- rowPairApply(train_df, correctGreater(1))
  expect_equal(c(CorrectGreater=0), out[1,1])
})

test_that("rowPairApply correctGreater less", {
  train_df <- data.frame(y=c(5,9))
  out <- rowPairApply(train_df, correctGreater(1))
  expect_equal(c(CorrectGreater=-1), out[1,1])
})



test_that("rowPairApply rowIndexes function", {
  train_df <- data.frame(y=c(5,4,3), x1=c(1,0,0), x2=c(0,1,0))
  model <- ttbModel(train_df, 1, c(2,3))
  out1 <- rowPairApply(train_df, rowIndexes())
  expect_equal(3, nrow(out1))
  expect_equal(2, ncol(out1))
  out2 <- rowPairApply(train_df, rowIndexes(), rowIndexes())
  expect_equal(3, nrow(out2))
  expect_equal(4, ncol(out2))
})

test_that("rowPairApply colPairValues function numeric", {
  train_df <- data.frame(y=c(5,4,3), x1=c(1,0,0), x2=c(0,1,0))
  model <- ttbModel(train_df, 1, c(2,3))
  # colPairValues outputs 2 columns
  out1 <- rowPairApply(train_df, colPairValues(1, "a"))
  expect_equal(3, nrow(out1))
  expect_equal(2, ncol(out1))
  out2 <- rowPairApply(train_df, colPairValues(1, "a"),
                          colPairValues(1, "b"))
  expect_equal(3, nrow(out2))
  expect_equal(4, ncol(out2))
})

test_that("rowPairApply heuristics with fn", {
  train_matrix <- cbind(y=c(5,4), x1=c(1,1))
  model <- ttbModel(train_matrix, 1, c(2))
  # Using heuristicsList.
  out <- rowPairApply(train_matrix,
                         heuristicsList(list(model), fn=predictProbInternal),
                         heuristicsList(list(model), fn=predictPairInternal))
  # predictProbInternal guesses wtih 0.5.  predictPairInternal guesses with 0.
  expect_equal(cbind(0.5, 0), unname(out))
  # Using the shortcut wrappers, heuristicsProb and heuristics.
  out2 <- rowPairApply(train_matrix, heuristicsProb(model),
                         heuristics(model))
  expect_equal(cbind(0.5, 0), unname(out))
})
