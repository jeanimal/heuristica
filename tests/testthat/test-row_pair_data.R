context("row_pair_data")


test_that("toRowPairData matrix forward", {
  train_data <- cbind(y=c(9,4), x1=c(3,0), x2=c(1,1))
  out <- toRowPairData(train_data, 1, c(2,3), rowDiff)
  # The criterion is scaled to 0-1.
  expect_equal(cbind(y=c(1), x1=c(3), x2=c(0)), out)
})

test_that("toRowPairData matrix backward", {
  # Reverse rows relative to prior test.
  train_data <- cbind(y=c(4,9), x1=c(0,3), x2=c(1,1))
  out <- toRowPairData(train_data, 1, c(2,3), rowDiff)
  # The criterion is scaled to 0-1.
  expect_equal(cbind(y=c(0), x1=c(-3), x2=c(0)), out)
})

test_that("toRowPairData data.frame forward", {
  train_data <- data.frame(y=c(9,4), x1=c(3,0), x2=c(1,1))
  out <- toRowPairData(train_data, 1, c(2,3), rowDiff)
  # The criterion is scaled to 0-1.
  expect_equal(cbind(y=c(1), x1=c(3), x2=c(0)), out)
})

test_that("toRowPairData matrix 3 row", {
  train_data <- cbind(y=c(9,4,4), x1=c(3,0,0), x2=c(1,1,1))
  out <- toRowPairData(train_data, 1, c(2,3), rowDiff)
  # Row 1 - row 2.
  expect_equal(cbind(y=c(1), x1=c(3), x2=c(0)), oneRow(out, 1))
  # Row 1 - row 3.
  expect_equal(cbind(y=c(1), x1=c(3), x2=c(0)), oneRow(out, 2))
  # Row 2 - row 3.
  expect_equal(cbind(y=c(0.5), x1=c(0), x2=c(0)), oneRow(out, 3))
  expect_equal(3, nrow(out))
})

test_that("allPairData matrix", {
  train_data <- cbind(y=c(9,4), x1=c(3,0), x2=c(1,1))
  out <- allPairData(train_data, 1, c(2,3), rowDiff)
  # The criterion is scaled to 0-1.
  expect_equal(cbind(y=c(1), x1=c(3), x2=c(0)), oneRow(out, 1))
  expect_equal(cbind(y=c(0), x1=c(-3), x2=c(0)), oneRow(out, 2))
  expect_equal(2, nrow(out))
})



