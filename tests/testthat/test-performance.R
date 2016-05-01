context("performance")

# require('testthat')

test_that("confusionMatrix match all factors in data", {
  data1 <- c(0,1,1)
  data2 <- c(0,1,1)
  out <- confusionMatrix(data1, data2, c(0,1))
  expected <- cbind(c(1,0), c(0,2))
  expect_equal(expected, matrix(out, 2, 2))
  expect_equal(c("0", "1"), dimnames(out)[[1]])
  expect_equal(c("0", "1"), dimnames(out)[[2]])
})

test_that("confusionMatrix non-match all factors in data", {
  data1 <- c(0,0,1,1)
  data2 <- c(0,1,0,1)
  out <- confusionMatrix(data1, data2, c(0,1))
  expected <- cbind(c(1,1), c(1,1))
  expect_equal(expected, matrix(out, 2, 2))
  expect_equal(c("0", "1"), dimnames(out)[[1]])
  expect_equal(c("0", "1"), dimnames(out)[[2]])
})

test_that("confusionMatrix non-match missing -1 factor in data", {
  data1 <- c(0,0,1,1)
  data2 <- c(0,1,0,1)
  out <- confusionMatrix(data1, data2, c(-1,0,1))
  expected <- cbind(c(0,0,0), c(0,1,1), c(0,1,1))
  expect_equal(expected, matrix(out, 3, 3))
  expect_equal(c("-1", "0", "1"), dimnames(out)[[1]])
  expect_equal(c("-1", "0", "1"), dimnames(out)[[2]])
})

test_that("confusionMatrix non-match missing 2 factor in data", {
  data1 <- c(0,0,1,1)
  data2 <- c(0,1,0,1)
  out <- confusionMatrix(data1, data2, c(0,1,2))
  expected <- cbind(c(1,1,0), c(1,1,0), c(0,0,0))
  expect_equal(expected, matrix(out, 3, 3))
  expect_equal(c("0", "1", "2"), dimnames(out)[[1]])
  expect_equal(c("0", "1", "2"), dimnames(out)[[2]])
})

test_that("confusionMatrix missing 1 factor in data1", {
  data1 <- c(0,0,0)
  data2 <- c(0,1,1)
  out <- confusionMatrix(data1, data2, c(0,1))
  expected <- cbind(c(1,0), c(2,0))
  expect_equal(expected, matrix(out, 2, 2))
  expect_equal(c("0", "1"), dimnames(out)[[1]])
  expect_equal(c("0", "1"), dimnames(out)[[2]])
})

test_that("confusionMatrix missing 1 factor in data2", {
  data1 <- c(0,1,1)
  data2 <- c(0,0,0)
  out <- confusionMatrix(data1, data2, c(0,1))
  expected <- cbind(c(1,2), c(0,0))
  expect_equal(expected, matrix(out, 2, 2))
  expect_equal(c("0", "1"), dimnames(out)[[1]])
  expect_equal(c("0", "1"), dimnames(out)[[2]])
})

test_that("confusionMatrixPairPredict missing -1 factor in data", {
  out <- confusionMatrixPairPredict(c(1,1,1), c(1,0,0))
  expected <- cbind(c(0,0,0), c(0,0,2), c(0,0,1))
  expect_equal(expected, matrix(out, 3, 3))
  expect_equal(c("-1", "0", "1"), dimnames(out)[[1]])
  expect_equal(c("-1", "0", "1"), dimnames(out)[[2]])
})

