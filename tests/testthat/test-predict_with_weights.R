context("predict_with_weights")


test_that("rowPairGenerator 2", {
  row_pairs <- rowPairGenerator(2)
  expect_equal(2, row_pairs[(row_pairs$Row1==1),]$Row2)
  expect_equal(1, row_pairs[(row_pairs$Row1==2),]$Row2)
  expect_equal(2, nrow(row_pairs))
})

test_that("rowPairGenerator 3", {
  row_pairs <- rowPairGenerator(3)
  expect_equal(c(2,3), row_pairs[(row_pairs$Row1==1),]$Row2)
  expect_equal(c(1,3), row_pairs[(row_pairs$Row1==2),]$Row2)
  expect_equal(c(1,2), row_pairs[(row_pairs$Row1==3),]$Row2)
  expect_equal(6, nrow(row_pairs))
})

expect_equal(matrix(c(2,2), 2, 1), predictWithWeights(matrix(c(1,1,1,1), 2, 2), c(1:2), c(1,1)))
expect_equal(matrix(c(1,1), 2, 1), predictWithWeights(matrix(c(1,1,1,1), 2, 2), c(1:2), c(0,1)))
expect_equal(matrix(c(4,2), 2, 1), predictWithWeights(matrix(c(2,1,2,1), 2, 2), c(1:2), c(1,1)))

expect_equal(matrix(c(2,1), 2, 1), predictWithWeights(matrix(c(2,1,2,1), 2, 2), c(1:2), c(1,NA)))
expect_equal(c(4,2), predictWithWeights(matrix(c(2,1), 2, 1), c(1:1), c(2)))

test_that("predictWithWeights with intercept", {
  named_vec_weights <- c(100,1,1)
  names(named_vec_weights) <- c("(Intercept)", "V1", "V2")
  expect_equal(matrix(c(102,102), 2, 1), predictWithWeights(matrix(c(1,1,1,1), 2, 2), c(1:2), named_vec_weights))  
})

test_that("predictWithWeights data.frame with intercept", {
  named_vec_weights <- c(100,1,1)
  names(named_vec_weights) <- c("(Intercept)", "V1", "V2")
  expect_equal(matrix(c(102,102), 2, 1), 
      predictWithWeights(as.data.frame(matrix(c(1,1,1,1), 2, 2)), c(1:2), named_vec_weights))  
})

test_that("predictWithWeights with intercept return list", {
  named_vec_weights <- c(100,1)
  names(named_vec_weights) <- c("(Intercept)", "V1")
  expect_equal(c(101, 101), predictWithWeights(matrix(c(1,1), 2, 1), c(1:1), named_vec_weights))  
})

test_that("predictWithWeights with intercept and NA", {
  named_vec_weights <- c(100,1,NA)
  names(named_vec_weights) <- c("(Intercept)", "V1", "V2")
  expect_equal(matrix(c(101,101), 2, 1), predictWithWeights(matrix(c(1,1,1,1), 2, 2), c(1:2), named_vec_weights))  
})

