context("heuristic_classes")

# require('testthat')

test_that("ttbModel 2x2 pos", {
  model <- ttbModel(matrix(c(5,4,1,0), 2, 2), 1, c(2))
  expect_equal(c(1),  model$cue_validities) 
  expect_equal(1,  coef(model)[[1]]) 
  expect_equal(1, length(coef(model))) 
})

test_that("ttbModel 2x2 neg", {
  model <- ttbModel(matrix(c(5,4,0,1), 2, 2), 1, c(2))
  expect_equal(c(0),  model$cue_validities) 
  expect_equal(1,  coef(model)[[1]])  
  expect_equal(1, length(coef(model))) 
})

test_that("ttbModel 2x3 pos neg", {
  model <- ttbModel(matrix(c(5,4,1,0,0,1), 2, 3), 1, c(2,3))
  expect_equal(c(1,0),  model$cue_validities) 
  expect_equal(2,  coef(model)[[1]])  
  expect_equal(1,  coef(model)[[2]])  
  expect_equal(2, length(coef(model))) 
})

test_that("ttbModel 2x3 neg pos", {
  model <- ttbModel(matrix(c(5,4,0,1,1,0), 2, 3), 1, c(2,3))
  expect_equal(c(0,1),  model$cue_validities) 
  expect_equal(1,  coef(model)[[1]])  
  expect_equal(2,  coef(model)[[2]])  
  expect_equal(2, length(coef(model))) 
})

test_that("ttbModel 2x3 pos neg (col 3 not fit)", {
  model <- ttbModel(matrix(c(5,4,1,0,0,1), 2, 3), 1, c(2))
  expect_equal(c(1),  model$cue_validities) 
  expect_equal(1,  coef(model)[[1]])  
  expect_equal(1, length(coef(model))) 
})

test_that("ttbModel 2x3 pos neg (col 2 not fit)", {
  model <- ttbModel(matrix(c(5,4,1,0,0,1), 2, 3), 1, c(3))
  expect_equal(c(0),  model$cue_validities) 
  expect_equal(1,  coef(model)[[1]])  
  expect_equal(1, length(coef(model))) 
})

test_that("ttbModel 3x3 pos neg", {
  model <- ttbModel(matrix(c(5,4,3,1,0,0,0,0,1), 3, 3), 1, c(2,3))
  expect_equal(c(1,0),  model$cue_validities) 
  expect_equal(2,  coef(model)[[1]])  
  expect_equal(1,  coef(model)[[2]]) 
  expect_equal(2, length(coef(model))) 
})

test_that("ttbModel 3x3 pos mixed", {
  model <- ttbModel(matrix(c(5,4,3,1,0,0,1,0,1), 3, 3), 1, c(2,3))
  expect_equal(c(1,0.5),  model$cue_validities) 
  expect_equal(2,  coef(model)[[1]])  
  expect_equal(1,  coef(model)[[2]]) 
  expect_equal(2, length(coef(model)))  
})

# Most testing of predict is with predictWithWeights, so here I am
# just making sure it is correctly wired into the ttbModel.
# ttb only guarantees the ordering of its predictions, not values, 
# so only the ordering is tested.
test_that("ttbModel 3x3 pos pos predict", {
  model <- ttbModel(matrix(c(5,4,3,1,0,0,1,1,0), 3, 3), 1, c(2,3))
  expect_equal(c(1,1),  model$cue_validities) 
  good <- predict(model, matrix(c(5,4,3,1,0,0,1,1,0), 3, 3))
  expect_more_than(good[1,1], good[2,1])
  expect_more_than(good[2,1], good[3,1])
  bad <- predict(model, matrix(c(5,4,3,0,1,1,0,0,1), 3, 3))
  expect_less_than(bad[1,1], bad[2,1])
  expect_less_than(bad[2,1], bad[3,1])
})
