context("heuristic_classes")

# require('testthat')

### Take The Best (ttbBinModel) ###

test_that("ttbBinModel 2x2 pos", {
  model <- ttbBinModel(matrix(c(5,4,1,0), 2, 2), 1, c(2))
  expect_equal(c(1),  model$cue_validities) 
  expect_equal(1,  coef(model)[[1]]) 
  expect_equal(1, length(coef(model))) 
})

test_that("ttbBinModel 2x2 neg", {
  model <- ttbBinModel(matrix(c(5,4,0,1), 2, 2), 1, c(2))
  expect_equal(c(0),  model$cue_validities) 
  expect_equal(1,  coef(model)[[1]])  
  expect_equal(1, length(coef(model))) 
})

test_that("ttbBinModel 2x3 pos neg", {
  m <- matrix(c(5,4,1,0,0,1), 2, 3)
  model <- ttbBinModel(m, 1, c(2,3))
  expect_equal(c(1,0),  model$cue_validities) 
  expect_equal(2,  coef(model)[[1]])  
  expect_equal(1,  coef(model)[[2]])  
  expect_equal(2, length(coef(model))) 
})

test_that("ttbBinModel 2x3 pos neg data.frame", {
  df <- data.frame(criterion=c(5,4), cue1=c(1,0), cue2=c(0,1))
  model <- ttbBinModel(df, 1, c(2,3))
  expect_equal(c(cue1=1, cue2=0), model$cue_validities)
  # Below I test the inner implementation.  Maybe I shouldn't.
  expect_equal(2,  coef(model)[["cue1"]])
  expect_equal(1,  coef(model)[["cue2"]])
  expect_equal(2, length(coef(model))) 
})

test_that("ttbBinModel 2x3 neg pos", {
  m <- matrix(c(5,4,0,1,1,0), 2, 3)
  model <- ttbBinModel(m, 1, c(2,3))
  expect_equal(c(0,1),  model$cue_validities) 
  expect_equal(1,  coef(model)[[1]])  
  expect_equal(2,  coef(model)[[2]])  
  expect_equal(2, length(coef(model))) 
})

test_that("ttbBinModel 2x3 pos neg (col 3 not fit)", {
  model <- ttbBinModel(matrix(c(5,4,1,0,0,1), 2, 3), 1, c(2))
  expect_equal(c(1),  model$cue_validities) 
  expect_equal(1,  coef(model)[[1]])  
  expect_equal(1, length(coef(model))) 
})

test_that("ttbBinModel 2x3 pos neg (col 2 not fit)", {
  model <- ttbBinModel(matrix(c(5,4,1,0,0,1), 2, 3), 1, c(3))
  expect_equal(c(0),  model$cue_validities) 
  expect_equal(1,  coef(model)[[1]])  
  expect_equal(1, length(coef(model))) 
})

test_that("ttbBinModel 3x3 pos neg", {
  model <- ttbBinModel(matrix(c(5,4,3,1,0,0,0,0,1), 3, 3), 1, c(2,3))
  expect_equal(c(1,0),  model$cue_validities) 
  expect_equal(2,  coef(model)[[1]])  
  expect_equal(1,  coef(model)[[2]]) 
  expect_equal(2, length(coef(model))) 
})

test_that("ttbBinModel 3x3 pos mixed", {
  model <- ttbBinModel(matrix(c(5,4,3,1,0,0,1,0,1), 3, 3), 1, c(2,3))
  expect_equal(c(1,0.5),  model$cue_validities) 
  expect_equal(2,  coef(model)[[1]])  
  expect_equal(1,  coef(model)[[2]]) 
  expect_equal(2, length(coef(model)))  
})

test_that("ttbBinModel 4x3 reverses cue rank", {
  model <- ttbBinModel(matrix(c(5,4,3,1,0,0,1,0,1,1,1,0), 3, 4), 1, c(2:4))
  expect_equal(c(1,0.5,1), model$cue_validities)
  # The 1st and 3rd cue are tied, so only check the 2nd cue rank, 
  # which should be last (3rd).
  expect_equal(3, model$cue_ranks[2])
})

test_that("ttbBinModel 3x3 names shifted criterion", {
  df <- data.frame(matrix(c(99, 99, 99, 5,4,3,1,0,1), 3, 3))
  names(df) <- c('Garbage', 'Criterion', 'Cue')
  model <- ttbBinModel(df, 2, c(3))
  expect_equal(c(0.5), unname(model$cue_validities))
  expect_equal(c('Cue'), names(model$cue_validities))
})

# predictAlternative

# getPrediction makes test code below more readable.
getPrediction <- function(out, row1=NULL, row2=NULL) {
  if (is.null(row1) || is.null(row2)) {
    stop("You must set both row1 and row2")
  }
  lastCol <- ncol(out)
  return(out[(out$Row1==row1) & (out$Row2==row2),][[lastCol]])
}

test_that("ttbBinModel 2x3 predictAlternative forward", {
  model <- ttbBinModel(matrix(c(5,4,1,0,0,1), 2, 3), 1, c(2,3))
  expect_equal(c(1,0), model$cue_validities)
  out <- predictAlternative(model, matrix(c(5,4,1,0,0,1), 2, 3))
  expect_equal(1, getPrediction(out, row1=1, row2=2))
  expect_equal(0, getPrediction(out, row1=2, row2=1))
  # No other rows.
  expect_equal(2, nrow(out))
})

test_that("ttbBinModel 2x3 predictAlternative backward cues", {
  model <- ttbBinModel(matrix(c(5,4,1,0,0,1), 2, 3), 1, c(2,3))
  expect_equal(c(1,0), model$cue_validities)
  # Cues in test_data below have been reversed.
  out <- predictAlternative(model, matrix(c(5,4,0,1,1,0), 2, 3))
  expect_equal(0, getPrediction(out, row1=1, row2=2))
  expect_equal(1, getPrediction(out, row1=2, row2=1))
  # No other rows.
  expect_equal(2, nrow(out))
})

test_that("ttbBinModel 2x3 predictAlternative forward rowPairs", {
  model <- ttbBinModel(matrix(c(5,4,1,0,0,1), 2, 3), 1, c(2,3))
  expect_equal(c(1,0), model$cue_validities)
  rowPairs <- data.frame(Row1=c(1), Row2=c(2))
  out <- predictAlternative(model, matrix(c(5,4,1,0,0,1), 2, 3),
                            rowPairs=rowPairs)
  expect_equal(1, getPrediction(out, row1=1, row2=2))
  # No other rows.
  expect_equal(1, nrow(out))
})

test_that("ttbBinModel 2x3 predictAlternative backward rowPairs", {
  model <- ttbBinModel(matrix(c(5,4,1,0,0,1), 2, 3), 1, c(2,3))
  expect_equal(c(1,0), model$cue_validities)
  # The rowPairs below are reversed relative to those above.
  rowPairs <- data.frame(Row1=c(2), Row2=c(1))
  out <- predictAlternative(model, matrix(c(5,4,1,0,0,1), 2, 3),
                            rowPairs=rowPairs)
  expect_equal(0, getPrediction(out, row1=2, row2=1))
  # No other rows.
  expect_equal(1, nrow(out))
})

#TODO(jean): Test invalid rowPairs.

test_that("ttbBinModel 3x3 pos pos predictAlternative forward", {
  model <- ttbBinModel(matrix(c(5,4,3,1,0,0,1,1,0), 3, 3), 1, c(2,3))
  expect_equal(c(1,1),  model$cue_validities)

  # All cues same as in training data.
  out <- predictAlternative(model, matrix(c(5,4,3,1,0,0,1,1,0), 3, 3))
  expect_equal(1, getPrediction(out, row1=1, row2=2))
  expect_equal(0, getPrediction(out, row1=2, row2=1))

  expect_equal(1, getPrediction(out, row1=1, row2=3))
  expect_equal(0, getPrediction(out, row1=3, row2=1))

  expect_equal(1, getPrediction(out, row1=2, row2=3))
  expect_equal(0, getPrediction(out, row1=3, row2=2))
  # No other rows.
  expect_equal(6, nrow(out))
})

test_that("ttbBinModel 3x3 pos pos predictAlternative backward cues", {
  model <- ttbBinModel(matrix(c(5,4,3,1,0,0,1,1,0), 3, 3), 1, c(2,3))
  expect_equal(c(1,1),  model$cue_validities)

  # All cues backwards relative to training data.
  out <- predictAlternative(model, matrix(c(5,4,3,0,1,1,0,0,1), 3, 3))
  expect_equal(0, getPrediction(out, row1=1, row2=2))
  expect_equal(1, getPrediction(out, row1=2, row2=1))

  expect_equal(0, getPrediction(out, row1=1, row2=3))
  expect_equal(1, getPrediction(out, row1=3, row2=1))

  expect_equal(0, getPrediction(out, row1=2, row2=3))
  expect_equal(1, getPrediction(out, row1=3, row2=2))
  # No other rows.
  expect_equal(6, nrow(out))
})

test_that("ttbBinModel 2x2,3x2 predictAlternative", {
  model <- ttbBinModel(matrix(c(5,4,3,1,0,0), 2, 3), 1, c(2))
  expect_equal(c(1), model$cue_validities)
  out <- predictAlternative(model, matrix(c(5,4,3,1,0,0), 3, 2))
  expect_equal(1, getPrediction(out, row1=1, row2=2))
  expect_equal(1, getPrediction(out, row1=1, row2=3))
  expect_equal(0.5, getPrediction(out, row1=2, row2=3), tolerance=0.0001)
})

# Most testing of predict is with predictWithWeights, so here I am
# just making sure it is correctly wired into the ttbBinModel.
# ttb only guarantees the ordering of its predictions, not values, 
# so only the ordering is tested.
test_that("ttbBinModel 3x3 pos pos predict", {
  model <- ttbBinModel(matrix(c(5,4,3,1,0,0,1,1,0), 3, 3), 1, c(2,3))
  expect_equal(c(1,1),  model$cue_validities) 
  good <- predict(model, matrix(c(5,4,3,1,0,0,1,1,0), 3, 3))
  expect_more_than(good[1,1], good[2,1])
  expect_more_than(good[2,1], good[3,1])
  expect_equal(3, length(good))
  bad <- predict(model, matrix(c(5,4,3,0,1,1,0,0,1), 3, 3))
  expect_less_than(bad[1,1], bad[2,1])
  expect_less_than(bad[2,1], bad[3,1])
  expect_equal(3, length(bad))
})

test_that("ttbBinModel 3x3 predict without test_data", {
  model <- ttbBinModel(matrix(c(5,4,3,1,0,0,1,1,0), 3, 3), 1, c(2,3))
  expect_equal(c(1,1),  model$cue_validities) 
  good <- predict(model)
  expect_more_than(good[1,1], good[2,1])
  expect_more_than(good[2,1], good[3,1])
  expect_equal(3, length(good))
  expect_equal(model$fit_predictions, good)
})



### dawesModel ###

test_that("dawesModel 2x3 pos neg", {
  model <- dawesModel(matrix(c(5,4,1,0,0,1), 2, 3), 1, c(2,3))
  expect_equal(c(1,0),  model$cue_validities) 
  expect_equal(1,  coef(model)[[1]])  
  expect_equal(-1,  coef(model)[[2]])  
  expect_equal(2, length(coef(model))) 
})

test_that("dawesModel 5x1 75", {
  model <- dawesModel(matrix(c(5,4,3,2,1,1,1,1,0,1), 5, 2), 1, c(2))
  expect_equal(c(0.75),  model$cue_validities) 
  expect_equal(1,  coef(model)[[1]])
  expect_equal(1, length(coef(model))) 
})

test_that("dawesModel 5x1 25", {
  model <- dawesModel(matrix(c(5,4,3,2,1,1,0,1,1,1), 5, 2), 1, c(2))
  expect_equal(c(0.25),  model$cue_validities) 
  expect_equal(-1,  coef(model)[[1]])
  expect_equal(1, length(coef(model))) 
})

test_that("dawesModel 3x3 pos pos predict", {
  model <- dawesModel(matrix(c(5,4,3,1,0,0,1,1,0), 3, 3), 1, c(2,3))
  expect_equal(c(1,1),  coef(model)) 
  good <- predict(model, matrix(c(5,4,3,1,0,0,1,1,0), 3, 3))
  expect_equal(matrix(c(2,1,0), 3, 1), good)
  bad <- predict(model, matrix(c(5,4,3,0,1,1,0,0,1), 3, 3))
  expect_equal(matrix(c(0,1,2), 3, 1), bad)
})


### franklinModel ###

test_that("franklinModel 2x3 pos neg", {
  model <- franklinModel(matrix(c(5,4,1,0,0,1), 2, 3), 1, c(2,3))
  expect_equal(c(1,0),  model$cue_validities) 
  expect_equal(1,  coef(model)[[1]])  
  expect_equal(0,  coef(model)[[2]])  
  expect_equal(2, length(coef(model))) 
})

test_that("franklinModel 5x1 75", {
  model <- franklinModel(matrix(c(5,4,3,2,1,1,1,1,0,1), 5, 2), 1, c(2))
  expect_equal(c(0.75),  model$cue_validities) 
  expect_equal(0.75,  coef(model)[[1]])
  expect_equal(1, length(coef(model))) 
})

test_that("franklinModel 5x1 25", {
  model <- franklinModel(matrix(c(5,4,3,2,1,1,0,1,1,1), 5, 2), 1, c(2))
  expect_equal(c(0.25),  model$cue_validities) 
  expect_equal(0.25,  coef(model)[[1]])
  expect_equal(1, length(coef(model))) 
})

test_that("franklinModel 3x3 pos pos predict", {
  model <- franklinModel(matrix(c(5,4,3,1,0,0,1,1,0), 3, 3), 1, c(2,3))
  expect_equal(c(1,1),  coef(model)) 
  good <- predict(model, matrix(c(5,4,3,1,0,0,1,1,0), 3, 3))
  expect_equal(matrix(c(2,1,0), 3, 1), good)
  bad <- predict(model, matrix(c(5,4,3,0,1,1,0,0,1), 3, 3))
  expect_equal(matrix(c(0,1,2), 3, 1), bad)
})


### regModel ###

test_that("regModel 2x2 fit pos slope", {
  model <- regModel(matrix(c(5,4,1,0), 2, 2), 1, c(2))
  expect_equal(4,  coef(model)[[1]])  # intercept
  expect_equal(1,  coef(model)[[2]])  # slope
  expect_equal(2, length(coef(model))) 
})

test_that("regModel 2x2 fit neg slope", {
  model <- regModel(matrix(c(5,4,0,1), 2, 2), 1, c(2))
  expect_equal(5,  coef(model)[[1]])  # intercept
  expect_equal(-1,  coef(model)[[2]])  # slope
})

test_that("regModel 2x2 fit pos slope -- data.frame", {
  model <- regModel(as.data.frame( matrix(c(5,4,1,0), 2, 2)), 1, c(2))
  expect_equal(4,  coef(model)[[1]])  # intercept
  expect_equal(1,  coef(model)[[2]])  # slope
  expect_equal(2, length(coef(model))) 
})

test_that("lmWrapper 2x2 fit pos slope -- no intercept", {
  model <- lmWrapper(matrix(c(5,4,1,0), 2, 2), 1, c(2),  include_intercept=FALSE)
  expect_equal(5,  coef(model)[[1]])  # slope
  expect_equal(1, length(coef(model))) 
})

test_that("regModel 2x3 fit 4.5,1,NA", {
  model <- regModel(matrix(c(5,4,1,0,0,1), 2, 3), 1, c(2,3))
  expect_equal(4,  coef(model)[[1]])  # intercept
  expect_equal(1,  coef(model)[[2]])  # x1
  expect_true( is.na(coef(model)[[3]]) )  # x2 excluded because too many columns
})

test_that("regModel 2x3 fit 4,1 (col 3 not fit)", {
  model <- regModel(matrix(c(5,4,1,0,0,1), 2, 3), 1, c(2))
  expect_equal(4,  coef(model)[[1]])  # intercept
  expect_equal(1,  coef(model)[[2]])  # x1
  expect_equal(2, length(coef(model))) 
})

test_that("regModel 2x3 fit 5,-1 (col 2 not fit)", {
  model <- regModel(matrix(c(5,4,1,0,0,1), 2, 3), 1, c(3))
  expect_equal(5,  coef(model)[[1]])  # intercept
  expect_equal(-1,  coef(model)[[2]])  # x1
  expect_equal(2, length(coef(model))) 
})

test_that("regModel 3x3 fit positive negative", {
  model <- regModel(matrix(c(5,4,3,1,0,0,0,0,1), 3, 3), 1, c(2,3))
  expect_equal(4,  coef(model)[[1]])  # intercept
  expect_equal(1,  coef(model)[[2]])  # V2
  expect_equal(-1,  coef(model)[[3]])  # V3
  expect_equal(3, length(coef(model))) 
})

test_that("regModel 3x3 fit positive mixed", {
  model <- regModel(matrix(c(5,4,3,1,0,0,1,0,1), 3, 3), 1, c(2,3))
  expect_equal(4,  coef(model)[[1]])  # intercept
  expect_equal(2,  coef(model)[[2]])  # V2
  expect_equal(-1, coef(model)[[3]])  # V3
  expect_equal(3, length(coef(model)))  
})


### regNoIModel ###

### logRegModel ###

test_that("logRegModel predictWithWeightsLog 2x2 fit train_data", {
  train_data <- matrix(c(5,4,1,0), 2, 2)
  model <- logRegModel(train_data, 1, c(2))
  out <- predictWithWeightsLog(train_data, logr$cols_to_fit, 1, logr$linear_coef)
  expect_equal(1, getPrediction(out, row1=1, row2=2))
  expect_equal(0, getPrediction(out, row1=2, row2=1))
  expect_equal(2, nrow(out)) # No other rows.
})

test_that("logRegModel predictWithWeightsLog 2x2,2x3 all correct", {
  train_data <- matrix(c(5,4,1,0), 2, 2)
  model <- logRegModel(train_data, 1, c(2))
  test_data <- matrix(c(5,4,3,1,0,0), 3, 2)
  out <- predictWithWeightsLog(test_data, logr$cols_to_fit, 1, logr$linear_coef)
  expect_equal(1, getPrediction(out, row1=1, row2=2))
  expect_equal(0, getPrediction(out, row1=2, row2=1))
  expect_equal(1, getPrediction(out, row1=1, row2=3))
  expect_equal(0, getPrediction(out, row1=3, row2=1))
  expect_equal(0.5, getPrediction(out, row1=2, row2=3), tolerance=0.0001)
  expect_equal(0.5, getPrediction(out, row1=3, row2=2), tolerance=0.0001)
  expect_equal(6, nrow(out)) # No other rows.
})

