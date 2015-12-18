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
  model <- ttbBinModel(matrix(c(5,4,0,1), 2, 2), 1, c(2), reverse_cues=FALSE)
  expect_equal(c(0),  model$cue_validities) 
  expect_equal(1,  coef(model)[[1]])  
  expect_equal(1, length(coef(model))) 
})

test_that("ttbBinModel 2x3 pos neg", {
  m <- matrix(c(5,4,1,0,0,1), 2, 3)
  model <- ttbBinModel(m, 1, c(2,3), reverse_cues=FALSE)
  expect_equal(c(1,0),  model$cue_validities) 
  expect_equal(2,  coef(model)[[1]])  
  expect_equal(1,  coef(model)[[2]])  
  expect_equal(2, length(coef(model))) 
})

test_that("ttbBinModel 2x3 pos neg data.frame", {
  df <- data.frame(criterion=c(5,4), cue1=c(1,0), cue2=c(0,1))
  model <- ttbBinModel(df, 1, c(2,3), reverse_cues=FALSE)
  expect_equal(c(cue1=1, cue2=0), model$cue_validities)
  # Below I test the inner implementation.  Maybe I shouldn't.
  expect_equal(2,  coef(model)[["cue1"]])
  expect_equal(1,  coef(model)[["cue2"]])
  expect_equal(2, length(coef(model))) 
})

test_that("ttbBinModel 2x3 neg pos", {
  m <- matrix(c(5,4,0,1,1,0), 2, 3)
  model <- ttbBinModel(m, 1, c(2,3), reverse_cues=FALSE)
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
  model <- ttbBinModel(matrix(c(5,4,1,0,0,1), 2, 3), 1, c(3), reverse_cues=FALSE)
  expect_equal(c(0),  model$cue_validities) 
  expect_equal(1,  coef(model)[[1]])  
  expect_equal(1, length(coef(model))) 
})

test_that("ttbBinModel 3x3 pos neg", {
  model <- ttbBinModel(matrix(c(5,4,3,1,0,0,0,0,1), 3, 3), 1, c(2,3), reverse_cues=FALSE)
  expect_equal(c(1,0),  model$cue_validities) 
  expect_equal(2,  coef(model)[[1]])  
  expect_equal(1,  coef(model)[[2]]) 
  expect_equal(2, length(coef(model))) 
})

test_that("ttbBinModel 3x3 pos mixed", {
  model <- ttbBinModel(matrix(c(5,4,3,1,0,0,1,0,1), 3, 3), 1, c(2,3), reverse_cues=FALSE)
  expect_equal(c(1,0.5),  model$cue_validities) 
  expect_equal(2,  coef(model)[[1]])  
  expect_equal(1,  coef(model)[[2]]) 
  expect_equal(2, length(coef(model)))  
})

test_that("ttbBinModel 3x3 names shifted criterion", {
  df <- data.frame(matrix(c(99, 99, 99, 5,4,3,1,0,1), 3, 3))
  names(df) <- c('Garbage', 'Criterion', 'Cue')
  model <- ttbBinModel(df, 2, c(3))
  expect_equal(c(0.5), unname(model$cue_validities))
  expect_equal(c('Cue'), names(model$cue_validities))
})

# predictAlternative

# getPredictionT makes test code below more readable.
getPredictionT <- function(out, row1=NULL, row2=NULL) {
  if (is.null(row1) || is.null(row2)) {
    stop("You must set both row1 and row2")
  }
  lastCol <- ncol(out)
  return(out[(out$Row1==row1) & (out$Row2==row2),][[lastCol]])
}

#TODO: Convert all tests to predictPair and use new getPrediction function.

# ttbBinModel
# TODO: Delete these when fully migrated to predictPair

test_that("ttbBinModel 2x3 predictAlternative forward", {
  train_matrix <- matrix(c(5,4,1,0,0,1), 2, 3)
  model <- ttbBinModel(train_matrix, 1, c(2,3))
  expect_equal(c(1,0), model$cue_validities)
  out <- predictAlternative(model, train_matrix)
  expect_equal(1, getPredictionT(out, row1=1, row2=2))
  expect_equal(0, getPredictionT(out, row1=2, row2=1))
})

test_that("ttbBinModel 2x3 predictPair backward cues", {
  train_matrix <- matrix(c(5,4,1,0,0,1), 2, 3)
  model <- ttbBinModel(train_matrix, 1, c(2,3))
  expect_equal(c(1,0), model$cue_validities)
  # Cues in test_data below have been reversed.
  out <- predictAlternative(model, matrix(c(5,4,0,1,1,0), 2, 3))
  # So predictions should be reversed.
  expect_equal(0, getPredictionT(out, row1=1, row2=2))
  expect_equal(1, getPredictionT(out, row1=2, row2=1))
  # No other rows.
  expect_equal(2, nrow(out))
})

test_that("ttbModel 2x3 predictPair forward", {
  train_matrix <- matrix(c(5,4,1,0,0,1), 2, 3)
  model <- ttbModel(train_matrix, 1, c(2,3))
  expect_equal(c(1,0), model$cue_validities)
  out <- predictPair(model, train_matrix)
  expect_equal(1, getPredictiono(out, row1=1, row2=2))
  expect_equal(0, getPredictiono(out, row1=2, row2=1))
  # No other rows.
  expect_equal(1, nrow(out$predictions))
  expect_equal(1, nrow(out$verbose_predictions))
})

test_that("ttbModel 2x3 predictPair backward cues", {
  train_matrix <- matrix(c(5,4,1,0,0,1), 2, 3)
  model <- ttbModel(train_matrix, 1, c(2,3))
  expect_equal(c(1,0), model$cue_validities)
  # Cues in test_data below have been reversed.
  out <- predictPair(model, matrix(c(5,4,0,1,1,0), 2, 3))
  # So predictions should be reversed.
  expect_equal(0, getPredictiono(out, row1=1, row2=2))
  expect_equal(1, getPredictiono(out, row1=2, row2=1))
  # No other rows.
  expect_equal(1, nrow(out$predictions))
  expect_equal(1, nrow(out$verbose_predictions))
})

test_that("ttbModel 2x2 predictPair cue_reversal", {
  train_matrix <- matrix(c(5,4,0,1), 2, 2)
  model <- ttbModel(train_matrix, 1, c(2))
  expect_equal(c(0), model$cue_validities)
  out <- predictPair(model, train_matrix)
  expect_equal(1, getPredictiono(out, row1=1, row2=2))
  expect_equal(0, getPredictiono(out, row1=2, row2=1))
  # No other rows.
  expect_equal(1, nrow(out$predictions))
})

test_that("ttbModel 3x3 predictPair cue_reversal", {
  train_matrix <- matrix(c(5,4,3,1,0,1,0,0,1), 3, 3)
  model <- ttbModel(train_matrix, 1, c(2,3))
  expect_equal(c(0.5, 0), model$cue_validities)
  out <- predictPair(model, train_matrix)
  # First cue discriminates but has 0.5 validity.
  # 2nd cue does not discriminate.
  # So it's a guess = 0.5.
  expect_equal(0.5, getPredictiono(out, row1=1, row2=2))
  # Reverse the 2nd cue, and it discriminates to get these right.
  expect_equal(1, getPredictiono(out, row1=1, row2=3))
  expect_equal(1, getPredictiono(out, row1=1, row2=3))
})

test_that("ttbBinModel 2x3 predictAlternative forward row_pairs", {
  model <- ttbBinModel(matrix(c(5,4,1,0,0,1), 2, 3), 1, c(2,3))
  expect_equal(c(1,0), model$cue_validities)
  row_pairs <- data.frame(Row1=c(1), Row2=c(2))
  out <- predictAlternative(model, matrix(c(5,4,1,0,0,1), 2, 3),
                            row_pairs=row_pairs)
  expect_equal(1, getPredictionT(out, row1=1, row2=2))
  # No other rows.
  expect_equal(1, nrow(out))
})

test_that("ttbModel 2x3 predictPair forward subset_rows", {
  model <- ttbModel(matrix(c(5,4,1,0,0,1), 2, 3), 1, c(2,3))
  expect_equal(c(1,0), model$cue_validities)
  out <- predictPair(model, matrix(c(5,4,1,0,0,1), 2, 3),
                     subset_rows=c(1,2))
  expect_equal(1, getPredictiono(out, row1=1, row2=2))
  expect_equal(0, getPredictiono(out, row1=2, row2=1))
  # No other rows.
  expect_equal(1, nrow(out$predictions))
})

test_that("ttbModel 2x3 predictPair backward subset_rows", {
  model <- ttbModel(matrix(c(5,4,1,0,0,1), 2, 3), 1, c(2,3))
  expect_equal(c(1,0), model$cue_validities)
  out <- predictPair(model, matrix(c(5,4,1,0,0,1), 2, 3),
                     subset_rows=c(2,1))
  expect_equal(1, getPredictiono(out, row1=1, row2=2))
  expect_equal(0, getPredictiono(out, row1=2, row2=1))
  # No other rows.
  expect_equal(1, nrow(out$predictions))
})

#TODO(jean): Test invalid row_pairs.

test_that("ttbBinModel 3x3 pos pos predictAlternative forward", {
  model <- ttbBinModel(matrix(c(5,4,3,1,0,0,1,1,0), 3, 3), 1, c(2,3))
  expect_equal(c(1,1),  model$cue_validities)

  # All cues same as in training data.
  out <- predictAlternative(model, matrix(c(5,4,3,1,0,0,1,1,0), 3, 3))
  expect_equal(1, getPredictionT(out, row1=1, row2=2))
  expect_equal(0, getPredictionT(out, row1=2, row2=1))

  expect_equal(1, getPredictionT(out, row1=1, row2=3))
  expect_equal(0, getPredictionT(out, row1=3, row2=1))

  expect_equal(1, getPredictionT(out, row1=2, row2=3))
  expect_equal(0, getPredictionT(out, row1=3, row2=2))
  # No other rows.
  expect_equal(6, nrow(out))
})

test_that("ttbModel 3x3 pos pos predictPair forward", {
  model <- ttbModel(matrix(c(5,4,3,1,0,0,1,1,0), 3, 3), 1, c(2,3))
  expect_equal(c(1,1),  model$cue_validities)

  # All cues same as in training data.
  out <- predictPair(model, matrix(c(5,4,3,1,0,0,1,1,0), 3, 3))
  expect_equal(1, getPredictiono(out, row1=1, row2=2))
  expect_equal(0, getPredictiono(out, row1=2, row2=1))

  expect_equal(1, getPredictiono(out, row1=1, row2=3))
  expect_equal(0, getPredictiono(out, row1=3, row2=1))

  expect_equal(1, getPredictiono(out, row1=2, row2=3))
  expect_equal(0, getPredictiono(out, row1=3, row2=2))
  # No other rows.
  expect_equal(3, nrow(out$predictions))
})

test_that("ttbModel 3x3 pos pos predictPair backward cues", {
  model <- ttbModel(matrix(c(5,4,3,1,0,0,1,1,0), 3, 3), 1, c(2,3))
  expect_equal(c(1,1),  model$cue_validities)

  # All cues backwards relative to training data.
  out <- predictPair(model, matrix(c(5,4,3,0,1,1,0,0,1), 3, 3))
  expect_equal(0, getPredictiono(out, row1=1, row2=2))
  expect_equal(1, getPredictiono(out, row1=2, row2=1))

  expect_equal(0, getPredictiono(out, row1=1, row2=3))
  expect_equal(1, getPredictiono(out, row1=3, row2=1))

  expect_equal(0, getPredictiono(out, row1=2, row2=3))
  expect_equal(1, getPredictiono(out, row1=3, row2=2))
  # No other rows.
  expect_equal(3, nrow(out$predictions))
})

test_that("ttbModel 2x2,3x2 predictPair", {
  model <- ttbModel(matrix(c(5,4,3,1,0,0), 2, 3), 1, c(2))
  expect_equal(c(1), model$cue_validities)
  out <- predictPair(model, matrix(c(5,4,3,1,0,0), 3, 2))
  expect_equal(1, getPredictiono(out, row1=1, row2=2))
  expect_equal(1, getPredictiono(out, row1=1, row2=3))
  expect_equal(0.5, getPredictiono(out, row1=2, row2=3), tolerance=0.0001)
})

test_that("ttbBinModel 4x4 predictAlternative first cue dominates", {
  train_data <- matrix(c(9,8,7,6,1,1,1,0,1,1,0,1,1,1,0,1), 4, 4)
  # How this data looks:
  # > train_data
  #       [,1] [,2] [,3] [,4]
  # [1,]    9    1    1    1
  # [2,]    8    1    1    1
  # [3,]    7    1    0    0
  # [4,]    6    0    1    1
  # Cue 1 has validity 1.0, cue 2 and cue 3 have validity 2/3.
  # Cue one predicts Row 3 > Row 4.
  # But if you sum cue weights, predict Row 4 > Row 3
  model <- ttbBinModel(train_data, 1, c(2:4))
  expect_equal(c(1, 0.667, 0.667), model$cue_validities, tolerance=0.002)
  out <- predictAlternative(model, train_data)
  expect_equal(1, getPredictionT(out, row1=3, row2=4))
  expect_equal(0, getPredictionT(out, row1=4, row2=3))
})

test_that("ttbModel 4x4 predictPair first cue dominates", {
  train_data <- matrix(c(9,8,7,6,1,1,1,0,1,1,0,1,1,1,0,1), 4, 4)
  # How this data looks:
  # > train_data
  #       [,1] [,2] [,3] [,4]
  # [1,]    9    1    1    1
  # [2,]    8    1    1    1
  # [3,]    7    1    0    0
  # [4,]    6    0    1    1
  # Cue 1 has validity 1.0, cue 2 and cue 3 have validity 2/3.
  # Cue one predicts Row 3 > Row 4.
  # But if you sum cue weights, predict Row 4 > Row 3
  model <- ttbModel(train_data, 1, c(2:4))
  expect_equal(c(1, 0.667, 0.667), model$cue_validities, tolerance=0.002)
  out <- predictPair(model, train_data)
  expect_equal(1, getPredictiono(out, row1=3, row2=4))
  expect_equal(0, getPredictiono(out, row1=4, row2=3))
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

### ttbModel ###

test_that("ttbModel 4x4 predictAlternative first cue dominates", {
  train_data <- matrix(c(9,8,7,6,1,1,1,0,1,1,0,1,1,1,0,1), 4, 4)
  # How this data looks:
  # > train_data
  #       [,1] [,2] [,3] [,4]
  # [1,]    9    1    1    1
  # [2,]    8    1    1    1
  # [3,]    7    1    0    0
  # [4,]    6    0    1    1
  # Cue 1 has validity 1.0, cue 2 and cue 3 have validity 2/3.
  # Cue one predicts Row 3 > Row 4.
  # But if you sum cue weights, predict Row 4 > Row 3
  model <- ttbModel(train_data, 1, c(2:4))
  expect_equal(c(1, 0.667, 0.667), model$cue_validities, tolerance=0.002)
  out <- predictAlternative(model, train_data)
  expect_equal(1, getPredictionT(out, row1=3, row2=4))
  expect_equal(0, getPredictionT(out, row1=4, row2=3))
})

test_that("ttbModel 4x4 predictPair first cue dominates", {
  train_data <- matrix(c(9,8,7,6,1,1,1,0,1,1,0,1,1,1,0,1), 4, 4)
  # How this data looks:
  # > train_data
  #       [,1] [,2] [,3] [,4]
  # [1,]    9    1    1    1
  # [2,]    8    1    1    1
  # [3,]    7    1    0    0
  # [4,]    6    0    1    1
  # Cue 1 has validity 1.0, cue 2 and cue 3 have validity 2/3.
  # Cue one predicts Row 3 > Row 4.
  # But if you sum cue weights, predict Row 4 > Row 3
  model <- ttbModel(train_data, 1, c(2:4))
  expect_equal(c(1, 0.667, 0.667), model$cue_validities, tolerance=0.002)
  out <- predictPair(model, train_data)
  expect_equal(1, getPredictiono(out, row1=3, row2=4))
  expect_equal(0, getPredictiono(out, row1=4, row2=3))
})

test_that("ttbModel 4x4 predictAlternative first cue dominates non-binary", {
  train_data <- matrix(c(9,8,7,6,0.1,0.1,0.1,0,1,1,0,1,1,1,0,1), 4, 4)
  # How this data looks:
  # > train_data
  #       [,1] [,2] [,3] [,4]
  # [1,]    9   0.1    1    1
  # [2,]    8   0.1    1    1
  # [3,]    7   0.1    0    0
  # [4,]    6     0    1    1
  # Cue 1 has validity 1.0, cue 2 and cue 3 have validity 2/3.
  # Cue one predicts Row 3 > Row 4.
  # But if you sum cue weights, predict Row 4 > Row 3
  model <- ttbModel(train_data, 1, c(2:4))
  expect_equal(c(1, 0.667, 0.667), model$cue_validities, tolerance=0.002)
  out <- predictAlternative(model, train_data)
  expect_equal(1, getPredictionT(out, row1=3, row2=4))
  expect_equal(0, getPredictionT(out, row1=4, row2=3))
})

test_that("ttbModel 4x4 predictPair first cue dominates non-binary", {
  train_data <- matrix(c(9,8,7,6,0.1,0.1,0.1,0,1,1,0,1,1,1,0,1), 4, 4)
  # How this data looks:
  # > train_data
  #       [,1] [,2] [,3] [,4]
  # [1,]    9   0.1    1    1
  # [2,]    8   0.1    1    1
  # [3,]    7   0.1    0    0
  # [4,]    6     0    1    1
  # Cue 1 has validity 1.0, cue 2 and cue 3 have validity 2/3.
  # Cue one predicts Row 3 > Row 4.
  # But if you sum cue weights, predict Row 4 > Row 3
  model <- ttbModel(train_data, 1, c(2:4))
  expect_equal(c(1, 0.667, 0.667), model$cue_validities, tolerance=0.002)
  out <- predictPair(model, train_data)
  expect_equal(1, getPredictiono(out, row1=3, row2=4))
  expect_equal(0, getPredictiono(out, row1=4, row2=3))
})

test_that("ttbModel 4x4 predictPair 3nd cue dominates non-binary", {
  train_data <- matrix(c(9,8,7,6,1,1,0,1,1,1,0,1,0.1,0.1,0.1,0), 4, 4)
  # How this data looks:
  # > train_data
  #      [,1] [,2] [,3] [,4]
  # [1,]    9    1    1  0.1
  # [2,]    8    1    1  0.1
  # [3,]    7    0    0  0.1
  # [4,]    6    1    1  0.0
  # Cue 1 and 2 have validity 2/3, cue 1 has validity 1.0.
  # Cue 3 predicts Row 3 > Row 4.
  # But if you sum cue weights, predict Row 4 > Row 3
  model <- ttbModel(train_data, 1, c(2:4))
  expect_equal(c(0.667, 0.667, 1), model$cue_validities, tolerance=0.002)
  out <- predictPair(model, train_data)
  expect_equal(1, getPredictiono(out, row1=3, row2=4))
  expect_equal(0, getPredictiono(out, row1=4, row2=3))
})

test_that("ttbModel 4x4 predictPair 3nd cue dominates non-binary reverse cue", {
  train_data <- matrix(c(9,8,7,6,1,1,0,1,1,1,0,1,0,0,0,0.1), 4, 4)
  # How this data looks:
  # > train_data
  #      [,1] [,2] [,3] [,4]
  # [1,]    9    1    1  0.0
  # [2,]    8    1    1  0.0
  # [3,]    7    0    0  0.0
  # [4,]    6    1    1  0.1
  # Column 1 is the criterion column.  Cues follow.
  # Cue 1 and 2 have validity 2/3, cue 3 has validity 0,
  # but that validity is 1.0 when reversed.
  # Cue 3 predicts Row 3 > Row 4.
  # But if you sum all cue weights, predict Row 4 > Row 3
  model <- ttbModel(train_data, 1, c(2:4))
  expect_equal(c(0.667, 0.667, 0), model$cue_validities, tolerance=0.002)
  out <- predictPair(model, train_data)
  expect_equal(1, getPredictiono(out, row1=3, row2=4))
  expect_equal(0, getPredictiono(out, row1=4, row2=3))
})

test_that("ttbModel 4x4 predictAlternative 3nd cue dominates non-binary reverse cue data.frame", {
  train_df <- data.frame(criterion=c(9,8,7,6), a=c(1,1,0,1), b=c(1,1,0,1),
                         c=c(0,0,0,0.1))
  # How this data looks:
  # > train_df
  #   criterion a b   c
  # 1         9 1 1 0.0
  # 2         8 1 1 0.0
  # 3         7 0 0 0.0
  # 4         6 1 1 0.1
  # Cue 1 and 2 have validity 2/3, cue 3 has validity 0,
  # but that validity is 1.0 when reversed.
  # Cue c predicts Row 3 > Row 4.
  # But if you sum all cue weights, predict Row 4 > Row 3
  model <- ttbModel(train_df, 1, c(2:4))
  expect_equal(c(a=0.667, b=0.667, c=0), model$cue_validities, tolerance=0.002)
  expect_equal(c(a=0.667, b=0.667, c=1), model$cue_validities_with_reverse, tolerance=0.002)
  out <- predictAlternative(model, train_df)
  expect_equal(1, getPredictionT(out, row1=3, row2=4))
  expect_equal(0, getPredictionT(out, row1=4, row2=3))
})

test_that("ttbModel 4x4 predictPair 3nd cue dominates non-binary reverse cue data.frame", {
  train_df <- data.frame(criterion=c(9,8,7,6), a=c(1,1,0,1), b=c(1,1,0,1),
                         c=c(0,0,0,0.1))
  # How this data looks:
  # > train_df
  #   criterion a b   c
  # 1         9 1 1 0.0
  # 2         8 1 1 0.0
  # 3         7 0 0 0.0
  # 4         6 1 1 0.1
  # Cue 1 and 2 have validity 2/3, cue 3 has validity 0,
  # but that validity is 1.0 when reversed.
  # Cue c predicts Row 3 > Row 4.
  # But if you sum all cue weights, predict Row 4 > Row 3
  model <- ttbModel(train_df, 1, c(2:4))
  expect_equal(c(a=0.667, b=0.667, c=0), model$cue_validities, tolerance=0.002)
  expect_equal(c(a=0.667, b=0.667, c=1), model$cue_validities_with_reverse, tolerance=0.002)
  # The coefficient for column c should be negative.
  expect_equal(c(c=-1), sign(model$linear_coef["c"]), tolerance=0.002)
  out <- predictPair(model, train_df)
  expect_equal(1, getPredictiono(out, row1=3, row2=4))
  expect_equal(0, getPredictiono(out, row1=4, row2=3))
})

test_that(paste("ttbModel 4x4 predictPair 3nd cue dominates reverse cue data.frame",
          "non-binary small diffs, big diffs, small diffs"), {
  train_df <- data.frame(criterion=c(9,8,7,6), a=c(1.1,1.1,1.0,1.1), b=c(10,10,-10,10),
                         c=c(0,0,0,0.1))
  # How this data looks:
  #   criterion   a   b   c
  # 1         9 1.1  10 0.0
  # 2         8 1.1  10 0.0
  # 3         7 1.0 -10 0.0
  # 4         6 1.1  10 0.1
  # Cue 1 and 2 have validity 2/3, cue 3 has validity 0,
  # but that validity is 1.0 when reversed.
  # Cue c predicts Row 3 > Row 4.
  # But if you sum all cue weights, predict Row 4 > Row 3
  model <- ttbModel(train_df, 1, c(2:4))
  expect_equal(c(a=0.667, b=0.667, c=0), model$cue_validities, tolerance=0.002)
  expect_equal(c(a=0.667, b=0.667, c=1), model$cue_validities_with_reverse, tolerance=0.002)
  # The coefficient for column c should be negative.
  expect_equal(c(c=-1), sign(model$linear_coef["c"]), tolerance=0.002)
  out <- predictPair(model, train_df)
  expect_equal(1, getPredictiono(out, row1=3, row2=4))
  expect_equal(0, getPredictiono(out, row1=4, row2=3))
})

test_that(paste("ttbModel 4x4 predictPair 3nd cue dominates cue data.frame",
                "non-binary big diffs, big diffs, big diffs"), {
  train_df <- data.frame(criterion=c(9,8,7,6), a=c(101,101,20,101), b=c(59,59,5,59),
                         c=c(90,90,90,10))
  # Cue a and b have validity 2/3, cue c has validity 0,
  # but that validity is 1.0 when reversed.
  # Cue c predicts Row 3 > Row 4.
  # But if you sum all cue weights, predict Row 4 > Row 3
  model <- ttbModel(train_df, 1, c(2:4))
  expect_equal(c(a=0.667, b=0.667, c=1), model$cue_validities, tolerance=0.002)
  expect_equal(c(a=0.667, b=0.667, c=1), model$cue_validities_with_reverse, tolerance=0.002)
  # The coefficient for column c should be negative.
  expect_equal(c(c=1), sign(model$linear_coef["c"]), tolerance=0.002)
  out <- predictPair(model, train_df)
  expect_equal(1, getPredictiono(out, row1=3, row2=4))
  expect_equal(0, getPredictiono(out, row1=4, row2=3))
})

test_that(paste("ttbModel 4x4 predictPair 3nd cue dominates cue data.frame",
                "non-binary big criteriondiffs, big diffs, big diffs, big diffs"), {
  train_df <- data.frame(criterion=c(900,400,100,6), a=c(101,101,20,101), b=c(59,59,5,59),
                         c=c(90,90,90,10))
  # Cue a and b have validity 2/3, cue c has validity 0,
  # but that validity is 1.0 when reversed.
  # Cue c predicts Row 3 > Row 4.
  # But if you sum all cue weights, predict Row 4 > Row 3
  model <- ttbModel(train_df, 1, c(2:4))
  expect_equal(c(a=0.667, b=0.667, c=1), model$cue_validities, tolerance=0.002)
  expect_equal(c(a=0.667, b=0.667, c=1), model$cue_validities_with_reverse, tolerance=0.002)
  # The coefficient for column c should be negative.
  expect_equal(c(c=1), sign(model$linear_coef["c"]), tolerance=0.002)
  out <- predictPair(model, train_df)
  expect_equal(1, getPredictiono(out, row1=3, row2=4))
  expect_equal(0, getPredictiono(out, row1=4, row2=3))
})

test_that(paste("ttbModel 4x4 predictPair 3nd cue dominates cue data.frame",
                "non-binary big criteriondiffs, big diffs, big diffs, big unique diffs"), {
  train_df <- data.frame(criterion=c(900,400,100,6), a=c(101,101,20,101), b=c(59,59,5,59),
                         c=c(90,80,70,10))
  # Cue a and b have validity 2/3, cue c has validity 0,
  # but that validity is 1.0 when reversed.
  # Cue c predicts Row 3 > Row 4.
  # But if you sum all cue weights, predict Row 4 > Row 3
  model <- ttbModel(train_df, 1, c(2:4))
  expect_equal(c(a=0.667, b=0.667, c=1), model$cue_validities, tolerance=0.002)
  expect_equal(c(a=0.667, b=0.667, c=1), model$cue_validities_with_reverse, tolerance=0.002)
  # The coefficient for column c should be negative.
  expect_equal(c(c=1), sign(model$linear_coef["c"]), tolerance=0.002)
  out <- predictPair(model, train_df)
  expect_equal(1, getPredictiono(out, row1=3, row2=4))
  expect_equal(0, getPredictiono(out, row1=4, row2=3))
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

test_that("dawesModel 4x4 predictPair 3nd cue dominates non-binary reverse cue", {
  train_data <- matrix(c(9,8,7,6,1,1,0,1,1,1,0,1,0,0,0,0.1), 4, 4)
  train_df <- as.data.frame(train_data)
  names(train_df) <- c("Y", "a", "b", "c")
  # How this data looks:
  # > train_df
  #   Y a b   c
  # 1 9 1 1 0.0
  # 2 8 1 1 0.0
  # 3 7 0 0 0.0
  # 4 6 1 1 0.1
  # Column Y is the criterion column.  Cues follow.
  # Cue a and b have validity 2/3, cue c has validity validity 0,
  # but that validity is 1.0 when reversed.
  # Cue c predicts Row 3 > Row 4.
  # But if you sum all cue weights, predict Row 4 > Row 3
  model <- dawesModel(train_df, 1, c(2:4))
  expect_equal(c(a=0.667, b=0.667, c=0), model$cue_validities, tolerance=0.002)
  #TODO(jean): Return cue reversal.
  #expect_equal(c(a=0.667, b=0.667, c=1), model$cue_validities_with_reverse, tolerance=0.002)
  expect_equal(c(a=1, b=1, c=-1), model$linear_coef, tolerance=0.002)
  out <- predictPair(model, train_data)
  expect_equal(0, getPredictiono(out, row1=3, row2=4))
  expect_equal(1, getPredictiono(out, row1=4, row2=3))
})

### franklinModel ###

test_that("franklinModel 2x3 pos neg reverse_cues FALSE", {
  train_matrix <- matrix(c(5,4,1,0,0,1), 2, 3)
  model <- franklinModel(train_matrix, 1, c(2,3), reverse_cues=FALSE)
  expect_equal(c(1,0),  model$cue_validities) 
  expect_equal(1,  coef(model)[[1]])  
  expect_equal(0,  coef(model)[[2]])  
  expect_equal(2, length(coef(model)))
  out1 <- predictAlternative(model, train_matrix)
  expect_equal(1, getPredictionT(out1, row1=1, row2=2), tolerance=0.002)
  out2 <- predictPair(model, train_matrix)
  expect_equal(1, getPredictiono(out2, row1=1, row2=2), tolerance=0.002)
})

test_that("franklinModel 2x3 pos neg", {
  train_matrix <- matrix(c(5,4,1,0,0,1), 2, 3)
  model <- franklinModel(train_matrix, 1, c(2,3))
  expect_equal(c(1,0),  model$cue_validities) 
  expect_equal(1,  coef(model)[[1]])  
  expect_equal(-1,  coef(model)[[2]])  
  expect_equal(2, length(coef(model)))
  out1 <- predictAlternative(model, train_matrix)
  expect_equal(1, getPredictionT(out1, row1=1, row2=2), tolerance=0.002)
  out2 <- predictPair(model, train_matrix)
  expect_equal(1, getPredictiono(out2, row1=1, row2=2), tolerance=0.002)
})

test_that("franklinModel 5x1 75", {
  train_matrix <- matrix(c(5,4,3,2,1,1,1,1,0,1), 5, 2)
  model <- franklinModel(train_matrix, 1, c(2))
  expect_equal(c(0.75),  model$cue_validities) 
  expect_equal(c(0.75),  coef(model))
  out <- predictPair(model, train_matrix)
  expect_equal(0.5, getPredictiono(out, row1=1, row2=2), tolerance=0.002)
  expect_equal(0.5, getPredictiono(out, row1=1, row2=3), tolerance=0.002)
  expect_equal(1, getPredictiono(out, row1=1, row2=4), tolerance=0.002)
  expect_equal(0.5, getPredictiono(out, row1=1, row2=5), tolerance=0.002)
  expect_equal(0, getPredictiono(out, row1=4, row2=5), tolerance=0.002)
})

test_that("franklinModel 5x1 25 reverse_cues FALSE", {
  train_matrix <- matrix(c(5,4,3,2,1,1,0,1,1,1), 5, 2)
  model <- franklinModel(train_matrix, 1, c(2), reverse_cues=FALSE)
  expect_equal(c(0.25),  model$cue_validities)
  # No cue reversal means cue_validities_with_reversal is NULL.
  expect_true(is.null(model$cue_validities_with_reversal))
  # No cue reversal means coefficients are same as cue validities.
  expect_equal(c(0.25),  coef(model))
  out <- predictPair(model, train_matrix)
  # Cue reversal will change below to 0.
  expect_equal(1, getPredictiono(out, row1=1, row2=2), tolerance=0.002)
  expect_equal(0.5, getPredictiono(out, row1=1, row2=3), tolerance=0.002)
  expect_equal(0.5, getPredictiono(out, row1=1, row2=4), tolerance=0.002)
  expect_equal(0.5, getPredictiono(out, row1=1, row2=5), tolerance=0.002)
  # Cue reversal will change below to 1.
  expect_equal(0, getPredictiono(out, row1=2, row2=3), tolerance=0.002)
})

test_that("franklinModel 5x1 25", {
  train_matrix <- matrix(c(5,4,3,2,1,1,0,1,1,1), 5, 2)
  # By default, reverse_cues is TRUE
  model <- franklinModel(train_matrix, 1, c(2))
  expect_equal(c(0.25),  model$cue_validities)
  # Cue reversal changes validity 0.25 to 0.75.
  expect_equal(c(0.75),  model$cue_validities_with_reverse)
  # Cue reversal changes coefficient from 0.75 to -0.75.
  expect_equal(c(-0.75),  coef(model))
  out <- predictPair(model, train_matrix)
  # Cue reversal will change below to 0.
  expect_equal(0, getPredictiono(out, row1=1, row2=2), tolerance=0.002)
  expect_equal(0.5, getPredictiono(out, row1=1, row2=3), tolerance=0.002)
  expect_equal(0.5, getPredictiono(out, row1=1, row2=4), tolerance=0.002)
  expect_equal(0.5, getPredictiono(out, row1=1, row2=5), tolerance=0.002)
  # Cue reversal will change below to 1.
  expect_equal(1, getPredictiono(out, row1=2, row2=3), tolerance=0.002)
})


test_that("franklinModel 3x3 pos pos predict", {
  model <- franklinModel(matrix(c(5,4,3,1,0,0,1,1,0), 3, 3), 1, c(2,3))
  expect_equal(c(1,1),  coef(model)) 
  good <- predict(model, matrix(c(5,4,3,1,0,0,1,1,0), 3, 3))
  expect_equal(matrix(c(2,1,0), 3, 1), good)
  bad <- predict(model, matrix(c(5,4,3,0,1,1,0,0,1), 3, 3))
  expect_equal(matrix(c(0,1,2), 3, 1), bad)
})

test_that("franklinModel 4x4 predictPair 3nd cue dominates non-binary reverse cue", {
  train_data <- matrix(c(9,8,7,6,1,1,0,1,1,1,0,1,0,0,0,0.1), 4, 4)
  train_df <- as.data.frame(train_data)
  names(train_df) <- c("Y", "a", "b", "c")
  # How this data looks:
  # > train_df
  #   Y a b   c
  # 1 9 1 1 0.0
  # 2 8 1 1 0.0
  # 3 7 0 0 0.0
  # 4 6 1 1 0.1
  # Column Y is the criterion column.  Cues follow.
  # Cue a and b have validity 2/3, cue c has validity validity 0,
  # but that validity is 1.0 when reversed.
  # Cue c predicts Row 3 > Row 4.
  # But if you sum all cue weights, predict Row 4 > Row 3
  model <- franklinModel(train_df, 1, c(2:4), reverse_cues=FALSE)
  expect_equal(c(a=0.667, b=0.667, c=0), model$cue_validities, tolerance=0.002)
  # Soon: Linear coef will include reversing the cue pointed the wrong way.
  expect_equal(c(a=0.667, b=0.667, c=0), model$linear_coef, tolerance=0.002)
  out <- predictPair(model, train_df)
  expect_equal(0, getPredictiono(out, row1=3, row2=4))
  expect_equal(1, getPredictiono(out, row1=4, row2=3))
})

test_that("franklinModel 4x4 predictAlternative 3nd cue dominates non-binary reverse cue", {
  train_data <- matrix(c(9,8,7,6,1,1,0,1,1,1,0,1,0,0,0,0.1), 4, 4)
  train_df <- as.data.frame(train_data)
  names(train_df) <- c("Y", "a", "b", "c")
  # How this data looks:
  # > train_df
  #   Y a b   c
  # 1 9 1 1 0.0
  # 2 8 1 1 0.0
  # 3 7 0 0 0.0
  # 4 6 1 1 0.1
  # Column Y is the criterion column.  Cues follow.
  # Cue a and b have validity 2/3, cue c has validity validity 0,
  # but that validity is 1.0 when reversed.
  # Cue c predicts Row 3 > Row 4.
  # But if you sum all cue weights, predict Row 4 > Row 3
  model <- franklinModel(train_df, 1, c(2:4), reverse_cues=FALSE)
  expect_equal(c(a=0.667, b=0.667, c=0), model$cue_validities, tolerance=0.002)
  # Soon: Linear coef will include reversing the cue pointed the wrong way.
  expect_equal(c(a=0.667, b=0.667, c=0), model$linear_coef, tolerance=0.002)
  out <- predictAlternative(model, train_df)
  expect_equal(0, getPredictionT(out, row1=3, row2=4))
  expect_equal(1, getPredictionT(out, row1=4, row2=3))
})

### regModel ###

test_that("regModel 2x2 fit pos slope", {
  train_matrix <- matrix(c(5,4,1,0), 2, 2)
  model <- regModel(train_matrix, 1, c(2))
  expect_equal(4,  coef(model)[[1]])  # intercept
  expect_equal(1,  coef(model)[[2]])  # slope
  expect_equal(2, length(coef(model)))
  out <- predictPair(model, train_matrix)
  expect_equal(1, getPredictiono(out, row1=1, row2=2), tolerance=0.002)
})

test_that("regModel 2x2 fit neg slope", {
  train_matrix <- matrix(c(5,4,0,1), 2, 2)
  model <- regModel(train_matrix, 1, c(2))
  expect_equal(5,  coef(model)[[1]])  # intercept
  expect_equal(-1,  coef(model)[[2]])  # slope
  out <- predictPair(model, train_matrix)
  expect_equal(1, getPredictiono(out, row1=1, row2=2), tolerance=0.002)
})

test_that("regModel 2x2 fit pos slope -- data.frame", {
  train_df <- as.data.frame( matrix(c(5,4,1,0), 2, 2))
  model <- regModel(train_df, 1, c(2))
  expect_equal(4,  coef(model)[[1]])  # intercept
  expect_equal(1,  coef(model)[[2]])  # slope
  expect_equal(2, length(coef(model))) 
  out <- predictPair(model, train_df)
  expect_equal(1, getPredictiono(out, row1=1, row2=2), tolerance=0.002)
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
  tol <- 0.0001
  train_data <- matrix(c(5,4,1,0), 2, 2)
  model <- logRegModel(train_data, 1, c(2))
  out <- predictAlternative(model, train_data)
  expect_equal(1, getPredictionT(out, row1=1, row2=2), tolerance=tol)
  expect_equal(0, getPredictionT(out, row1=2, row2=1), tolerance=tol)
  expect_equal(2, nrow(out)) # No other rows.
})

test_that("logRegModel predictPair 2x2 fit train_data", {
  tol <- 0.0001
  train_data <- matrix(c(5,4,1,0), 2, 2)
  model <- logRegModel(train_data, 1, c(2))
  out <- predictPair(model, train_data)
  expect_equal(1, getPredictiono(out, row1=1, row2=2), tolerance=tol)
  expect_equal(0, getPredictiono(out, row1=2, row2=1), tolerance=tol)
  # There is only one unique pair.
  expect_equal(1, nrow(out$verbose_predictions))
})

test_that("logRegModel predictWithWeightsLog 2x2 fit train_data reverse cue", {
  tol <- 0.0001
  train_data <- matrix(c(5,4,0,1), 2, 2)
  model <- logRegModel(train_data, 1, c(2))
  out <- predictAlternative(model, train_data)
  expect_equal(1, getPredictionT(out, row1=1, row2=2), tolerance=tol)
  expect_equal(0, getPredictionT(out, row1=2, row2=1), tolerance=tol)
  expect_equal(2, nrow(out)) # No other rows.
})

test_that("logRegModel predictPair 2x2 fit train_data reverse cue", {
  tol <- 0.0001
  train_data <- matrix(c(5,4,0,1), 2, 2)
  model <- logRegModel(train_data, 1, c(2))
  out <- predictPair(model, train_data)
  expect_equal(1, getPredictiono(out, row1=1, row2=2), tolerance=tol)
  expect_equal(0, getPredictiono(out, row1=2, row2=1), tolerance=tol)
  # There is only one unique pair.
  expect_equal(1, nrow(out$verbose_predictions))
})

test_that("logRegModel predictPair 2x2,3x2 all correct", {
  tol <- 0.0001
  train_data <- matrix(c(5,4,1,0), 2, 2)
  model <- logRegModel(train_data, 1, c(2))
  test_data <- matrix(c(5,4,3,1,0,0), 3, 2)
  out <- predictPair(model, test_data)
  expect_equal(1, getPredictiono(out, row1=1, row2=2), tolerance=tol)
  expect_equal(0, getPredictiono(out, row1=2, row2=1), tolerance=tol)
  expect_equal(1, getPredictiono(out, row1=1, row2=3), tolerance=tol)
  expect_equal(0, getPredictiono(out, row1=3, row2=1), tolerance=tol)
  # Row 2 and 3 have same cue values, so Row1 is equally likely to be greater.
  expect_equal(0.5, getPredictiono(out, row1=2, row2=3), tolerance=tol)
  expect_equal(0.5, getPredictiono(out, row1=3, row2=2), tolerance=tol)
  # There are three unique pairs.
  expect_equal(3, nrow(out$verbose_predictions))
})

test_that("logRegModel predictPair 2x2,3x2 all incorrect", {
  tol <- 0.0001
  train_data <- matrix(c(5,4,1,0), 2, 2)
  model <- logRegModel(train_data, 1, c(2))
  test_data <- matrix(c(5,4,3,0,1,1), 3, 2)
  out <- predictPair(model, test_data)
  expect_equal(0, getPredictiono(out, row1=1, row2=2), tolerance=tol)
  expect_equal(1, getPredictiono(out, row1=2, row2=1), tolerance=tol)
  expect_equal(0, getPredictiono(out, row1=1, row2=3), tolerance=tol)
  expect_equal(1, getPredictiono(out, row1=3, row2=1), tolerance=tol)
  expect_equal(0.5, getPredictiono(out, row1=2, row2=3), tolerance=tol)
  expect_equal(0.5, getPredictiono(out, row1=3, row2=2), tolerance=tol)
  # There are three unique pairs.
  expect_equal(3, nrow(out$verbose_predictions))
})

test_that("logRegModel predictPair 2x3 fit train_data", {
  tol <- 0.0001
  train_data <- matrix(c(5,4,1,0,1,0), 2, 3)
  model <- logRegModel(train_data, 1, c(2,3))
  out <- predictPair(model, train_data)
  expect_equal(1, getPredictiono(out, row1=1, row2=2), tolerance=tol)
  expect_equal(0, getPredictiono(out, row1=2, row2=1), tolerance=tol)
  # There is one unique pair.
  expect_equal(1, nrow(out$verbose_predictions))
})

test_that("logRegModel predictPairg 2x3 fit train_data 2nd cue useless", {
  tol <- 0.0001
  train_data <- matrix(c(5,4,1,0,1,1), 2, 3)
  model <- logRegModel(train_data, 1, c(2,3))
  out <- predictPair(model, train_data)
  expect_equal(1, getPredictiono(out, row1=1, row2=2), tolerance=tol)
  expect_equal(0, getPredictiono(out, row1=2, row2=1), tolerance=tol)
  # There is one unique pair.
  expect_equal(1, nrow(out$verbose_predictions))
})

test_that("logRegModel predictPair 2x3 fit train_data 2nd cue reverse", {
  tol <- 0.0001
  train_data <- matrix(c(5,4,1,0,0,1), 2, 3)
  model <- logRegModel(train_data, 1, c(2,3))
  out <- predictPair(model, train_data)
  expect_equal(1, getPredictiono(out, row1=1, row2=2), tolerance=tol)
  expect_equal(0, getPredictiono(out, row1=2, row2=1), tolerance=tol)
  # There is one unique pair.
  expect_equal(1, nrow(out$verbose_predictions))
})

test_that("logRegModel predictPair 2x3 fit train_data 1st cue useless", {
  tol <- 0.0001
  train_data <- matrix(c(5,4,0,0,1,0), 2, 3)
  model <- logRegModel(train_data, 1, c(2,3))
  out <- predictPair(model, train_data)
  expect_equal(1, getPredictiono(out, row1=1, row2=2), tolerance=tol)
  expect_equal(0, getPredictiono(out, row1=2, row2=1), tolerance=tol)
  # There is one unique pair.
  expect_equal(1, nrow(out$verbose_predictions))
})

test_that("logRegModel predictWithWeightsLog 3x2 fit train_data 1st cue useless", {
  tol <- 0.0001
  train_data <- cbind(c(5:1), c(1,1,1,0,1))
  model <- logRegModel(train_data, 1, c(2))
  fit_predictions <- predictWithWeightsLog(train_data,2,1,model$linear_coef)
  fit_accuracy <- logAccuracy(fit_predictions,train_data,1,2)
  expect_equal(0.6, fit_accuracy, tolerance=tol)
})

test_that("logRegModel error when train_data zero rows", {
  train_data <- data.frame(y=c(), x1=c(), x2=c())
  expect_error(logRegModel(train_data, 1, c(2,3)),
               "Training set must have at least 2 rows but had 0 rows",
               fixed=TRUE)
})

test_that("logRegModel error when train_data one row", {
  train_data <- data.frame(y=c(5), x1=c(1), x2=c(0))
  expect_error(logRegModel(train_data, 1, c(2,3)),
               "Training set must have at least 2 rows but had 1 row",
               fixed=TRUE)
})
