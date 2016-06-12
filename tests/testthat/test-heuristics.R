context("heuristics")

# require('testthat')

test_that("predictPair error does not have row dimension", {
  train_matrix <- matrix(c(5,4,1,0,0,1), 2, 3)
  model <- ttbModel(train_matrix, 1, c(2,3))
  expect_error(predictPair(train_matrix[1,], train_matrix[2,], model),
               "Object does not have row dimension")
})

test_that("predictPair error too many rows", {
  train_matrix <- matrix(c(5,4,1,0,0,1), 2, 3)
  model <- ttbModel(train_matrix, 1, c(2,3))
  expect_error(predictPair(train_matrix[c(1:2),], train_matrix[2,], model),
               "Expected a single row but got 2 rows")
})

test_that("predictPairProb error does not have row dimension", {
  train_matrix <- matrix(c(5,4,1,0,0,1), 2, 3)
  model <- ttbModel(train_matrix, 1, c(2,3))
  expect_error(predictPairProb(train_matrix[1,], train_matrix[2,], model),
               "Object does not have row dimension")
})

test_that("predictPairProb error too many rows", {
  train_matrix <- matrix(c(5,4,1,0,0,1), 2, 3)
  model <- ttbModel(train_matrix, 1, c(2,3))
  expect_error(predictPairProb(train_matrix[c(1:2),], train_matrix[2,], model),
               "Expected a single row but got 2 rows")
})

# Variations with data types

test_that("ttbModel 2x3 predictPair/Prob forward data.frame", {
  train_df <- data.frame(y=c(5,4), x1=c(1,0), x2=c(0,1))
  model <- ttbModel(train_df, 1, c(2,3))
  expect_equal(c(x1=1, x2=1), model$cue_validities)
  expect_equal(c(x1=1, x2=-1), model$cue_directions)
  expect_equal(c(x1=1, x2=0), model$cue_validities_unreversed)
  # The prediction whether row 1 or row 2 is greater is 1, meaning row1.
  expect_equal(1, predictPair(oneRow(train_df, 1),
                              oneRow(train_df, 2), model))
  # The logical opposite: the prediction whether row 1 or row 2 is greater is
  # -1, meaning row2.
  expect_equal(-1, predictPair(oneRow(train_df, 2),
                               oneRow(train_df, 1), model))
  
  # predictPairProb
  # The probability that row 1 > row 2 is 1.
  expect_equal(1, predictPairProb(oneRow(train_df, 1),
                                  oneRow(train_df, 2), model))
  # The logical opposite: the probability that row2 > row 1 is 0.
  expect_equal(0, predictPairProb(oneRow(train_df, 2),
                                  oneRow(train_df, 1), model))
})

test_that("ttbModel 2x3 predictPair/Prob forward data.frame ignore extra", {
  train_df <- data.frame(y=c(5,4), name=c("a", "b"), x1=c(1,0), x2=c(0,1))
  model <- ttbModel(train_df, 1, c(3,4))
  expect_equal(c(x1=1, x2=0), model$cue_validities_unreversed)
  # The prediction whether row 1 or row 2 is greater is 1, meaning row1.
  expect_equal(1, predictPair(oneRow(train_df, 1),
                              oneRow(train_df, 2), model))
  # The logical opposite: the prediction whether row 1 or row 2 is greater is
  # -1, meaning row2.
  expect_equal(-1, predictPair(oneRow(train_df, 2),
                               oneRow(train_df, 1), model))
  
  # The probability that row 1 > row 2 is 1.
  expect_equal(1, predictPairProb(oneRow(train_df, 1),
                                  oneRow(train_df, 2), model))
  # The logical opposite: the probability that row2 > row 1 is 0.
  expect_equal(0, predictPairProb(oneRow(train_df, 2),
                                  oneRow(train_df, 1), model))
})

# ttbModel on binary cues

test_that("ttbModel 2x3 predictPair predictPairProb forward", {
  train_matrix <- matrix(c(5,4,1,0,0,1), 2, 3)
  model <- ttbModel(train_matrix, 1, c(2,3))
  expect_equal(c(1, 0), model$cue_validities_unreversed)
  # The probability that row 1 > row 2 is 1.
  expect_equal(1, predictPairProb(oneRow(train_matrix, 1),
                                  oneRow(train_matrix, 2), model))
  expect_equal(1, predictPair(oneRow(train_matrix, 1),
                              oneRow(train_matrix, 2), model))
  # The logical opposite: the probability that row2 > row 1 is 0.
  expect_equal(0, predictPairProb(oneRow(train_matrix, 2),
                                  oneRow(train_matrix, 1), model))
  # So predict row 1 is NOT greater.
  expect_equal(-1, predictPair(oneRow(train_matrix, 2),
                               oneRow(train_matrix, 1), model))
})

test_that("ttbModel 2x3 predictPair predictPairProb test_matrix backward cues", {
  train_matrix <- cbind(y=c(5,4), x1=c(1,0), x2=c(0,1))
  model <- ttbModel(train_matrix, 1, c(2,3))
  expect_equal(c(x1=1, x2=0), model$cue_validities_unreversed)
  # Cues in test_data below have been reversed.
  # So predictions should be reversed.
  test_matrix <- cbind(y=c(5,4), x1=c(0,1), x2=c(1,0))
  expect_equal(-1, predictPair(oneRow(test_matrix, 1),
                               oneRow(test_matrix, 2), model))
  expect_equal(0, predictPairProb(oneRow(test_matrix, 1),
                                  oneRow(test_matrix, 2), model))
  # Check symmetry (row 2 vs. row1).
  expect_equal(1, predictPair(oneRow(test_matrix, 2),
                              oneRow(test_matrix, 1), model))
  expect_equal(1, predictPairProb(oneRow(test_matrix, 2),
                                  oneRow(test_matrix, 1), model))
})

test_that("ttbModel 2x3 predictPair predictPairProb test_matrix backward criterion", {
  train_matrix <- cbind(y=c(5,4), x1=c(1,0), x2=c(0,1))
  model <- ttbModel(train_matrix, 1, c(2,3))
  expect_equal(c(x1=1, x2=1), model$cue_validities)
  expect_equal(c(x1=1, x2=0), model$cue_validities_unreversed)
  # The criterion in test_data below has been reversed.
  # It should be ignored-- continue to use the validities from train_matrix.
  test_matrix <- cbind(y=c(4,5), x1=c(1,0), x2=c(0,1))
  expect_equal(1, predictPair(oneRow(test_matrix, 1),
                              oneRow(test_matrix, 2), model))
  expect_equal(1, predictPairProb(oneRow(test_matrix, 1),
                                 oneRow(test_matrix, 2), model))
  # Check symmetry (row 2 vs. row1).
  expect_equal(-1, predictPair(oneRow(test_matrix, 2),
                               oneRow(test_matrix, 1), model))
  expect_equal(0, predictPairProb(oneRow(test_matrix, 2),
                                 oneRow(test_matrix, 1), model))
})

test_that("ttbModel 2x2 predictPair predictPairProb cue_reversal", {
  train_matrix <- cbind(y=c(5,4), x1=c(0,1))
  model <- ttbModel(train_matrix, 1, c(2))
  expect_equal(c(x1=0), model$cue_validities_unreversed)
  expect_equal(c(x1=1), model$cue_validities)
  expect_equal(1, predictPair(oneRow(train_matrix, 1),
                              oneRow(train_matrix, 2), model))
  expect_equal(1, predictPairProb(oneRow(train_matrix, 1),
                                  oneRow(train_matrix, 2), model))
  # Check symmetry (row 2 vs. row1).
  expect_equal(-1, predictPair(oneRow(train_matrix, 2),
                               oneRow(train_matrix, 1), model))
  expect_equal(0, predictPairProb(oneRow(train_matrix, 2),
                                 oneRow(train_matrix, 1), model))
})

test_that("ttbModel 3x3 predictPair predictPairProb forward", {
  train_matrix <- cbind(y=c(5,4,3), x1=c(1,0,1), x2=c(1,0,0))
  model <- ttbModel(train_matrix, 1, c(2,3))
  expect_equal(c(x1=0.5, x2=1), model$cue_validities)
  # Row 1 vs. row 2.
  expect_equal(1, predictPair(oneRow(train_matrix, 1),
                              oneRow(train_matrix, 2), model))
  expect_equal(1, predictPairProb(oneRow(train_matrix, 1),
                                 oneRow(train_matrix, 2), model))
  # Row 1 vs. row 3.
  expect_equal(1, predictPair(oneRow(train_matrix, 1),
                              oneRow(train_matrix, 3), model))
  expect_equal(1, predictPairProb(oneRow(train_matrix, 1),
                                 oneRow(train_matrix, 3), model))
  # Row 2 vs. row 3.
  # Cue x1 discriminates rows 2 and 3, but validity=0.5, so not used.
  expect_equal(0, predictPair(oneRow(train_matrix, 2),
                              oneRow(train_matrix, 3), model))
  expect_equal(0.5, predictPairProb(oneRow(train_matrix, 2),
                                    oneRow(train_matrix, 3), model))
})

test_that("ttbModel 3x3 predictPair/Prob cue_reversal", {
  train_matrix <- cbind(y=c(5,4,3), x1=c(1,0,1), x2=c(0,0,1))
  model <- ttbModel(train_matrix, 1, c(2,3))
  expect_equal(c(x1=0.5, x2=0), model$cue_validities_unreversed)
  # x1 discriminates but has 0.5 validity.
  # x2 does not discriminate.
  # So it's a guess = 0.
  expect_equal(0, predictPair(oneRow(train_matrix, 1),
                              oneRow(train_matrix, 2), model))
  # Reverse the 2nd cue, and it discriminates to get these right.
  expect_equal(1, predictPair(oneRow(train_matrix, 2),
                              oneRow(train_matrix, 3), model))
  expect_equal(1, predictPair(oneRow(train_matrix, 1),
                              oneRow(train_matrix, 3), model))
  
  # predictPairProb: guess = 0.5.
  expect_equal(0.5, predictPairProb(oneRow(train_matrix, 1),
                                   oneRow(train_matrix, 2), model))
  # Reverse the 2nd cue, and it discriminates to get these right.
  expect_equal(1, predictPairProb(oneRow(train_matrix, 2),
                                 oneRow(train_matrix, 3), model))
  expect_equal(1, predictPairProb(oneRow(train_matrix, 1),
                                 oneRow(train_matrix, 3), model))
})

test_that("ttbModel 3x3 pos pos predictPair/Prob forward", {
  train_matrix <- cbind(y=c(5,4,3), x1=c(1,0,0), x2=c(1,1,0))
  model <- ttbModel(train_matrix, 1, c(2,3))
  expect_equal(c(x1=1, x2=1),  model$cue_validities_unreversed)
  
  expect_equal(1, predictPair(oneRow(train_matrix, 1),
                              oneRow(train_matrix, 2), model))
  expect_equal(-1, predictPair(oneRow(train_matrix, 2),
                               oneRow(train_matrix, 1), model))
  
  expect_equal(1, predictPair(oneRow(train_matrix, 1),
                              oneRow(train_matrix, 3), model))
  expect_equal(-1, predictPair(oneRow(train_matrix, 3),
                               oneRow(train_matrix, 1), model))
  
  expect_equal(1, predictPair(oneRow(train_matrix, 2),
                              oneRow(train_matrix, 3), model))
  expect_equal(-1, predictPair(oneRow(train_matrix, 3),
                               oneRow(train_matrix, 2), model))
  
  # predictPairProb
  expect_equal(1, predictPairProb(oneRow(train_matrix, 1),
                                 oneRow(train_matrix, 2), model))
  expect_equal(0, predictPairProb(oneRow(train_matrix, 2),
                                 oneRow(train_matrix, 1), model))
  
  expect_equal(1, predictPairProb(oneRow(train_matrix, 1),
                                 oneRow(train_matrix, 3), model))
  expect_equal(0, predictPairProb(oneRow(train_matrix, 3),
                                 oneRow(train_matrix, 1), model))
  
  expect_equal(1, predictPairProb(oneRow(train_matrix, 2),
                                 oneRow(train_matrix, 3), model))
  expect_equal(0, predictPairProb(oneRow(train_matrix, 3),
                                 oneRow(train_matrix, 2), model))
})

test_that("ttbModel 3x3 pos pos predictPair/Prob backward cues in test", {
  train_matrix <- cbind(y=c(5,4,3), x1=c(1,0,0), x2=c(1,1,0))
  model <- ttbModel(train_matrix, 1, c(2,3))
  expect_equal(c(x1=1, x2=1),  model$cue_validities_unreversed)

  # All cues backwards relative to training data.
  test_matrix <- cbind(y=c(5,4,3), x1=c(0,1,1), x2=c(0,0,1))
  expect_equal(-1, predictPair(oneRow(test_matrix, 1),
                               oneRow(test_matrix, 2), model))
  expect_equal(1, predictPair(oneRow(test_matrix, 2),
                              oneRow(test_matrix, 1), model))
  
  expect_equal(-1, predictPair(oneRow(test_matrix, 1),
                               oneRow(test_matrix, 3), model))
  expect_equal(1, predictPair(oneRow(test_matrix, 3),
                              oneRow(test_matrix, 1), model))
  
  expect_equal(-1, predictPair(oneRow(test_matrix, 2),
                               oneRow(test_matrix, 3), model))
  expect_equal(1, predictPair(oneRow(test_matrix, 3),
                              oneRow(test_matrix, 2), model))
  
  # predictPairProb
  expect_equal(0, predictPairProb(oneRow(test_matrix, 1),
                                 oneRow(test_matrix, 2), model))
  expect_equal(1, predictPairProb(oneRow(test_matrix, 2),
                                 oneRow(test_matrix, 1), model))
  
  expect_equal(0, predictPairProb(oneRow(test_matrix, 1),
                                 oneRow(test_matrix, 3), model))
  expect_equal(1, predictPairProb(oneRow(test_matrix, 3),
                                 oneRow(test_matrix, 1), model))
  
  expect_equal(0, predictPairProb(oneRow(test_matrix, 2),
                                 oneRow(test_matrix, 3), model))
  expect_equal(1, predictPairProb(oneRow(test_matrix, 3),
                                 oneRow(test_matrix, 2), model))
})

test_that("ttbModel 2x2,3x2 predictPair/Prob", {
  train_matrix <- cbind(y=c(5,4,3), x1=c(1,0,0))
  model <- ttbModel(train_matrix, 1, c(2))
  expect_equal(c(x1=1), model$cue_validities)
  expect_equal(c(x1=1), model$cue_validities_unreversed)
  expect_equal(1, predictPair(oneRow(train_matrix, 1),
                              oneRow(train_matrix, 2), model))
  expect_equal(1, predictPairProb(oneRow(train_matrix, 1),
                                 oneRow(train_matrix, 2), model))

  expect_equal(1, predictPair(oneRow(train_matrix, 1),
                              oneRow(train_matrix, 3), model))
  expect_equal(1, predictPairProb(oneRow(train_matrix, 1),
                                 oneRow(train_matrix, 3), model))

  expect_equal(0, predictPair(oneRow(train_matrix, 2),
                              oneRow(train_matrix, 3), model))
  expect_equal(0.5, predictPairProb(oneRow(train_matrix, 2),
                                    oneRow(train_matrix, 3), model))
})

test_that("ttbModel 4x4 predictPairProb x1 cue dominates", {
  train_data <- cbind(y=c(9,8,7,6), x1=c(1,1,1,0), x2=c(1,1,0,1),
                      x3=c(1,1,0,1))
  # How this data looks:
  # > train_data
  #         y   x1   x2   x3
  # [1,]    9    1    1    1
  # [2,]    8    1    1    1
  # [3,]    7    1    0    0
  # [4,]    6    0    1    1
  # Cue x2 has validity 1.0, cue x2 and cue x3 have validity 2/3.
  # Cue x1 predicts Row 3 > Row 4.
  # But if you sum cue weights, predict Row 4 > Row 3
  model <- ttbModel(train_data, 1, c(2:4))
  expect_equal(c(x1=1, x2=0.667, x3=0.667), model$cue_validities_unreversed,
               tolerance=0.002)
  expect_equal(1, predictPair(oneRow(train_data, 3),
                              oneRow(train_data, 4), model))
  expect_equal(-1, predictPair(oneRow(train_data, 4),
                               oneRow(train_data, 3), model))
})

test_that("ttbModel 4x4 predictPairProb cue x1 dominates non-binary", {
  train_data <- cbind(y=c(9,8,7,6), x1=c(0.1, 0.1, 0.1, 0),
                      x2=c(1,1,0,1), x3=c(1,1,0,1))
  # How this data looks:
  # > train_data
  #         y    x1   x2   x3
  # [1,]    9   0.1    1    1
  # [2,]    8   0.1    1    1
  # [3,]    7   0.1    0    0
  # [4,]    6     0    1    1
  # Cue x1 has validity 1.0, cue x2 and cue x3 have validity 2/3.
  # Cue x1 predicts Row 3 > Row 4.
  # But if you sum cue weights, predict Row 4 > Row 3
  model <- ttbModel(train_data, 1, c(2:4))
  expect_equal(c(x1=1, x2=0.667, x3=0.667), model$cue_validities_unreversed,
               tolerance=0.002)
  expect_equal(1, predictPair(oneRow(train_data, 3),
                              oneRow(train_data, 4), model))
  expect_equal(-1, predictPair(oneRow(train_data, 4),
                               oneRow(train_data, 3), model))
})

test_that("ttbModel 4x4 predictPairProb cue x3 dominates non-binary", {
  train_data <- cbind(y=c(9,8,7,6), x1=c(1,1,0,1), x2=c(1,1,0,1),
                      x3=c(0.1, 0.1, 0.1, 0))
  # How this data looks:
  # > train_data
  #         y   x1   x2   x3
  # [1,]    9    1    1  0.1
  # [2,]    8    1    1  0.1
  # [3,]    7    0    0  0.1
  # [4,]    6    1    1  0.0
  # Cue x1 and x2 have validity 2/3, cue x3 has validity 1.0.
  # Cue x3 predicts Row 3 > Row 4.
  # But if you sum cue weights, predict Row 4 > Row 3
  model <- ttbModel(train_data, 1, c(2:4))
  expect_equal(c(x1=0.667, x2=0.667, x3=1), model$cue_validities_unreversed,
               tolerance=0.002)
  expect_equal(1, predictPair(oneRow(train_data, 3),
                              oneRow(train_data, 4), model))
  expect_equal(-1, predictPair(oneRow(train_data, 4),
                               oneRow(train_data, 3), model))
})

test_that("ttbModel 4x4 predictPairProb 3nd cue dominates non-binary reverse cue", {
  train_data <- cbind(y=c(9,8,7,6), x1=c(1,1,0,1), x2=c(1,1,0,1),
                      x3=c(0, 0, 0, 0.1))
  # How this data looks:
  # > train_data
  #         y   x1   x2   x3
  # [1,]    9    1    1  0.0
  # [2,]    8    1    1  0.0
  # [3,]    7    0    0  0.0
  # [4,]    6    1    1  0.1
  # Column y is the criterion column.  Cues follow.
  # Cue x1 and x2 have validity 2/3, cue x3 has validity 0,
  # but that validity is 1.0 when reversed.
  # Cue x3 predicts Row 3 > Row 4.
  # But if you sum all cue weights, predict Row 4 > Row 3
  model <- ttbModel(train_data, 1, c(2:4))
  expect_equal(c(x1=0.667, x2=0.667, x3=0), model$cue_validities_unreversed,
               tolerance=0.002)
  expect_equal(1, predictPair(oneRow(train_data, 3),
                              oneRow(train_data, 4), model))
  expect_equal(-1, predictPair(oneRow(train_data, 4),
                              oneRow(train_data, 3), model))
})

test_that("ttbModel 4x4 predictPairProb 3nd cue dominates non-binary reverse cue data.frame", {
  train_df <- data.frame(y=c(9,8,7,6), x1=c(1,1,0,1), x2=c(1,1,0,1),
                         x3=c(0,0,0,0.1))
  # How this data looks:
  # > train_df
  #   y x1 x2  x3
  # 1 9  1  1 0.0
  # 2 8  1  1 0.0
  # 3 7  0  0 0.0
  # 4 6  1  1 0.1
  # Cue x1 and x2 have validity 2/3, cue x3 has validity 0,
  # but that validity is 1.0 when reversed.
  # Cue x3 predicts Row 3 > Row 4.
  # But if you sum all cue weights, predict Row 4 > Row 3
  model <- ttbModel(train_df, 1, c(2:4))
  expect_equal(c(x1=0.667, x2=0.667, x3=0), model$cue_validities_unreversed,
               tolerance=0.002)
  expect_equal(c(x1=0.667, x2=0.667, x3=1), model$cue_validities,
               tolerance=0.002)
  # The coefficient for column c should be negative.
  expect_equal(c(x3=-1), sign(coef(model)["x3"]), tolerance=0.002)
  expect_equal(1, predictPairProb(oneRow(train_df, 3),
                                 oneRow(train_df, 4), model))
  expect_equal(0, predictPairProb(oneRow(train_df, 4),
                                 oneRow(train_df, 3), model))
})

### ttbModel on real-valued cues ###

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
  # Cue a and b have validity 2/3, cue c has validity 0,
  # but that validity is 1.0 when reversed.
  # Cue c predicts Row 3 > Row 4.
  # But if you sum all cue weights, predict Row 4 > Row 3
  model <- ttbModel(train_df, 1, c(2:4))
  expect_equal(c(a=0.667, b=0.667, c=0), model$cue_validities_unreversed, tolerance=0.002)
  expect_equal(c(a=0.667, b=0.667, c=1), model$cue_validities, tolerance=0.002)
  # The coefficient for column c should be negative.
  expect_equal(c(c=-1), sign(coef(model)["c"]), tolerance=0.002)
  expect_equal(1, predictPair(oneRow(train_df, 3),
                              oneRow(train_df, 4), model))
  expect_equal(-1, predictPair(oneRow(train_df, 4),
                               oneRow(train_df, 3), model))
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
  expect_equal(c(a=0.667, b=0.667, c=1), model$cue_validities_unreversed, tolerance=0.002)
  expect_equal(c(a=0.667, b=0.667, c=1), model$cue_validities, tolerance=0.002)
  expect_equal(c(c=1), sign(coef(model)["c"]), tolerance=0.002)
  expect_equal(1, predictPair(oneRow(train_df, 3),
                              oneRow(train_df, 4), model))
  expect_equal(-1, predictPair(oneRow(train_df, 4),
                               oneRow(train_df, 3), model))
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
  expect_equal(c(a=0.667, b=0.667, c=1), model$cue_validities_unreversed, tolerance=0.002)
  expect_equal(c(a=0.667, b=0.667, c=1), model$cue_validities, tolerance=0.002)
  expect_equal(c(c=1), sign(coef(model)["c"]), tolerance=0.002)
  expect_equal(1, predictPair(oneRow(train_df, 3),
                              oneRow(train_df, 4), model))
  expect_equal(-1, predictPair(oneRow(train_df, 4),
                               oneRow(train_df, 3), model))
})

test_that(paste("ttbModel 4x4 predictPair 3nd cue dominates cue data.frame",
                "non-binary big criteriondiffs, big diffs, big diffs, big unique diffs"), {
  train_df <- data.frame(criterion=c(900,400,100,6), a=c(101,101,20,101), b=c(59,59,5,59),
                         c=c(90,80,70,10))
  # Cue a and b have validity 2/3, cue c has validity 1.
  # Cue c predicts Row 3 > Row 4.
  # But if you sum all cue weights, predict Row 4 > Row 3
  model <- ttbModel(train_df, 1, c(2:4))
  expect_equal(c(a=0.667, b=0.667, c=1), model$cue_validities_unreversed, tolerance=0.002)
  expect_equal(c(a=0.667, b=0.667, c=1), model$cue_validities, tolerance=0.002)
  expect_equal(c(c=1), sign(coef(model)["c"]), tolerance=0.002)
  expect_equal(1, predictPair(oneRow(train_df, 3),
                              oneRow(train_df, 4), model))
  expect_equal(-1, predictPair(oneRow(train_df, 4),
                               oneRow(train_df, 3), model))
})

test_that(paste("ttbModel 4x4 predictPair 3nd cue dominates cue data.frame REVERSE",
                "non-binary big criteriondiffs, big diffs, big diffs, big unique diffs"), {
  train_df <- data.frame(criterion=c(6,100,400,900), a=c(101,20,101,101), b=c(59,5,59,59),
                         c=c(10,70,80,90))
  # Cue a and b have validity 2/3, cue c has validity 0,
  # but that validity is 1.0 when reversed.
  # Cue c predicts Row 3 > Row 4.
  # But if you sum all cue weights, predict Row 4 > Row 3
  model <- ttbModel(train_df, 1, c(2:4))
  expect_equal(c(a=0.667, b=0.667, c=1), model$cue_validities_unreversed, tolerance=0.002)
  expect_equal(c(a=0.667, b=0.667, c=1), model$cue_validities, tolerance=0.002)
  expect_equal(c(c=1), sign(coef(model)["c"]), tolerance=0.002)
  expect_equal(-1, predictPair(oneRow(train_df, 3),
                               oneRow(train_df, 4), model))
  expect_equal(1, predictPair(oneRow(train_df, 4),
                              oneRow(train_df, 3), model))
})

### ttbModel on same data as ttbGreedyModel ###

test_that("ttbModel on 3x3 where differs from greedy ttb", {
  matrix <- cbind(y=c(3:1), x1=c(1,0,0), x2=c(1,0,1))
  model <- ttbModel(matrix, 1, c(2:3))
  expect_equal(1, predictPair(oneRow(matrix, 1),
                              oneRow(matrix, 2), model))
  expect_equal(1, predictPair(oneRow(matrix, 1),
                              oneRow(matrix, 3), model))
  # Below is the row pair that differs from greedy ttb.
  expect_equal(0, predictPair(oneRow(matrix, 2),
                                oneRow(matrix, 3), model))
  expect_equal(c(x1=1.0, x2=0.5), model$cue_validities)
})

test_that("ttbModel on 2 same cues- differs from greedy ttb", {
  matrix <- cbind(y=c(2:1), x1=c(1,0), x2=c(1,0))
  model <- ttbModel(matrix, 1, c(2:3))
  full_matrix <- cbind(y=c(3:1), x1=c(1,0,0), x2=c(1,1,0))
  out <- percentCorrectList(full_matrix, list(model))
  # TTB uses both cues, so it can get 100% correct.
  expect_equal(100, out$ttbModel)
  expect_equal(c(x1=1.0, x2=1.0), model$cue_validities)
})

test_that("ttbModel default fit_name", {
  train_matrix <- matrix(c(5,4,1,0,0,1), 2, 3)
  ttb_default <- ttbModel(train_matrix, 1, c(2,3))
  expect_equal(class(ttb_default), ttb_default$fit_name)
})

test_that("ttbModel override fit_name", {
  train_matrix <- matrix(c(5,4,1,0,0,1), 2, 3)
  ttb1 <- ttbModel(train_matrix, 1, c(2,3), fit_name="fit1")
  expect_equal("fit1", ttb1$fit_name)
  ttb2 <- ttbModel(train_matrix, 1, c(2,3), fit_name="fit2")
  expect_equal("fit2", ttb2$fit_name)
  
  # Checks taht the fit_name was properly used by other code.
  out <- percentCorrectList(train_matrix, list(ttb1, ttb2))
  expect_equal(c("fit1", "fit2"), colnames(out))
})

### ttbGreedyModel ###

test_that("ttbGreedyModel on 3x3 where differs from regular ttb", {
  matrix <- cbind(y=c(3:1), x1=c(1,0,0), x2=c(1,0,1))
  model <- ttbGreedyModel(matrix, 1, c(2:3))
  expect_equal(1, predictPair(oneRow(matrix, 1),
                              oneRow(matrix, 2), model))
  expect_equal(1, predictPair(oneRow(matrix, 1),
                              oneRow(matrix, 3), model))
  # Below is the row pair that differs from regular ttb.
  expect_equal(1, predictPair(oneRow(matrix, 2),
                              oneRow(matrix, 3), model))
  # After using x1, it sees only last two rows of x2.  It reverses them
  # to get a validity 1.0 cue, which is why it predicts 2 vs. 3 correctly.
  expect_equal(c(x1=1.0, x2=1.0), model$cue_validities)
})

test_that("ttbGreedyModel on 3x3 where differs from regular ttb reverse cue values", {
  matrix <- cbind(y=c(3:1), x1=c(0,1,1), x2=c(0,1,0))
  model <- ttbGreedyModel(matrix, 1, c(2:3))
  expect_equal(1, predictPair(oneRow(matrix, 1),
                              oneRow(matrix, 2), model))
  expect_equal(1, predictPair(oneRow(matrix, 1),
                              oneRow(matrix, 3), model))
  # Below is the row pair that differs from regular ttb.
  expect_equal(1, predictPair(oneRow(matrix, 2),
                              oneRow(matrix, 3), model))
  # After using x1, it sees only last two rows of x2.  It reverses them
  # to get a validity 1.0 cue, which is why it predicts 2 vs. 3 correctly.
  expect_equal(c(x1=1.0, x2=1.0), model$cue_validities)
})

test_that("ttbGreedyModel on 2 same cues- differs from regular ttb", {
  matrix <- cbind(y=c(2:1), x1=c(1,0), x2=c(1,0))
  model <- ttbGreedyModel(matrix, 1, c(2:3))
  full_matrix <- cbind(y=c(3:1), x1=c(1,0,0), x2=c(1,1,0))
  out <- percentCorrectList(full_matrix, list(model))
  # Greedy TTB uses only one cue (random which one), so it has to guess on
  # one row pair.  Accuracy = (1+1+0.5)/3.
  expect_equal(83.333, out$ttbGreedyModel, tolerance=0.001)
  # One cue has NA, but we don't know which one.
  expect_equal(c(1.0, NA), sort(unname(model$cue_validities),
                                na.last=TRUE))
})

### prob helper functions ###

test_that("indexOfCueUsed 3 simple cues", {
  cv <- c(0.9, 0.8, 0.7)
  # Indexes are 1-based.  As long as cue 1 discriminates, use it.
  expect_equal(1, indexOfCueUsed(cv, cbind(1,1,1), cbind(0,0,0)))
  expect_equal(1, indexOfCueUsed(cv, cbind(1,1,1), cbind(0,0,1)))
  expect_equal(1, indexOfCueUsed(cv, cbind(1,1,1), cbind(0,1,0)))
  expect_equal(1, indexOfCueUsed(cv, cbind(1,1,1), cbind(0,1,1)))
  # First cue does not discriminate.  Have to go to 2nd cue.
  expect_equal(2, indexOfCueUsed(cv, cbind(1,1,1), cbind(1,0,0)))
  expect_equal(2, indexOfCueUsed(cv, cbind(1,1,1), cbind(1,0,1)))
  # First 2 cues do not discriminate.  Have to go to 3rd cue.
  expect_equal(3, indexOfCueUsed(cv, cbind(1,1,1), cbind(1,1,0)))
  # The above are 7 of the 8 possible combinations.
  # The case where no cue discriminates is a separate test.
})


test_that("indexOfCueUsed 3 simple cues none discriminate", {
  cv <- c(0.9, 0.8, 0.7)
  expect_equal(-1, indexOfCueUsed(cv, cbind(1,1,1), cbind(1,1,1)))
  # Should get same result with reversed
  expect_equal(-1, indexOfCueUsed(rev(cv), cbind(1,1,1), cbind(1,1,1)))
})

test_that("indexOfCueUsed 3 simple cue order reversed", {
  # Same cue validities as above.
  cv_orig <- c(0.9, 0.8, 0.7)
  # Reverse them.
  cv <- rev(cv_orig)
  # Indexes ar 1-based.  All cues discriminate, so choose the
  # highest-validity cue.
  expect_equal(3, indexOfCueUsed(cv, cbind(1,1,1), cbind(0,0,0)))
  expect_equal(3, indexOfCueUsed(cv, cbind(1,1,1), cbind(1,0,0)))
  expect_equal(3, indexOfCueUsed(cv, cbind(1,1,1), cbind(0,1,0)))
  expect_equal(3, indexOfCueUsed(cv, cbind(1,1,1), cbind(1,1,0)))
  # First cue does not discriminate.  Have to go to 2nd cue.
  expect_equal(2, indexOfCueUsed(cv, cbind(1,1,1), cbind(0,0,1)))
  expect_equal(2, indexOfCueUsed(cv, cbind(1,1,1), cbind(1,0,1)))
  # First 2 cues do not discriminate.  Have to go to 3rd cue.
  expect_equal(1, indexOfCueUsed(cv, cbind(1,1,1), cbind(0,1,1)))
})
  
test_that("indexOfCueUsed 3 simple cues rows reversed", {
  cv <- c(0.9, 0.8, 0.7)
  # Indexes are 1-based.  As long as cue 1 discriminates, use it.
  expect_equal(1, indexOfCueUsed(cv, cbind(0,0,0), cbind(1,1,1)))
  expect_equal(1, indexOfCueUsed(cv, cbind(0,0,1), cbind(1,1,1)))
  expect_equal(1, indexOfCueUsed(cv, cbind(0,1,0), cbind(1,1,1)))
  expect_equal(1, indexOfCueUsed(cv, cbind(0,1,1), cbind(1,1,1)))
  # First cue does not discriminate.  Have to go to 2nd cue.
  expect_equal(2, indexOfCueUsed(cv, cbind(1,0,0), cbind(1,1,1)))
  expect_equal(2, indexOfCueUsed(cv, cbind(1,0,1), cbind(1,1,1)))
  # First 2 cues do not discriminate.  Have to go to 3rd cue.
  expect_equal(3, indexOfCueUsed(cv, cbind(1,1,0), cbind(1,1,1)))
  # The above are 7 of the 8 possible combinations.
  # The case where no cue discriminates is same when rows reversed.
})

### unitWeightModel ###

test_that("unitWeightModel 2x3 pos neg", {
  model <- unitWeightModel(matrix(c(5,4,1,0,0,1), 2, 3), 1, c(2,3))
  expect_equal(c(1,0),  model$cue_validities_unreversed) 
  expect_equal(1,  coef(model)[[1]])  
  expect_equal(-1,  coef(model)[[2]])  
  expect_equal(2, length(coef(model))) 
})

test_that("unitWeightModel 5x1 75", {
  model <- unitWeightModel(matrix(c(5,4,3,2,1,1,1,1,0,1), 5, 2), 1, c(2))
  expect_equal(c(0.75),  model$cue_validities_unreversed) 
  expect_equal(1,  coef(model)[[1]])
  expect_equal(1, length(coef(model))) 
})

test_that("unitWeightModel 5x1 25", {
  model <- unitWeightModel(matrix(c(5,4,3,2,1,1,0,1,1,1), 5, 2), 1, c(2))
  expect_equal(c(0.25),  model$cue_validities_unreversed) 
  expect_equal(-1,  coef(model)[[1]])
  expect_equal(1, length(coef(model))) 
})

test_that("unitWeightModel 4x4 predictPair 3nd cue dominates non-binary reverse cue", {
  train_df <- data.frame(Y=c(9,8,7,6), a=c(1,1,0,1), b=c(1,1,0,1),
                         c=c(0,0,0,0.1))
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
  model <- unitWeightModel(train_df, 1, c(2:4))
  expect_equal(c(a=0.667, b=0.667, c=0), model$cue_validities_unreversed,
               tolerance=0.002)
  expect_equal(c(a=0.667, b=0.667, c=1), model$cue_validities,
               tolerance=0.002)
  expect_equal(c(a=1, b=1, c=-1), model$linear_coef, tolerance=0.002)
  expect_equal(-1, predictPair(oneRow(train_df, 3),
                               oneRow(train_df, 4), model))
  expect_equal(1, predictPair(oneRow(train_df, 4),
                              oneRow(train_df, 3), model))
})

### validityWeightModel ###

test_that("validityWeightModel 2x3 predictPair pos neg reverse_cues FALSE", {
  train_matrix <- cbind(y=c(5,4), x1=c(1,0), x2=c(0,1))
  model <- validityWeightModel(train_matrix, 1, c(2,3), reverse_cues=FALSE)
  expect_equal(c(x1=1, x2=0),  model$cue_validities_unreversed)
  expect_equal(c(x1=1, x2=0),  model$linear_coef)
  # Another way to get the linear coefficients.
  expect_equal(c(x1=1, x2=0),  coef(model))
  expect_equal(1, predictPair(oneRow(train_matrix, 1),
                              oneRow(train_matrix, 2), model))
})

test_that("validityWeightModel 2x3 predictPair pos neg", {
  train_matrix <- cbind(y=c(5,4), x1=c(1,0), x2=c(0,1))
  model <- validityWeightModel(train_matrix, 1, c(2,3))
  expect_equal(c(x1=1, x2=0),  model$cue_validities_unreversed)
  expect_equal(c(x1=1, x2=-1),  model$linear_coef)
  expect_equal(1, predictPair(oneRow(train_matrix, 1),
                              oneRow(train_matrix, 2), model))
})

test_that("validityWeightModel rowPairApply 5x1 75", {
  train_matrix <- cbind(y=c(5,4,3,2,1), x1=c(1,1,1,0,1))
  model <- validityWeightModel(train_matrix, 1, c(2))
  expect_equal(c(x1=0.75),  model$cue_validities_unreversed) 
  expect_equal(c(x1=0.75),  model$linear_coef)
  out <- rowPairApply(train_matrix, rowIndexes(), heuristics(model))
  
  expect_equal(0,  getPrediction_raw(out, c(1,2)), tolerance=0.002)
  expect_equal(0,  getPrediction_raw(out, c(1,3)), tolerance=0.002)
  expect_equal(1,  getPrediction_raw(out, c(1,4)), tolerance=0.002)
  expect_equal(0,  getPrediction_raw(out, c(1,5)), tolerance=0.002)
  
  expect_equal(-1, getPrediction_raw(out, c(4,5)), tolerance=0.002)
})

test_that("validityWeightModel 5x1 25 reverse_cues FALSE", {
  train_matrix <- cbind(y=c(5,4,3,2,1), x1=c(1,0,1,1,1))
  model <- validityWeightModel(train_matrix, 1, c(2), reverse_cues=FALSE)
  expect_equal(c(x1=0.25),  model$cue_validities_unreversed)
  # No cue reversal means cue_validities is the same.
  expect_equal(c(x1=0.25),  model$cue_validities)
  # No cue reversal means coefficients are same as cue validities.
  expect_equal(c(x1=0.25),  coef(model))
  out <- rowPairApply(train_matrix, rowIndexes(), heuristics(model))
  expect_equal(1,  getPrediction_raw(out, c(1,2)), tolerance=0.002)
  expect_equal(0,  getPrediction_raw(out, c(1,3)), tolerance=0.002)
  expect_equal(0,  getPrediction_raw(out, c(1,4)), tolerance=0.002)
  expect_equal(0,  getPrediction_raw(out, c(1,5)), tolerance=0.002)
  
  expect_equal(-1, getPrediction_raw(out, c(2,3)), tolerance=0.002)
})

test_that("validityWeightModel 5x1 25", {
  train_matrix <- cbind(y=c(5,4,3,2,1), x1=c(1,0,1,1,1))
  # By default, reverse_cues is TRUE
  model <- validityWeightModel(train_matrix, 1, c(2))
  expect_equal(c(x1=0.25),  model$cue_validities_unreversed)
  # Cue reversal changes validity 0.25 to 0.75.
  expect_equal(c(x1=0.75),  model$cue_validities)
  # Cue reversal changes coefficient from 0.75 to -0.75.
  expect_equal(c(x1=-0.75),  coef(model))
  
  out <- rowPairApply(train_matrix, rowIndexes(), heuristics(model))
  expect_equal(-1, getPrediction_raw(out, c(1,2)), tolerance=0.002)
  expect_equal(0,  getPrediction_raw(out, c(1,3)), tolerance=0.002)
  expect_equal(0,  getPrediction_raw(out, c(1,4)), tolerance=0.002)
  expect_equal(0,  getPrediction_raw(out, c(1,5)), tolerance=0.002)
  
  expect_equal(1,  getPrediction_raw(out, c(2,3)), tolerance=0.002)
})

test_that("validityWeightModel 4x4 predictPair 3nd cue dominates non-binary reverse cue", {
  train_df <- data.frame(Y=c(9,8,7,6), a=c(1,1,0,1), b=c(1,1,0,1), c=c(0,0,0,0.1))
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
  model <- validityWeightModel(train_df, 1, c(2:4), reverse_cues=FALSE)
  expect_equal(c(a=0.667, b=0.667, c=0), model$cue_validities_unreversed,
               tolerance=0.002)
  # Soon: Linear coef will include reversing the cue pointed the wrong way.
  expect_equal(c(a=0.667, b=0.667, c=0), model$linear_coef, tolerance=0.002)
  
  expect_equal(-1, predictPair(oneRow(train_df, 3),
                              oneRow(train_df, 4), model))
  expect_equal(1, predictPair(oneRow(train_df, 4),
                              oneRow(train_df, 3), model))
})

### regInterceptModel ###

test_that("regInterceptModel 2x2 fit pos slope", {
  train_matrix <- cbind(y=c(5,4), x1=c(1,0))
  model <- regInterceptModel(train_matrix, 1, c(2))
  expect_equal(4,  coef(model)[[1]])  # intercept
  expect_equal(1,  coef(model)[[2]])  # slope
  expect_equal(2, length(coef(model)))
  expect_equal(1, predictPair(oneRow(train_matrix, 1),
                              oneRow(train_matrix, 2), model))
})

test_that("regInterceptModel 2x2 fit neg slope", {
  train_matrix <- cbind(y=c(5,4), x1=c(0,1))
  model <- regInterceptModel(train_matrix, 1, c(2))
  expect_equal(5,  coef(model)[[1]])  # intercept
  expect_equal(-1,  coef(model)[[2]])  # slope
  expect_equal(1, predictPair(oneRow(train_matrix, 1),
                              oneRow(train_matrix, 2), model))
})

test_that("regInterceptModel 2x2 fit pos slope -- data.frame", {
  train_df <- data.frame(y=c(5,4), x1=c(1,0))
  model <- regInterceptModel(train_df, 1, c(2))
  expect_equal(4,  coef(model)[[1]])  # intercept
  expect_equal(1,  coef(model)[[2]])  # slope
  expect_equal(2, length(coef(model))) 
  expect_equal(1, predictPair(oneRow(train_df, 1),
                              oneRow(train_df, 2), model))
})

test_that("lmWrapper 2x2 fit pos slope -- no intercept", {
  train_matrix <- cbind(y=c(5,4), x1=c(1,0))
  model <- lmWrapper(train_matrix, 1, c(2),  include_intercept=FALSE)
  expect_equal(5,  coef(model)[[1]])  # slope
  expect_equal(1, length(coef(model))) 
})

test_that("regInterceptModel 2x3 fit 4.5,1,NA", {
  train_matrix <- cbind(y=c(5,4), x1=c(1,0), x2=c(0,1))
  model <- regInterceptModel(train_matrix, 1, c(2,3))
  expect_equal(4,  coef(model)[[1]])  # intercept
  expect_equal(1,  coef(model)[[2]])  # x1
  #TODO(jean): Ideally regInterceptModel would randomize which cue got the NA coef.
  # Right now, it's always the 2nd cue that gets the NA.
  expect_true( is.na(coef(model)[[3]]) )  # x2 excluded because too many columns
  expect_equal(1, predictPair(oneRow(train_matrix, 1),
                              oneRow(train_matrix, 2), model))
})

test_that("regInterceptModel 2x3 fit 4,1 (col 3 not fit)", {
  model <- regInterceptModel(cbind(y=c(5,4), x1=c(1,0), x2=c(0,1)), 1, c(2))
  expect_equal(4,  coef(model)[[1]])  # intercept
  expect_equal(1,  coef(model)[[2]])  # x1
  expect_equal(2, length(coef(model))) 
})

test_that("regInterceptModel 2x3 fit 5,-1 (col 2 not fit)", {
  model <- regInterceptModel(cbind(y=c(5,4), x1=c(1,0), x2=c(0,1)), 1, c(3))
  expect_equal(5,  coef(model)[[1]])  # intercept
  expect_equal(-1,  coef(model)[[2]])  # x1
  expect_equal(2, length(coef(model))) 
})

test_that("regInterceptModel 3x3 fit positive negative", {
  model <- regInterceptModel(cbind(y=c(5,4,3), x1=c(1,0,0), x2=c(0,0,1)), 1, c(2,3))
  expect_equal(4,  coef(model)[[1]])  # intercept
  expect_equal(1,  coef(model)[[2]])  # V2
  expect_equal(-1,  coef(model)[[3]])  # V3
  expect_equal(3, length(coef(model))) 
})

test_that("regInterceptModel 3x3 fit positive mixed", {
  model <- regInterceptModel(cbind(y=c(5,4,3), x1=c(1,0,0), x2=c(1,0,1)), 1, c(2,3))
  expect_equal(4,  coef(model)[[1]])  # intercept
  expect_equal(2,  coef(model)[[2]])  # V2
  expect_equal(-1, coef(model)[[3]])  # V3
  expect_equal(3, length(coef(model)))  
})

test_that("regInterceptModel predictPair with intercept (check bug)", {
  tol <- 0.0001
  m_train <- data.frame(y=c(5:1), x1=c(1,1,1,0,1))
  model <- regInterceptModel(m_train, 1, c(2))
  # Reg cannot distinguish between rows 1 and 2 based on x1.
  # But in the past there was a bug where the intercept weight was
  # applied to the criterion column so reg was always correct!
  expect_equal(0, predictPair(oneRow(m_train, 1),
                              oneRow(m_train, 2), model))
})

# Warning: Not a self-contained test.  Uses city_population.
test_that("regInterceptModel predictPair city_population", {
  tol <- 0.0001
  model <- regInterceptModel(city_population, 3, c(4:ncol(city_population)))
  # Hamburg (row 2) and Munich (row 3) differ only on the license plate, which has a
  # coefficient of about 25,000.
  # There is an intercept of 75k, but you can ignore it in pairs.
  # So because Hamburg does not have a license plate, it should not be chosen.
  expect_equal(-1, predictPair(oneRow(city_population, 2),
                               oneRow(city_population, 3), model))
})

### regModel ###

test_that("regModel predictPair", {
  tol <- 0.0001
  m_train <- data.frame(y=c(5:1), x1=c(1,1,1,0,1))
  model <- regModel(m_train, 1, c(2))
  # Reg cannot distinguish between rows 1 and 2 based on x1.
  expect_equal(0, predictPair(oneRow(m_train, 1),
                                oneRow(m_train, 2), model))
  # But this should be predicted correctly.
  expect_equal(1, predictPair(oneRow(m_train, 1),
                              oneRow(m_train, 4), model))
})

### logRegModel ###

test_that("logRegModel predictPairProb 2x2 fit train_data", {
  tol <- 0.0001
  train_data <- cbind(y=c(5,4), x1=c(1,0))
  model <- logRegModel(train_data, 1, c(2))
  expect_equal(1, predictPairProb(oneRow(train_data, 1),
                                 oneRow(train_data, 2), model))
  expect_equal(0, predictPairProb(oneRow(train_data, 2),
                                 oneRow(train_data, 1), model))
  out <- rowPairApply(train_data, heuristicsProb(model))
  # There is only one unique pair.
  expect_equal(1, nrow(out))
})

test_that("logRegModel predictPair 2x2 fit train_data", {
  tol <- 0.0001
  train_data <- cbind(y=c(5,4), x1=c(1,0))
  model <- logRegModel(train_data, 1, c(2))
  expect_equal(1, predictPair(oneRow(train_data, 1),
                                 oneRow(train_data, 2), model))
  expect_equal(-1, predictPair(oneRow(train_data, 2),
                                 oneRow(train_data, 1), model))
  out <- rowPairApply(train_data, heuristicsProb(model))
  # There is only one unique pair.
  expect_equal(1, nrow(out))
})

test_that("logRegModel predictPairProb 2x2 fit train_data reverse cue", {
  tol <- 0.0001
  train_data <- cbind(y=c(5,4), x1=c(1,0))
  model <- logRegModel(train_data, 1, c(2))
  expect_equal(1, predictPairProb(oneRow(train_data, 1),
                                 oneRow(train_data, 2), model))
  expect_equal(0, predictPairProb(oneRow(train_data, 2),
                                 oneRow(train_data, 1), model))
})

test_that("logRegModel predictPairProb 2x2,3x2 all correct", {
  tol <- 0.0001
  train_data <- cbind(y=c(5,4), x1=c(1,0))
  model <- logRegModel(train_data, 1, c(2))
  test_data <- cbind(y=c(5,4,3), x1=c(1,0,0))
  
  expect_equal(1, predictPairProb(oneRow(test_data, 1),
                                 oneRow(test_data, 2), model))
  expect_equal(0, predictPairProb(oneRow(test_data, 2),
                                 oneRow(test_data, 1), model))
  expect_equal(1, predictPairProb(oneRow(test_data, 1),
                                 oneRow(test_data, 3), model))
  expect_equal(0, predictPairProb(oneRow(test_data, 3),
                                 oneRow(test_data, 1), model))
  # Row 2 and 3 have same cue values, so Row1 is equally likely to be greater.
  expect_equal(0.5, predictPairProb(oneRow(test_data, 2),
                                   oneRow(test_data, 3), model))
  expect_equal(0.5, predictPairProb(oneRow(test_data, 3),
                                   oneRow(test_data, 2), model))
  
  out <- rowPairApply(test_data, heuristicsProb(model))
  # There are three unique pairs.
  expect_equal(3, nrow(out))
})

test_that("logRegModel predictPairProb 2x2,3x2 all incorrect", {
  tol <- 0.0001
  train_data <- cbind(y=c(5,4), x1=c(1,0))
  model <- logRegModel(train_data, 1, c(2))
  test_data <- cbind(y=c(5,4,3), x1=c(0,1,1))
  
  out <- rowPairApply(test_data, rowIndexes(), heuristicsProb(model))
  expect_equal(0,   getPrediction_raw(out, c(1,2)))
  expect_equal(0,   getPrediction_raw(out, c(1,3)))
  expect_equal(0.5, getPrediction_raw(out, c(2,3)))
  # Confirm opposite pairs have opposite predictions.
  expect_equal(1,   getPrediction_raw(out, c(2,1)))
  expect_equal(1,   getPrediction_raw(out, c(3,1)))
  expect_equal(0.5, getPrediction_raw(out, c(3,2)))
  # There are three unique pairs.
  expect_equal(3, nrow(out))
})

test_that("logRegModel predictPairProb 2x3 fit train_data", {
  train_data <- cbind(y=c(5,4), x1=c(1,0), x2=c(1,0))
  model <- logRegModel(train_data, 1, c(2,3))
  expect_equal(1, predictPairProb(oneRow(train_data, 1),
                                 oneRow(train_data, 2), model))
  expect_equal(0, predictPairProb(oneRow(train_data, 2),
                                 oneRow(train_data, 1), model))
})

test_that("logRegModel predictPairProb 2x3 fit train_data 2nd cue useless", {
  train_data <- cbind(y=c(5,4), x1=c(1,0), x2=c(1,1))
  model <- logRegModel(train_data, 1, c(2,3))
  expect_equal(1, predictPairProb(oneRow(train_data, 1),
                                 oneRow(train_data, 2), model))
  expect_equal(0, predictPairProb(oneRow(train_data, 2),
                                 oneRow(train_data, 1), model))
})

test_that("logRegModel predictPairProb 2x3 fit train_data 2nd cue reverse", {
  train_data <- cbind(y=c(5,4), x1=c(1,0), x2=c(0,1))
  model <- logRegModel(train_data, 1, c(2,3))
  expect_equal(1, predictPairProb(oneRow(train_data, 1),
                                 oneRow(train_data, 2), model))
  expect_equal(0, predictPairProb(oneRow(train_data, 2),
                                 oneRow(train_data, 1), model))
})

test_that("logRegModel predictPairProb 2x3 fit train_data 1st cue useless", {
  train_data <- cbind(y=c(5,4), x1=c(0,0), x2=c(1,0))
  model <- logRegModel(train_data, 1, c(2,3))
  expect_equal(1, predictPairProb(oneRow(train_data, 1),
                                 oneRow(train_data, 2), model))
  expect_equal(0, predictPairProb(oneRow(train_data, 2),
                                 oneRow(train_data, 1), model))
})

test_that("logRegModel percentCorrectList", {
  tol <- 0.0001
  train_data <- cbind(y=c(5:1), x=c(1,1,1,0,1))
  model <- logRegModel(train_data, 1, c(2))
  fit_accuracy <- percentCorrectList(train_data, list(model))
  expect_equal(60, fit_accuracy$logRegModel, tolerance=0.01)
})

test_that("logRegModel predictPairProb 2x2 data.frame", {
  train_data <- data.frame(y=c(5,4), x1=c(1,0))
  model <- logRegModel(train_data, 1, c(2))
  expect_equal(1, predictPairProb(oneRow(train_data, 1),
                                 oneRow(train_data, 2), model))
  expect_equal(0, predictPairProb(oneRow(train_data, 2),
                                 oneRow(train_data, 1), model))
  out <- rowPairApply(train_data, heuristicsProb(model))
  # There is only one unique pair.
  expect_equal(1, nrow(out))
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

# Test helper functions that allow logRegModel to better handle overspecified
# data.
test_that("rankByCueValidity", {
  df <- data.frame(Criterion=c(5,4,3,2,1), a=c(1,0,0,0,1),
                   b=c(1,1,1,0,0), c=c(1,1,1,0,1))
  # cue validities are a=0.5, b=1.0, c=0.75, so the 2nd cue should be used
  # first, then the 3rd cue, then the 1st cue.
  expect_equal(c(3,1,2), rankByCueValidity(df, 1, c(2:4)))
})

test_that("keepOrder so simple I could not mess this up, right", {
  df <- data.frame(Criterion=c(5,4,3,2,1), a=c(1,0,0,0,1),
                   b=c(1,1,1,0,0), c=c(1,1,1,0,1))
  expect_equal(c(1,2,3), keepOrder(df, 1, c(2:4)))
})

test_that("rankByCorrelation", {
  df <- data.frame(Criterion=c(5,4,3,2,1), a=c(1,0,0,0,1),
                   b=c(1,1,1,0,0), c=c(1,1,1,0,1))
  # correlations are a=0, b=0.866, c=0.354, so the 2nd cue should be used
  # first, then the 3rd cue, then the 1st cue.
  expect_equal(c(3,1,2), rankByCorrelation(df, 1, c(2:4)))
})

test_that("logRegModel predictPairProb cue order by validity", {
  train_data <- cbind(y=c(5,4), x1=c(1,1), x2=c(1,0))
  model_2_3 <- logRegModel(train_data, 1, c(2, 3))
  expect_equal(c(2,3), model_2_3$cols_to_fit)
  # The stronger cue, x2, should be ordered first.
  expect_equal(c(3,2), model_2_3$sorted_cols_to_fit)
  expect_equal(c(2,1), model_2_3$cue_ordering)

  model_3_2 <- logRegModel(train_data, 1, c(3, 2))
  # The stronger cue, x2, should be ordered first.
  expect_equal(c(3,2), model_3_2$cols_to_fit)
  
  expect_equal(1, predictPairProb(oneRow(train_data, 1),
                                 oneRow(train_data, 2), model_2_3))
  expect_equal(1, predictPairProb(oneRow(train_data, 1),
                                 oneRow(train_data, 2), model_3_2))
})

test_that("logRegModel predictPairProb cue order by validity shifted", {
  train_data <- data.frame(y=c(5,4), name=c("a", "b"), x1=c(1,1), x2=c(1,0))
  model_2_3 <- logRegModel(train_data, 1, c(3, 4))
  # The stronger cue, x2, should be ordered first.
  expect_equal(c(3,4), model_2_3$cols_to_fit)
  expect_equal(c(4,3), model_2_3$sorted_cols_to_fit)
  expect_equal(c(2,1), model_2_3$cue_ordering)
  
  expect_equal(1, predictPairProb(oneRow(train_data, 1),
                                 oneRow(train_data, 2), model_2_3))
})

test_that("logRegModel overspecified keepOrder", {
  # Cues x1 and x2 are the same in training data, equally predictive of y.
  # Either cue could be used while the other is dropped because the model is
  # overspecified (two cues for only one row).
  train_data <- cbind(y=c(5,4), x1=c(1,0), x2=c(1,0))
  model_2_3 <- logRegModel(train_data, 1, c(2, 3),
                           cue_order_fn=keepOrder)
  expect_equal(c(2 ,3), model_2_3$cols_to_fit)
  
  model_3_2 <- logRegModel(train_data, 1, c(3, 2),
                           cue_order_fn=keepOrder)
  expect_equal(c(3, 2), model_3_2$cols_to_fit)
  
  # In test data, cues x1 and x2 point in opposite direction, so the
  # prediction depends on which cue is used first.
  test_data <- cbind(y=c(5,4), x1=c(1,0), x2=c(0,1))
  
  expect_equal(1, predictPairProb(oneRow(test_data, 1),
                                  oneRow(test_data, 2), model_2_3))
  expect_equal(0, predictPairProb(oneRow(test_data, 1),
                                  oneRow(test_data, 2), model_3_2))
})

# TODO: Move tests below to a new batch test file.

test_that("logRegModel percentCorrectList easy 100%", {
  df <- data.frame(Criterion=c(5,4,3,2,1), a=c(5,4,3,2,1))
  model <- logRegModel(df, 1, c(2))
  out <- percentCorrectList(df, list(model))
  expect_equal(100, out[, "logRegModel"])
})

test_that("logRegModel percentCorrectList cue reverse easy 100%", {
  df <- data.frame(Criterion=c(5,4,3,2,1), a=c(1,2,3,4,5))
  model <- logRegModel(df, 1, c(2))
  out <- percentCorrectList(df, list(model))
  expect_equal(100, out[, "logRegModel"])
})

test_that("logRegModel percentCorrectList criterion reverse easy 100%", {
  df <- data.frame(Criterion=c(1,2,3,4,5), a=c(5,4,3,2,1))
  model <- logRegModel(df, 1, c(2))
  out <- percentCorrectList(df, list(model))
  expect_equal(100, out[, "logRegModel"])
})

## singleCueModel

test_that("singleCueModel 4x2 guess when first cue non-discriminate", {
  train_df <- data.frame(criterion=c(9,8,7,6), a=c(101,101,2,2), b=c(59,58,5,59))
  # Cue a has validity 1, cue b has validity 0.6.
  # Cue a cannot discriminate between row 1 and 2, so it will return 0.5.
  # Single cue will not use cue b to help.
  model <- singleCueModel(train_df, 1, c(2:3))
  expect_equal(c(a=1, b=0.6), model$cue_validities_unreversed, tolerance=0.002)
  expect_equal(c(a=1, b=0.6), model$cue_validities, tolerance=0.002)
  # Only the highest-validity cue gets a weight-- the rest are zeroes.
  expect_equal(c(a=1, b=0), coef(model), tolerance=0.002)
  expect_equal(0, predictPair(oneRow(train_df, 1),
                              oneRow(train_df, 2), model))
  expect_equal(0, predictPair(oneRow(train_df, 2),
                              oneRow(train_df, 1), model))
})

test_that("singleCueModel 4x3 real value cue c dominates", {
  train_df <- data.frame(criterion=c(900,400,100,6), a=c(101,101,20,101),
                         b=c(59,59,5,59), c=c(90,80,70,10))
  # Cue a and b have validity 2/3, cue c has validity 1.
  # Cue c predicts Row 3 > Row 4.
  # But if you sum all cue weights, predict Row 4 > Row 3
  model <- singleCueModel(train_df, 1, c(2:4))
  expect_equal(c(a=0.667, b=0.667, c=1), model$cue_validities_unreversed, tolerance=0.002)
  expect_equal(c(a=0.667, b=0.667, c=1), model$cue_validities, tolerance=0.002)
  # Only the highest-validity cue gets a weight-- the rest are zeroes.
  expect_equal(c(a=0, b=0, c=1), coef(model), tolerance=0.002)
  expect_equal(1, predictPair(oneRow(train_df, 3),
                              oneRow(train_df, 4), model))
  expect_equal(-1, predictPair(oneRow(train_df, 4),
                               oneRow(train_df, 3), model))
})

test_that("singleCueModel 4x3 real value cue c dominates after reversal", {
  train_df <- data.frame(criterion=c(900,400,100,6), a=c(101,101,20,101), b=c(59,59,5,59),
                         c=c(10,70,80,90))
  # Cue a and b have validity 2/3, cue c has validity 0, reversed to 1.
  # Cue c predicts Row 3 > Row 4.
  # But if you sum all cue weights, predict Row 4 > Row 3
  model <- singleCueModel(train_df, 1, c(2:4))
  expect_equal(c(a=0.667, b=0.667, c=0), model$cue_validities_unreversed, tolerance=0.002)
  expect_equal(c(a=0.667, b=0.667, c=1), model$cue_validities, tolerance=0.002)
  # Only the highest-validity cue gets a weight-- the rest are zeroes.
  expect_equal(c(a=0, b=0, c=-1), coef(model), tolerance=0.002)
  
  expect_equal(1, predictPair(oneRow(train_df, 3),
                              oneRow(train_df, 4), model))
  expect_equal(-1, predictPair(oneRow(train_df, 4),
                               oneRow(train_df, 3), model))
})


test_that("singleCueModel 4x3 real value cue a and b have same validity", {
  train_df <- data.frame(criterion=c(900,400,100,6), a=c(101,101,20,101), b=c(59,59,5,59))
  # Cue a and b have validity 2/3, the model should pick one cue at random rather than using both
  model <- singleCueModel(train_df, 1, c(2:3))

  expect_equal(1, sum(coef(model)))
})

## minModel

test_that("minModel predictPair 2x2 forward", {
  train_data <- cbind(y=c(5,4), x1=c(1,0))
  model <- minModel(train_data, 1, c(2))
  expect_equal(1, predictPair(oneRow(train_data, 1),
                              oneRow(train_data, 2), model))
  expect_equal(-1, predictPair(oneRow(train_data, 2),
                               oneRow(train_data, 1), model))
})

test_that("minModel predictPair 2x2 reverse cue", {
  train_data <- cbind(y=c(5,4), x1=c(0,1))
  model <- minModel(train_data, 1, c(2))
  expect_equal(1, predictPair(oneRow(train_data, 1),
                              oneRow(train_data, 2), model))
  expect_equal(-1, predictPair(oneRow(train_data, 2),
                               oneRow(train_data, 1), model))
})

test_that("minModel predictPair 5x4 all cues same after reverse", {
  train_data <- cbind(y=c(5,4,3,2,1), x1=c(1,0,0,0,0), x2=c(1,0,0,0,0),
                      x3=c(0,1,1,1,1))
  model <- minModel(train_data, 1, c(2:4))
  expect_equal(c(x1=1, x2=1, x3=-1), model$cue_directions)
  # Note x3 is same when reversed.
  # Gives same answer consistently, no matter which cue is selected.
  for (i in 1:5) {
    expect_equal(1, predictPair(oneRow(train_data, 1),
                                oneRow(train_data, 2), model))
  }
})

test_that("minModel predictPair 5x4 cue_sample_fn in_order", {
  train_data <- cbind(y=c(5,4,3,2,1), x1=c(1,0,0,0,0), x2=c(1,1,0,0,1),
                      x3=c(1,0,0,0,1))
  model <- minModel(train_data, 1, c(2:4))
  # For testing purposes, force cue order to always be x1, x2, x3.
  in_order <- function(x) return(x)
  model$cue_sample_fn <- in_order
  expect_equal(c(x1=1, x2=1, x3=0), model$cue_directions)
  expect_equal(c(x1=1, x2=0.667, x3=0.5), model$cue_validities_unreversed,
               tolerance=0.002)
  
  # Between row 1 and 2, x1 predicts 1 (x2: guess, x3: 1)
  expect_equal(1, predictPair(oneRow(train_data, 1),
                              oneRow(train_data, 2), model))

  # Between row 4 and 5, x1 doesn't discriminate, then x2 predicts -1.
  # (x3: -1).
  expect_equal(-1, predictPair(oneRow(train_data, 4),
                               oneRow(train_data, 5), model))
})

test_that("minModel predictPair 5x4 cue_sample_fn reverse_order", {
  train_data <- cbind(y=c(5,4,3,2,1), x1=c(1,0,0,0,0), x2=c(1,1,0,0,1),
                      x3=c(1,0,0,0,1))
  model <- minModel(train_data, 1, c(2:4))
  # For testing purposes, force cue order to always be x3, x2, x1.
  model$cue_sample_fn <- rev
  expect_equal(c(x1=1, x2=1, x3=0), model$cue_directions)
  expect_equal(c(x1=1, x2=0.667, x3=0.5), model$cue_validities_unreversed,
               tolerance=0.002)
  
  # Because cue_sample_fn is reverse, start from x3.
  # Between row 1 and 2, x3 predicts 1 (x1: 1, x2: guess)
  expect_equal(1, predictPair(oneRow(train_data, 1),
                              oneRow(train_data, 2), model))
  
  # Between row 4 and 5, x3 predicts -1 (x1: guess, x2: -1)
  expect_equal(-1, predictPair(oneRow(train_data, 4),
                               oneRow(train_data, 5), model))
})
