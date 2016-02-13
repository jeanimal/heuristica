context("heuristics")

# require('testthat')


test_that("predictRowPair error does not have row dimension", {
  train_matrix <- matrix(c(5,4,1,0,0,1), 2, 3)
  model <- ttbModel(train_matrix, 1, c(2,3))
  expect_error(predictRowPair(train_matrix[1,], train_matrix[2,], model),
               "Object does not have row dimension")
})

test_that("predictRowPair error too many rows", {
  train_matrix <- matrix(c(5,4,1,0,0,1), 2, 3)
  model <- ttbModel(train_matrix, 1, c(2,3))
  expect_error(predictRowPair(train_matrix[c(1:2),], train_matrix[2,], model),
               "Expected a single row but got 2 rows")
})

# Variations with data taypes

test_that("ttbModel 2x3 predictRowPair forward data.frame", {
  train_df <- data.frame(y=c(5,4), x1=c(1,0), x2=c(0,1))
  model <- ttbModel(train_df, 1, c(2,3))
  expect_equal(c(1,0), unname(model$cue_validities))
  # The probability that row 1 > row 2 is 1.
  expect_equal(1, predictRowPair(oneRow(train_df, 1),
                                 oneRow(train_df, 2), model))
  # The logical opposite: the probability that row2 > row 1 is 0.
  expect_equal(0, predictRowPair(oneRow(train_df, 2),
                                 oneRow(train_df, 1), model))
})

test_that("allRowPairApply ttb test: matrix, 2 rows = 1 pair", {
  train_matrix <- matrix(c(5,4,1,0,0,1), 2, 3)
  ttb <- ttbModel(train_matrix, 1, c(2,3))

  out1 <- allRowPairApply(train_matrix, heuristics(ttb))
  # output should look like
  #      ttbModel
  # [1,]        1
  expect_equal(cbind(ttbModel=c(1)), out1)

  out2 <- allRowPairApply(train_matrix, heuristics(ttb, ttb))
  expected_out2 <- matrix(c(1,1), 1, 2, dimnames=list(NULL, c("ttbModel", "ttbModel")))
  #           ttbModel ttbModel
  # [1,]        1        1
  expect_equal(expected_out2, out2)
})


# ttbModel on binary cues

test_that("ttbModel 2x3 predictRowPair forward", {
  train_matrix <- matrix(c(5,4,1,0,0,1), 2, 3)
  model <- ttbModel(train_matrix, 1, c(2,3))
  expect_equal(c(1,0), model$cue_validities)
  # The probability that row 1 > row 2 is 1.
  expect_equal(1, predictRowPair(oneRow(train_matrix, 1),
                                 oneRow(train_matrix, 2), model))
  # The logical opposite: the probability that row2 > row 1 is 0.
  expect_equal(0, predictRowPair(oneRow(train_matrix, 2),
                                 oneRow(train_matrix, 1), model))
})

test_that("ttbModel 2x3 predictRowPair test_matrix backward cues", {
  train_matrix <- cbind(y=c(5,4), x1=c(1,0), x2=c(0,1))
  model <- ttbModel(train_matrix, 1, c(2,3))
  expect_equal(c(1,0), model$cue_validities)
  # Cues in test_data below have been reversed.
  # So predictions should be reversed.
  test_matrix <- cbind(y=c(5,4), x1=c(0,1), x2=c(1,0))
  expect_equal(0, predictRowPair(oneRow(test_matrix, 1),
                                 oneRow(test_matrix, 2), model))
  expect_equal(1, predictRowPair(oneRow(test_matrix, 2),
                                 oneRow(test_matrix, 1), model))
})

test_that("ttbModel 2x3 predictRowPair test_matrix backward criterion", {
  train_matrix <- cbind(y=c(5,4), x1=c(1,0), x2=c(0,1))
  model <- ttbModel(train_matrix, 1, c(2,3))
  expect_equal(c(1,0), model$cue_validities)
  # The criterion in test_data below has been reversed.
  # It should be ignored-- continue to use the validities from train_matrix.
  test_matrix <- cbind(y=c(4,5), x1=c(1,0), x2=c(0,1))
  expect_equal(1, predictRowPair(oneRow(test_matrix, 1),
                                 oneRow(test_matrix, 2), model))
  expect_equal(0, predictRowPair(oneRow(test_matrix, 2),
                                 oneRow(test_matrix, 1), model))
})

test_that("ttbModel 2x2 predictRowPair cue_reversal", {
  train_matrix <- cbind(y=c(5,4), x1=c(0,1))
  model <- ttbModel(train_matrix, 1, c(2))
  expect_equal(c(0), model$cue_validities)
  expect_equal(1, predictRowPair(oneRow(train_matrix, 1),
                                 oneRow(train_matrix, 2), model))
  expect_equal(0, predictRowPair(oneRow(train_matrix, 2),
                                 oneRow(train_matrix, 1), model))
})

test_that("ttbModel 3x3 predictRowPair forward", {
  train_matrix <- cbind(y=c(5,4,3), x1=c(1,0,1), x2=c(1,0,0))
  model <- ttbModel(train_matrix, 1, c(2,3))
  expect_equal(c(0.5, 1), model$cue_validities)
  expect_equal(1, predictRowPair(oneRow(train_matrix, 1),
                                 oneRow(train_matrix, 2), model))
  expect_equal(1, predictRowPair(oneRow(train_matrix, 1),
                                 oneRow(train_matrix, 3), model))
  # Cue x1 discriminates rows 2 and 3, but validity=0.5, so not used.
  expect_equal(0.5, predictRowPair(oneRow(train_matrix, 2),
                                   oneRow(train_matrix, 3), model))
})

test_that("ttbModel 3x3 predictRowPair cue_reversal", {
  train_matrix <- cbind(y=c(5,4,3), x1=c(1,0,1), x2=c(0,0,1))
  model <- ttbModel(train_matrix, 1, c(2,3))
  expect_equal(c(0.5, 0), model$cue_validities)
  # x1 discriminates but has 0.5 validity.
  # x2 does not discriminate.
  # So it's a guess = 0.5.
  expect_equal(0.5, predictRowPair(oneRow(train_matrix, 1),
                                   oneRow(train_matrix, 2), model))
  # Reverse the 2nd cue, and it discriminates to get these right.
  expect_equal(1, predictRowPair(oneRow(train_matrix, 2),
                                 oneRow(train_matrix, 3), model))
  expect_equal(1, predictRowPair(oneRow(train_matrix, 1),
                                 oneRow(train_matrix, 3), model))
})

test_that("ttbModel 3x3 pos pos predictRowPair forward", {
  train_matrix <- cbind(y=c(5,4,3), x1=c(1,0,0), x2=c(1,1,0))
  model <- ttbModel(train_matrix, 1, c(2,3))
  expect_equal(c(1,1),  model$cue_validities)
  
  expect_equal(1, predictRowPair(oneRow(train_matrix, 1),
                                 oneRow(train_matrix, 2), model))
  expect_equal(0, predictRowPair(oneRow(train_matrix, 2),
                                 oneRow(train_matrix, 1), model))
  
  expect_equal(1, predictRowPair(oneRow(train_matrix, 1),
                                 oneRow(train_matrix, 3), model))
  expect_equal(0, predictRowPair(oneRow(train_matrix, 3),
                                 oneRow(train_matrix, 1), model))
  
  expect_equal(1, predictRowPair(oneRow(train_matrix, 2),
                                 oneRow(train_matrix, 3), model))
  expect_equal(0, predictRowPair(oneRow(train_matrix, 3),
                                 oneRow(train_matrix, 2), model))
})

test_that("ttbModel 3x3 pos pos predictRowPair backward cues in test", {
  train_matrix <- cbind(y=c(5,4,3), x1=c(1,0,0), x2=c(1,1,0))
  model <- ttbModel(train_matrix, 1, c(2,3))
  expect_equal(c(1,1),  model$cue_validities)

  # All cues backwards relative to training data.
  test_matrix <- cbind(y=c(5,4,3), x1=c(0,1,1), x2=c(0,0,1))
  expect_equal(0, predictRowPair(oneRow(test_matrix, 1),
                                 oneRow(test_matrix, 2), model))
  expect_equal(1, predictRowPair(oneRow(test_matrix, 2),
                                 oneRow(test_matrix, 1), model))
  
  expect_equal(0, predictRowPair(oneRow(test_matrix, 1),
                                 oneRow(test_matrix, 3), model))
  expect_equal(1, predictRowPair(oneRow(test_matrix, 3),
                                 oneRow(test_matrix, 1), model))
  
  expect_equal(0, predictRowPair(oneRow(test_matrix, 2),
                                 oneRow(test_matrix, 3), model))
  expect_equal(1, predictRowPair(oneRow(test_matrix, 3),
                                 oneRow(test_matrix, 2), model))
})

test_that("ttbModel 2x2,3x2 predictRowPair", {
  train_matrix <- cbind(y=c(5,4,3), x1=c(1,0,0))
  model <- ttbModel(train_matrix, 1, c(2))
  expect_equal(c(1), model$cue_validities)
  expect_equal(1, predictRowPair(oneRow(train_matrix, 1),
                                 oneRow(train_matrix, 2), model))
  expect_equal(1, predictRowPair(oneRow(train_matrix, 1),
                                 oneRow(train_matrix, 3), model))
  expect_equal(0.5, predictRowPair(oneRow(train_matrix, 2),
                                   oneRow(train_matrix, 3), model))
})

test_that("ttbModel 4x4 predictRowPair x1 cue dominates", {
  train_data <- cbind(y=c(9,8,7,6), x1=c(1,1,1,0), x2=c(1,1,0,1),
                      x3=c(1,1,0,1))
  # How this data looks:
  # > train_data
  #         y  x1   x2   x3
  # [1,]    9    1    1    1
  # [2,]    8    1    1    1
  # [3,]    7    1    0    0
  # [4,]    6    0    1    1
  # Cue x2 has validity 1.0, cue x2 and cue x3 have validity 2/3.
  # Cue x1 predicts Row 3 > Row 4.
  # But if you sum cue weights, predict Row 4 > Row 3
  model <- ttbModel(train_data, 1, c(2:4))
  expect_equal(c(1, 0.667, 0.667), model$cue_validities, tolerance=0.002)
  expect_equal(1, predictRowPair(oneRow(train_data, 3),
                                 oneRow(train_data, 4), model))
  expect_equal(0, predictRowPair(oneRow(train_data, 4),
                                 oneRow(train_data, 3), model))
})

test_that("ttbModel 4x4 predictRowPair cue x1 dominates non-binary", {
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
  expect_equal(c(1, 0.667, 0.667), model$cue_validities, tolerance=0.002)
  expect_equal(1, predictRowPair(oneRow(train_data, 3),
                                 oneRow(train_data, 4), model))
  expect_equal(0, predictRowPair(oneRow(train_data, 4),
                                 oneRow(train_data, 3), model))
})

test_that("ttbModel 4x4 predictRowPair cue x3 dominates non-binary", {
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
  expect_equal(c(0.667, 0.667, 1), model$cue_validities, tolerance=0.002)
  expect_equal(1, predictRowPair(oneRow(train_data, 3),
                                 oneRow(train_data, 4), model))
  expect_equal(0, predictRowPair(oneRow(train_data, 4),
                                 oneRow(train_data, 3), model))
})

test_that("ttbModel 4x4 predictRowPair 3nd cue dominates non-binary reverse cue", {
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
  expect_equal(c(0.667, 0.667, 0), model$cue_validities, tolerance=0.002)
  expect_equal(1, predictRowPair(oneRow(train_data, 3),
                                 oneRow(train_data, 4), model))
  expect_equal(0, predictRowPair(oneRow(train_data, 4),
                                 oneRow(train_data, 3), model))
})

test_that("ttbModel 4x4 predictRowPair 3nd cue dominates non-binary reverse cue data.frame", {
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
  expect_equal(c(x1=0.667, x2=0.667, x3=0), model$cue_validities, tolerance=0.002)
  expect_equal(c(x1=0.667, x2=0.667, x3=1), model$cue_validities_with_reverse,
               tolerance=0.002)
  # The coefficient for column c should be negative.
  expect_equal(c(x3=-1), sign(coef(model)["x3"]), tolerance=0.002)
  expect_equal(1, predictRowPair(oneRow(train_df, 3),
                                 oneRow(train_df, 4), model))
  expect_equal(0, predictRowPair(oneRow(train_df, 4),
                                 oneRow(train_df, 3), model))
})

### ttbModel on real-valued cues ###

test_that(paste("ttbModel 4x4 predictRowPair 3nd cue dominates reverse cue data.frame",
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
  expect_equal(c(a=0.667, b=0.667, c=0), model$cue_validities, tolerance=0.002)
  expect_equal(c(a=0.667, b=0.667, c=1), model$cue_validities_with_reverse, tolerance=0.002)
  # The coefficient for column c should be negative.
  expect_equal(c(c=-1), sign(coef(model)["c"]), tolerance=0.002)
  expect_equal(1, predictRowPair(oneRow(train_df, 3),
                                 oneRow(train_df, 4), model))
  expect_equal(0, predictRowPair(oneRow(train_df, 4),
                                 oneRow(train_df, 3), model))
})

test_that(paste("ttbModel 4x4 predictRowPair 3nd cue dominates cue data.frame",
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
  expect_equal(c(c=1), sign(coef(model)["c"]), tolerance=0.002)
  expect_equal(1, predictRowPair(oneRow(train_df, 3),
                                 oneRow(train_df, 4), model))
  expect_equal(0, predictRowPair(oneRow(train_df, 4),
                                 oneRow(train_df, 3), model))
})

test_that(paste("ttbModel 4x4 predictRowPair 3nd cue dominates cue data.frame",
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
  expect_equal(c(c=1), sign(coef(model)["c"]), tolerance=0.002)
  expect_equal(1, predictRowPair(oneRow(train_df, 3),
                                 oneRow(train_df, 4), model))
  expect_equal(0, predictRowPair(oneRow(train_df, 4),
                                 oneRow(train_df, 3), model))
})

test_that(paste("ttbModel 4x4 predictRowPair 3nd cue dominates cue data.frame",
                "non-binary big criteriondiffs, big diffs, big diffs, big unique diffs"), {
  train_df <- data.frame(criterion=c(900,400,100,6), a=c(101,101,20,101), b=c(59,59,5,59),
                         c=c(90,80,70,10))
  # Cue a and b have validity 2/3, cue c has validity 1.
  # Cue c predicts Row 3 > Row 4.
  # But if you sum all cue weights, predict Row 4 > Row 3
  model <- ttbModel(train_df, 1, c(2:4))
  expect_equal(c(a=0.667, b=0.667, c=1), model$cue_validities, tolerance=0.002)
  expect_equal(c(a=0.667, b=0.667, c=1), model$cue_validities_with_reverse, tolerance=0.002)
  expect_equal(c(c=1), sign(coef(model)["c"]), tolerance=0.002)
  expect_equal(1, predictRowPair(oneRow(train_df, 3),
                                 oneRow(train_df, 4), model))
  expect_equal(0, predictRowPair(oneRow(train_df, 4),
                                 oneRow(train_df, 3), model))
})

test_that(paste("ttbModel 4x4 predictRowPair 3nd cue dominates cue data.frame REVERSE",
                "non-binary big criteriondiffs, big diffs, big diffs, big unique diffs"), {
  train_df <- data.frame(criterion=c(6,100,400,900), a=c(101,20,101,101), b=c(59,5,59,59),
                         c=c(10,70,80,90))
  # Cue a and b have validity 2/3, cue c has validity 0,
  # but that validity is 1.0 when reversed.
  # Cue c predicts Row 3 > Row 4.
  # But if you sum all cue weights, predict Row 4 > Row 3
  model <- ttbModel(train_df, 1, c(2:4))
  expect_equal(c(a=0.667, b=0.667, c=1), model$cue_validities, tolerance=0.002)
  expect_equal(c(a=0.667, b=0.667, c=1), model$cue_validities_with_reverse, tolerance=0.002)
  expect_equal(c(c=1), sign(coef(model)["c"]), tolerance=0.002)
  expect_equal(0, predictRowPair(oneRow(train_df, 3),
                                 oneRow(train_df, 4), model))
  expect_equal(1, predictRowPair(oneRow(train_df, 4),
                                 oneRow(train_df, 3), model))
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

test_that("dawesModel 4x4 predictRowPair 3nd cue dominates non-binary reverse cue", {
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
  model <- dawesModel(train_df, 1, c(2:4))
  expect_equal(c(a=0.667, b=0.667, c=0), model$cue_validities, tolerance=0.002)
  #TODO(jean): Return cue reversal.
  #expect_equal(c(a=0.667, b=0.667, c=1), model$cue_validities_with_reverse, tolerance=0.002)
  expect_equal(c(a=1, b=1, c=-1), model$linear_coef, tolerance=0.002)
  expect_equal(0, predictRowPair(oneRow(train_df, 3),
                                 oneRow(train_df, 4), model))
  expect_equal(1, predictRowPair(oneRow(train_df, 4),
                                 oneRow(train_df, 3), model))
})

### franklinModel ###

test_that("franklinModel 2x3 predictRowPair pos neg reverse_cues FALSE", {
  train_matrix <- cbind(y=c(5,4), x1=c(1,0), x2=c(0,1))
  model <- franklinModel(train_matrix, 1, c(2,3), reverse_cues=FALSE)
  expect_equal(c(1,0),  model$cue_validities) 
  expect_equal(1,  coef(model)[[1]])  
  expect_equal(0,  coef(model)[[2]])  
  expect_equal(2, length(coef(model)))
  expect_equal(1, predictRowPair(oneRow(train_matrix, 1),
                                 oneRow(train_matrix, 2), model))
})

test_that("franklinModel 2x3 predictRowPair pos neg", {
  train_matrix <- cbind(y=c(5,4), x1=c(1,0), x2=c(0,1))
  model <- franklinModel(train_matrix, 1, c(2,3))
  expect_equal(c(1,0),  model$cue_validities) 
  expect_equal(1,  coef(model)[[1]])  
  expect_equal(-1,  coef(model)[[2]])  
  expect_equal(2, length(coef(model)))
  expect_equal(1, predictRowPair(oneRow(train_matrix, 1),
                                 oneRow(train_matrix, 2), model))
})

test_that("franklinModel allRowPairApply 5x1 75", {
  train_matrix <- cbind(y=c(5,4,3,2,1), x1=c(1,1,1,0,1))
  model <- franklinModel(train_matrix, 1, c(2))
  expect_equal(c(0.75),  model$cue_validities) 
  expect_equal(c(0.75),  coef(model))
  out <- allRowPairApply(train_matrix, rowIndexes(), heuristics(model))
  
  expect_equal(0.5, getPrediction_raw(out, c(1,2)), tolerance=0.002)
  expect_equal(0.5, getPrediction_raw(out, c(1,3)), tolerance=0.002)
  expect_equal(1,   getPrediction_raw(out, c(1,4)), tolerance=0.002)
  expect_equal(0.5, getPrediction_raw(out, c(1,5)), tolerance=0.002)
  
  expect_equal(0, getPrediction_raw(out, c(4,5)), tolerance=0.002)
})

test_that("franklinModel 5x1 25 reverse_cues FALSE", {
  train_matrix <- cbind(y=c(5,4,3,2,1), x1=c(1,0,1,1,1))
  model <- franklinModel(train_matrix, 1, c(2), reverse_cues=FALSE)
  expect_equal(c(0.25),  model$cue_validities)
  # No cue reversal means cue_validities_with_reversal is NULL.
  expect_true(is.null(model$cue_validities_with_reversal))
  # No cue reversal means coefficients are same as cue validities.
  expect_equal(c(0.25),  coef(model))
  out <- allRowPairApply(train_matrix, rowIndexes(), heuristics(model))
  expect_equal(1,   getPrediction_raw(out, c(1,2)), tolerance=0.002)
  expect_equal(0.5, getPrediction_raw(out, c(1,3)), tolerance=0.002)
  expect_equal(0.5, getPrediction_raw(out, c(1,4)), tolerance=0.002)
  expect_equal(0.5, getPrediction_raw(out, c(1,5)), tolerance=0.002)
  
  expect_equal(0,   getPrediction_raw(out, c(2,3)), tolerance=0.002)
})

test_that("franklinModel 5x1 25", {
  train_matrix <- cbind(y=c(5,4,3,2,1), x1=c(1,0,1,1,1))
  # By default, reverse_cues is TRUE
  model <- franklinModel(train_matrix, 1, c(2))
  expect_equal(c(0.25),  model$cue_validities)
  # Cue reversal changes validity 0.25 to 0.75.
  expect_equal(c(0.75),  model$cue_validities_with_reverse)
  # Cue reversal changes coefficient from 0.75 to -0.75.
  expect_equal(c(-0.75),  coef(model))
  
  out <- allRowPairApply(train_matrix, rowIndexes(), heuristics(model))
  expect_equal(0,   getPrediction_raw(out, c(1,2)), tolerance=0.002)
  expect_equal(0.5, getPrediction_raw(out, c(1,3)), tolerance=0.002)
  expect_equal(0.5, getPrediction_raw(out, c(1,4)), tolerance=0.002)
  expect_equal(0.5, getPrediction_raw(out, c(1,5)), tolerance=0.002)
  
  expect_equal(1,   getPrediction_raw(out, c(2,3)), tolerance=0.002)
})

test_that("franklinModel 4x4 predictRowPair 3nd cue dominates non-binary reverse cue", {
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
  model <- franklinModel(train_df, 1, c(2:4), reverse_cues=FALSE)
  expect_equal(c(a=0.667, b=0.667, c=0), model$cue_validities, tolerance=0.002)
  # Soon: Linear coef will include reversing the cue pointed the wrong way.
  expect_equal(c(a=0.667, b=0.667, c=0), model$linear_coef, tolerance=0.002)
  
  expect_equal(0, predictRowPair(oneRow(train_df, 3),
                                 oneRow(train_df, 4), model))
  expect_equal(1, predictRowPair(oneRow(train_df, 4),
                                 oneRow(train_df, 3), model))
})

### regInterceptModel ###

test_that("regInterceptModel 2x2 fit pos slope", {
  train_matrix <- cbind(y=c(5,4), x1=c(1,0))
  model <- regInterceptModel(train_matrix, 1, c(2))
  expect_equal(4,  coef(model)[[1]])  # intercept
  expect_equal(1,  coef(model)[[2]])  # slope
  expect_equal(2, length(coef(model)))
  expect_equal(1, predictRowPair(oneRow(train_matrix, 1),
                                 oneRow(train_matrix, 2), model))
})

test_that("regInterceptModel 2x2 fit neg slope", {
  train_matrix <- cbind(y=c(5,4), x1=c(0,1))
  model <- regInterceptModel(train_matrix, 1, c(2))
  expect_equal(5,  coef(model)[[1]])  # intercept
  expect_equal(-1,  coef(model)[[2]])  # slope
  expect_equal(1, predictRowPair(oneRow(train_matrix, 1),
                                 oneRow(train_matrix, 2), model))
})

test_that("regInterceptModel 2x2 fit pos slope -- data.frame", {
  train_df <- data.frame(y=c(5,4), x1=c(1,0))
  model <- regInterceptModel(train_df, 1, c(2))
  expect_equal(4,  coef(model)[[1]])  # intercept
  expect_equal(1,  coef(model)[[2]])  # slope
  expect_equal(2, length(coef(model))) 
  expect_equal(1, predictRowPair(oneRow(train_df, 1),
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
  expect_equal(1, predictRowPair(oneRow(train_matrix, 1),
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

test_that("regInterceptModel predictRowPair with intercept (check bug)", {
  tol <- 0.0001
  m_train <- data.frame(y=c(5:1), x1=c(1,1,1,0,1))
  model <- regInterceptModel(m_train, 1, c(2))
  # Reg cannot distinguish between rows 1 and 2 based on x1.
  # But in the past there was a bug where the intercept weight was
  # applied to the criterion column so reg was always correct!
  expect_equal(0.5, predictRowPair(oneRow(m_train, 1),
                                   oneRow(m_train, 2), model))
})

# Warning: Not a self-contained test.  Uses city_population.
test_that("regInterceptModel predictRowPair city_population", {
  tol <- 0.0001
  model <- regInterceptModel(city_population, 3, c(4:ncol(city_population)))
  # Hamburg (row 2) and Munich (row 3) differ only on the license plate, which has a
  # coefficient of about 25,000.
  # There is an intercept of 75k, but you can ignore it in pairs.
  # So because Hamburg does not have a license plate, the prob it has a greater
  # population should be zero.
  expect_equal(0, predictRowPair(oneRow(city_population, 2),
                                 oneRow(city_population, 3), model))
})

### regModel ###

test_that("regModel predictRowPair", {
  tol <- 0.0001
  m_train <- data.frame(y=c(5:1), x1=c(1,1,1,0,1))
  model <- regModel(m_train, 1, c(2))
  # Reg cannot distinguish between rows 1 and 2 based on x1.
  expect_equal(0.5, predictRowPair(oneRow(m_train, 1),
                                   oneRow(m_train, 2), model))
  # But this should be predicted correctly.
  expect_equal(1, predictRowPair(oneRow(m_train, 1),
                                 oneRow(m_train, 4), model))
})

### logRegModel ###

test_that("logRegModel predictRowPair 2x2 fit train_data", {
  tol <- 0.0001
  train_data <- cbind(y=c(5,4), x1=c(1,0))
  model <- logRegModel(train_data, 1, c(2))
  expect_equal(1, predictRowPair(oneRow(train_data, 1),
                                 oneRow(train_data, 2), model))
  expect_equal(0, predictRowPair(oneRow(train_data, 2),
                                 oneRow(train_data, 1), model))
  out <- allRowPairApply(train_data, heuristics(model))
  # There is only one unique pair.
  expect_equal(1, nrow(out))
})

test_that("logRegModel predictRowPair 2x2 fit train_data reverse cue", {
  tol <- 0.0001
  train_data <- cbind(y=c(5,4), x1=c(1,0))
  model <- logRegModel(train_data, 1, c(2))
  expect_equal(1, predictRowPair(oneRow(train_data, 1),
                                 oneRow(train_data, 2), model))
  expect_equal(0, predictRowPair(oneRow(train_data, 2),
                                 oneRow(train_data, 1), model))
})

test_that("logRegModel predictRowPair 2x2,3x2 all correct", {
  tol <- 0.0001
  train_data <- cbind(y=c(5,4), x1=c(1,0))
  model <- logRegModel(train_data, 1, c(2))
  test_data <- cbind(y=c(5,4,3), x1=c(1,0,0))
  
  expect_equal(1, predictRowPair(oneRow(test_data, 1),
                                 oneRow(test_data, 2), model))
  expect_equal(0, predictRowPair(oneRow(test_data, 2),
                                 oneRow(test_data, 1), model))
  expect_equal(1, predictRowPair(oneRow(test_data, 1),
                                 oneRow(test_data, 3), model))
  expect_equal(0, predictRowPair(oneRow(test_data, 3),
                                 oneRow(test_data, 1), model))
  # Row 2 and 3 have same cue values, so Row1 is equally likely to be greater.
  expect_equal(0.5, predictRowPair(oneRow(test_data, 2),
                                   oneRow(test_data, 3), model))
  expect_equal(0.5, predictRowPair(oneRow(test_data, 3),
                                   oneRow(test_data, 2), model))
  
  out <- allRowPairApply(test_data, heuristics(model))
  # There are three unique pairs.
  expect_equal(3, nrow(out))
})

test_that("logRegModel predictRowPair 2x2,3x2 all incorrect", {
  tol <- 0.0001
  train_data <- cbind(y=c(5,4), x1=c(1,0))
  model <- logRegModel(train_data, 1, c(2))
  test_data <- cbind(y=c(5,4,3), x1=c(0,1,1))
  
  out <- allRowPairApply(test_data, rowIndexes(), heuristics(model))
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

test_that("logRegModel predictRowPair 2x3 fit train_data", {
  train_data <- cbind(y=c(5,4), x1=c(1,0), x2=c(1,0))
  model <- logRegModel(train_data, 1, c(2,3))
  expect_equal(1, predictRowPair(oneRow(train_data, 1),
                                 oneRow(train_data, 2), model))
  expect_equal(0, predictRowPair(oneRow(train_data, 2),
                                 oneRow(train_data, 1), model))
})

## logRegData ##

test_that("logRegData simple", {
  train_data <- cbind(y=c(5,4), x1=c(1,0), x2=c(1,1))
  out <- logRegData(train_data, 1, c(2,3), rowDiff)
  # Check criterion
  expect_equal(c(1,0), out[,1])
  # Check predictors
  expect_equal(c(1,-1), out[,2])
  expect_equal(c(0,0), out[,3])
  # Check column names
  expect_equal(c("y", "x1", "x2"), colnames(out))
})

test_that("logRegData data.frame", {
  train_data <- data.frame(y=c(5,4), x1=c(1,0), x2=c(1,1))
  out <- logRegData(train_data, 1, c(2,3), rowDiff)
  # Check criterion
  expect_equal(c(1,0), out[,1])
  # Check predictors
  expect_equal(c(1,-1), out[,2])
  expect_equal(c(0,0), out[,3])
  # Check column names
  expect_equal(c("y", "x1", "x2"), colnames(out))
})

test_that("logRegModel predictRowPair 2x3 fit train_data 2nd cue useless", {
  train_data <- cbind(y=c(5,4), x1=c(1,0), x2=c(1,1))
  model <- logRegModel(train_data, 1, c(2,3))
  expect_equal(1, predictRowPair(oneRow(train_data, 1),
                                 oneRow(train_data, 2), model))
  expect_equal(0, predictRowPair(oneRow(train_data, 2),
                                 oneRow(train_data, 1), model))
})

test_that("logRegModel predictRowPair 2x3 fit train_data 2nd cue reverse", {
  train_data <- cbind(y=c(5,4), x1=c(1,0), x2=c(0,1))
  model <- logRegModel(train_data, 1, c(2,3))
  expect_equal(1, predictRowPair(oneRow(train_data, 1),
                                 oneRow(train_data, 2), model))
  expect_equal(0, predictRowPair(oneRow(train_data, 2),
                                 oneRow(train_data, 1), model))
})

test_that("logRegModel predictRowPair 2x3 fit train_data 1st cue useless", {
  train_data <- cbind(y=c(5,4), x1=c(0,0), x2=c(1,0))
  model <- logRegModel(train_data, 1, c(2,3))
  expect_equal(1, predictRowPair(oneRow(train_data, 1),
                                 oneRow(train_data, 2), model))
  expect_equal(0, predictRowPair(oneRow(train_data, 2),
                                 oneRow(train_data, 1), model))
})

test_that("logRegModel pctCorrectOfPredictPair", {
  tol <- 0.0001
  train_data <- cbind(y=c(5:1), x=c(1,1,1,0,1))
  model <- logRegModel(train_data, 1, c(2))
  fit_accuracy <- pctCorrectOfPredictPair(list(model), train_data)
  expect_equal(0.55, fit_accuracy$logRegModel, tolerance=0.001)
})

test_that("logRegModel predictRowPair 2x2 data.frame", {
  train_data <- data.frame(y=c(5,4), x1=c(1,0))
  model <- logRegModel(train_data, 1, c(2))
  expect_equal(1, predictRowPair(oneRow(train_data, 1),
                                 oneRow(train_data, 2), model))
  expect_equal(0, predictRowPair(oneRow(train_data, 2),
                                 oneRow(train_data, 1), model))
  out <- allRowPairApply(train_data, heuristics(model))
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


## singleCueModel

test_that("singleCueModel 4x2 guess when first cue non-discriminate", {
  train_df <- data.frame(criterion=c(9,8,7,6), a=c(101,101,2,2), b=c(59,58,5,59))
  # Cue a has validity 1, cue b has validity 0.6.
  # Cue a cannot discriminate between row 1 and 2, so it will return 0.5.
  # Single cue will not use cue b to help.
  model <- singleCueModel(train_df, 1, c(2:3))
  expect_equal(c(a=1, b=0.6), model$cue_validities, tolerance=0.002)
  expect_equal(c(a=1, b=0.6), model$cue_validities_with_reverse, tolerance=0.002)
  # Only the highest-validity cue gets a weight-- the rest are zeroes.
  expect_equal(c(a=1, b=0), coef(model), tolerance=0.002)
  expect_equal(0.5, predictRowPair(oneRow(train_df, 1),
                                   oneRow(train_df, 2), model))
  expect_equal(0.5, predictRowPair(oneRow(train_df, 2),
                                   oneRow(train_df, 1), model))
})

test_that("singleCueModel 4x3 real value cue c dominates", {
  train_df <- data.frame(criterion=c(900,400,100,6), a=c(101,101,20,101),
                         b=c(59,59,5,59), c=c(90,80,70,10))
  # Cue a and b have validity 2/3, cue c has validity 1.
  # Cue c predicts Row 3 > Row 4.
  # But if you sum all cue weights, predict Row 4 > Row 3
  model <- singleCueModel(train_df, 1, c(2:4))
  expect_equal(c(a=0.667, b=0.667, c=1), model$cue_validities, tolerance=0.002)
  expect_equal(c(a=0.667, b=0.667, c=1), model$cue_validities_with_reverse, tolerance=0.002)
  # Only the highest-validity cue gets a weight-- the rest are zeroes.
  expect_equal(c(a=0, b=0, c=1), coef(model), tolerance=0.002)
  
  expect_equal(1, predictRowPair(oneRow(train_df, 3),
                                 oneRow(train_df, 4), model))
  expect_equal(0, predictRowPair(oneRow(train_df, 4),
                                 oneRow(train_df, 3), model))
})

test_that("singleCueModel 4x3 real value cue c dominates after reversal", {
  train_df <- data.frame(criterion=c(900,400,100,6), a=c(101,101,20,101), b=c(59,59,5,59),
                         c=c(10,70,80,90))
  # Cue a and b have validity 2/3, cue c has validity 0, reversed to 1.
  # Cue c predicts Row 3 > Row 4.
  # But if you sum all cue weights, predict Row 4 > Row 3
  model <- singleCueModel(train_df, 1, c(2:4))
  expect_equal(c(a=0.667, b=0.667, c=0), model$cue_validities, tolerance=0.002)
  expect_equal(c(a=0.667, b=0.667, c=1), model$cue_validities_with_reverse, tolerance=0.002)
  # Only the highest-validity cue gets a weight-- the rest are zeroes.
  expect_equal(c(a=0, b=0, c=-1), coef(model), tolerance=0.002)
  
  expect_equal(1, predictRowPair(oneRow(train_df, 3),
                                 oneRow(train_df, 4), model))
  expect_equal(0, predictRowPair(oneRow(train_df, 4),
                                 oneRow(train_df, 3), model))
})

## minModel

test_that("minModel predictRowPair 2x2 forward", {
  train_data <- cbind(y=c(5,4), x1=c(1,0))
  model <- minModel(train_data, 1, c(2))
  expect_equal(1, predictRowPair(oneRow(train_data, 1),
                                 oneRow(train_data, 2), model))
  expect_equal(0, predictRowPair(oneRow(train_data, 2),
                                 oneRow(train_data, 1), model))
})

test_that("minModel predictRowPair 2x2 reverse cue", {
  train_data <- cbind(y=c(5,4), x1=c(0,1))
  model <- minModel(train_data, 1, c(2))
  expect_equal(1, predictRowPair(oneRow(train_data, 1),
                                 oneRow(train_data, 2), model))
  expect_equal(0, predictRowPair(oneRow(train_data, 2),
                                 oneRow(train_data, 1), model))
})

test_that("minModel predictRowPair 5x4 all cues same after reverse", {
  train_data <- cbind(y=c(5,4,3,2,1), x1=c(1,0,0,0,0), x2=c(1,0,0,0,0),
                      x3=c(0,1,1,1,1))
  model <- minModel(train_data, 1, c(2:4))
  expect_equal(c(1,1,-1), model$cue_directions)
  # Note x3 is same when reversed.
  # Gives same answer consistently, no matter which cue is selected.
  for (i in 1:5) {
    expect_equal(1, predictRowPair(oneRow(train_data, 1),
                                   oneRow(train_data, 2), model))
  }
})

test_that("minModel predictRowPair 5x4 cue_sample_fn in_order", {
  train_data <- cbind(y=c(5,4,3,2,1), x1=c(1,0,0,0,0), x2=c(1,1,0,0,1),
                      x3=c(1,0,0,0,1))
  model <- minModel(train_data, 1, c(2:4))
  # For testing purposes, force cue order to always be x1, x2, x3.
  in_order <- function(x) return(x)
  model$cue_sample_fn <- in_order
  expect_equal(c(1,1,0), model$cue_directions)
  expect_equal(c(1, 0.667, 0.5), model$cue_validities, tolerance=0.002)
  
  # Between row 1 and 2, x1 predicts 1 (others would predict 0.5)
  expect_equal(1, predictRowPair(oneRow(train_data, 1),
                                 oneRow(train_data, 2), model))

  # Between row 4 and 5, x1 doesn't discriminate, then x2 predicts 0
  expect_equal(0, predictRowPair(oneRow(train_data, 4),
                                 oneRow(train_data, 5), model))
})

test_that("minModel predictRowPair 5x4 cue_sample_fn reverse_order", {
  train_data <- cbind(y=c(5,4,3,2,1), x1=c(1,0,0,0,0), x2=c(1,1,0,0,1),
                      x3=c(1,0,0,0,1))
  model <- minModel(train_data, 1, c(2:4))
  # For testing purposes, force cue order to always be x3, x2, x1.
  reverse_order <- function(x) return(rev(x))
  model$cue_sample_fn <- reverse_order
  expect_equal(c(1,1,0), model$cue_directions)
  expect_equal(c(1, 0.667, 0.5), model$cue_validities, tolerance=0.002)
  
  # Between row 1 and 2, x3 predicts 0 (x1 would also predict 1)
  expect_equal(1, predictRowPair(oneRow(train_data, 1),
                                 oneRow(train_data, 2), model))
  
  # Between row 4 and 5, x3 predict 0, while x1 would guess.
  expect_equal(0, predictRowPair(oneRow(train_data, 4),
                                 oneRow(train_data, 5), model))
})

