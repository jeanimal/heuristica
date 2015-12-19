context("aggregators")

# require('testthat')

test_that("predictPairWithCorrect correctProb -1 bug", {
  fitted_ttb <- structure(list(criterion_col=1, cols_to_fit=c(2:4),
                               cue_validities=c(1, 0.8, 0.7),
                               cue_validities_with_reverse=c(1, 0.8, 0.7),
                               linear_coef=c(4,2,1)),
                          class="ttbModel")
  # Below are rows from the fish fertility data set where we uncovered this bug.
  test_data <- data.frame(criterion=c(33200, 36184), a=c(5,3), b=c(976,1437), c=c(50, 49.74))
  results <- predictPairWithCorrect(list(fitted_ttb), test_data)
  expect_equal(data.frame(row1=c(1), row2=c(2), correctProb=c(0), ttbModel=c(1)),
               results)
})

test_that("pctCorrectOfPredictPair -1 prediction bug", {
  fitted_ttb <- structure(list(criterion_col=1, cols_to_fit=c(2:4),
                               cue_validities=c(1, 0.8, 0.7),
                               cue_validities_with_reverse=c(1, 0.8, 0.7),
                               linear_coef=c(4,2,1)),
            class="ttbModel")
  test_data <- data.frame(criterion=c(33200, 36184), a=c(5,3), b=c(976,1437), c=c(50, 49.74))
  results <- pctCorrectOfPredictPair(list(fitted_ttb), test_data)
  expect_equal(data.frame(ttbModel=c(0)), results)
})

test_that("pctCorrectOfPredictAlternative checking for -1 prediction bug", {
  fitted_ttb <- structure(list(criterion_col=1, cols_to_fit=c(2:4),
                               cue_validities=c(1, 0.8, 0.7),
                               cue_validities_with_reverse=c(1, 0.8, 0.7),
                               linear_coef=c(4,2,1)),
                          class="ttbModel")
  test_data <- data.frame(criterion=c(33200, 36184), a=c(5,3), b=c(976,1437), c=c(50, 49.74))
  results <- pctCorrectOfPredictAlternative(list(fitted_ttb), test_data)
  expect_equal(data.frame(ttbModel=c(0)), results)
})

test_that("end to end test ttb vs. logistic regression", {
  train_data <- matrix(c(5,4,3,1,0,0), 3, 2)
  ttb <- ttbModel(train_data, 1, c(2))
  lreg <- logRegModel(train_data, 1, c(2))
  pred_df <- predictPairWithCorrect(list(ttb, lreg),
                                     train_data)
  row <- getPredictionRowLC(pred_df, row1=1, row2=2)
  expect_equal(1, row$ttbModel, tolerance=0.001)
  expect_equal(1, row$logRegModel, tolerance=0.001)
  row <- getPredictionRowLC(pred_df, row1=2, row2=3)
  expect_equal(0.5, row$ttbModel, tolerance=0.001)
  expect_equal(0.5, row$logRegModel, tolerance=0.001)
  expect_equal(3, nrow(pred_df))
  
  pct_correct_df <- pctCorrectOfPredictPair(list(ttb, lreg),
                                            train_data)
  #expect_equal(c("ttbModel", "logRegModel"), names(pct_correct_df))
  expect_equal(0.8333, pct_correct_df$ttbModel, tolerance=0.001)
  expect_equal(0.8333, pct_correct_df$logRegModel, tolerance=0.001)
  expect_equal(1, nrow(pct_correct_df))
})

test_that("end to end test ttb vs. logistic regression data.frame", {
  train_df <- data.frame(y=c(5,4,3), x=c(1,0,0), name=c("jo", "bo", "da"))
  ttb <- ttbModel(train_df, 1, c(2))
  lreg <- logRegModel(train_df, 1, c(2))
  pred_df <- predictPairWithCorrect(list(ttb, lreg),
                                    train_df)
  row <- getPredictionRowLC(pred_df, row1=1, row2=2)
  expect_equal(1, row$ttbModel, tolerance=0.001)
  expect_equal(1, row$logRegModel, tolerance=0.001)
  row <- getPredictionRowLC(pred_df, row1=2, row2=3)
  expect_equal(0.5, row$ttbModel, tolerance=0.001)
  expect_equal(0.5, row$logRegModel, tolerance=0.001)
  expect_equal(3, nrow(pred_df))
  
  pct_correct_df <- pctCorrectOfPredictPair(list(ttb, lreg),
                                            train_df)
  #expect_equal(c("ttbModel", "logRegModel"), names(pct_correct_df))
  expect_equal(0.8333, pct_correct_df$ttbModel, tolerance=0.001)
  expect_equal(0.8333, pct_correct_df$logRegModel, tolerance=0.001)
  expect_equal(1, nrow(pct_correct_df))
})

test_that("initial test of everything old", {
  train_data <- matrix(c(5,4,3,1,0,0), 3, 2)
  ttb <- ttbBinModel(train_data, 1, c(2))
  lreg <- logRegModel(train_data, 1, c(2))
  pred_df <- predictAlternativeWithCorrect(list(ttb, lreg),
                                           train_data)
  row <- getPredictionRow(pred_df, row1=1, row2=2)
  expect_equal(1, row$ttbBinModel, tolerance=0.001)
  expect_equal(1, row$logRegModel, tolerance=0.001)
  row <- getPredictionRow(pred_df, row1=2, row2=3)
  expect_equal(0.5, row$ttbBinModel, tolerance=0.001)
  expect_equal(0.5, row$logRegModel, tolerance=0.001)
  expect_equal(6, nrow(pred_df))

  pct_correct_df <- pctCorrectOfPredictAlternative(list(ttb, lreg),
                                                   train_data)
  expect_equal(c("ttbBinModel", "logRegModel"),
               names(pct_correct_df))
  expect_equal(0.8333, pct_correct_df$ttbBinModel, tolerance=0.001)
  expect_equal(0.8333, pct_correct_df$logRegModel, tolerance=0.001)
  expect_equal(1, nrow(pct_correct_df))
})

test_that("getPredictionRow all good", {
  pred_df <- data.frame(Row1=c(1,2), Row2=c(2,1), A=c(10,11), B=c(30, 31))
#  Row1 Row2  A   B
#  1    1    2 10 30
#  2    2    1 11 31
  row <- getPredictionRow(pred_df, row1=1, row2=2)
  expect_equal(10, row$A, tolerance=0.001)
  expect_equal(30, row$B, tolerance=0.001)
  row <- getPredictionRow(pred_df, row1=2, row2=1)
  expect_equal(11, row$A, tolerance=0.001)
  expect_equal(31, row$B, tolerance=0.001)
})

# TODO(jean): Test error conditions, e.g. fail to set row1 or row2
