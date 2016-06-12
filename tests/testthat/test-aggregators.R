context("aggregators")

# require('testthat')

test_that("rowPairApply ProbGreater -1 bug", {
  # Specify enough of a ttb model for prediction.
  fitted_ttb <- structure(list(criterion_col=1, cols_to_fit=c(2:4),
                               linear_coef=c(4,2,1)),
                          class="ttbModel")
  # Below are rows from the fish fertility data set where we uncovered this bug.
  test_data <- data.frame(criterion=c(33200, 36184), a=c(5,3),
                          b=c(976,1437), c=c(50, 49.74))
  results <- rowPairApplyList(test_data,
                                 list(heuristics(fitted_ttb), probGreater(1)))
  expect_equal(cbind(ttbModel=c(1), ProbGreater=c(0)), results)
})

test_that("percentCorrectList -1 prediction bug", {
  # Specify enough of a ttb model for prediction.
  fitted_ttb <- structure(list(criterion_col=1, cols_to_fit=c(2:4),
                               linear_coef=c(4,2,1)),
            class="ttbModel")
  test_data <- data.frame(criterion=c(33200, 36184), a=c(5,3), b=c(976,1437),
                          c=c(50, 49.74))
  results <- percentCorrectListReturnMatrix(test_data, list(fitted_ttb))
  expect_equal(cbind(ttbModel=c(0)), results)
})

test_that("percentCorrectList -1 prediction bug reverse rows", {
  # Specify enough of a ttb model for prediction.
  fitted_ttb <- structure(list(criterion_col=1, cols_to_fit=c(2:4),
                               linear_coef=c(4,2,1)),
                          class="ttbModel")
  test_data <- data.frame(criterion=rev(c(33200, 36184)), a=rev(c(5,3)),
                          b=rev(c(976,1437)), c=rev(c(50, 49.74)))
  results <- percentCorrectListReturnMatrix(test_data, list(fitted_ttb))
  expect_equal(cbind(ttbModel=c(0)), results)
})

test_that("end to end test ttb vs. logistic regression input matrix", {
  train_data <- cbind(y=c(5,4,3), x1=c(1,0,0))
  ttb <- ttbModel(train_data, 1, c(2))
  lreg <- logRegModel(train_data, 1, c(2))
  pred <- rowPairApplyList(
    train_data, list(rowIndexes(), heuristics(ttb, lreg), correctGreater(1)))
  pred_df <- as.data.frame(pred)
  
  row <- pred_df[which(pred_df$Row1==1 && pred_df$Row2==2),]
  expect_equal(1, row$ttbModel, tolerance=0.001)
  expect_equal(1, row$logRegModel, tolerance=0.001)
  row <- pred_df[which(pred_df$Row1==2),]
  # Below are guesses, which means output of 0 for predictPair.
  expect_equal(0, row$ttbModel, tolerance=0.001)
  expect_equal(0, row$logRegModel, tolerance=0.001)
  expect_equal(3, nrow(pred_df))
  
  pct_correct_df <- percentCorrectList(train_data, list(ttb, lreg))
  #expect_equal(c("ttbModel", "logRegModel"), names(pct_correct_df))
  expect_equal(83.33, pct_correct_df$ttbModel, tolerance=0.001)
  expect_equal(83.33, pct_correct_df$logRegModel, tolerance=0.001)
  expect_equal(1, nrow(pct_correct_df))
})


# predictPairSummary

test_that("predictPairSummary 1 model", {
  data <- cbind(y=c(5,4,3), x1=c(1,0,0), x2=c(1,0,1))
  ttb <- ttbModel(data, 1, c(2:3))
  out <- predictPairSummary(data, ttb)
  expect_equal(cbind(Row1=c(1), Row2=c(2), CorrectGreater=c(1), ttbModel=c(1)),
               oneRow(out, 1))
  expect_equal(cbind(Row1=c(1), Row2=c(3), CorrectGreater=c(1), ttbModel=c(1)),
               oneRow(out, 2))
  expect_equal(cbind(Row1=c(2), Row2=c(3), CorrectGreater=c(1), ttbModel=c(0)),
               oneRow(out, 3))
})

test_that("predictPairSummary 3 models", {
  data <- cbind(y=c(5,4,3), x1=c(1,0,0), x2=c(1,0,1))
  ttb <- ttbModel(data, 1, c(2:3))
  ttbG <- ttbGreedyModel(data, 1, c(2:3))
  reg <- regModel(data, 1, c(2:3))
  out <- predictPairSummary(data, ttb, ttbG, reg)
  expect_equal(cbind(Row1=c(1), Row2=c(2), CorrectGreater=c(1), ttbModel=c(1),
                     ttbGreedyModel=c(1), regModel=c(1)), oneRow(out, 1))
  expect_equal(cbind(Row1=c(1), Row2=c(3), CorrectGreater=c(1), ttbModel=c(1),
                     ttbGreedyModel=c(1), regModel=c(1)), oneRow(out, 2))
  expect_equal(cbind(Row1=c(2), Row2=c(3), CorrectGreater=c(1), ttbModel=c(0),
                     ttbGreedyModel=c(1), regModel=c(-1)), oneRow(out, 3))
})

test_that("predictPairSummary custom model fit_name", {
  data <- cbind(y=c(5,4,3), x1=c(1,0,0), x2=c(1,0,1))
  ttb <- ttbModel(data, 1, c(2:3), reverse_cues=FALSE, fit_name="ttbNoRev")
  out <- predictPairSummary(data, ttb)
  expect_equal(colnames(out), c('Row1', 'Row2', 'CorrectGreater', 'ttbNoRev'))
})

# percentCorrect and related functions

test_that("end to end test ttb vs. logistic regression input data.frame", {
  train_df <- data.frame(y=c(5,4,3), x=c(1,0,0), name=c("jo", "bo", "da"))
  ttb <- ttbModel(train_df, 1, c(2))
  lreg <- logRegModel(train_df, 1, c(2))
  pred <- rowPairApplyList(
    train_df, list(rowIndexes(), heuristics(ttb, lreg), correctGreater(1)))
  pred_df <- data.frame(pred)

  row <- pred_df[which(pred_df$Row1==1 && pred_df$Row2==2),]
  expect_equal(1, row$ttbModel, tolerance=0.001)
  expect_equal(1, row$logRegModel, tolerance=0.001)
  row <- pred_df[which(pred_df$Row1==2),]
  expect_equal(0, row$ttbModel, tolerance=0.001)
  expect_equal(0, row$logRegModel, tolerance=0.001)
  expect_equal(3, nrow(pred_df))
  
  pct_correct_df <- percentCorrectList(train_df, list(ttb, lreg))
  expect_equal(83.33, pct_correct_df$ttbModel, tolerance=0.01)
  expect_equal(83.33, pct_correct_df$logRegModel, tolerance=0.01)
  expect_equal(1, nrow(pct_correct_df))
})

# Warning: This test is NOT self-contained.  It relies on the provided
# city_population data set.
test_that("city_population ttb vs. regression on dirty four cities", {
  ttb <- ttbModel(city_population, 3, c(4:ncol(city_population)))
  reg <- regModel(city_population, 3, c(4:ncol(city_population)))

  pct_correct_df <- percentCorrectList(city_population[c(27,30,52,68),],
                                   list(ttb, reg))
  expect_equal(0, pct_correct_df$ttbModel, tolerance=0.001)
  expect_equal(0, pct_correct_df$regModel, tolerance=0.001)

  # Confirm same results even with a different order of rows.
  pct_correct_df <- percentCorrectList(city_population[c(68,52,30,27),],
                                   list(ttb, reg))
  expect_equal(0, pct_correct_df$ttbModel, tolerance=0.001)
  expect_equal(0, pct_correct_df$regModel, tolerance=0.001)

  # ttb and reg are symmetric, so everything is the same with
  # percentCorrectListNonSymmetric
  pct_correct_df <- percentCorrectListNonSymmetric(
    city_population[c(27,30,52,68),], list(ttb, reg))
  expect_equal(0, pct_correct_df$ttbModel, tolerance=0.001)
  expect_equal(0, pct_correct_df$regModel, tolerance=0.001)
})

test_that("percentCorrectList vs percentCorrect", {
  # This uses all1Model, not exported from aggregators.  Find a better way.
  data <- cbind(y=c(4,3,2,1), x1=c(1, 1, 0, 0))

  expect_equal(
    percentCorrectList(data, list(fitted_always_1)),
    percentCorrect(data, fitted_always_1))

  expect_equal(
    percentCorrectList(data, list(fitted_always_1, fitted_always_1)),
    percentCorrect(data, fitted_always_1, fitted_always_1))
})

test_that("percentCorrectList vs percentCorrectListNonSymmetric", {
  # This uses all1Model, not exported from aggregators.  Find a better way.
  data <- cbind(y=c(4,3,2,1), x1=c(1, 1, 0, 0))

  # percentCorrectList incorrectly says this gets it all right.
  # It is incorrect because it assumes the heuristic makes symmetric
  # decisions: if it chooses A > B, it wil chose B < A, so the function
  # only checks the former of those two cases.
  pct_correct_df <- percentCorrectList(data, list(fitted_always_1))
  expect_equal(100, pct_correct_df$all1Model, tolerance=0.001)

  # percentCorrectListNonSymmetric does not assume symmetry and so
  # correctly says this heuristic is 50% correct.
  pct_correct_df <- percentCorrectListNonSymmetric(data, list(fitted_always_1))
  expect_equal(50, pct_correct_df$all1Model, tolerance=0.01)
})

test_that("percentCorrectList vs percentCorrectListNonSymmetric", {
  expect_error(percentCorrectList(data, fitted_always_1),
    "Second argument to percentCorrectList should be list but got all1Model")
})

#
# predictPairConfusionMatrix
#

test_that("predictPairConfusionMatrix 1 pair ttb correct", {
  data <- cbind(y=c(2,1), x1=c(1, 0))
  ttb <- ttbModel(data, 1, c(2))
  out <- predictPairConfusionMatrix(data, ttb)
  # Check all 4 quadrants.
  expect_equal(1, out["-1", "-1"])
  expect_equal(0, out["-1", "1"])
  expect_equal(0, out["1", "-1"])
  expect_equal(1, out["1", "1"])
  # Check dimensions.
  expect_equal(2, nrow(out))
  expect_equal(2, ncol(out))
})

test_that("predictPairConfusionMatrix 1 pair ttb wrong", {
  data <- cbind(y=c(2,1), x1=c(1, 0))
  ttb <- ttbModel(data, 1, c(2))
  # Now have it predict on data with the cue pointed the "wrong" way.
  out <- predictPairConfusionMatrix(cbind(y=c(2,1), x1=c(0, 1)), ttb)
  # Check all 4 quadrants.
  expect_equal(0, out["-1", "-1"])
  expect_equal(1, out["-1", "1"])
  expect_equal(1, out["1", "-1"])
  expect_equal(0, out["1", "1"])
  # Check dimensions.
  expect_equal(2, nrow(out))
  expect_equal(2, ncol(out))
})

test_that("predictPairConfusionMatrix 1 pair fitted_always_1", {
  data <- cbind(y=c(2,1), x1=c(1, 0))
  # fitted_always_1 always predicts 1, regardless of data.  This makes it
  # asymmetric.
  out <- predictPairConfusionMatrix(data, fitted_always_1,
                                    symmetric_model=FALSE)
  # Check all 4 quadrants.
  expect_equal(0, out["-1", "-1"])
  expect_equal(1, out["-1", "1"])
  expect_equal(0, out["1", "-1"])
  expect_equal(1, out["1", "1"])
  # Check dimensions.
  expect_equal(2, nrow(out))
  expect_equal(2, ncol(out))
})

test_that("predictPairFullConfusionMatrix 1 pair fitted_always_0", {
  data <- cbind(y=c(2,1), x1=c(1, 0))
  # fitted_always_0 always predicts 0, regardless of data.  This is
  # accidentally symmetric because the negative of 0 is 0.
  out <- predictPairFullConfusionMatrix(data, fitted_always_0)
  # In cases when correct value was -1, it guessed.
  expect_equal(c("-1"=0, "0"=1, "1"=0), out["-1",])
  # There were no cases when correct value was 0.
  expect_equal(c("-1"=0, "0"=0, "1"=0), out["0",])
  # In cases when correct value was 1, it guessed.
  expect_equal(c("-1"=0, "0"=1, "1"=0), out["1",])

  # Check dimensions-- now there are 3.
  expect_equal(3, nrow(out))
  expect_equal(3, ncol(out))
})

test_that("predictPairFullConfusionMatrix 1 pair fitted_always_0 guess expected value", {
  data <- cbind(y=c(2,1), x1=c(1, 0))
  # fitted_always_0 always predicts 0, regardless of data.  This is
  # accidentally symmetric because the negative of 0 is 0.
  out_with_guesses <- predictPairFullConfusionMatrix(data, fitted_always_0)
  out <- distributeGuessAsExpectedValue(out_with_guesses)
  # In cases when correct value was -1, it guessed and was 50% right.
  expect_equal(c("-1"=0.5, "0"=0, "1"=0.5), out["-1",])
  # There were no cases when correct value was 0.
  expect_equal(c("-1"=0, "0"=0, "1"=0), out["0",])
  # In cases when correct value was 1, it guessed and was 50% right..
  expect_equal(c("-1"=0.5, "0"=0, "1"=0.5), out["1",])
  
  # Check dimensions-- now there are 3.
  expect_equal(3, nrow(out))
  expect_equal(3, ncol(out))
})
