context("aggregators")

# require('testthat')

# stopIfNonProbability
# When functions work, we test within range worked, so here we only
# need to test that outside of range works.
test_that("stopIfNonProbability ", {
  test_data <- cbind(x1=c(0.1,0.9), x2=c(1.1,0.5), c3=c(0.5,0.5), c4=c(-0.001, 0.2))
  stopIfNonProbability(test_data, c(1,3))
  expect_error(stopIfNonProbability(test_data, c(1,2)), "<= 1 are not all TRUE")
  expect_error(stopIfNonProbability(test_data, c(1,4)), ">= 0 are not all TRUE")
})

test_that("predictPairWithCorrect ProbGreater -1 bug", {
  # Specify enough of a ttb model for prediction.
  fitted_ttb <- structure(list(criterion_col=1, cols_to_fit=c(2:4),
                               linear_coef=c(4,2,1)),
                          class="ttbModel")
  # Below are rows from the fish fertility data set where we uncovered this bug.
  test_data <- data.frame(criterion=c(33200, 36184), a=c(5,3), b=c(976,1437), c=c(50, 49.74))
  results <- predictPairWithCorrect(list(fitted_ttb), test_data)
  expect_equal(cbind(Row1=c(1), Row2=c(2), ProbGreater=c(0), ttbModel=c(1)),
               results)
})

test_that("pctCorrectOfPredictPair -1 prediction bug", {
  # Specify enough of a ttb model for prediction.
  fitted_ttb <- structure(list(criterion_col=1, cols_to_fit=c(2:4),
                               linear_coef=c(4,2,1)),
            class="ttbModel")
  test_data <- data.frame(criterion=c(33200, 36184), a=c(5,3), b=c(976,1437), c=c(50, 49.74))
  results <- pctCorrectOfPredictPair(list(fitted_ttb), test_data)
  expect_equal(data.frame(ttbModel=c(0)), results)
})

test_that("pctCorrectOfPredictPair -1 prediction bug reverse rows", {
  # Specify enough of a ttb model for prediction.
  fitted_ttb <- structure(list(criterion_col=1, cols_to_fit=c(2:4),
                               linear_coef=c(4,2,1)),
                          class="ttbModel")
  test_data <- data.frame(criterion=rev(c(33200, 36184)), a=rev(c(5,3)),
                          b=rev(c(976,1437)), c=rev(c(50, 49.74)))
  results <- pctCorrectOfPredictPair(list(fitted_ttb), test_data)
  expect_equal(data.frame(ttbModel=c(0)), results)
})

test_that("end to end test ttb vs. logistic regression input matrix", {
  train_data <- cbind(y=c(5,4,3), x1=c(1,0,0))
  ttb <- ttbModel(train_data, 1, c(2))
  lreg <- logRegModel(train_data, 1, c(2))
  pred_df <- data.frame(predictPairWithCorrect(list(ttb, lreg), train_data))
  
  row <- pred_df[which(pred_df$Row1==1 && pred_df$Row2==2),]
  expect_equal(1, row$ttbModel, tolerance=0.001)
  expect_equal(1, row$logRegModel, tolerance=0.001)
  row <- pred_df[which(pred_df$Row1==2),]
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


# aggregatePredictPair

test_that("aggregatePredictPair 3 models", {
  data <- cbind(y=c(5,4,3), x1=c(1,0,0), x2=c(1,0,1))
  ttb <- ttbModel(data, 1, c(2:3))
  ttbG <- ttbGreedyModel(data, 1, c(2:3))
  reg <- regModel(data, 1, c(2:3))
  out <- aggregatePredictPair(list(ttb, ttbG, reg), data)
  expect_equal(cbind(ProbGreater=c(1), ttbModel=c(1), ttbGreedyModel=c(1),
                     regModel=c(1)), oneRow(out, 1))
  expect_equal(cbind(ProbGreater=c(1), ttbModel=c(1), ttbGreedyModel=c(1),
                     regModel=c(1)), oneRow(out, 2))
  expect_equal(cbind(ProbGreater=c(1), ttbModel=c(0.5), ttbGreedyModel=c(1),
                     regModel=c(0)), oneRow(out, 3))
})

test_that("aggregatePredictPair 3 models ChooseGreater", {
  data <- cbind(y=c(5,4,3), x1=c(1,0,0), x2=c(1,0,1))
  ttb <- ttbModel(data, 1, c(2:3))
  ttbG <- ttbGreedyModel(data, 1, c(2:3))
  reg <- regModel(data, 1, c(2:3))
  out <- aggregatePredictPair(list(ttb, ttbG, reg), data, goal_type="ChooseGreater")
  expect_equal(cbind(CorrectGreater=c(1), ttbModel=c(1), ttbGreedyModel=c(1),
                     regModel=c(1)), oneRow(out, 1))
  expect_equal(cbind(CorrectGreater=c(1), ttbModel=c(1), ttbGreedyModel=c(1),
                     regModel=c(1)), oneRow(out, 2))
  # This is the row that differs.  Success is still 1, but undecided is 0 not 0.5,
  # and bad is -1.
  expect_equal(cbind(CorrectGreater=c(1), ttbModel=c(0), ttbGreedyModel=c(1),
                     regModel=c(-1)), oneRow(out, 3))
})

# pctCorrectOfPredictPair

test_that("end to end test ttb vs. logistic regression input data.frame", {
  train_df <- data.frame(y=c(5,4,3), x=c(1,0,0), name=c("jo", "bo", "da"))
  ttb <- ttbModel(train_df, 1, c(2))
  lreg <- logRegModel(train_df, 1, c(2))
  pred_df <- data.frame(predictPairWithCorrect(list(ttb, lreg), train_df))

  row <- pred_df[which(pred_df$Row1==1 && pred_df$Row2==2),]
  expect_equal(1, row$ttbModel, tolerance=0.001)
  expect_equal(1, row$logRegModel, tolerance=0.001)
  row <- pred_df[which(pred_df$Row1==2),]
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

# Warning: This test is NOT self-contained.  It relies on the provided
# city_population data set.
test_that("city_population ttb vs. regression on dirty four cities", {
  ttb <- ttbModel(city_population, 3, c(4:ncol(city_population)))
  reg <- regInterceptModel(city_population, 3, c(4:ncol(city_population)))

  pct_correct_df <- pctCorrectOfPredictPair(list(ttb, reg),
                                            city_population[c(27,30,52,68),])
  expect_equal(0, pct_correct_df$ttbModel, tolerance=0.001)
  expect_equal(0, pct_correct_df$regInterceptModel, tolerance=0.001)

  # Confirm same results even with a different order of rows.
  pct_correct_df <- pctCorrectOfPredictPair(list(ttb, reg),
                                            city_population[c(68,52,30,27),])
  expect_equal(0, pct_correct_df$ttbModel, tolerance=0.001)
  expect_equal(0, pct_correct_df$regInterceptModel, tolerance=0.001)

  # Now try on reversed rows
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
