context("aggregators")

# require('testthat')

test_that("getPredictionRow", {
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

test_that("initial test of everything", {
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

