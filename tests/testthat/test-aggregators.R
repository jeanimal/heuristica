context("aggregators")

# require('testthat')

#TODO: break this up into several independent tests.

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

