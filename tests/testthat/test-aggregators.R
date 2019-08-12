context("aggregators")

# require('testthat')

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
