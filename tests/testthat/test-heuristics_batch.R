########################################
# Batch testing of multiple heuristics #
########################################

context("heuristics_batch")

# This test is named by the cue validities of the two cues, 1.0 and 0.6.
test_10_06 <- function(model, expected, has_cv=TRUE) {
  train_df <- data.frame(criterion=c(9,8,7,6), a=c(101,101,2,2), b=c(59,58,5,59))
#    criterion   a  b
#  1         9 101 59
#  2         8 101 58
#  3         7   2  5
#  4         6   2 59
  fitted_model <- model(train_df, 1, c(2:3))
  # Checking fit only applies to some heuristics.
  if (has_cv) {
    expect_equal(c(a=1, b=0.6), fitted_model$cue_validities, tolerance=0.001)
    expect_equal(c(a=1, b=0.6), fitted_model$cue_validities_with_reverse,
                 tolerance=0.001)
  }
  # Check output.
  expect_equal(expected, predictRowPair(oneRow(train_df, 1),
                                        oneRow(train_df, 2), fitted_model))
  expect_equal(1-expected, predictRowPair(oneRow(train_df, 2),
                                          oneRow(train_df, 1), fitted_model))
}

# The correct answer is 1, but we confirm each model works as designed.
test_that("test_10_06 ttb",      {test_10_06(ttbModel,       1)})
test_that("test_10_06 singleCue",{test_10_06(singleCueModel, 0.5)})
test_that("test_10_06 dawes",    {test_10_06(dawesModel,     1)})
test_that("test_10_06 franklin", {test_10_06(franklinModel,  1)})
test_that("test_10_06 reg",      {test_10_06(regModel,       0, has_cv=FALSE)})
test_that("test_10_06 regNoI",   {test_10_06(regNoIModel,    1, has_cv=FALSE)})
#TODO(Daniel): Why does logReg get this prediction wrong?  Is it a bug?
test_that("test_10_06 logReg",   {test_10_06(logRegModel,    0, has_cv=FALSE)})


# This test is named by the cue validities of the two cues, 1.0 and 0.6,
# and "rc" means "reversed criterion."
test_00_04_rc <- function(model, expected, has_cv=TRUE) {
  train_df <- data.frame(criterion=c(6,7,8,9), a=c(101,101,2,2), b=c(59,58,5,59))
  #    criterion   a  b
  #  1         6 101 59
  #  2         7 101 58
  #  3         8   2  5
  #  4         9   2 59
  fitted_model <- model(train_df, 1, c(2:3))
  # Checking fit only applies to some heuristics.
  if (has_cv) {
    expect_equal(c(a=0, b=0.4), fitted_model$cue_validities, tolerance=0.001)
    expect_equal(c(a=1, b=0.6), fitted_model$cue_validities_with_reverse,
                 tolerance=0.001)
  }
  # Check output.
  expect_equal(expected, predictRowPair(oneRow(train_df, 1),
                                        oneRow(train_df, 2), fitted_model))
  expect_equal(1-expected, predictRowPair(oneRow(train_df, 2),
                                          oneRow(train_df, 1), fitted_model))
}

# The correct answer is 0, but we confirm each model works as designed.
test_that("test_00_04_rc ttb",      {test_00_04_rc(ttbModel,       0)})
test_that("test_00_04_rc singleCue",{test_00_04_rc(singleCueModel, 0.5)})
test_that("test_00_04_rc dawes",    {test_00_04_rc(dawesModel,     0)})
test_that("test_00_04_rc franklin", {test_00_04_rc(franklinModel,  0)})
#TODO(jean): Why do regModel and regNoI get this wrong?
test_that("test_00_04_rc reg",      {test_00_04_rc(regModel,       1, has_cv=FALSE)})
test_that("test_00_04_rc regNoI",   {test_00_04_rc(regNoIModel,    1, has_cv=FALSE)})
test_that("test_00_04_rc logReg",   {test_10_06(logRegModel,       0, has_cv=FALSE)})


test_ab_vs_c <- function(model, expected, has_cv=TRUE) {
  train_df <- data.frame(criterion=c(900,400,100,6), a=c(101,101,20,101), b=c(59,59,5,59),
                         c=c(90,80,70,10))
  # Cue a and b have validity 2/3, cue c has validity 1.
  # Cue c predicts Row 3 > Row 4.
  # But if you sum all cue weights, predict Row 4 > Row 3
  fitted_model <- model(train_df, 1, c(2:4))
  if (has_cv) {
    expect_equal(c(a=0.667, b=0.667, c=1), fitted_model$cue_validities, tolerance=0.002)
    expect_equal(c(a=0.667, b=0.667, c=1), fitted_model$cue_validities_with_reverse,
                 tolerance=0.002)
  }
  # Check prediction.
  expect_equal(expected, predictRowPair(oneRow(train_df, 3),
                                        oneRow(train_df, 4), fitted_model))
  expect_equal(1-expected, predictRowPair(oneRow(train_df, 4),
                                          oneRow(train_df, 3), fitted_model))
}

# The correct answer is 0, but we confirm each model works as designed.
test_that("test_ab_vs_c ttb",      {test_ab_vs_c(ttbModel,       1)})
test_that("test_ab_vs_c singleCue",{test_ab_vs_c(singleCueModel, 1)})
test_that("test_ab_vs_c dawes",    {test_ab_vs_c(dawesModel,     0)})
test_that("test_ab_vs_c franklin", {test_ab_vs_c(franklinModel,  0)})
test_that("test_ab_vs_c reg",      {test_ab_vs_c(regModel,       1, has_cv=FALSE)})
test_that("test_ab_vs_c regNoI",   {test_ab_vs_c(regNoIModel,    0, has_cv=FALSE)})
test_that("test_ab_vs_c logReg",   {test_ab_vs_c(logRegModel,    1, has_cv=FALSE)})


d_useless_cue_3 <- function(model, expected, has_cv=TRUE) {
  # This is based on real data where a bug was found.  Some models think the first
  # two cues are useful, but all agree the 3rd cue is useless.
  train_df <- data.frame(criterion=c(397,385,327), x1=c(99,100,85), x2=c(3.6,2.9,3.2),
                         x3=c(0,1,0))
  fitted_model <- model(train_df, 1, c(2:4))
  if (has_cv) {
    expect_equal(c(x1=0.667, x2=0.667, x3=0.5), fitted_model$cue_validities, tolerance=0.002)
    expect_equal(c(x1=0.667, x2=0.667, x3=0.5), fitted_model$cue_validities_with_reverse,
                 tolerance=0.002)
  }
  # Check prediction.
  expect_equal(expected, predictRowPair(oneRow(train_df, 1),
                                        oneRow(train_df, 2), fitted_model))
  expect_equal(1-expected, predictRowPair(oneRow(train_df, 2),
                                          oneRow(train_df, 1), fitted_model))
  
}

# The correct answer is 1, but models disagree a lot.
#TODO(jean): Find a way to test ttb and singleCue despite random order of x1 and x2.
#test_that("d_useless_cue_3 ttb",      {d_useless_cue_3(ttbModel,       #random(0,1))})
#test_that("d_useless_cue_3 singleCue",{d_useless_cue_3(singleCueModel, #random(0,1))})
test_that("d_useless_cue_3 dawes",    {d_useless_cue_3(dawesModel,     0.5)})
test_that("d_useless_cue_3 franklin", {d_useless_cue_3(franklinModel,  0.5)})
test_that("d_useless_cue_3 reg",      {d_useless_cue_3(regModel,       1, has_cv=FALSE)})
test_that("d_useless_cue_3 regNoI",   {d_useless_cue_3(regNoIModel,    0, has_cv=FALSE)})
test_that("d_useless_cue_3 logReg",   {d_useless_cue_3(logRegModel,    1, has_cv=FALSE)})
test_that("d_useless_cue_3 logRegCueDiffs",   {d_useless_cue_3(logRegModelCueDiffs,    0, has_cv=FALSE)})
# minModel
