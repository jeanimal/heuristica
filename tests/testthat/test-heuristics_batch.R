########################################
# Batch testing of multiple heuristics #
########################################

context("heuristics_batch")

# First test some invariants. #

test_cue_reorder <- function(model, has_cv=TRUE) {
  train_df <- data.frame(criterion=c(9,8,7,6), a=c(101,101,2,2), b=c(59,58,5,59))
  #    criterion   a  b
  #  1         9 101 59
  #  2         8 101 58
  #  3         7   2  5
  #  4         6   2 59
  fitted_model <- model(train_df, 1, c(2, 3))
  fitted_model_rev <- model(train_df, 1, c(3, 2))
  # Checking fit only applies to some heuristics.
  if (has_cv) {
    expect_equal(c(a=1, b=0.6), fitted_model$cue_validities, tolerance=0.001)
    expect_equal(c(b=0.6, a=1), fitted_model_rev$cue_validities, tolerance=0.001)
  }

  # Make sure models agree on outputs when reversing *cues*.
  # predictPair:
  out <- rowPairApply(train_df, heuristics(fitted_model))
  out_rev <- rowPairApply(train_df, heuristics(fitted_model_rev))
  expect_equal(out, out_rev)
  # predictPairProb:
  # out <- rowPairApply(train_df, heuristicsProb(fitted_model))
  # out_rev <- rowPairApply(train_df, heuristicsProb(fitted_model_rev))
  # expect_equal(out, out_rev)
}

test_that("test_cue_reorder ttb",      {test_cue_reorder(ttbModel)})
test_that("test_cue_reorder ttbGreedy",{test_cue_reorder(ttbGreedyModel, has_cv=FALSE)})
test_that("test_cue_reorder singleCue",{test_cue_reorder(singleCueModel)})
test_that("test_cue_reorder dawes",    {test_cue_reorder(unitWeightModel)})
test_that("test_cue_reorder validityWeight", {test_cue_reorder(validityWeightModel)})
test_that("test_cue_reorder reg",      {test_cue_reorder(regModel, has_cv=FALSE)})
test_that("test_cue_reorder regIntercept", {test_cue_reorder(regInterceptModel, has_cv=FALSE)})
test_that("test_cue_reorder logReg",   {test_cue_reorder(logRegModel, has_cv=FALSE)})

test_row_reorder <- function(model, has_cv=TRUE) {
  train_df <- data.frame(criterion=c(9,8,7,6), a=c(101,101,2,2), b=c(59,58,5,59))
  #    criterion   a  b
  #  1         9 101 59
  #  2         8 101 58
  #  3         7   2  5
  #  4         6   2 59
  fitted_model <- model(train_df, 1, c(2:3))
  # Reverse the rows
  train_df_rev <- train_df[c(4:1),]
  fitted_model_rev <- model(train_df_rev, 1, c(2:3))
  # Checking fit only applies to some heuristics.
  if (has_cv) {
    expect_equal(c(a=1, b=0.6), fitted_model$cue_validities, tolerance=0.001)
    expect_equal(c(a=1, b=0.6), fitted_model_rev$cue_validities, tolerance=0.001)
  }
  # Make sure models agree on outputs when reversing *rows*.
  # predictPair:
  out <- rowPairApply(train_df, heuristics(fitted_model))
  out_rev <- rowPairApply(train_df, heuristics(fitted_model_rev))
  expect_equal(out, out_rev)
  # predictPairProb:
  # out <- rowPairApply(train_df, heuristicsProb(fitted_model))
  # out_rev <- rowPairApply(train_df, heuristicsProb(fitted_model_rev))
  # expect_equal(out, out_rev)
}

test_that("test_row_reorder ttb",      {test_row_reorder(ttbModel)})
test_that("test_row_reorder ttbGreedy",{test_row_reorder(ttbGreedyModel, has_cv=FALSE)})
test_that("test_row_reorder singleCue",{test_row_reorder(singleCueModel)})
test_that("test_row_reorder dawes",    {test_row_reorder(unitWeightModel)})
test_that("test_row_reorder validityWeight", {test_row_reorder(validityWeightModel)})
test_that("test_row_reorder reg",      {test_row_reorder(regModel, has_cv=FALSE)})
test_that("test_row_reorder regIntercept", {test_row_reorder(regInterceptModel, has_cv=FALSE)})
test_that("test_row_reorder logReg",   {test_row_reorder(logRegModel, has_cv=FALSE)})

# Test specific data sets. #

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
    expect_equal(c(a=1, b=0.6), fitted_model$cue_validities_unreversed, tolerance=0.001)
    expect_equal(c(a=1, b=0.6), fitted_model$cue_validities,
                 tolerance=0.001)
  }
  # Check output.
  expect_equal(expected, predictPair(oneRow(train_df, 1),
                                     oneRow(train_df, 2), fitted_model), tolerance=0.001)
  expect_equal(-expected, predictPair(oneRow(train_df, 2),
                                      oneRow(train_df, 1), fitted_model), tolerance=0.001)
}

# The correct answer is 1, but we confirm each model works as designed.
test_that("test_10_06 ttb",      {test_10_06(ttbModel,       1)})
test_that("test_10_06 singleCue",{test_10_06(singleCueModel, 0)})
test_that("test_10_06 dawes",    {test_10_06(unitWeightModel,     1)})
test_that("test_10_06 validityWeight", {test_10_06(validityWeightModel,  1)})
test_that("test_10_06 reg",      {test_10_06(regModel,    1, has_cv=FALSE)})
test_that("test_10_06 regIntercept", {test_10_06(regInterceptModel,       -1, has_cv=FALSE)})
test_that("test_10_06 logReg",   {test_10_06(logRegModel,    -1, has_cv=FALSE)})


# This test is named by the cue validities of the two cues, 1.0 and 0.4,
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
    expect_equal(c(a=0, b=0.4), fitted_model$cue_validities_unreversed, tolerance=0.001)
    expect_equal(c(a=1, b=0.6), fitted_model$cue_validities,
                 tolerance=0.001)
  }
  # Check output.
  expect_equal(expected, predictPair(oneRow(train_df, 1),
                                     oneRow(train_df, 2), fitted_model),
               tolerance=0.001)
  expect_equal(-expected, predictPair(oneRow(train_df, 2),
                                       oneRow(train_df, 1), fitted_model),
               tolerance=0.001)
}

# The correct answer is 0, but we confirm each model works as designed.
test_that("test_00_04_rc ttb",      {test_00_04_rc(ttbModel,        -1)})
test_that("test_00_04_rc singleCue",{test_00_04_rc(singleCueModel,   0)})
test_that("test_00_04_rc dawes",    {test_00_04_rc(unitWeightModel, -1)})
test_that("test_00_04_rc validityWeight", {test_00_04_rc(validityWeightModel,  -1)})
#TODO(jean): Why do reg models get this wrong?
test_that("test_00_04_rc reg",      {test_00_04_rc(regModel,       1, has_cv=FALSE)})
test_that("test_00_04_rc regIntercept", {test_00_04_rc(regInterceptModel,       1, has_cv=FALSE)})
test_that("test_00_04_rc logReg",   {test_00_04_rc(logRegModel,       1, has_cv=FALSE)})


test_ab_vs_c <- function(model, expected, has_cv=TRUE) {
  train_df <- data.frame(criterion=c(900,400,100,6), a=c(101,101,20,101), b=c(59,59,5,59),
                         c=c(90,80,70,10))
  # Cue a and b have validity 2/3, cue c has validity 1.
  # Cue c predicts Row 3 > Row 4.
  # But if you sum all cue weights, predict Row 4 > Row 3
  fitted_model <- model(train_df, 1, c(2:4))
  if (has_cv) {
    expect_equal(c(a=0.667, b=0.667, c=1), fitted_model$cue_validities_unreversed, tolerance=0.002)
    expect_equal(c(a=0.667, b=0.667, c=1), fitted_model$cue_validities,
                 tolerance=0.002)
  }
  # Check prediction.
  expect_equal(expected, predictPair(oneRow(train_df, 3),
                                     oneRow(train_df, 4), fitted_model))
  expect_equal(-expected, predictPair(oneRow(train_df, 4),
                                       oneRow(train_df, 3), fitted_model))
}

# The correct answer is 0, but we confirm each model works as designed.
test_that("test_ab_vs_c ttb",      {test_ab_vs_c(ttbModel,       1)})
test_that("test_ab_vs_c singleCue",{test_ab_vs_c(singleCueModel, 1)})
test_that("test_ab_vs_c dawes",    {test_ab_vs_c(unitWeightModel,     -1)})
test_that("test_ab_vs_c validityWeight", {test_ab_vs_c(validityWeightModel,  -1)})
test_that("test_ab_vs_c reg",      {test_ab_vs_c(regModel,                    1, has_cv=FALSE)})
test_that("test_ab_vs_c regIntercept", {test_ab_vs_c(regInterceptModel,       1, has_cv=FALSE)})
test_that("test_ab_vs_c logReg",   {test_ab_vs_c(logRegModel,                 1, has_cv=FALSE)})


d_useless_cue_3 <- function(model, expected, has_cv=TRUE) {
  # This is based on real data where a bug was found.  Some models think the first
  # two cues are useful, but all agree the 3rd cue is useless.
  train_df <- data.frame(criterion=c(397,385,327), x1=c(99,100,85), x2=c(3.6,2.9,3.2),
                         x3=c(0,1,0))
  fitted_model <- model(train_df, 1, c(2:4))
  if (has_cv) {
    expect_equal(c(x1=0.667, x2=0.667, x3=0.5), fitted_model$cue_validities_unreversed,
                 tolerance=0.002)
    expect_equal(c(x1=0.667, x2=0.667, x3=0.5), fitted_model$cue_validities,
                 tolerance=0.002)
  }
  # Check prediction.
  expect_equal(expected, predictPair(oneRow(train_df, 1),
                                     oneRow(train_df, 2), fitted_model))
  expect_equal(-expected, predictPair(oneRow(train_df, 2),
                                      oneRow(train_df, 1), fitted_model))
  
}

# The correct answer is 1, but models disagree a lot.
#TODO(jean): Find a way to test ttb and singleCue despite random order of x1 and x2.
#test_that("d_useless_cue_3 ttb",      {d_useless_cue_3(ttbModel,       #random(0,1))})
#test_that("d_useless_cue_3 singleCue",{d_useless_cue_3(singleCueModel, #random(0,1))})
test_that("d_useless_cue_3 dawes",    {d_useless_cue_3(unitWeightModel,     0)})
test_that("d_useless_cue_3 validityWeight", {d_useless_cue_3(validityWeightModel,  0)})
test_that("d_useless_cue_3 reg",      {d_useless_cue_3(regModel,       1, has_cv=FALSE)})
test_that("d_useless_cue_3 regIntercept", {d_useless_cue_3(regInterceptModel, 1, has_cv=FALSE)})
test_that("d_useless_cue_3 logReg",   {d_useless_cue_3(logRegModel,    1, has_cv=FALSE)})
# minModel




a_reordered_columns <- function(model) {
  # Re-order the column and make sure the output is the same.  I had to make sure there
  # was enough data for regression models to fit parameters for all columns.
  df1 <- data.frame(criterion=c(397,385,327,301), x1=c(99,100,85,92), x2=c(0,1,2,3))
  fitted_model1 <- model(df1, 1, c(2,3))
  out1 <- rowPairApply(df1, heuristics(fitted_model1))
  df2 <- data.frame(x1=df1$x1, criterion=df1$criterion, x2=df1$x2)
  fitted_model2 <- model(df2, 2, c(1,3))
  out2 <- rowPairApply(df2, heuristics(fitted_model2))
  expect_equal(out1, out2)
  
  # Fitted parameters should be the same, too.
  if (!is.null(coef(fitted_model1))) {
    expect_equal(coef(fitted_model1), coef(fitted_model2))
  }
}

test_that("a_reordered_columns ttb",      {a_reordered_columns(ttbModel)})
test_that("a_reordered_columns singleCue",{a_reordered_columns(singleCueModel)})
test_that("a_reordered_columns dawes",    {a_reordered_columns(unitWeightModel)})
test_that("a_reordered_columns validityWeight", {a_reordered_columns(validityWeightModel)})
test_that("a_reordered_columns reg",      {a_reordered_columns(regModel)})
test_that("a_reordered_columns regIntercept", {a_reordered_columns(regInterceptModel)})
test_that("a_reordered_columns logReg",   {a_reordered_columns(logRegModel)})
# minModel

