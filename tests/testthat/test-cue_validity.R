context("cue_validity")

# cueValidity

expect_equal(1   , cueValidity(c(5,4), c(1,0)),tolerance=1)
expect_equal(   0, cueValidity(c(5,4), c(0,1)),tolerance=1)
expect_equal(0.5, cueValidity(c(5,4), c(0,0)),tolerance=0.1)
expect_equal(0.5, cueValidity(c(5,4), c(1,1)),tolerance=0.1)
expect_equal(0.5, cueValidity(c(4,5), c(1,1)),tolerance=0.1)
expect_equal(0   , cueValidity(c(4,5), c(1,0)),tolerance=1)

expect_equal(0.5, cueValidity(c(5,4,3), c(0,0,0)),tolerance=0.1)
expect_equal(   0, cueValidity(c(5,4,3), c(0,0,1)),tolerance=1)
expect_equal(0.5, cueValidity(c(5,4,3), c(0,1,0)),tolerance=0.1)
expect_equal(   0, cueValidity(c(5,4,3), c(0,1,1)),tolerance=1)
expect_equal(   1, cueValidity(c(5,4,3), c(1,0,0)),tolerance=1)
expect_equal(0.5, cueValidity(c(5,4,3), c(1,0,1)),tolerance=0.1)
expect_equal(   1, cueValidity(c(5,4,3), c(1,1,0)),tolerance=1)
expect_equal(0.5, cueValidity(c(5,4,3), c(1,1,1)),tolerance=0.1)

expect_equal(0.667, cueValidity(c(5,4,3,2), c(1,1,0,1)), tolerance=0.001)
expect_equal(0.75,  cueValidity(c(5,4,3,2,1), c(1,1,1,0,1)),tolerance=0.01)

# Real values.
expect_equal(0.667, cueValidity(c(397,385,327), c(99,100,85)),
  tolerance=0.001)

expect_equal(-10, cueValidity(c(5,4,3), c(1,1,1), replaceNanWith=-10),tolerance=1)
expect_equal(0.75,  cueValidity(c(5,4,3,2,1), c(1,1,1,0,1), replaceNanWith=-10),tolerance=0.01)


# cueValidityAppliedToColumns

test_that("cueValidityAppliedToColumns with 3-cue matrix", {
  matrix <- cbind(y=c(5,4), x1=c(1,0), x2=c(0,1), x3=c(0,0))
  cv_just1 <- cueValidityAppliedToColumns(matrix, 1, c(3)) 
  expect_equal(c(x2=0), cv_just1)
  cv <- cueValidityAppliedToColumns(matrix, 1, c(2:4)) 
  expect_equal(c(x1=1.0, x2=0, x3=0.5), cv)
})

# cueValidityComplete

expect_equal(c(1), cueValidityComplete(
  matrix(c(5,4,1,0), 2, 2), 1, c(2))$cue_validities, tolerance=0.001)
expect_equal(c(1), cueValidityComplete(
  matrix(c(5,4,1,0,0,1), 2, 2), 1, c(2))$cue_validities, tolerance=0.001)
expect_equal(c(1,0), cueValidityComplete(
  matrix(c(5,4,1,0,0,1), 2, 3), 1, c(2,3))$cue_validities, tolerance=0.001)
expect_equal(c(1,0), cueValidityComplete(
  matrix(c(1,0,5,4,0,1), 2, 3), 2, c(1,3))$cue_validities, tolerance=0.001)
expect_equal(c(1,0), cueValidityComplete(
  matrix(c(5,4,0,1), 2, 2), 1, c(1, 2))$cue_validities, tolerance=0.001)

test_that("cueValidityComplete 5x4 names", {
  df <- data.frame(Criterion=c(5,4,3,2,1), a=c(1,0,0,0,1),
                   b=c(1,1,1,0,0), c=c(1,1,1,0,1))
  cv <- cueValidityComplete(df, 1, c(2,3,4)) 
  expect_equal(c(a=0.5, b=1.0, c=0.75), cv$cue_validities)
})

test_that("cueValidityComplete 3x3 names shifted criterion", {
  df <- data.frame(Garbage=c(99, 99, 99), Criterion=c(5,4,3),
                   Cue=c(1,0,1))
  names(df) <- c('Garbage', 'Criterion', 'Cue')
  cv <- cueValidityComplete(df, 2, c(3)) 
  expect_equal(c(Cue=0.5), cv$cue_validities)
})

# cueAccuracy

# cueValidity == cueAccuracy in the cases below.
expect_equal(1, cueAccuracy(c(5,4), c(1,0)))
expect_equal(0.5, cueAccuracy(c(5,4), c(1,1)))
expect_equal(0.5, cueAccuracy(c(5,4), c(0,0)))
expect_equal(0, cueAccuracy(c(5,4), c(0,1)))
# Below has a cueValidity of 1.0.  But ties are handled differently for
# cueAccuracy.
# Row 1 vs. 2 is correct -> 1.0
# Row 1 vs. 3 is correct -> 1.0
# Row 1 vs. 2 is a tie,  -> 0.5
# The average of these is 2.5 / 3 = 0.8333
expect_equal(0.8333, cueAccuracy(c(5,4,3), c(1,0,0)), tolerance=0.001)
expect_equal(0.8333, cueAccuracy(c(5,4,3), c(1,1,0)), tolerance=0.001)
# Reversed cue:  (0 + 0 + 0.5) / 3 = 0.1666667
expect_equal(0.16667, cueAccuracy(c(5,4,3), c(0,1,1)), tolerance=0.001)
expect_equal(0.16667, cueAccuracy(c(5,4,3), c(0,0,1)), tolerance=0.001)
# Real-valued cue has no ties, so it's still 1.0.
expect_equal(1, cueAccuracy(c(5,4,3), c(3.3, 2.2, 1.1)), tolerance=0.001)
# Real values and a tie.
# (3*1 + 2*1 + 1*0.5)/6 = 5.5/6
expect_equal(0.916667, cueAccuracy(c(5,4,3,2), c(3.3, 2.2, 1, 1)), tolerance=0.001)
expect_equal(1.0, cueValidity(c(5,4,3,2), c(3.3, 2.2, 1, 1)), tolerance=0.001)
# Same but mix it up a bit for lower accuracy.
# (3*1 + 0 + 0.5 + 1)/6 = 4.5/6
expect_equal(0.75, cueAccuracy(c(5,4,3,2), c(3.3, 1, 2.2, 1)), tolerance=0.001)
# cueValidity ignores the tie
# (3*1 + -1 + __ + 1)/4 = 3/5
expect_equal(0.8, cueValidity(c(5,4,3,2), c(3.3, 1, 2.2, 1)), tolerance=0.001)

test_that("cueAccuracyAppliedToColumns with 1-cue matrix", {
  matrix <- cbind(y=c(5,4,3), x1=c(1,0,0))
  cv <- cueAccuracyAppliedToColumns(matrix, 1, c(2)) 
  expect_equal(c(x1=0.83333), cv, tolerance=0.001)
})

test_that("cueAccuracyAppliedToColumns with 3-cue matrix", {
  matrix <- cbind(y=c(5,4), x1=c(1,0), x2=c(0,1), x3=c(0,0))
  cv_just1 <- cueAccuracyAppliedToColumns(matrix, 1, c(3)) 
  expect_equal(c(x2=0), cv_just1)
  cv <- cueAccuracyAppliedToColumns(matrix, 1, c(2:4)) 
  expect_equal(c(x1=1.0, x2=0, x3=0.5), cv)
})

test_that("cueAccuracyAppliedToColumns with 3-cue data.frame", {
  df <- data.frame(y=c(5,4), x1=c(1,0), x2=c(0,1), x3=c(0,0))
  cv <- cueAccuracyAppliedToColumns(df, 1, c(2:4)) 
  expect_equal(c(x1=1.0, x2=0, x3=0.5), cv)
})

# agreementWithCriterionMatrix

test_that("agreementWithCriterionMatrix simple 1", {
  matrix <- cbind(y=c(2,1), x=c(1,0))
  agreement <- agreementWithCriterionMatrix(matrix, 1, c(2))
  expect_equal(cbind(x=1), oneRow(agreement, 1))
})

test_that("agreementWithCriterionMatrix simple 1 reverse", {
  # Both the criterion and cue are reversed, so they still agree.
  matrix <- cbind(y=c(1,2), x=c(0,1))
  agreement <- agreementWithCriterionMatrix(matrix, 1, c(2))
  expect_equal(cbind(x=1), oneRow(agreement, 1))
})

test_that("agreementWithCriterionMatrix simple 0", {
  matrix <- cbind(y=c(2,1), x=c(1,1))
  agreement <- agreementWithCriterionMatrix(matrix, 1, c(2))
  expect_equal(cbind(x=0), oneRow(agreement, 1))
})

test_that("agreementWithCriterionMatrix simple -1", {
  matrix <- cbind(y=c(2,1), x=c(0,1))
  agreement <- agreementWithCriterionMatrix(matrix, 1, c(2))
  expect_equal(cbind(x=-1), oneRow(agreement, 1))
})

test_that("agreementWithCriterionMatrix 2 columns", {
  # The criterion is not the first column.
  matrix <- cbind(y=c(2,1), x1=c(0,1), x2=c(1,0))
  agreement <- agreementWithCriterionMatrix(matrix, 1, c(2,3))
  expect_equal(cbind(x1=-1, x2=1), oneRow(agreement, 1))
})

test_that("agreementWithCriterionMatrix 2 mixed up columns data.frame", {
  # There is a non-data column, and the criterion is not the first column.
  df <- data.frame(ignore=c("a", "b"), x1=c(0,1), y=c(2,1), x2=c(1,0))
  agreement <- agreementWithCriterionMatrix(df, 3, c(2,4))
  expect_equal(cbind(x1=-1, x2=1), oneRow(agreement, 1))
})

test_that("agreementWithCriterionMatrix realistic 5 rows 2 cues", {
  # x1 and x2 both have validity 0.75 but x2 has more rights and wrongs
  # (higher discrimination).
  matrix <- cbind(y=c(5:1), x1=c(1,1,1,0,1), x2=c(12,10,6,6,10))
  agreement <- agreementWithCriterionMatrix(matrix, 1, 2:3)
  # TODO: Make it easier to match row pair indexes to row.
  # Row 1 vs. 2, x1 can't discriminate, x2 is right.
  expect_equal(cbind(x1=0, x2=1), oneRow(agreement, 1))
  # Row 1 vs. 3, same.
  expect_equal(cbind(x1=0, x2=1), oneRow(agreement, 2))
  # Row 1 vs. 4, both right.
  expect_equal(cbind(x1=1, x2=1), oneRow(agreement, 3))
  # Row 4 vs. 5, both wrong.
  expect_equal(cbind(x1=-1, x2=-1), oneRow(agreement, 10))
})

 # conditionalCueValidityComplete

test_that("conditionalCueValidityComplete 1 cue same as cueValidity", {
  matrix <- cbind(y=c(6:1), x1=c(15,0,2,0,12,1))
  out <- conditionalCueValidityComplete(matrix, 1, c(2))
  cv <- cueValidityComplete(matrix, 1, c(2))
  expect_equal(cv$cue_validities, out$cue_validities)
})

test_that("conditionalCueValidityComplete 1 cue will reverse", {
  matrix <- cbind(y=c(4:1), x1=c(0, 0, 0, 1))
  out <- conditionalCueValidityComplete(matrix, 1, c(2))
  expect_equal(c(x1=1), out$cue_validities)
  expect_equal(c(x1=-1), out$cue_directions)
  # Below for comparison
  cv <- cueValidityComplete(matrix, 1, c(2))
  expect_equal(c(x1=0), cv$cue_validities)
})

test_that("conditionalCueValidityComplete 2 cues", {
  # x2 has initial validity 0.5, then validity -1.0 after x1 is chosen.
  matrix <- cbind(y=c(3:1), x1=c(1,0,0), x2=c(1,0,1))
  out <- conditionalCueValidityComplete(matrix, 1, c(2:3))
  expect_equal(c(x1=1, x2=1), out$cue_validities)
  expect_equal(c(x1=1, x2=2), out$cue_ranks)
  expect_equal(c(x1=1, x2=-1), out$cue_directions)
})

test_that("conditionalCueValidityComplete 2 same cues", {
  matrix <- cbind(y=c(3:1), x1=c(1,0,0), x2=c(1,0,0))
  out <- conditionalCueValidityComplete(matrix, 1, c(2:3))
  # Either cue could be chosen as the one with cue validity 1.  The other cue
  # will have validity NA because it does not discriminate on anything new.
  # Below output is sorted to make it consistent.
  expect_equal(c(1, NA), sort(unname(out$cue_validities), na.last=TRUE))
  expect_equal(c(1, NA), sort(unname(out$cue_ranks), na.last=TRUE))
  expect_equal(c(1, NA), sort(unname(out$cue_directions), na.last=TRUE))
})

test_that("conditionalCueValidityComplete too many cues", {
  matrix <- cbind(y=c(3:1), x1=c(1,0,0), x2=c(1,0,1), x3=c(0,0,0))
  out <- conditionalCueValidityComplete(matrix, 1, c(2:4))
  # TODO: Convert NA to defaults, e.g. validity is 0.5.
  # That can be applied for all cue performance functions.
  expect_equal(c(x1=1, x2=1, x3=NA), out$cue_validities)
  expect_equal(c(x1=1, x2=2, x3=NA), out$cue_ranks)
  expect_equal(c(x1=1, x2=-1, x3=NA), out$cue_directions)
})
