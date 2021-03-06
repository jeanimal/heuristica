% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/performance.R
\name{accuracyFromConfusionMatrix3x3}
\alias{accuracyFromConfusionMatrix3x3}
\title{Accuracy based on a predictPair confusion matrix.}
\usage{
accuracyFromConfusionMatrix3x3(confusion_matrix, zero_as_guess = TRUE)
}
\arguments{
\item{confusion_matrix}{A 3x3 matrix where rows are correct outcomes
(-1, 0, 1) and columns are predicted outcomes (-1, 0, 1).}

\item{zero_as_guess}{Optional parameter which by default treats the 2nd zero
column as guesses and assigns half of them to be correct.}
}
\value{
A value from 0 to 1 for the proportion correct.
}
\description{
Given a confusion matrix from pair predict (the output of
confusionMatrixFor_Neg1_0_1), calculate an accuracy.  By default assumes
zeroes are guesses and that half of them are correct.  This guessing
assumptions helps measures of accuracy converge faster for small samples,
but it will artificially reduce the variance of an algorithm's predictions,
if that is what you are trying to measure.
}
\examples{
# Below accuracy is 1 (100\% correct) because 4 -1's were correctly predicted,
# and 2 1's were correctly predicted.  (On-diagonal elements are correct
# predictions.)
accuracyFromConfusionMatrix3x3(cbind(c(4,0,0), c(0,0,0), c(0,0,2)))

# 3 wrong and 3 more wrong for 0 accuracy.
accuracyFromConfusionMatrix3x3(cbind(c(0,0,3), c(0,0,0), c(3,0,0)))

# Below is 4 + 5 correct, 1 incorrect, for 9/10 = 0.9 accuracy.
accuracyFromConfusionMatrix3x3(cbind(c(4,0,1), c(0,0,0), c(0,0,5)))

# Below has 3+1=4 guesses, and 0.5 are assigned correct.
accuracyFromConfusionMatrix3x3(cbind(c(0,0,0), c(3,0,1), c(0,0,0)))
}
\references{
Wikipedia's entry on
\url{https://en.wikipedia.org/wiki/Confusion_matrix}.
}
\seealso{
\code{\link{confusionMatrixFor_Neg1_0_1}} for generating the confusion
  matrix.
}
