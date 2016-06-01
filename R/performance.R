# Functions for measuring the performance of model predictions vs. actual data.

# A general-ish function that always returns a square matrix with dimensions
# the length of required_categories.
confusionMatrixRequiredCategories <- function(data_1, data_2, required_categories) {
  correct <- c(required_categories, data_1)
  predictions <- c(required_categories, data_2)
  munged <- table(correct, predictions)
  cleaned <- munged - diag(length(required_categories))
  return(cleaned)
}

predict_pair_categories <- c(-1,0,1)

#' Confusion matrix for categories -1, 0, 1, the output of predictPair.
#' 
#' Measuring accuracy of predicting categories, where in the predictPair paradigm
#' the categories are the relative ranks of a pair of rows.  The categories are:
#' -1 means Row2 is bigger
#' 0 means the rows are equal or guess
#' 1 means Row1 is bigger
#' @param ref_data A vector with outcome categories from a reference source, typically
#'   correct values.
#' @param predicted_data A vector with outcome categories from a prediction source.
#' @return A 3x3 matrix of counts.  Rows are outcomes of the reference data.
#'   Columns are outcomes of predicted dta.
#' @examples
#' # Example 1
#' # Below, the correct outcome is always 1, so only the last row of the
#' # confusion matrix has non-zero counts.  But the predictor makes a few
#' # mistakes, so some non-zero counts are off the diagonal.
#' confusionMatrixPredictPair(c(1,1,1), c(1,-1,-1))
#' # outputs:
#' #    -1 0 1
#' # -1  0 0 0
#' # 0   0 0 0
#' # 1   2 0 1
#' #
#' # Example 2
#' # The prediction always matches the reference outcome, so all non-zero
#' # counts are on the diagonal.
#' confusionMatrixPredictPair(c(1,1,0,0,-1,-1), c(1,1,0,0,-1,-1))
#' # outputs:
#' #    -1 0 1
#' # -1  2 0 0
#' # 0   0 2 0
#' # 1   0 0 2
#' #
#' @references
#' Wikipedia's entry on
#' \url{https://en.wikipedia.org/wiki/Confusion_matrix}.
#'
#' @export
confusionMatrixPredictPair <- function(ref_data, predicted_data) {
  return(confusionMatrixRequiredCategories(ref_data, predicted_data,
                                           predict_pair_categories))
}

#' Given a 3x3 confusion matrix, distributes guesses in column 2 using 
#'
#' Given a 3x3 confusion matrix, distributes guesses in column 2 using the
#' expected value.  That is, moves half of guess counts (in column 2) to -1
#' (column 1) and the other half to 1 (column 3).
#'
#'    -1 0 1
#' -1  2 2 2
#' 0   4 4 4
#' 1   6 6 6
#' becomes
#'    -1 0 1
#' -1  3 0 3
#' 0   6 0 6
#' 1   9 0 9
#'
#' @param confusionMatrix3x3 A 3x3 matrix where the middle column is counts of
#'   guesses.
#' @return A 3x3 confusion matrix with 0's in the middle column.
#'
#' @export
distributeGuessAsExpectedValue <- function(confusionMatrix3x3) {
  guesses <- confusionMatrix3x3[1,2]
  confusionMatrix3x3[1,1] <- confusionMatrix3x3[1,1] + 0.5 * guesses
  confusionMatrix3x3[1,3] <- confusionMatrix3x3[1,3] + 0.5 * guesses
  confusionMatrix3x3[1,2] <- 0
  guesses <- confusionMatrix3x3[2,2]
  confusionMatrix3x3[2,1] <- confusionMatrix3x3[2,1] + 0.5 * guesses
  confusionMatrix3x3[2,3] <- confusionMatrix3x3[2,3] + 0.5 * guesses
  confusionMatrix3x3[2,2] <- 0
  guesses <- confusionMatrix3x3[3,2]
  confusionMatrix3x3[3,1] <- confusionMatrix3x3[3,1] + 0.5 * guesses
  confusionMatrix3x3[3,3] <- confusionMatrix3x3[3,3] + 0.5 * guesses
  confusionMatrix3x3[3,2] <- 0
  return(confusionMatrix3x3)
}

# All counts in the ties row (0 correct) are set to zero.  This will mean
# the matrix' total counts will no longer be the number of row pairs.
zeroOutTies <- function(confusionMatrix3x3) {
  confusionMatrix3x3[2,] <- c(0,0,0)
}

confusionMatrix3x3to2x2 <- function(confusionMatrix3x3,
                                    guess_handling_fn=guessExpectedValue,
                                    tie_handling_fn=zeroOutTies) {
  if (!is.null(guess_handling_fn)) {
    confusionMatrix3x3 <- guess_handling_fn(confusionMatrix3x3)
  }
  
  # Drop all zero rows.  TOODO(error checking).
  matrix2x2 <- matrix3x3[c(1,3), c(1,3)]
}

#' Accuracy based on a predictPair confusion matrix.
#' 
#' Given a confusion matrix from pair predict (the output of
#' confusionMatrixPredictPair), calculate an accuracy.  By default assumes
#' zeroes are guesses and that half of them are correct.  This guessing
#' assumptions helps measures of accuracy converge faster for small samples,
#' but it will artificially reduce the variance of an algorithm's predictions,
#' if that is what you are trying to measure.
#' 
#' @param confusion_matrix A 3x3 matrix where rows are correct outcomes
#'   (-1, 0, 1) and columns are predicted outcomes (-1, 0, 1).
#' @param zero_as_guess Optional parameter which by default treats the 2nd zero
#'   column as guesses and assigns half of them to be correct.
#' @return A value from 0 to 1 for the proportion correct.
#' @examples
#' # Below accuracy is 1 (100% correct) because 4 -1's were correctly predicted,
#' # and 2 1's were correctly predicted.  (On-diagonal elements are correct
#' # predictions.)
#' accuracyFromConfusionMatrix(cbind(c(4,0,0), c(0,0,0), c(0,0,2)))
#' 
#' # 3 wrong and 3 more wrong for 0 accuracy.
#' accuracyFromConfusionMatrix(cbind(c(0,0,3), c(0,0,0), c(3,0,0)))
#' 
#' # Below is 4 + 5 correct, 1 incorrect, for 9/10 = 0.9 accuracy.
#' accuracyFromConfusionMatrix(cbind(c(4,0,1), c(0,0,0), c(0,0,5)))
#' 
#' # Below has 3+1=4 guesses, and 0.5 are assigned correct.
#' accuracyFromConfusionMatrix(cbind(c(0,0,0), c(3,0,1), c(0,0,0)))
#' @seealso
#' \code{\link{confusionMatrixPredictPair}} for generating the confusion
#'   matrix.
#' @references
#' Wikipedia's entry on
#' \url{https://en.wikipedia.org/wiki/Confusion_matrix}.
#'
#' @export
accuracyFromConfusionMatrix <- function(confusion_matrix, zero_as_guess=TRUE) {
  if (nrow(confusion_matrix) != ncol(confusion_matrix)) {
    stop(paste("Matrix must be square but had ", nrow(confusion_matrix)),
         "rows and", ncol(confusion_matrix), "columns")
  }
  # Categories are matched correctly on the diagonal.
  correct <- sum(diag(confusion_matrix))
  if (zero_as_guess) {
    # Then assume guesses are in column 2 and half of them will be correct.
    guessed_correct <- 0.5*(confusion_matrix[1,2] + confusion_matrix[3,2])
  } else {
    guessed_correct <- 0
  }
  # Finally the total number of trials is the sum of the whole matrix.
  sum(correct, guessed_correct) / sum(confusion_matrix)
}

# calculate accuracy of predictions in cols_to_compare.
categoryAccuracyAll <- function(data, reference_col, cols_to_compare) {
  accuracy <- matrix(rep(NA, length(cols_to_compare)), 1, length(cols_to_compare))
  i <- 1
  for (col in cols_to_compare) {
    confusion_matrix <- confusionMatrixPredictPair(data[,reference_col], data[,col])
    accuracy[,i] <- accuracyFromConfusionMatrix(confusion_matrix)
    i <- i+1
  }
  colnames(accuracy) <- colnames(data)[cols_to_compare]
  return(accuracy)
}
