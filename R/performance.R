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

categories_neg1_0_1 <- c(-1,0,1)

#' Confusion matrix for categories -1, 0, 1 (the output of predictPair).
#' 
#' Measuring accuracy of predicting categories, where in the predictPair paradigm
#' the categories are the relative ranks of a pair of rows.  The categories are:
#' -1 means Row1 < Row2
#' 0 means the rows are equal or guess
#' 1 means Row1 > Row2
#' @param ref_data A vector with outcome categories from a reference source to
#'   be predicted (e.g. the output of correctGreater.)
#' @param predicted_data A vector with outcome categories from a prediction
#'   source that is trying to match ref_data (e.g. ttbModel predictions).
#' @return A 3x3 matrix of counts.  Rows are outcomes of the reference data.
#'   Columns are outcomes of predicted data.
#' @examples
#' # Example 1
#' # Below, the correct outcome is always 1, so only the last row of the
#' # confusion matrix has non-zero counts.  But the predictor makes a few
#' # mistakes, so some non-zero counts are off the diagonal.
#' confusionMatrixFor_Neg1_0_1(c(1,1,1), c(1,-1,-1))
#' # outputs:
#' #    -1 0 1
#' # -1  0 0 0
#' # 0   0 0 0
#' # 1   2 0 1
#' #
#' # Example 2
#' # The prediction always matches the reference outcome, so all non-zero
#' # counts are on the diagonal.
#' confusionMatrixFor_Neg1_0_1(c(1,1,0,0,-1,-1), c(1,1,0,0,-1,-1))
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
confusionMatrixFor_Neg1_0_1 <- function(ref_data, predicted_data) {
  return(confusionMatrixRequiredCategories(ref_data, predicted_data,
                                           categories_neg1_0_1))
}

#' Reverse rows and columns of data
#'
#' This matrix
#'      [,1] [,2]
#' [1,]    1    2
#' [2,]    3    4
#' becomes
#'       [,1] [,2]
#' [1,]    4    3
#' [2,]    2    1
#'
#' 
#' @param data A data.frame or matrix.
#' @return A data.frame or matrix with rows reversed and columns reversed.
#' @export
reverseRowsAndReverseColumns <- function(data) {
  return(data[c(nrow(data):1),c(ncol(data):1)])
}


#' Distributes guesses of 3x3 confusion matrix to expected value of 1 and -1. 
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
#' @param confusion_matrix_3x3 A 3x3 matrix where the middle column is counts of
#'   guesses.
#' @return A 3x3 confusion matrix with 0's in the middle column.
#'
#' @export
distributeGuessAsExpectedValue <- function(confusion_matrix_3x3) {
  guesses <- confusion_matrix_3x3[1,2]
  confusion_matrix_3x3[1,1] <- confusion_matrix_3x3[1,1] + 0.5 * guesses
  confusion_matrix_3x3[1,3] <- confusion_matrix_3x3[1,3] + 0.5 * guesses
  confusion_matrix_3x3[1,2] <- 0
  guesses <- confusion_matrix_3x3[2,2]
  confusion_matrix_3x3[2,1] <- confusion_matrix_3x3[2,1] + 0.5 * guesses
  confusion_matrix_3x3[2,3] <- confusion_matrix_3x3[2,3] + 0.5 * guesses
  confusion_matrix_3x3[2,2] <- 0
  guesses <- confusion_matrix_3x3[3,2]
  confusion_matrix_3x3[3,1] <- confusion_matrix_3x3[3,1] + 0.5 * guesses
  confusion_matrix_3x3[3,3] <- confusion_matrix_3x3[3,3] + 0.5 * guesses
  confusion_matrix_3x3[3,2] <- 0
  return(confusion_matrix_3x3)
}

# Just like distributeGuessAsExpectedValue but applied to the tie row in row 2.
distributeTies <- function(confusion_matrix_3x3) {
  ties <- confusion_matrix_3x3[2,1]
  confusion_matrix_3x3[1,1] <- confusion_matrix_3x3[1,1] + 0.5 * ties
  confusion_matrix_3x3[3,1] <- confusion_matrix_3x3[3,1] + 0.5 * ties
  confusion_matrix_3x3[2,1] <- 0
  ties <- confusion_matrix_3x3[2,2]
  confusion_matrix_3x3[1,2] <- confusion_matrix_3x3[1,2] + 0.5 * ties
  confusion_matrix_3x3[3,2] <- confusion_matrix_3x3[3,2] + 0.5 * ties
  confusion_matrix_3x3[2,2] <- 0
  ties <- confusion_matrix_3x3[2,3]
  confusion_matrix_3x3[1,3] <- confusion_matrix_3x3[1,3] + 0.5 * ties
  confusion_matrix_3x3[3,3] <- confusion_matrix_3x3[3,3] + 0.5 * ties
  confusion_matrix_3x3[2,3] <- 0
  return(confusion_matrix_3x3)
}

#' Collapses a 3x3 confusion matrix to a 2x2 confusion matrix.
#'
#' A 3x3 confusion matrix results from predictPair.
#' 
#' The middle column repressents guesses.
#' The middle row represents ties.  T
#'
#' @param confusion_matrix_3x3 A 3x3 confusion matrix.
#' @param guess_handling_fn A function to call on the 3x3 confusion matrix to
#'   assign a model's guesses-- 0 predictions tracked in the 2nd column-- to
#'   -1 or 1 counts.
#' @param tie_handling_fn A function to call on the 3x3 confusion matrix to
#'   distribute ties-- 0 correct answers tracked in the 2nd row-- to -1 or 1
#'   counts.
#' @return A 2x2 confusion matrix.
#' @export
collapseConfusionMatrix3x3To2x2 <- function(
  confusion_matrix_3x3, guess_handling_fn=distributeGuessAsExpectedValue,
  tie_handling_fn=distributeTies) {
  matrix3x3 <- guess_handling_fn(confusion_matrix_3x3)
  matrix3x3 <- distributeTies(matrix3x3)
  # Return a matrix without row 2 or column 2.
  return(matrix3x3[c(1,3), c(1,3)])
}

#' Accuracy, sensitivity, specificity, and precision of 2x2 confusion matrix.
#'
#' In heuristica, "positive" means the row1 > row2.  Other heuristica create
#' confusion matrices with the expected layout, but below is documentation of
#' that layout.  A package like 'caret' offers a more general-purpose
#' confusion matrix.
#'
#' This assumes the input matrix is 2x2 qnd will STOP if not.  It also 
#' assumes negatives are left and higher, and predictions are the rows,
#' that is:
#' true negative  [-1,-1]    false negative [-1,1]
#' false negative [1, -1]    true positive  [1, 1]
#'
#' The outputs are defined as:
#' accuracy = (true positive + true negatve) / all
#' sensitivity = true pasitive rate = true positive / all positive
#'   (sensitivity is also called recall)
#' specificity = true negative rate = true negative / all negative
#' precision = positive predictive velue = true positive
#' 
#' @param confusion_matrix A 2x2 confusion matrix.
#' @return A list with accuracy, sensitivity, specificity, and precision
#' @export
statsFromConfusionMatrix <- function(confusion_matrix) {
  if (nrow(confusion_matrix) != 2 || ncol(confusion_matrix) != 2) {
    stop(paste("Expected 2x2 confusion matrix but got ",
               nrow(confusion_matrix), "x", ncol(confusion_matrix)))
  }
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  # Sensitivity = true positive rate.
  sensitivity <- confusion_matrix["1", "1"] / 
    sum(confusion_matrix[, "1"])
  # Specificity = true negative rate.
  specificity <- confusion_matrix["-1", "-1"] / 
    sum(confusion_matrix[, "-1"])
  # Precision = positive predictive value.
  precision <- confusion_matrix["1", "1"] / sum(confusion_matrix["1", ])
  return(list(accuracy=accuracy, sensitivity=sensitivity,
              specificity=specificity, precision=precision))
}

#' Accuracy based on a predictPair confusion matrix.
#' 
#' Given a confusion matrix from pair predict (the output of
#' confusionMatrixFor_Neg1_0_1), calculate an accuracy.  By default assumes
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
#' accuracyFromConfusionMatrix3x3(cbind(c(4,0,0), c(0,0,0), c(0,0,2)))
#' 
#' # 3 wrong and 3 more wrong for 0 accuracy.
#' accuracyFromConfusionMatrix3x3(cbind(c(0,0,3), c(0,0,0), c(3,0,0)))
#' 
#' # Below is 4 + 5 correct, 1 incorrect, for 9/10 = 0.9 accuracy.
#' accuracyFromConfusionMatrix3x3(cbind(c(4,0,1), c(0,0,0), c(0,0,5)))
#' 
#' # Below has 3+1=4 guesses, and 0.5 are assigned correct.
#' accuracyFromConfusionMatrix3x3(cbind(c(0,0,0), c(3,0,1), c(0,0,0)))
#' @seealso
#' \code{\link{confusionMatrixFor_Neg1_0_1}} for generating the confusion
#'   matrix.
#' @references
#' Wikipedia's entry on
#' \url{https://en.wikipedia.org/wiki/Confusion_matrix}.
#'
#' @export
accuracyFromConfusionMatrix3x3 <- function(confusion_matrix, zero_as_guess=TRUE) {
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
    confusion_matrix <- confusionMatrixFor_Neg1_0_1(data[,reference_col], data[,col])
    accuracy[,i] <- accuracyFromConfusionMatrix3x3(confusion_matrix)
    i <- i+1
  }
  colnames(accuracy) <- colnames(data)[cols_to_compare]
  return(accuracy)
}
