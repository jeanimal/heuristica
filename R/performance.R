# Functions for measuring the performance of predictions vs. actual data.

# A general-ish function that always returns a square matrix with dimensions
# the length of required_categories.
confusionMatrix <- function(data_1, data_2, required_categories) {
  munged <- table(c(required_categories, data_1), c(required_categories, data_2))
  cleaned <- munged - diag(length(required_categories))
  return(cleaned)
}

pair_predict_categories <- c(-1,0,1)

#' Confusion matrix for categories -1, 0, 1, the output of pairPredict.
#' 
#' Measuring accuracy of predicting categories, where in the pairPredict paradigm
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
#' confusionMatrixPairPredict(c(1,1,1), c(1,-1,-1))
#' # outputs:
#' #    -1 0 1
#' # -1  0 0 0
#' # 0   0 0 0
#' # 1   2 0 1
#' #
#' # Example 2
#' # The prediction always matches the reference outcome, so all non-zero
#' # counts are on the diagonal.
#' confusionMatrixPairPredict(c(1,1,0,0,-1,-1), c(1,1,0,0,-1,-1))
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
confusionMatrixPairPredict <- function(ref_data, predicted_data) {
  return(confusionMatrix(ref_data, predicted_data, pair_predict_categories))
}



