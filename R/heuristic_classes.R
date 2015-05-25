#' Take The Best
#'
#' An S3 class implementation of the Take The Best heuristic.
#' It sorts cues in order of cue validity, making a decision based on the first cue that
#' discriminates (has differing values on the two objects).
#' Implements:
#'   predict
#' However, its implementation of the S3 predict generic function has to output values.
#' Do not take the actual values seriously-- only the sort order is guaranteed. 
#' 
#' @param train_data Training/fitting data as a matrix or data.frame.
#' @param criterion_col The index of the colum in train_data that has the criterion.
#' @param cols_to_fit A vector of column indices in train_data.
#'
#' @return The ttb S3 object.
#'
#' @examples
#' # Fit column (5,4) to column (1,0), having validity 1.0, and column (0,1), validity 0.
#' ttb <- ttbModel(matrix(c(5,4,1,0,0,1), 2, 3), 1, c(2,3))
#' predict(ttb, matrix(c(5,4,1,0,0,1), 2, 3))  # Only sort order of output guaranteed.
#' @export
ttbModel <- function(train_data, criterion_col, cols_to_fit) {
  cue_validities <- matrixCueValidity(train_data[,c(criterion_col,cols_to_fit)], criterion_col)
  cue_ranks <- rev(rank(cue_validities, ties.method="random"))
  linear_coef <- sapply(cue_ranks, function(n) 2^(length(cue_ranks)-n) )
  structure(list(criterion_col=criterion_col, cols_to_fit=cols_to_fit, cue_validities=cue_validities, cue_ranks=cue_ranks, linear_coef=linear_coef), class="ttbModel")
}

# Private.  The external world should not know the implementation actual uses a linear model under the hood.
coef.ttbModel <- function(model) model$linear_coef

#' @export
predict.ttbModel <- function(model, test_df) predictWithWeights(test_df, model$cols_to_fit, model$linear_coef)

