## Shared helper functions ##

#' Convenience function to get one row from a matrix or data frame.
#'
#' This simply calls matrix_or_data_frame[row_index,,drop=FALSE] for you
#' but is shorter and helps you avoid forgetting drop=FALSE.  The need
#' for drop=FALSE when selecting just one row is explained here:
#' http://www.hep.by/gnu/r-patched/r-faq/R-FAQ_56.html
#'
#' @param matrix_or_data_frame A matrix or data frome from which you want one
#'   row.
#' @param row_index The integer index of the row
#' @return The selected row of the data frame.
#'
#' @export
oneRow <- function(matrix_or_data_frame, row_index) {
  matrix_or_data_frame[row_index,,drop=FALSE]
}

# Input should be a range from -1 to 1.  Output is a range from 0 to 1.
# rescale0to1(-1) is 0
# rescale0to1(0) is 0.5
# rescale0to1(1) is 1
rescale0To1 <- function(direction_plus_minus_1) {
  0.5 * (direction_plus_minus_1 + 1)
}

# private
stopIfTrainingSetHasLessThanTwoRows <- function(train_data) {
  if (nrow(train_data) == 0) {
    stop("Training set must have at least 2 rows but had 0 rows")
  }
  if (nrow(train_data) == 1) {
    stop("Training set must have at least 2 rows but had 1 row")
  }
}

# TODO(jean): If this gets used a lot, export it and give it a nicer name.
# Returns just one number.  Assumes you want just the last column.
getPrediction_raw <- function(prediction_matrix, row_pair) {
  if (length(row_pair) != 2) {
    stop("row_pair should be length 2 (row1, row2) but got length "
         + length(row_pair))
  }
  # Comparing a row with itself is undefined.  I assign it 0.5.
  if (row_pair[1] == row_pair[2]) {
    stop(paste("Comparing row", row_pair[1], "with itself."))
  }
  min_index <- min(row_pair)
  max_index <- max(row_pair)
  #print(min_index)
  #print(max_index)
  mat <- prediction_matrix
  #print(head(mat))
  last_col <- ncol(mat)
  #print(last_col)
  val <- mat[(mat[,1]==min_index) & (mat[,2]==max_index),][[last_col]]
  if (min_index == row_pair[1]) {
    return(val)
  } else {
    return(1-val)
  }
}

