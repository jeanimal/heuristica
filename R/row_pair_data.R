####################################
# Turn regular data into row pairs #
####################################


rowDiff <- function(row1, row2) row1 - row2

rowDiffSign <- function(row1, row2) sign(row1 - row2)

# Apply row_pair_fn to all rows with criterion_col and cols_to fit.
# Criterion_col will then get special treatment, rescaled to 0 to 1.
# The row_pair_function has the signature function(row1, row2).  Examples are
# rowDiff and rowDiffSign.
toRowPairData <- function(train_data, criterion_col, cols_to_fit,
                          row_pair_fn) {
  transform <- applyFunctionToRowPairs(
    train_data[,c(criterion_col,cols_to_fit)], row_pair_fn)
  # The criterion has been moved to the first colum.  But it should not be a
  # diff-- it is the probability row 1 is greater, which is 1 if row 1 is
  # greater, 0 if row2 is greater, and 0.5 if they are the same size.
  transform[,1] <- rescale0To1(sign(transform[,1,drop=FALSE]))
  return(transform)
}

# Forward and backward row pairs.
logRegData <- function(train_data, criterion_col, cols_to_fit, row_pair_fn) {
  forwardPairs <- toRowPairData(train_data, criterion_col, cols_to_fit,
                                row_pair_fn)
  n <- nrow(train_data)
  backwardPairs <- toRowPairData(train_data[c(n:1),], criterion_col,
                                 cols_to_fit, row_pair_fn)
  return(rbind(forwardPairs, backwardPairs))
}
