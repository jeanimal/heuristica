####################################
# Turn regular data into row pairs #
####################################

rowDiff <- function(row1, row2) row1 - row2

rowDiffSign <- function(row1, row2) sign(row1 - row2)

# Apply row_pair_fn to all rows with criterion_col and cols_to fit.
# Criterion_col will then get special treatment, rescaled to 0 to 1.
# The row_pair_function has the signature function(row1, row2).  Examples are
# rowDiff and rowDiffSign.
# Applies row_pair_fn to all unique pairs of rows-- except a row with itself.
# If the rows are A, B, C, then this returns
# row_pair_fn(A, B)
# row_pair_fn(A, C)
# row_pair_fn(B, C)
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

# If you require symmetric data, this returns all unique pairs and permutations
# of pairs of rows-- except a row with itself.
# If the rows are A, B, C, then this returns
# row_pair_fn(A, B)
# row_pair_fn(A, C)
# row_pair_fn(B, C)
# row_pair_fn(C, B)
# row_pair_fn(C, A)
# row_pair_fn(B, A)
allPairData <- function(train_data, criterion_col, cols_to_fit, row_pair_fn) {
  forwardPairs <- toRowPairData(train_data, criterion_col, cols_to_fit,
                                row_pair_fn)
  n <- nrow(train_data)
  backwardPairs <- toRowPairData(train_data[c(n:1),], criterion_col,
                                 cols_to_fit, row_pair_fn)
  return(rbind(forwardPairs, backwardPairs))
}
