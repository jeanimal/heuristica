# install.packages("Hmisc")
# library("Hmisc")

#' Calculate the cue validity
#'
#' Calculate the
#' \href{http://en.wikipedia.org/wiki/Cue_validity}{cue validity}
#' for a pair of vectors.
#'
#' @param criterion A vector of values to be predicted.
#' @param cue A vector of values to predict with.  Should have the same
#'         length as the criterion.
#' @param replaceNanWith The value to return as cue validity in case it
#'         cannot be calculated.
#' @return The cue validity, a value in the range [0,1].
#' @export
cueValidity <- function(criterion, cue, replaceNanWith=0.5) {
  out <- Hmisc::rcorr.cens(criterion, cue, outx=TRUE)
  justDxy <- as.double(out["Dxy"])
  cv <- 0.5 + 0.5*justDxy
  if (is.nan(cv)) { 
    cv <- replaceNanWith 
  }
  return(cv)
}

# Proof of concept.  Not in use yet.
cueValidity2 <- function(criterion, cue, replaceNanWith=0.5) {
  data <- cbind(criterion, cue)
  correctIncorrectFn <- function(row1, row2) {
    sign_diff <- sign(row1 - row2)
    if (sign_diff[,2,drop = FALSE] == 0) {
      return(cbind(0,0))
    } else if (sign_diff[,2,drop = FALSE] == sign_diff[,1,drop = FALSE]) {
      return(cbind(1,0))
    } else {
      return(cbind(0,1))
    }
  }
  fn2 <- bindFunctionToRowPairs(data, correctIncorrectFn)
  correct_incorrect <- pairMatrix(nrow(data), fn2)
  correct <- sum(correct_incorrect[,1,drop = FALSE])
  incorrect <- sum(correct_incorrect[,2,drop = FALSE])
  cv <- correct / (correct + incorrect)
  if (is.nan(cv)) {
    cv <- replaceNanWith
  }
  return(cv)
}

#' Calculate the cue validity for all specified columns
#'
#' If you know you want cue validities for many columns in your data,
#' this function makes it easy.  
#'
#' @param data The matrix or data.frame whose columns are treated as cues.
#' @param criterion_col The index of the column used as criterion.
#' @param cols_to_fit A vector of indexes of the columns to calculate cue
#'   validity for
#' @param replaceNanWith The value to return as cue validity in case it
#'         cannot be calculated.
#' @return A vector of cue validities applying to each of the columns in
#'         cols_to_fit, in that order.
#' @export
cueValidityMatrix <- function(data, criterion_col, cols_to_fit, 
                              replaceNanWith=0.5) {
  out <- sapply(cols_to_fit, function(col) {
    cueValidity(data[,criterion_col], data[,col],
                replaceNanWith=replaceNanWith)
  })
  out <- c(out)
  if (length(names(data)) > 0) {
    names(out) <- names(data)[cols_to_fit]
  }
  return(out)
}

agreementWithCriterionMatrix <- function(data, criterion_col, cols_to_fit) {
  # The line below puts the criterion into column 1.
  trim_data <- data[,c(criterion_col, cols_to_fit), drop=FALSE]
  sign_diffs <- applyFunctionToRowPairs(trim_data, rowDiffSign)
  # Multiply the criterion column by the rest.  Easy matrix math requires
  # repeating the criterion column, one for each cue.
  cue_cols <- seq(length(cols_to_fit)) + 1      # e.g. 2,3,4
  criterion_cols <- rep(1, length(cols_to_fit)) # e.g. 1,1,1
  # Multiply by cue_col first to preserve its column name.
  concordance <- sign_diffs[,cue_cols, drop=FALSE] *
    sign_diffs[,criterion_cols, drop=FALSE]
  return(concordance)
}

cueValidityMatrix_new <- function(data, criterion_col, cols_to_fit, replaceNanWith=0.5) {
  agreement <- agreementWithCriterionMatrix(data, criterion_col, cols_to_fit)
  pos <- apply(agreement, 2, function(x) { sum(x[x>0]) })
  neg <- apply(agreement, 2, function(x) { sum(abs(x[x<0])) })
  return(pos / (pos+neg))
}

cueAccuracy <- function(criterion, cue, replaceNanWith=0.5) {
  out <- Hmisc::rcorr.cens(cue, criterion, outx=FALSE)
  justDxy <- as.double(out["Dxy"])
  cv <- 0.5 + 0.5*justDxy
  if (is.nan(cv)) {
    cv <- replaceNanWith
  }
  return(cv)
}

cueAccuracyMatrix  <- function(data, criterion_col, cols_to_fit,
                               replaceNanWith=0.5) {
  out <- sapply(cols_to_fit, function(col) {
    cueAccuracy(data[,criterion_col], data[,col],
                replaceNanWith=replaceNanWith)
  })
  out <- c(out)
  if (length(names(data)) > 0) {
    names(out) <- names(data)[cols_to_fit]
  }
  return(out)
}
