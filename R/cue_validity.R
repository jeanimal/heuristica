# install.packages("Hmisc")
# library("Hmisc")

#' Documentation stub.
#'
#' @param reverse_cues Optional parameter to reverse cues as needed.  By
#' default, the model will reverse the cue values for cues with cue validity
#' < 0.5, so a cue with validity 0 becomes a cue with validity 1.
#' Set this to FALSE if you do not want that, i.e. the cue stays validity 0.
#' @keywords internal
# Private.  This is just an easy way to share parameter documentation.
zzDocumentationStubReverseCues <- function(reverse_cues=TRUE) NULL

#' Calculate the cue validity.
#'
#' Calculate the
#' \href{https://en.wikipedia.org/wiki/Cue_validity}{cue validity}
#' for a pair of vectors.  It is calculated as
#' correct / (correct + incorrect).
#'
#' @param criterion A vector of values to be predicted.
#' @param cue A vector of values to predict with.  Should have the same
#'         length as the criterion.
#' @param replaceNanWith The value to return as cue validity in case it
#'         cannot be calculated, e.g. no variance in the values.
#' @return The cue validity, a value in the range [0,1].
#' @seealso
#' \code{\link{cueValidityComplete}} for more complete output.
#' @seealso
#' \code{\link{conditionalCueValidityComplete}} for a version where validity
#'   is conditional on cues already used to make decisions.
#'
#' @examples
#' cueValidity(c(5,1), c(1,0))
#' # Returns 1.
#' cueValidity(c(5,2,1), c(1,0,0))
#' # Also returns 1
#' cueValidity(c(5,2,1), c(0,0,1))
#' # Returns 0.
#' cueValidity(c(5,2,1), c(1,0,1))
#' # Returns 0.5.
#' @seealso
#' \code{\link{cueAccuracy}} for a measure that takes guesses into account.
#' @references
#' Wikipedia's entry on
#' \url{https://en.wikipedia.org/wiki/Cue_validity}
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

#' Calculate the cue validity for the cols_to_fit columns.
#'
#' This returns only the cue validities, without reversing when a cue
#' points in the wrong direction-- e.g. education is negatively associated
#' with number of felonies, so we should use LESS education as a predictor.
#' Use cueValidityComplete for help with that.
#'
#' @param data The matrix or data.frame whose columns are treated as cues.
#' @param criterion_col The index of the column used as criterion.
#' @param cols_to_fit A vector of indexes of the columns to calculate cue
#'   validity for.
#' @param replaceNanWith The value to return as cue validity in case it
#'         cannot be calculated.
#' @return A list where $cue_validities has a vector of validities for
#'   each of the columns in cols_to_fit.
#'
#' @seealso
#' \code{\link{cueValidityComplete}} for more complete output.
#' @references
#' Wikipedia's entry on
#' \url{https://en.wikipedia.org/wiki/Cue_validity}
#' @export
cueValidityAppliedToColumns <- function(data, criterion_col, cols_to_fit,
                                        replaceNanWith=0.5) {
  out <- sapply(cols_to_fit, function(col) {
    cueValidity(data[,criterion_col], data[,col],
                replaceNanWith=replaceNanWith)
  })
  out <- c(out)
  if (length(colnames(data)) > 0) {
    names(out) <- colnames(data)[cols_to_fit]
  }
  return(out)
}

# Given a vector of cue validities, looks for values less than 0.5
# and reverses those, returning them as cue_validities_with_reverse.
# cue_directions are +1 or -1 for the validities that were reversed.
# This is a helper function.
# Private.
reverseAsNeeded <- function(cue_validities) {
  cue_validities_with_reverse <- abs(cue_validities - 0.5) + 0.5
  cue_directions <- sign(cue_validities - 0.5)
  structure(list(cue_validities_with_reverse=cue_validities_with_reverse,
                 cue_directions=cue_directions))
}

#' Calculate cue validity with reverse, cue directions, and cue ranks.
#'
#' This provides a vector of cue_validities and potentially other useful
#' information, particularly if reverse_cues=TRUE.  For example, education
#' is negatively associated with number of felonies.  If reverse_cues=FALSE,
#' education will get validity < 0.5.  If reverse_cues=TRUE, then LESS
#' education will be used as a predictor, resulting in:
#' 1) cue_validity > 0.5
#' 2) cue_direction == -1
#' To use the cue for prediction, be sure to multiply it by the cue_direction.
#' For ranking, based heuristics, cue_ranks gives the rank order of cues where
#' highest validity = rank 1 (after reversing, if any).
#' 
#' @param data The matrix or data.frame whose columns are treated as cues.
#' @param criterion_col The index of the column used as criterion.
#' @param cols_to_fit A vector of indexes of the columns to calculate cue
#'   validity for.
#' @param replaceNanWith The value to return as cue validity in case it
#'         cannot be calculated.
#' @inheritParams zzDocumentationStubReverseCues
#' @param ties.method An optional parameter passed to rank: A character string
#'   sepcifying how ties (in cue validity) are treated.
#' @return A list where $cue_validities has a vector of validities for
#'   each of the columns in cols_to_fit.
#' @references
#' Wikipedia's entry on
#' \url{https://en.wikipedia.org/wiki/Cue_validity}
#' @export
cueValidityComplete <- function(data, criterion_col, cols_to_fit,
                                replaceNanWith=0.5, reverse_cues=FALSE,
                                ties.method="random") {
  cue_validities <- cueValidityAppliedToColumns(
    data, criterion_col, cols_to_fit, replaceNanWith)
  if (reverse_cues) {
    reverse_info = reverseAsNeeded(cue_validities)
    cue_validities_with_reverse <- reverse_info$cue_validities_with_reverse
    cue_directions <- reverse_info$cue_directions
  } else {
    cue_validities_with_reverse <- cue_validities
    cue_directions <- rep(1, length(cue_validities_with_reverse))
  }
  raw_ranks <- rank(cue_validities_with_reverse, ties.method=ties.method)
  # Reverse ranks so first is last.
  cue_ranks <- length(cue_validities_with_reverse) - raw_ranks + 1

  return(list(cue_validities=cue_validities_with_reverse,
              cue_validities_unreversed=cue_validities,
              cue_ranks=cue_ranks,
              cue_directions=cue_directions))
}

# Returns a matrix of 1,0,-1 for when cols_to_fit agree with criterion_col.
# 1 means the col_to_fit agreed with the criterion (both had row1 higher or
# both had row2 higher).  -1 meant they disagreed (col_to fit had row1 higher
# when the criterion had row2 higher).  And 0 meant col_to_fit did not
# discriminate between row1 and row2.
# Each row returned is a row pair, so if data has N rows, N*(N-1)/2 rows
# are returned.
# This is a helper function.
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

#' Calculate conditional cue validity, which includes reversing and ranks.
#'
#' Conditional cue validity is the validity of a cue taking into account
#' decisions already made by higher-ranked cues.  For a single cue, it
#' is the same as cue validity.  For two cues, the higher validity cue will
#' have conditional cue validity = cue validity.  However, the remaining cue
#' will have its validity re-calculated on just those pairs of object where
#' cue validity did not discriminate.  In the case of binary data, there will
#' be many pairs where the first cue did not discriminate.  With real-valued
#' data, there may be no such cases.
#'
#' @param data The matrix or data.frame whose columns are treated as cues.
#' @param criterion_col The index of the column used as criterion.
#' @param cols_to_fit A vector of indexes of the columns to calculate cue
#'   validity for.
#' @return A list of vectors with values for each column in cols_to_fit:
#' $cue_validities: The validities based on reversed value, numbers ranging
#'   from 0 to 1.  It will include NA if the validity cannot be calculated
#'   (e.g. higher-validity cues made decisions for all cases in the data set).
#' $cue_ranks: Rank order from 1 to # of cues in cols_to_fit.  Will be NA if 
#'   validity was NA.
#' $cue_directions: 1 if cue is in same direction as criterion, -1 if
#'   reversed.  Will be NA if validity was NA.
#'
#' @examples
#' # The data below differentiates between cue validity and conditional cue
#' # validity.  Cue validity of x1 is 1.0.  Cue validity of x2 is 0.5.
#' # But after you've chosen x1 as the highest-validity cue, only row2
#' # vs. row3 is undecided  x2 predictions correctly on those, so its
#' # conditional cue validity is 1.0 (conditional on x1 being first).
#' data <- cbind(y=c(5,4,3), x1=c(1,0,0), x2=c(0,1,0))
#' out <- conditionalCueValidityComplete(data, 1, c(2:3))
#' # This tells you both cues had validity 1-- it returns 1, 1.
#' out$cue_validities
#' # This tells you to choose x1 first-- it returns 1, 0.
#' out$cue_ranks
#' # This tells you they already point in the correct direction.
#' out$cue_directions
#' # For a case with a negative cue direction, try this data:
#' data2 <- cbind(y=c(5,4,3), x1=c(1,0,0), x2=c(1,0,1))
#' conditionalCueValidityComplete(data2, 1, c(2:3))
#'
#' @seealso
#' \code{\link{cueValidity}} and \code{\link{cueValidityComplete}} for the
#' unconditional version.
#'
#' @references
#' Martignon, L., & Hoffrage, U.  (2002).  Fast, frugal, and fit: Simple
#' heuristics for paired comparisons.  Theory and Decision, 52: 29-71.
#' @export
conditionalCueValidityComplete <- function(data, criterion_col, cols_to_fit) {
  original_agreement <- agreementWithCriterionMatrix(data, criterion_col, cols_to_fit)
  conditional_cue_validities <- rep(NA, length(cols_to_fit))
  conditional_cue_ranks <- rep(NA, length(cols_to_fit))
  conditional_cue_directions <- rep(NA, length(cols_to_fit))
  cue_rank_so_far <- 1
  agreement <- original_agreement
  # Begin loop
  while (nrow(agreement) > 0 ) {  # && cue_rank_so_far < length(cols_to_fit)
    pos <- apply(agreement, 2, function(x) { sum(x[x>0]) })
    neg <- apply(agreement, 2, function(x) { sum(abs(x[x<0])) })
    cue_validities <- pos / (pos+neg)
    reverse_info = reverseAsNeeded(cue_validities)
    cue_validities_with_reverse <- reverse_info$cue_validities_with_reverse
    cue_directions <- reverse_info$cue_directions
    raw_ranks <- rank(cue_validities_with_reverse, ties.method="random",
                      na.last="keep")
    # Reverse ranks so first is last.
    num_non_na_ranks <- length(which(!is.na(cue_validities_with_reverse)))
    cue_ranks <- num_non_na_ranks - raw_ranks + 1
    # Find cue with cue_rank of 1, and save its info.
    index <- which(cue_ranks==1)
    conditional_cue_validities[index] <- cue_validities_with_reverse[index]
    conditional_cue_ranks[index] <- cue_rank_so_far
    conditional_cue_directions[index] <- cue_directions[index]
    cue_rank_so_far <- cue_rank_so_far + 1
    # Keep only rows where this cue has not decided
    agreement <- subset(agreement, agreement[,index] == 0)
    # If any rows are left, repeat this process.  
  }
  # If some cues not used by end, they stay NA.

  # Preserve column names.
  names <- colnames(data)[cols_to_fit]
  names(conditional_cue_validities) <- names
  names(conditional_cue_ranks) <- names
  names(conditional_cue_directions) <- names

  return(list(cue_validities=conditional_cue_validities,
              cue_ranks=conditional_cue_ranks,
              cue_directions=conditional_cue_directions))
}

#' Calculate the accuracy of using a cue to predict a criterion.
#'
#' \code{\link{cueValidity}} counts only correct and incorrect inferences,
#' ignoring cases where a cue does not discriminate.  Cue accuracy gives those
#' cases a weight of 0.5, the expected accuracy of guessing.
#' It is calculated as
#' (correct + 0.5 * guesses) / (correct + incorrect + guesses).
#'
#' @param criterion A vector of values to be predicted.
#' @param cue A vector of values to predict with.  Should have the same
#'         length as the criterion.
#' @param replaceNanWith The value to return as cue validity in case it
#'         cannot be calculated, e.g. no variance in the values.
#' @return The cue accuracy, a value in the range [0,1].
#'
#' @examples
#' cueValidity(c(5,1), c(1,0))
#' cueAccuracy(c(5,1), c(1,0))
#' # Both return 1.
#' cueValidity(c(5,2,1), c(1,0,0))
#' cueAccuracy(c(5,2,1), c(1,0,0))
#' # Cue validity still returns 1 but cue accuracy returns (2+0.5)/3 = 0.833.
# TODO(jean):  #' @seealso
# TODO(jean):  #' \code{\link{cueAccuracyComplete}} for more complete output.
#' @seealso
#' \code{\link{cueValidity}} for an alternate measure used in Take The Best.
#' @export
cueAccuracy <- function(criterion, cue, replaceNanWith=0.5) {
  out <- Hmisc::rcorr.cens(cue, criterion, outx=FALSE)
  justDxy <- as.double(out["Dxy"])
  cv <- 0.5 + 0.5*justDxy
  if (is.nan(cv)) {
    cv <- replaceNanWith
  }
  return(cv)
}

cueAccuracyAppliedToColumns <- function(data, criterion_col, cols_to_fit,
                                        replaceNanWith=0.5) {
  out <- sapply(cols_to_fit, function(col) {
    cueAccuracy(data[,criterion_col], data[,col],
                replaceNanWith=replaceNanWith)
  })
  out <- c(out)
  if (length(colnames(data)) > 0) {
    names(out) <- colnames(data)[cols_to_fit]
  }
  return(out)
}
