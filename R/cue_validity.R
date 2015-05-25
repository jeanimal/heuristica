# install.packages("Hmisc")
require("Hmisc")

#' Calculate the cue validity http://en.wikipedia.org/wiki/Cue_validity
#'
#' @param criterion A vector of values to be predicted.
#' @param cue A vector of values to predict with.  Should have the same
#'         length as the criterion.
#' @param replaceNanWith The value to return as cue validity in case it
#'         cannot be calculated.
#' @return The cue validity, a value in the range [0,1].
cueValidity <- function(criterion, cue, replaceNanWith=0.5) {
  out <- rcorr.cens(criterion, cue, outx=TRUE)
  justDxy <- as.double(out["Dxy"])
  cv <- 0.5 + 0.5*justDxy
  if (is.nan(cv)) { 
    cv <- replaceNanWith 
  }
  return(cv)
}

#' Calculate the cue validity for every column in the matrix.
#'
#' @param matrix The matrix whose columns are treated as cues.
#' @param criterionColIndex The index of the column used as criterion.
#' @param replaceNanWith See cueValidity.
#' @param includeCriterionValidity Whether to return the cue validity of
#'         the criterion, which will always be 1.
matrixCueValidity <- function(matrix, criterionColIndex, replaceNanWith=0.5,
includeCriterionValidity=FALSE) {
  rawOut <- apply(matrix, 2, function(x) cueValidity(matrix[,criterionColIndex], x, replaceNanWith=replaceNanWith))
  if (includeCriterionValidity == TRUE) {
    return(rawOut)
  } else {
    return(rawOut[-criterionColIndex])
  }
}

