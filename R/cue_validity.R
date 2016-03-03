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
  #out <- Hmisc::rcorr.cens(criterion, cue, outx=TRUE)
  out <- Hmisc::rcorr.cens(cue, criterion, outx=TRUE)
  justDxy <- as.double(out["Dxy"])
  cv <- 0.5 + 0.5*justDxy
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
#' @export
matrixCueValidity <- function(data, criterion_col, cols_to_fit, 
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

