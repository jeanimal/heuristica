
#' Assuming you have a matrix with a columns row1 and row2,
#' this helps you get the row which matches those columns.
#' getPrediction makes code below more readable.
#' @param df Data.frame to extra row from
#' @param row1 The value in the row1 column to look for
#' @param row2 The value in the row2 column to look for
#' @return a row of the data frame.  (This could be multiple rows
#'   if multiple rows match.)
#' @export
getPredictionRow <- function(df, row1=NULL, row2=NULL) {
  if (is.null(row1) || is.null(row2)) {
    stop("You must set both row1 and row2")
  }
  lastCol <- ncol(df)
  return(df[(df$Row1==row1) & (df$Row2==row2),])
}

#' Generates a matrix of correct values an predictions among alternatives.
#' 
#' @param fitted_heuristic_list List of heuristics that implement the generic function
#'  predictAlternative, e.g. ttbBinModel.  All heuristics must agree on the criterion_col.
#' @param test_data Data to try to predict; must match columns in fit.
#' @param rowPairs An optional matrix where the first two columns are the pairs
#'  of row indices to use in the test_data.  If not set, all pairs will be used.
#' @return Same matrix as predictAlternative but with columns on correctness
#' @seealso
#' \code{\link{predictAlternative}}
#' @export
predictAlternativeWithCorrect <- function(fitted_heuristic_list, test_data,
                                          rowPairs=NULL) {
  if (is.null(rowPairs)) {
    n <- nrow(test_data)
    rowPairs <- rowPairGenerator(n)
  }
  if (length(fitted_heuristic_list) == 0) {
    stop("No fitted heuristics.")
    # We could allow this if we had a different way to specify criterion_col
  }
  criterion_col = fitted_heuristic_list[[1]]$criterion_col
  #for (heuristic in fitted_heuristic_list) {
  #  criterion_col = heuristic$criterion_col
  #}
  #TODO: make sure no heuristics disagree with that criterion_col
  
  correctValues <- test_data[,criterion_col]
  correctProb <-  apply(rowPairs, 1,
                        function(rowPair) pairToValue(correctValues[rowPair]))
  resultMatrix <- cbind(rowPairs, correctProb)
  extendedMatrix <- resultMatrix
  for (heuristic in fitted_heuristic_list) {
    predictMatrix <- predictAlternative(heuristic, test_data, rowPairs=rowPairs)
    # TODO(jean): This assumes rowPairs match up.  Is that a safe assumption?
    extendedMatrix <- cbind(extendedMatrix, model=predictMatrix[,ncol(predictMatrix)])
    model_name <- class(heuristic)[1]
    names(extendedMatrix)[ncol(extendedMatrix)] = model_name
  }
  return(extendedMatrix)
}

#' Creates an error matrix from a matrix of predictions.
#' 
#' Given a matrix where the first 3 columns are basic info and the others are
#' predictions, with these values ranging from 0 to 1, returns the error of each.
#' 
#' @param df A dataframe to convert.  Assumes it has a column named correctProb
#'  and predictions are in all but the first 3 columns.
#'  (First 3 are Row1, Row2, and correct.) 
#' @return A dataframe with the last column converted to:
#'   0: if it matched correctProb
#'   1: if it was wrong.
#'   0.5: if it guessed or the true value was a guess and it wasn't a guess.
#'  This is technically a measure of error.
#' @export
createErrorsFromPredicts <- function(df) {
  for (col in 4:ncol(df)) {
    df[,col] <- (df[,col] - df$correctProb )
  }
  return(df)
}

#' Converts output of createErrorsFromPredicts to percent correct for each column.
#' 
#' @param errors A dataframe of heuristic errors to calculate with.  Assumes data
#'  starts in column 4. (First 3 are Row1, Row2, and correct.)
#' @return A dataframe with one row and the last column as a percent correct.
#' @export
createPctCorrectsFromErrors <- function(errors) {
  newDf <- NULL
  startCol <- 4
  for (col in startCol:ncol(errors)) {
    sumError <- sum(abs(errors[,col]))
    if (is.null(newDf)) {
      newDf <- data.frame((nrow(errors)-sumError) /nrow(errors))
    } else {
      newDf <- cbind(newDf, (nrow(errors)-sumError) /nrow(errors))
    }
    names(newDf)[[ncol(newDf)]] <- names(errors)[[ncol(newDf)+startCol-1]]
  }
  return(newDf)
}

#' Runs predictAlternative for fitted heuristics to get percent correct on test.
#' 
#' Combines other functons to 
#' 1. Create predictions of alternatives for all the heuristics in the list.
#' 2. Get errors of those predictions.
#' 3. Calculate overall percent correct for each heuristic.
#' Assumes the heuristics passed in have already been fitted to train_data.
#'  
#' Each of the 3 steps above can be output if you call the helper functions directly:
#' 1. predictAlternativeWithCorrect
#' 2. createErrorsFromPredicts
#' 3. createPctCorrectsFromErrors
#'  
#' @param fitted_heuristic_list A list of heuristics already fitted to data, e.g. ttbBinModel.
#' @param test_data Data to try to predict; must match columns in fit.
#' @return A one-row matrix of numbers from 0 to 1 indicating the percent correct.
#' @export
pctCorrectOfPredictAlternative <- function(fitted_heuristic_list, test_data) {
  predictions <- predictAlternativeWithCorrect(fitted_heuristic_list, test_data)
  errors <- createErrorsFromPredicts(predictions)
  return(createPctCorrectsFromErrors(errors))
}
