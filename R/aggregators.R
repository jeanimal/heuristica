fitAllModels <- function(vec_of_models, training_set, criterion_col, cols_to_fit) {
  models<-list()
  y=0
  for (mod in vec_of_models) {
    y=y+1
    models[[y]] <- mod(training_set, criterion_col, cols_to_fit)
  }
  return(models)
}

# Private.
pairToValue <- function(pair,na.replace=FALSE) {
  if(na.replace==TRUE ){
    pair[which(is.na(pair))] <- 0.5
    if (pair[1] > pair[2]) {
      return(1)
    } else if (pair[2] > pair[1]) {
      return(0)
    } else {
      return(0.5)
    }
  } else {
    if (pair[1] > pair[2]) {
      return(1)
    } else if (pair[2] > pair[1]) {
      return(0)
    } else {
      return(0.5)
    }
  }
}


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

#' Assuming you have a matrix with a columns row1 and row2,
#' this helps you get the row which matches those columns.
#' getPrediction makes code below more readable.
#' @param df Data.frame to extra row from
#' @param row1 The value in the row1 column to look for
#' @param row2 The value in the row2 column to look for
#' @return a row of the data frame.  (This could be multiple rows
#'   if multiple rows match.)
#' @export
getPredictionRowLC <- function(df, row1=NULL, row2=NULL) {
  if (is.null(row1) || is.null(row2)) {
    stop("You must set both row1 and row2")
  }
  lastCol <- ncol(df)
  #TODO(jean): Share this code with getPredictiono?
  #TODO(jean): Do sorting so it works even if row2 index < row1 index.
  return(df[(df$row1==row1) & (df$row2==row2),])
}

#' Generates a matrix of predictAlternative predictions.
#'
#' This geneartes a column of correct output (whether row 1 or row 2 is greater) from
#' the test matrix, then runs all the heuristics in order, generating a column of
#' predictions for each, naming each column from the heuristic class.
#'
#' @param fitted_heuristic_list List of heuristics that implement the generic function
#'  predictAlternative, e.g. ttbBinModel.  All heuristics must agree on the criterion_col.
#' @param test_data Data to try to predict; must match columns in fit.
#' @param row_pairs An optional matrix where the first two columns are the pairs
#'  of row indices to use in the test_data.  If not set, all pairs will be used.
#' @return Same matrix as predictAlternative but with columns on correctness
#' @seealso
#' \code{\link{predictAlternative}}
#' @export
predictAlternativeWithCorrect <- function(fitted_heuristic_list, test_data,
                                          row_pairs=NULL) {
  if (is.null(row_pairs)) {
    n <- nrow(test_data)
    row_pairs <- rowPairGenerator(n)
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
  correctProb <-  apply(row_pairs, 1,
                        function(rowPair) pairToValue(correctValues[rowPair]))
  resultMatrix <- cbind(row_pairs, correctProb)
  extendedMatrix <- resultMatrix
  for (heuristic in fitted_heuristic_list) {
    predictMatrix <- predictAlternative(heuristic, test_data, row_pairs=row_pairs)
    # TODO(jean): This assumes row_pairs match up.  Is that a safe assumption?
    extendedMatrix <- cbind(extendedMatrix, model=predictMatrix[,ncol(predictMatrix)])
    model_name <- class(heuristic)[1]
    names(extendedMatrix)[ncol(extendedMatrix)] = model_name
  }
  return(extendedMatrix)
}


#' Generates a matrix of predictPair predictions.
#'
#' This geneartes a column of correct output (whether row 1 or row 2 is greater) from
#' the test matrix, then runs all the heuristics in order, generating a column of
#' predictions for each, naming each column from the heuristic class.
#'
#' @param fitted_heuristic_list List of heuristics that implement the generic function
#'  predictAlternative, e.g. ttbBinModel.  All heuristics must agree on the criterion_col.
#' @param test_data Data to try to predict; must match columns in fit.
#' @param subset_rows An optional vector of row indices to use in the test_data.  If not
#'  set, all pairs will be used.  TODO(jean): Implement this!
#' @return Same matrix as predictAlternative but with columns on correctness
#' @seealso
#' \code{\link{predictAlternative}}
#' @export
predictPairWithCorrect <- function(fitted_heuristic_list, test_data, subset_rows=NULL) {
  if (length(fitted_heuristic_list) == 0) {
    stop("No fitted heuristics.")
    # We could allow this if we had a different way to specify criterion_col
  }
  criterion_col = fitted_heuristic_list[[1]]$criterion_col
  #for (heuristic in fitted_heuristic_list) {
  #  criterion_col = heuristic$criterion_col
  #}
  #TODO: make sure no heuristics disagree with that criterion_col
  
  #TODO(Jean): This is the nth time I wrote this function.
  correct_values <- test_data[,criterion_col]
  row_1_bigger_function <- function(row_pair) 0.5 * (1+sign(correct_values[row_pair[[1]]]
                                                   - correct_values[row_pair[[2]]]))
  #TODO: Use subset_rows
  correctProb <- as.vector(combn(nrow(test_data), 2, row_1_bigger_function ))
  #TODO: Only verbose will include a matrix with row_pairs
  row_pairs <- t(combn(nrow(test_data), 2))
  resultMatrix <- cbind(row_pairs, correctProb)
  extendedMatrix <- data.frame(resultMatrix)
  #TODO(jean): Make this work for matrix, not just data.frame.
  for (heuristic in fitted_heuristic_list) {
    predictMatrix <- predictPair(heuristic, test_data, subset_rows=subset_rows, verbose_output=FALSE)$predictions
    # TODO(jean): This assumes row_pairs match up.  Is that a safe assumption?
    model_name <- class(heuristic)[1]
    extendedMatrix <- cbind(extendedMatrix, model=predictMatrix[,ncol(predictMatrix)])
    names(extendedMatrix)[ncol(extendedMatrix)] <- model_name
  }
  names(extendedMatrix)[1] <- "row1"
  names(extendedMatrix)[2] <- "row2"
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
    df[,col] <- (df[,col] - df[,3] )
    #df[,col] <- (df[,col] - df$correctProb )
  }
  return(df)
}

#' Converts output of createErrorsFromPredicts to percent correct for each column.
#' 
#' @param errors_raw A dataframe of heuristic errors to calculate with.  Assumes data
#'  starts in column 4. (First 3 are Row1, Row2, and correct.)
#' @return A dataframe with one row and the last column as a percent correct.
#' @export
createPctCorrectsFromErrors <- function(errors_raw) {
  #TODO: Make this work without a data.frame
  errors <- data.frame(errors_raw)
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

#' Runs predictPair for fitted heuristics to get percent correct on test.
#'
#' Combines other functons to 
#' 1. Create predictions of alternatives for all the heuristics in the list.
#' 2. Get errors of those predictions.
#' 3. Calculate overall percent correct for each heuristic.
#' Assumes the heuristics passed in have already been fitted to train_data.
#'
#' @param fitted_heuristic_list A list of heuristics already fitted to data, e.g. ttbBinModel.
#' @param test_data Data to try to predict; must match columns in fit.
#' @return A one-row matrix of numbers from 0 to 1 indicating the percent correct.
#' @export
pctCorrectOfPredictPair <- function(fitted_heuristic_list, test_data) {
  predictions <- predictPairWithCorrect(fitted_heuristic_list, test_data)
  errors <- createErrorsFromPredicts(predictions)
  df <- createPctCorrectsFromErrors(errors)
  return(df)
}
