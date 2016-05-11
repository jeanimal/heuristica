fitAllModels <- function(vec_of_models, training_set, criterion_col,
                         cols_to_fit) {
  models<-list()
  y <- 0
  for (mod in vec_of_models) {
    y <- y+1
    models[[y]] <- mod(training_set, criterion_col, cols_to_fit)
  }
  return(models)
}

#' Creates an error matrix from a matrix of predictions.
#' 
#' Given a matrix where the first 3 columns are basic info and the others
#' are predictions, with these values ranging from 0 to 1, returns the error
#' of each.
#' 
#' @param df A dataframe to convert.  Must have a column named correctProb
#'  and predictions in all but the first 3 columns.
#'  (First 3 are Row1, Row2, and correct.) 
#' @return A dataframe with the last column converted to:
#'   0: if it matched correctProb
#'   1: if it was wrong.
#'   0.5: if it guessed or the true value was a guess and it wasn't a guess.
#'  This is technically a measure of error.
#' @export
createErrorsFromPredicts <- function(df) {
  return(createErrorsFromPredicts2(df, 3, c(4:ncol(df))))
}

stopIfNonProbability <- function(data, cols_to_check) {
  #TODO(Jean): Didn't Hadley Wickham have a better version of stopifnot?
  #TODO(Jean): Report first bad column in error message.
  stopifnot(data[,cols_to_check, drop=FALSE] <= 1,
            data[,cols_to_check, drop=FALSE] >= 0)
}

createErrorsFromPredicts2 <- function(data, reference_col, cols_to_compare) {
  stopIfNonProbability(data, c(reference_col, cols_to_compare))
  for (col in cols_to_compare) {
    data[,col] <- (data[,col] - data[,reference_col] )
  }
  return(data)
}

# Note: goal_type affects which "stop" checks we do.
classificationErrorsFromPredicts <- function(data, reference_col, cols_to_compare) {
  for (col in cols_to_compare) {
    data[,col] <- (data[,col] != data[,reference_col] )
  }
  return(data)
}

#' Converts errors to percent correct for each column.
#' 
#' The errors should be the output of createErrorsFromPredicts.
#' 
#' @param errors_raw A dataframe of heuristic errors to calculate with.
#'  Assumes data starts in column 4. (First 3 are Row1, Row2, and correct.)
#' @return A dataframe with one row and the last column as a percent correct.
#' @export
createPctCorrectsFromErrors <- function(errors_raw) {
  return(createPctCorrectsFromErrors2(errors_raw, 4))
}

createPctCorrectsFromErrors2 <- function(errors_raw, startCol) {
  #TODO: Make this work without a data.frame
  errors <- data.frame(errors_raw)
  newDf <- NULL
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

#' Predicts with heuristics and criterion, returning all row pairs.
#'
#' @param fitted_heuristic_list A list of heuristics already fitted to data,
#'   e.g. ttbModel.
#' @param test_data Data to try to predict; must match columns in fit.
#' @param goal_type String identifying the goal of the models and criterion.
#'   ChooseGreater' indicates to use correctGreater and predictPairInternal.
#'   'ProbGreater' indicates to use probGreater and predictProbInternal.
#' @param ... Optionally additional row pair functions, e.g. rowIndexes().
#' @return A one-row matrix of numbers from 0 to 1, meaning proportion
#'   correct.  Each column is named with the heuristic's class.
#' @examples
#' ttb <- ttbModel(city_population, 3, c(4:ncol(city_population)))
#' reg <- regInterceptModel(city_population, 3, c(4:ncol(city_population)))
#' # Which row pairs is ttb better on?  By "better" we mean chooses the
#' # correct larger row-- that's our goal_type.
#' goal_type <- 'ChooseGreater'
#' out <- aggregatePredictPair(list(ttb, reg), city_population, goal_type,
#'   rowIndexes())
#' out_df <- data.frame(out)
#' head(out_df[out_df$ttbModel > out_df$regInterceptModel,])
#' # So one example where ttb was right and reg was wrong was row 4 vs.
#' # row 7.  Let's put some city names on this.
#' out_df$Row1_City <- city_population$Name_of_City[out_df$Row1]
#' out_df$Row2_City <- city_population$Name_of_City[out_df$Row2]
#' head(out_df[out_df$ttbModel > out_df$regInterceptModel,])
#' # So row 4 vs. 7 was Cologne vs. Dortmund.  Looking through the list,
#' # this is the only time Cologne appeared.  In contrast, Halle appears
#' # 7 times, so it's a tougher city for regression to predict than ttb.
#' @export
aggregatePredictPair <- function(fitted_heuristic_list, test_data,
                                 goal_type, ...) {
  # Assume the criterion_col is same for all heuristics.
  criterion_col <- fitted_heuristic_list[[1]]$criterion_col
  # TODO: Check and stop if a heuristics disagrees with criterion_col.
  if (goal_type=='ProbGreater') {
    all_fn_creator_list <- list(probGreater(criterion_col),
                                heuristicsList(fitted_heuristic_list,
                                               fn=predictProbInternal), ...)
  } else if (goal_type=='ChooseGreater') {
    all_fn_creator_list <- list(correctGreater(criterion_col),
                                heuristicsList(fitted_heuristic_list,
                                               fn=predictPairInternal), ...)
  } else {
    stop(paste("Unrecognized goal_type: ", goal_type))
  }
  predictions <- allRowPairApplyList(test_data, all_fn_creator_list)
  return(predictions)
}

#'  Percent correct of all heuristics for all row pairs in test_data.
#'
#' Returns overall percent correct for all heuristics.
#' 1. Create predictions of row pairs for all heuristics in the list.
#' 2. Calculate overall percent correct for each heuristic.
#' Assumes the heuristics passed in have already been fitted to training
#' data and all have the same criterion column.
#'
#' @param fitted_heuristic_list A list of heuristics already fitted to data,
#'   e.g. ttbModel.
#' @param test_data Data to try to predict; must match columns in fit.
#' @return A one-row data.frame of numbers from 0 to 1, meaning proportion
#'   correct.  Each column is named with the heuristic's class or the fit name.
#'
#' @examples
#' df <- data.frame(y=c(30,20,10,5), name=c("a", "b", "c", "d"),
#'                  x1=c(1,1,0,0), x2=c(1,1,0,1))
#' ttb <- ttbModel(df, 1, c(3:4))
#' sing <- singleCueModel(df, 1, c(3:4))
#' pctCorrectOfPredictPair(list(ttb, sing), df)
#' #    ttbModel singleCueModel
#' #  1     0.75      0.8333333
#' # TTB gets 75% correct while single cue model gets 83%.
#'
#' # Now repeatedly sample 2 rows of the data set and see how outcomes are
#' # affected, tracking with the fit_name.
#' set.seed(1) # If you want to reproduce the same output as below.
#' ttb1 <- ttbModel(df[sample(nrow(df), 2),], 1, c(3:4), fit_name="fit1")
#' ttb2 <- ttbModel(df[sample(nrow(df), 2),], 1, c(3:4), fit_name="fit2")
#' ttb3 <- ttbModel(df[sample(nrow(df), 2),], 1, c(3:4), fit_name="fit3")
#' pctCorrectOfPredictPair(list(ttb1, ttb2, ttb3), df)
#' #        fit1 fit2 fit3
#' # 1 0.8333333 0.75 0.75
#'
#' @export
pctCorrectOfPredictPair <- function(fitted_heuristic_list, test_data) {
  return(as.data.frame(pctCorrectOfPredictPairReturnMatrix(
    fitted_heuristic_list, test_data)))
}

#' Percent correct of all heuristics on test_data, returning a matrix.
#'
#' Returns overall percent correct for all heuristics.
#' 1. Create predictions of row pairs for all heuristics in the list.
#' 2. Calculate overall percent correct for each heuristic.
#' Assumes the heuristics passed in have already been fitted to training
#' data and all have the same criterion column.
#'
#' @param fitted_heuristic_list A list of heuristics already fitted to data,
#'   e.g. ttbModel.
#' @param test_data Data to try to predict; must match columns in fit.
#' @return A one-row matrix of numbers from 0 to 1, meaning proportion correct.
#'   Each column is named with the heuristic's class or the fit name.
#'
#' @examples
#' # See examples for pctCorrectOfPredictPair, which returns a data.frame.
#'
#' @seealso
#' \code{\link{pctCorrectOfPredictPair}} for a version that returns a
#'   data.frame and includes several examples.
#' @export
pctCorrectOfPredictPairReturnMatrix <- function(fitted_heuristic_list,
                                                test_data) {
  goal_type <- 'ChooseGreater'
  predictions <- aggregatePredictPair(
    fitted_heuristic_list, test_data, goal_type)
  return(categoryAccuracyAll(predictions, 1, c(2:ncol(predictions))))
}


#' pctCorrectOfPredictPair for non-symmetric heuristics
#'
#' Same as pctCorrectOfPredictPair but for weird heuristics that do not
#' consistently choose the same row.  If a symmetric heuristics says
#' row1 > row2, then it will also says row2 < row1.  Those can be used
#' with pctCorrectOfPredictPair.  All heuristics built into heuristica
#' quality.  They will get the same answers for pctCorrectOfPredictPair
#' and pctCorrectOfPredictPairNonSymmetric.  But a non-symmetric heuristic
#' will only get correct answers for pctCorrectOfPredictPairNonSymmetric.
#'
#' @param fitted_heuristic_list A list of heuristics already fitted to data,
#'   e.g. ttbModel.
#' @param test_data Data to try to predict; must match columns in fit.
#' @return A one-row matrix of numbers from 0 to 1, meaning proportion
#'   correct.  Each column is named with the heuristic's class.
#' @seealso
#' \code{\link{pctCorrectOfPredictPair}} for prediction.
#' @export
pctCorrectOfPredictPairNonSymmetric <- function(fitted_heuristic_list,
                                                test_data) {
  # Assume the criterion_col is same for all heuristics.
  criterion_col <- fitted_heuristic_list[[1]]$criterion_col
  # TODO: Check and stop if a heuristics disagrees with criterion_col.
  all_fn_creator_list <- list(probGreater(criterion_col),
                              heuristicsList(fitted_heuristic_list))
  predictions_fwd <- allRowPairApplyList(test_data, all_fn_creator_list)
  test_data_rev <- test_data[c(nrow(test_data):1),]
  predictions_rev <- allRowPairApplyList(test_data_rev, all_fn_creator_list)
  predictions <- rbind(predictions_fwd, predictions_rev)
  errors <- createErrorsFromPredicts2(predictions, 1, c(2:ncol(predictions)))
  df <- createPctCorrectsFromErrors2(errors, 2)
  return(df)
}
