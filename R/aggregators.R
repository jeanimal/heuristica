#' Predicts with heuristics and criterion, returning all row pairs.
#'
#' @param fitted_heuristic_list A list of heuristics already fitted to data,
#'   e.g. ttbModel.
#' @param test_data Data to try to predict; must match columns in fit.
#' @param goal_type String identifying the goal of the models and criterion.
#'   'CorrectGreater' indicates to use correctGreater and predictPairInternal.
#'   'ProbGreater' indicates to use probGreater and predictProbInternal.
#' @param ... Optionally additional row pair functions, e.g. rowIndexes().
#' @return A one-row matrix of numbers from 0 to 1, meaning proportion
#'   correct.  Each column is named with the heuristic's class.
#' @examples
#' ttb <- ttbModel(city_population, 3, c(4:ncol(city_population)))
#' reg <- regModel(city_population, 3, c(4:ncol(city_population)))
#' # Which row pairs is ttb better on?  By "better" we mean chooses the
#' # correct larger row-- that's our goal_type.
#' goal_type <- 'CorrectGreater'
#' out <- aggregatePredictPair(list(ttb, reg), city_population, goal_type,
#'   rowIndexes())
#' out_df <- data.frame(out)
#' head(out_df[out_df$ttbModel > out_df$regModel,])
#' # So one example where ttb was right and reg was wrong was row 4 vs.
#' # row 7.  Let's put some city names on this.
#' out_df$Row1_City <- city_population$Name_of_City[out_df$Row1]
#' out_df$Row2_City <- city_population$Name_of_City[out_df$Row2]
#' head(out_df[out_df$ttbModel > out_df$regModel,])
#' # So row 4 vs. 7 was Cologne vs. Dortmund.  Looking through the list,
#' # this is the only time Cologne appeared.  In contrast, Halle appears
#' # 7 times, so it's a tougher city for regression to predict than ttb.
#' @keywords internal
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
  } else if (goal_type=='CorrectGreater') {
    all_fn_creator_list <- list(correctGreater(criterion_col),
                                heuristicsList(fitted_heuristic_list,
                                               fn=predictPairInternal), ...)
  } else {
    stop(paste("Unrecognized goal_type: ", goal_type))
  }
  predictions <- rowPairApplyList(test_data, all_fn_creator_list)
  return(predictions)
}

# Same as predictPairFullConfusionMatrix bur returns a 3x3 matrix,
# leaving guesses intact as 0's.
predictPairFullConfusionMatrix <- function(test_data, fitted_heuristic,
                                           symmetric_model=TRUE) {
  goal_type <- 'CorrectGreater'
  out_fwd <- aggregatePredictPair(list(fitted_heuristic), test_data, goal_type)
  test_data_rev <- test_data[c(nrow(test_data):1),]
  if (symmetric_model) {
    # The model's prediction in A vs. B = - prediction in B vs. A.
    out <- rbind(out_fwd, -out_fwd)
  } else {
    # Need to re-run the model to figure out what it says in B vs. A.
    out_rev <- aggregatePredictPair(list(fitted_heuristic), test_data_rev, goal_type)
    out <- rbind(out_fwd, out_rev)
  }
  correct <- out[,1]
  predictions <- out[,2]
  return(confusionMatrixRequiredCategories(correct, predictions, c(-1,0,1)))
}

#' Make a confusion matrix using predictPair with the heuristic and test_data.
#'
#' @param test_data A data set.  Must have the criterion_column and cols_to_fit
#'   that the heuristic expects.
#' @param fitted_heuristic A fitted heuristic that implements predictPair
#' @param guess_handling_fn A function to call on the 3x3 confusion matrix to
#'   assign a model's guesses-- 0 predictions tracked in the 2nd column-- to
#'   -1 or 1 counts.
#' @param tie_handling_fn A function to call on the 3x3 confusion matrix to
#'   distribute ties-- 0 correct answers tracked in the 2nd row-- to -1 or 1
#'   counts.
#' @param symmetric_model Optional parameter that is TRUE by default because
#'   all models in heuristica are symmtric.  (For an asymmteric model, this
#'   function will run both A vs. B and B vs. A through predicPair.)
#' @return A 2x2 confusion matrix with rows and columns for -1 and 1.  It will
#'   be 2x2 even if predictions and correct do not cover the full range.
#'
#' @references
#' Wikipedia's entry on
#' \url{https://en.wikipedia.org/wiki/Confusion_matrix}.
#'
#' @export
predictPairConfusionMatrix <- function(test_data, fitted_heuristic,
                                       guess_handling_fn=distributeGuessAsExpectedValue,
                                       tie_handling_fn=distributeTies,
                                       symmetric_model=TRUE) {
  matrix3x3 <- predictPairFullConfusionMatrix(test_data, fitted_heuristic,
                                              symmetric_model=symmetric_model)
  matrix2x2 <- collapseConfusionMatrix3x3To2x2(matrix3x3,
                                               guess_handling_fn=guess_handling_fn,
                                               tie_handling_fn=tie_handling_fn)
  return(matrix2x2)
}

#' Percent correct of heuristics' predictPair on test_data, returning a matrix.
#'
#' @param fitted_heuristic_list A list of heuristics already fitted to data,
#'   e.g. ttbModel.
#' @param test_data Data to try to predict; must match columns in fit.
#' @return A one-row matrix of numbers from 0 to 1, meaning proportion correct.
#'   Each column is named with the heuristic's class or the fit name.
#'
#' @examples
#' # See examples for percentCorrect, which returns a data.frame.
#'
#' @seealso
#' \code{\link{percentCorrect}} for a version that returns a
#'   data.frame and includes several examples.
#' @export
percentCorrectReturnMatrix <- function(fitted_heuristic_list,
                                       test_data) {
  goal_type <- 'CorrectGreater'
  predictions <- aggregatePredictPair(
    fitted_heuristic_list, test_data, goal_type)
  return(categoryAccuracyAll(predictions, 1, c(2:ncol(predictions))))
}

#' Percent correct of heuristics' predictPair on test_data.
#'
#' Returns overall percent correct for all heuristics.
#' 1. Create predictions using predictPair for all row pairs for all
#' fitted heuristics in the list.
#' 2. Calculate percent correct for each heuristic.
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
#' percentCorrect(list(ttb, sing), df)
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
#' percentCorrect(list(ttb1, ttb2, ttb3), df)
#' #        fit1 fit2 fit3
#' # 1 0.8333333 0.75 0.75
#'
#' @seealso
#'   using one fitted heuristic.
#' @export
percentCorrect <- function(fitted_heuristic_list, test_data) {
  return(as.data.frame(percentCorrectReturnMatrix(
    fitted_heuristic_list, test_data)))
}

#' percentCorrect for non-symmetric heuristics
#'
#' Same as percentCorrect but for weird heuristics that do not
#' consistently choose the same row.  If a symmetric heuristics says
#' row1 > row2, then it will also says row2 < row1.  Those can be used
#' with percentCorrect.  All heuristics built into heuristica
#' quality.  They will get the same answers for percentCorrect
#' and percentCorrectNonSymmetric.  But a non-symmetric heuristic
#' will only get correct answers for percentCorrectNonSymmetric.
#'
#' @param fitted_heuristic_list A list of heuristics already fitted to data,
#'   e.g. ttbModel.
#' @param test_data Data to try to predict; must match columns in fit.
#' @return A one-row matrix of numbers from 0 to 1, meaning proportion
#'   correct.  Each column is named with the heuristic's class.
#' @seealso
#' \code{\link{percentCorrect}} for prediction.
#' @export
percentCorrectNonSymmetric <- function(fitted_heuristic_list,
                                                test_data) {
  goal_type <- 'CorrectGreater'
  predictions_fwd <- aggregatePredictPair(
    fitted_heuristic_list, test_data, goal_type)
  test_data_rev <- test_data[c(nrow(test_data):1),]
  predictions_rev <- aggregatePredictPair(
    fitted_heuristic_list, test_data_rev, goal_type)
  predictions <- rbind(predictions_fwd, predictions_rev)
  pct_correct_matrix <- categoryAccuracyAll(
    predictions, 1, c(2:ncol(predictions)))
  return(data.frame(pct_correct_matrix))
}


# For tests.  Kinda hacky.  Need a better way.
fitted_always_1 <- structure(list(criterion_col=1, cols_to_fit=c(2)),
                             class="all1Model")
predictPairInternal.all1Model <- function(object, row1, row2) { return(1) }

fitted_always_0 <- structure(list(criterion_col=1, cols_to_fit=c(2)),
                             class="all0Model")
predictPairInternal.all0Model <- function(object, row1, row2) { return(0) }
