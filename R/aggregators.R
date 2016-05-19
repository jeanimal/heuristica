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
#' reg <- regModel(city_population, 3, c(4:ncol(city_population)))
#' # Which row pairs is ttb better on?  By "better" we mean chooses the
#' # correct larger row-- that's our goal_type.
#' goal_type <- 'ChooseGreater'
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

#'Percent correct of heuristics' predictPair on test_data.
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
  goal_type <- 'ChooseGreater'
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


# For the test.  Kinda hacky.  Need a better way.
fitted_always_1 <- structure(list(criterion_col=1, cols_to_fit=c(2)),
                             class="all1Model")
predictPairInternal.all1Model <- function(object, row1, row2) { return(1) }
