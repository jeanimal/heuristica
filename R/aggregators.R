#' Returns the row indices, correct answer, and predictions for all row pairs.
#'
#' This makes it easy to see and evaluate predictions for all row pairs on
#' a data set.  It is intended for beginners.  Advanced users can get more
#' fine-grained control with rowPairApply.
#'
#' @param test_data Data to try to predict.  Must have same criterion column
#'   and cols_to_fit as the data heuristics were fit to.
#' @param ... One or more heuristics already fitted to data, e.g. the output
#'   of ttbModel.
#'
#' @return A matrix with output for indices, the correct row pair answer, and
#'   predictions for each heuristic with as many rows as row pairs in the data.
#'   The columns names are Row1, Row2, CorrectGreater, and each heuristic fit_name
#'   (which is its class name by default, e.g. ttbModel).
#' @examples
#' # Get some data and fit it with two models.
#' train_df <- data.frame(criterion=c(9,8,7,6), a=c(101,101,2,2), b=c(59,58,5,59))
#' criterion_col <- 1
#' ttb <- ttbModel(train_df, criterion_col, c(2:3))
#' lreg <- logRegModel(train_df, criterion_col, c(2:3))
#'
#' # Generate predictions and correct answers with predictPairSummary.
#' out <- predictPairSummary(train_df, ttb, lreg)
#'
#' # Find rows where the models make differing predictions, subsetting on a
#' # data.frame.
#' out_df <- data.frame(out)
#' out_df[out_df$ttbModel != out_df$logRegModel,]
#' # Outputs:
#' #   Row1 Row2 CorrectGreater ttbModel logRegModel
#' #    1    2              1        1          -1
#' #    3    4              1       -1           1
#' # So there are only two cases of differing predictions.
#' # 1) For row 1 vs. 2, TTB predicted 1 and logReg predicted -1.
#' #    CorrectGreater says 1, so TTB was right.
#' # 2) For row 3 vs. 4, TTB predicted -1 and logReg predicted 1.
#' #    CorrectGreater says -1, so logReg was right.
#' 
#' # Note that under the hood, the above predictPairSummary call could be
#' # done with rowPairApply like this:
#' out2 <- rowPairApply(train_df, rowIndexes(),
#'                      correctGreater(criterion_col), heuristics(ttb, lreg))
#'
#' @seealso
#' \code{\link{rowPairApply}} for full flexibility.
#' @export
predictPairSummary <- function(test_data, ...) {
  fitted_heuristic_list <- list(...)
  # Assume the criterion_col is same for all heuristics.
  criterion_col <- fitted_heuristic_list[[1]]$criterion_col
  # TODO: Check and stop if a heuristics disagrees with criterion_col.

  predictions <- rowPairApply(test_data, rowIndexes(),
                              correctGreater(criterion_col),
                              heuristicsList(fitted_heuristic_list,
                                             fn=predictPairInternal))
  return(predictions)
}

# In the simplest case, if all heuristics have the same cols_to_fit, returns
# list(heuristicsList(fitted_heuristic_list), fn).
# If the heuristics have differing col_to_fit, there are put in separate
# heursticsList() groups, with order preserved.
# It will throw an error if any heuristics disagree on the criterion_col.
# fitted_heuristic_list: list of fitted heuristics
# fn: the function to pass to heuristicsList, e.g. predictPairInternal
# private
heuristicsListGroupedByColsToFit <- function(fitted_heuristic_list, fn) {
  if (length(fitted_heuristic_list) == 0) {
    stop("No fitted heuristics in list")
  }
  criterion_col <- fitted_heuristic_list[[1]]$criterion_col
  cols_to_fit <- fitted_heuristic_list[[1]]$cols_to_fit
  all_fn_creator_list <- list()
  implementer_list <- list()
  for (implementer in fitted_heuristic_list) {
    if (! isTRUE(all.equal(criterion_col, implementer$criterion_col)) ) {
      stop(paste("ERROR: Models with different criterion_col:", criterion_col,
                 "vs.", implementer$criterion_col, "."))
    }
    
    if (isTRUE(all.equal(cols_to_fit, implementer$cols_to_fit)) ) {
      # If cols_to_fit agree, they can be in the same list together.
      implementer_list[[length(implementer_list)+1]] <- implementer
    } else {
      # We detected an implementer with different cols_to_fit.
      # Finish off the previous list and start a new one.
      temp <- heuristicsList(implementer_list, fn=predictPairInternal)
      all_fn_creator_list[[length(all_fn_creator_list)+1]] <- temp
      implementer_list <- list(implementer)
      cols_to_fit <- implementer$cols_to_fit
    }
  }
  # Finish off any remaining implementers.
  all_fn_creator_list[[length(all_fn_creator_list)+1]] <-
    heuristicsList(implementer_list, fn=predictPairInternal)
  return(all_fn_creator_list)
}

#' Percent correct of heuristics' predictPair on test_data, returning a matrix.
#'
#' @param test_data Data to try to predict.  Must have same criterion column
#'   and cols_to_fit as the data heuristics were fit to.
#' @param fitted_heuristic_list A list of one or more heuristics fitted to
#'   data, e.g. the output of ttbModel.
#' @return A one-row matrix of numbers from 0 to 100, the percent correct
#'   of each heuristic.  Each column is named with the heuristic's class or
#'   the fit name.
#'
#' @examples
#' # See examples for percentCorrectList, which returns a data.frame.
#'
#' @seealso
#' \code{\link{percentCorrectList}} for a version that returns a
#'   data.frame and includes several examples.
#' @export
percentCorrectListReturnMatrix <- function(test_data, fitted_heuristic_list) {
  heuristic_fn_creator_list <- heuristicsListGroupedByColsToFit(fitted_heuristic_list)
  # The prior function ensured all heuristics have the same criterion_col, so get it
  # from any heuristic.
  criterion_col <- fitted_heuristic_list[[1]]$criterion_col
  all_fn_creator_list <- c(list(correctGreater(criterion_col)),
                           heuristic_fn_creator_list)
  
  predictions <- rowPairApplyList(test_data, all_fn_creator_list,
                                  also_reverse_row_pairs=FALSE)
  
  return(100 * categoryAccuracyAll(predictions, 1, c(2:ncol(predictions))))
}

#' Percent correct of a list of heuristics' predictPair on test_data.
#'
#' Returns overall percent correct for all heuristics.
#' 1. Create predictions using predictPair for all row pairs for all
#' fitted heuristics in the list.
#' 2. Calculate percent correct for each heuristic.
#' Assumes the heuristics passed in have already been fitted to training
#' data and all have the same criterion column.
#'
#' @param test_data Data to try to predict.  Must have same criterion column
#'   and cols_to_fit as the data heuristics were fit to.
#' @param fitted_heuristic_list A list of one or more heuristics fitted to
#'   data, e.g. the output of ttbModel.
#' @return A one-row data.frame of numbers from 0 to 100, the percent correc
#'   of each heuristic.  Each column is named with the heuristic's class or
#'   the fit name.
#'
#' @examples
#' df <- data.frame(y=c(30,20,10,5), name=c("a", "b", "c", "d"),
#'                  x1=c(1,1,0,0), x2=c(1,1,0,1))
#' ttb <- ttbModel(df, 1, c(3:4))
#' sing <- singleCueModel(df, 1, c(3:4))
#' percentCorrectList(df, list(ttb, sing))
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
#' percentCorrectList(df, list(ttb1, ttb2, ttb3))
#' #        fit1 fit2 fit3
#' # 1 0.8333333 0.75 0.75
#'
#' @seealso
#' \code{\link{percentCorrectList}} for a version which takes heuristics
#'   as parameters rather than wrapped in a list.
#' @export
percentCorrectList <- function(test_data, fitted_heuristic_list) {
  if (class(fitted_heuristic_list) != "list") {
    stop(paste("Second argument to percentCorrectList should be list but got",
               class(fitted_heuristic_list)))
  }
  return(as.data.frame(percentCorrectListReturnMatrix(
    test_data, fitted_heuristic_list)))
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
#' In cases where a heuristic guesses (predictPair outputs 0), percentCorrect
#' will use the expected value, so output will be deterministic and repeatable.
#' That is, if 10 guesses happen across the data set, percentCorrect will always
#' allocate 5 to 1 and 5 to -1.
#'
#' @param test_data Data to try to predict.  Must have same criterion column
#'   and cols_to_fit as the data heuristics were fit to.
#' @param ... One or more heuristics fitted to
#'   data, e.g. the output of ttbModel.
#' @return A one-row data.frame of numbers from 0 to 100, the percent correc
#'   of each heuristic.  Each column is named with the heuristic's class or
#'   the fit name.
#'
#' @examples
#' df <- data.frame(y=c(30,20,10,5), name=c("a", "b", "c", "d"),
#'                  x1=c(1,1,0,0), x2=c(1,1,0,1))
#' ttb <- ttbModel(df, 1, c(3:4))
#' sing <- singleCueModel(df, 1, c(3:4))
#' percentCorrect(df, ttb, sing)
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
#' percentCorrect(df, ttb1, ttb2, ttb3)
#' #        fit1 fit2 fit3
#' # 1 0.8333333 0.75 0.75
#'
#' @seealso
#' \code{\link{percentCorrectList}} for a version which takes a list of
#'   heuristics.
#' @export
percentCorrect <- function(test_data, ...) {
  fitted_heuristics_list <- list(...)
  return(percentCorrectList(test_data, fitted_heuristics_list))
}

#' percentCorrectList for non-symmetric heuristics
#'
#' Same as percentCorrectList but for weird heuristics that do not
#' consistently choose the same row.  When a symmetric heuristic predicts
#' row1 > row2, then it also predicts row2 < row1.  Those can be used
#' with percentCorrectList.  All heuristics built into heuristica
#' qualify.  They will get the same answers for percentCorrectList
#' and percentCorrectListNonSymmetric.  But a non-symmetric heuristic
#' will only get correct answers for percentCorrectListNonSymmetric.
#'
#' @param test_data Data to try to predict.  Must have same criterion column
#'   and cols_to_fit as the data heuristics were fit to.
#' @param fitted_heuristic_list A list of one or more heuristics fitted to
#'   data, e.g. the output of ttbModel.
#' @return A one-row data.frame of numbers from 0 to 100, the percent correc
#'   of each heuristic.  Each column is named with the heuristic's class or
#'   the fit name.
#'
#' @seealso
#' \code{\link{percentCorrectList}} which is faster but will only be accurate
#'   for symmetric heuristics.  (percentCorrectListNonSymmetric will be
#'   accurate for both symmetric and non-symmetric heuristics, but it's slower.)
#' @export
percentCorrectListNonSymmetric <- function(test_data, fitted_heuristic_list) {
  # Assume the criterion_col is same for all heuristics.
  criterion_col <- fitted_heuristic_list[[1]]$criterion_col
  
  all_fn_creator_list <- list(
    correctGreater(criterion_col),
    heuristicsList(fitted_heuristic_list, fn=predictPairInternal))
  predictions <- rowPairApplyList(test_data, all_fn_creator_list,
                                  also_reverse_row_pairs=TRUE)
  pct_correct_matrix <- categoryAccuracyAll(
    predictions, 1, c(2:ncol(predictions)))
  return(100 * data.frame(pct_correct_matrix))
}


# For tests.  Kinda hacky.  Need a better way.
fitted_always_1 <- structure(list(criterion_col=1, cols_to_fit=c(2)),
                             class="all1Model")
predictPairInternal.all1Model <- function(object, row1, row2) { return(1) }

fitted_always_0 <- structure(list(criterion_col=1, cols_to_fit=c(2)),
                             class="all0Model")
predictPairInternal.all0Model <- function(object, row1, row2) { return(0) }
