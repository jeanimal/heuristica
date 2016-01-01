#' Heuristica Model
#' @param train_data Training/fitting data as a matrix or data.frame.
#' @param criterion_col The index of the colum in train_data that has the criterion.
#' @param cols_to_fit A vector of column indices in train_data, used to fit the criterion.
# Private.  This is just an easy way to share parameter documentation.
heuristicaModel <- function(train_data, criterion_col, cols_to_fit) NULL 

#' Reversing Model
#' @param reverse_cues Optional parameter to reverse cues as needed.  By default, 
#' the model will reverse the cue values for cues with cue validity < 0.5, so a cue
#' with validity 0 becomes a cue with validity 1.
#' Set this to FALSE if you do not want that, i.e. the cue stays validity 0.
# Private.  This is just an easy way to share parameter documentation.
reversingModel <- function(reverse_cues=TRUE) NULL

## New generics ##

#' Generic function to predict which of a pair of rows has a higher criterion.
#'
#' @param object The object that implements predictPair, e.g. a ttb model.
#' @param test_data The matrix of data to predict on.  As with predict, columns
#'  must match those used for fitting.
#' @param verbose_output Controls how much output is generated, which may slow down
#'  computations and use more memory.  When automating, set to FALSE, which will turn
#'  off outputs preced by verbose_.
#' @return A pairPredictor, which is a structure of a list of
#'  1) predictions: A vector of probabilities that the first row has a greater criterion.
#'  2) verbose_predictions: A data.frame of Row1, Row2, and predictions, combining (1)
#'     and (2) in an easily-read form.  Only output if verbose_output = TRUE.
#' @export
predictPair <- function(object, test_data, verbose_output=TRUE) UseMethod("predictPair")

#' Generic function to predict which of a pair of rows has a higher criterion.
#' 
#' Implement this for every heuristic.
#'
#' @param object The object that implements predictPair, e.g. a ttb model.
#' @param row1 The first row of cues (object$cols_to_fit columns), as a
#'   one-row matrix.
#' @param row2 The second row of cues.
#' @return A value from 0 to 1, representing the probability that row1's criterion
#'   is greater than row2's criterion.
#' @export
predictRoot <- function(object, row1, row2) UseMethod("predictRoot")


pairPredictionDFo <- function(object) UseMethod("pairPredictionDFo")

#' Extract prediction from output of predictPair for {row1, row2}.
#' 
#' @param object a pairPredictor class
#' @param row1 The index of row1 of the test_data that was predicted
#' @param row2 The index of row2 of the test_data that was predicted
#' @return The probability that row1 is greater
#' @export
getPredictiono <- function(object, row1=NULL, row2=NULL) UseMethod("getPredictiono")

## Shared helper functions ##

# Example: inferNumOriginalRows(nrow(out$predictions))
inferNumOriginalRows <- function(num_combo_rows) {
  guess <- ceiling(sqrt(num_combo_rows * 2))
  # Validate the guess worked.
  if (guess * (guess-1) / 2 != num_combo_rows) {
    stop(paste("Cannot guess number of original rows for", num_combo_rows))
  }
  return(guess)
}

# private
stopIfTrainingSetHasLessThanTwoRows <- function(train_data) {
  if (nrow(train_data) == 0) {
    stop("Training set must have at least 2 rows but had 0 rows")
  }
  if (nrow(train_data) == 1) {
    stop("Training set must have at least 2 rows but had 1 row")
  }
}

# private
handleNAs <- function(train_data, replaceNanWith) {
 if(replaceNanWith==0){
   train_data[is.na(train_data)] <- 0
 } else if (replaceNanWith==0.5) {
   train_data[is.na(train_data)] <- 0.5
 } else if (replaceNanWith=="omit"){
   rowsWithNA <- which(rowSums(is.na(train_data)) >= 1)
   train_data <- train_data[-rowsWithNA,]
 } else {
   stop("NAs not replaced. Please choose NA handling method")
 }
 return(train_data)
}


#
# TODO(jean): Delete unused experimental functions.
#

# Assume predictions were all rows from 1 to some N,
# and it will back out N.
pairPredictionMatrix <- function(predictions) {
  #if (nrow(predictions) == 0) { # It dies here, not with my stop message.
  #  stop("Cannot generate matrix with zero data")
  #}
  num_original_rows <- inferNumOriginalRows(nrow(predictions))
  row_pairs <- t(combn(num_original_rows, 2))
  # Columns are Row1, Row2, and ProbRow1Greater
  out <- cbind(row_pairs, predictions)
  return(out) 
}

pairPredictionDF<- function(predictions) {
  out <- as.data.frame(pairPredictionMatrix(predictions))
  names(out) <- c("Row1", "Row2", "ProbRow1Greater")
  return(out)
}

pairPredictionDFo.pairPredictor <- function(object) {
  return(pairPredictionDF(object$predictions))
}

# Returns just one number.  Assumes you want just the last column.
getPrediction_raw <- function(prediction_matrix, row_pair) {
  if (length(row_pair) != 2) {
    stop("row_pair should be length 2 (row1, row2) but got length "
         + length(row_pair))
  }
  # Comparing a row with itself is undefined.  I assign it 0.5.
  if (row_pair[1] == row_pair[2]) {
    stop(paste("Comparing row", row_pair[1], "with itself."))
  }
  min_index <- min(row_pair)
  max_index <- max(row_pair)
  #print(min_index)
  #print(max_index)
  mat <- prediction_matrix
  #print(head(mat))
  last_col <- ncol(mat)
  #print(last_col)
  val <- mat[(mat[,1]==min_index) & (mat[,2]==max_index),][[last_col]]
  if (min_index == row_pair[1]) {
    return(val)
  } else {
    return(1-val)
  }
}

#' @export
getPredictiono <- function(object, row1=NULL, row2=NULL) {
  if (is.null(row1) || is.null(row2)) {
    stop("You must set both row1 and row2")
  }
  row_pair <- c(row1, row2)
  #TODO(jeanw): Does this perform a copy?  If so, then shorten it.
  if (is.null(object$verbose_predictions)) {
    prediction_matrix <- pairPredictionMatrix(object$predictions)
  } else {
    prediction_matrix <- object$verbose_predictions
  }
  return(getPrediction_raw(prediction_matrix, row_pair))
}

### Helper functions ###

#' Convenience function to get one row from a matrix or data frame.
#'
#' This simply calls matrix_or_data_frame[row_index,,drop=FALSE] for you
#' but is shorter and helps you avoid forgetting drop=FALSE.  The need
#' for drop=FALSE when selecting just one row is explained here:
#' http://www.hep.by/gnu/r-patched/r-faq/R-FAQ_56.html
#'
#' @param matrix_or_data_frame A matrix or data frome from which you want one row.
#' @param row_index The integer index of the row
#' @return The selected row of the data frame.
#'
#' @export
oneRow <- function(matrix_or_data_frame, row_index) {
  matrix_or_data_frame[row_index,,drop=FALSE]
}

# Private for now. Will export and test when I settle on a name.
reverseAsNeeded <- function(cue_validities) {
  cue_validities_with_reverse <- abs(cue_validities - 0.5) + 0.5
  cue_directions <- sign(cue_validities - 0.5)
  structure(list(cue_validities_with_reverse=cue_validities_with_reverse,
                 cue_directions=cue_directions))
}

# Private.
assert_single_row <- function(row) {
  num_rows <- nrow(row)
  if (is.null(num_rows)) {
    stop(paste("Error: Object does not have row dimension.  To get one row of a",
               "matrix, be sure to use drop=FALSE, e.g. my_matrix[row_num, , drop=FALSE]"))
  } else if (num_rows != 1) {
    stop(paste("Error: Expected a single row but got", num_rows, "rows."))
  }
}

#' Predict which of a pair of rows has a higher criterion.
#' Assumes the object implements predictRoot and has $cols_to_fit.
#' Experimental.  I will give it a different name later.
#'
#' @param object The object that implements predictPair, e.g. a ttb model.
#' @param row1 The first row of cues (will apply cols_to_fit for you, based on object).
#' @param row2 The second row (will apply cols_to_fit for you, based on object).
#' @return A value from 0 to 1, representing the probability that row1's criterion
#'   is greater than row2's criterion.
#' @export
predictP2 <- function(object, row1, row2) {
  assert_single_row(row1)
  assert_single_row(row2)
  row1_clean <- as.matrix(row1[,object$cols_to_fit, drop=FALSE])
  row2_clean <- as.matrix(row2[,object$cols_to_fit, drop=FALSE])
  unname(predictRoot(object, row1_clean, row2_clean))
}

### Take The Best ###

#' Take The Best
#'
#' An implementation of the Take The Best heuristic.
#' It sorts cues in order of \code{\link{cueValidity}}, making a decision based on the first cue that
#' discriminates (has differing values on the two objects).
#'
#' It does NOT implement predict on purpose.
#' 
#' @inheritParams heuristicaModel
#' @inheritParams reversingModel
#'
#' @return An object of \code{\link[base]{class}} ttbModel.  This is a list containing at least the following components:
#'   \itemize{
#'    \item "cue_validities": A list of cue validities for the cues in order of cols_to_fit.
#'   }
#'
#' @examples
#' ## Fit column (5,4) to column (1,0), having validity 1.0, and column (0,1), validity 0.
#' ttb <- ttbModel(matrix(c(5,4,1,0,0,1), 2, 3), 1, c(2,3))
#' predictPair(ttb, matrix(c(5,4,1,0,0,1), 2, 3)) 
#' ## But this input results in an incorrect prediction:
#' predictPair(ttb, matrix(c(5,4,0,1,0,1), 2, 3)) 
#'
#' @seealso
#' \code{\link{predictPair.ttbModel}} (via \code{\link{predictPair}}) for prediction.
#' @seealso
#' Wikipedia's entry on \url{http://en.wikipedia.org/wiki/Take-the-best_heuristic}.
#'
#' @export
ttbModel <- function(train_data, criterion_col, cols_to_fit, reverse_cues=TRUE) {
  stopIfTrainingSetHasLessThanTwoRows(train_data)
  cue_validities <- matrixCueValidity(train_data, criterion_col, cols_to_fit)
  if (reverse_cues) {
    reverse_info = reverseAsNeeded(cue_validities)
    cue_validities_with_reverse <- reverse_info$cue_validities_with_reverse
    cue_directions <- reverse_info$cue_directions
  } else {
    cue_validities_with_reverse <- cue_validities
    cue_directions <- rep(1, length(cue_validities_with_reverse))
  }
  raw_ranks <- rank(cue_validities_with_reverse, ties.method="random")
  # Reverse ranks so first is last.
  cue_ranks <- length(cue_validities_with_reverse) - raw_ranks + 1
  unsigned_linear_coef <- sapply(cue_ranks, function(n) 2^(length(cue_ranks)-n) )
  # Now give negative signs for cues pointing the other way.
  linear_coef <- cue_directions * unsigned_linear_coef
  
  structure(list(criterion_col=criterion_col, cols_to_fit=cols_to_fit,
                 cue_validities=cue_validities,
                 cue_validities_with_reverse=cue_validities_with_reverse,
                 linear_coef=linear_coef),
            class="ttbModel")
}

#' Linear weights that can be used to compare pairs of cue directions.
#'
#' Do NOT apply these directly to raw data.
#'
#' @inheritParams stats::coef
#' @export
coef.ttbModel <- function(object, ...) object$linear_coef

predictRoot.ttbModel <- function(object, row1, row2) {
  direction_plus_minus_1 <- sign(sign(row1 - row2)  %*% coef(object))
  0.5 * (direction_plus_minus_1 + 1)[1,1]
}

# private.  But I might re-use it.
pairMatrix <- function(num_row, pair_evaluator_fn) {
  as.matrix(combn(num_row, 2, pair_evaluator_fn))
}

#' Run predictPair for all row pairs in test_data to generate a one-column matrix.
#'
#' @param object A fitted model that implements predictRoot.
#' @param test_data A matrix of data frame or data with object$cols_to_fit columns.
#' @return An one-column matrix.  Each row represents a pair of rows.  Use get
#'   prediction functions to find the rows if you care.  (TODO(jean): Document that
#'   more-- give examples, too.)
#'
#' @seealso
#' \code{\link{predictRoot}} for the function the fitted model must implement.
#'
#' @export
predictPairMatrix <- function(object, test_data) {
  test_data_trim <- as.matrix(test_data[,object$cols_to_fit, drop=FALSE])
  pair_evaluator_fn <- function(index_pair) predictRoot(object,
                                                        test_data_trim[index_pair[1],],
                                                        test_data_trim[index_pair[2],])
  pairMatrix(nrow(test_data_trim), pair_evaluator_fn)
}

#' Predict which of a pair of rows has a higher criterion, using Take The Best.
#'
#' @param object A fitted ttbModel.
#' @inheritParams predictPair
#'
#' @seealso
#' \code{\link{ttbModel}} for example code.
#'
#' @export
predictPair.ttbModel  <- function(object, test_data, verbose_output=TRUE) {
  predictions <- predictPairMatrix(object, test_data)
  return(structure(list(predictions=predictions), class="pairPredictor"))
}


### Dawes Model ###

#' DawesModel, a unit-weight linear model
#'
#' DawesModel is a unit-weight linear model inspired by Robyn Dawes.
#' Dawes Model assigns unit (+1 or -1) weights based on \code{\link{cueValidity}}.
#'   \itemize{
#'     \item A cue validity > 0.5 results in a weight of +1.
#'     \item A cue validity < 0.5 results in a weight of -1.
#'   }
#' This version differs from others in that it uses a weight of 0 if cue validity is 0.5
#' (rather than randomly assigning +1 or -1) to give faster convergence of average accuracy.
#'
#' @inheritParams heuristicaModel
#' @inheritParams reversingModel
#'
#' @return An object of \code{\link[base]{class}} dawesModel.  This is a list containing at least the following components:
#'   \itemize{
#'    \item "cue_validities": A list of cue validities for the cues in order of cols_to_fit.
#'    \item "linear_coef": A list of linear model coefficents (-1 or +1)
#'           for the cues in order of cols_to_fit.  (It can only return -1's if reverse_cues=TRUE.)
#'   }
#'
#' @seealso
#' \code{\link{predict.dawesModel}} (via \code{\link[stats]{predict}}) for prediction.
#' @seealso
#' \code{\link{predictPair.dawesModel}} for predicting among a pair of alternatives.
#' @seealso
#' Wikipedia's entry on \url{http://en.wikipedia.org/wiki/Unit-weighted_regression}.
#'
#' @param reverse_cues Optional parameter to reverse cues as needed.
#' @export
dawesModel <- function(train_data, criterion_col, cols_to_fit, reverse_cues=TRUE) {
  stopIfTrainingSetHasLessThanTwoRows(train_data)
  cue_validities <- matrixCueValidity(train_data, criterion_col, cols_to_fit)
  
  if (reverse_cues == TRUE){ 
    reverse_info = reverseAsNeeded(cue_validities)
    cue_validities_with_reverse <- reverse_info$cue_validities_with_reverse
    cue_directions <- reverse_info$cue_directions
    linear_coef = cue_directions
  } else {
    cue_validities_with_reverse = cue_validities
    linear_coef <- rep(1, length(cue_validities_with_reverse))
  }
  
  # Need to save fit_predictions in case user calls predict without test_data.
  fit_predictions <- predictWithWeights(train_data, cols_to_fit, linear_coef)
  fit_accuracy <- cueValidity(train_data[,criterion_col], fit_predictions)
  structure(list(criterion_col=criterion_col, cols_to_fit=cols_to_fit,
                 fit_predictions=fit_predictions, fit_accuracy=fit_accuracy,
                 cue_validities=cue_validities,
                 cue_validities_with_reverse=cue_validities_with_reverse,
                 linear_coef=linear_coef), class="dawesModel")
}

#' @inheritParams stats::coef
#' @export
coef.dawesModel <- function(object, ...) object$linear_coef

#' Generates predictions for Dawes Model 
#'
#' Implementation of \code{\link[stats]{predict}} for dawesModel.
#'
#' @param object A dawesModel.
#' @param ... Normally this would be the test data. 
#'  It is used to predict and can be a matrix or data.frame.  
#'  It must have the same cols_to_fit indices as those used in train_data.
#'
#' @return An N x 1 matrix of predicted values, or a list if there was only one cue.
#' 
#' @export
predict.dawesModel <- function(object, ...) {
  args <- eval(substitute(alist(...)))
  if (length(args)==0) {
    return(object$fit_predictions)
  } else if (length(args)==1) {
    test_data <- eval(args[[1]])
    return(predictWithWeights(test_data, object$cols_to_fit, object$linear_coef))
  } else {
    stop("Expected only one unevaluated argument (test_data) but got " +
          length(args) + ":" + args)
  }
}

predictRoot.dawesModel <- function(object, row1, row2) {
  direction_plus_minus_1 <- sign(coef(object) %*% sign(row1 - row2))
  # Convert from the range [-1, 1] to the range [0, 1], which is the 
  # probability that row 1 > row 2.
  0.5 * (direction_plus_minus_1 + 1)
}

#' Predict which of a pair of rows has a higher criterion, using Dawes' Model.
#'
#' @param object A fitted dawesModel.
#' @inheritParams predictPair
#'
#' @seealso
#' \code{\link{dawesModel}} for example code.
#'
#' @export
predictPair.dawesModel <- function(object, test_data, verbose_output=TRUE) {
  predictions <- predictPairMatrix(object, test_data)
  return(structure(list(predictions=predictions), class="pairPredictor"))
}

### Franklin's Model ###

#' Franklin's Model, a linear model weighted by cue validities
#' 
#' Franklin's Model is a linear model with weights calculated by \code{\link{cueValidity}}.
#' The name is because it was inspired by a method used by Ben Franklin.
#'
#' @inheritParams heuristicaModel
#' @inheritParams reversingModel
#'
#' @return An object of \code{\link[base]{class}} franklinModel.  This is a list containing at least the following components:
#'   \itemize{
#'    \item "cue_validities": A list of cue validities for the cues in order of cols_to_fit.
#'    \item "linear_coef": Same as cue validities for this model.
#'   }
#'
#' @seealso
#' \code{\link{predict.franklinModel}} (via \code{\link[stats]{predict}}) for prediction.
#' @seealso
#' \code{\link{predictPair.franklinModel}} for predicting among a pair of rows.
#' @export 
franklinModel <- function(train_data, criterion_col, cols_to_fit, reverse_cues=TRUE) {
  stopIfTrainingSetHasLessThanTwoRows(train_data)
  cue_validities <- matrixCueValidity(train_data, criterion_col, cols_to_fit)
  if (reverse_cues) {
    reverse_info = reverseAsNeeded(cue_validities)
    cue_validities_with_reverse <- reverse_info$cue_validities_with_reverse
    cue_directions <- reverse_info$cue_directions
  } else {
    cue_validities_with_reverse <- cue_validities
    cue_directions <- rep(1, length(cue_validities_with_reverse))
  }
  linear_coef <- cue_directions * cue_validities_with_reverse
  structure(list(criterion_col = criterion_col, cols_to_fit = cols_to_fit,
                 cue_validities = cue_validities,
                 cue_validities_with_reverse = cue_validities_with_reverse,
                 linear_coef = linear_coef),
            class="franklinModel")
}

#' @inheritParams stats::coef
#' @export
coef.franklinModel <- function(object, ...) object$linear_coef

#' Generates predictions for Franklin's Model 
#'
#' Implementation of \code{\link[stats]{predict}} for franklinModel.
#'
#' @param object A fitted franklinModel.
#' @param ... Normally this would be the test data. 
#'  It is used to predict and can be a matrix or data.frame.  
#'  It must have the same cols_to_fit indices as those used in train_data.
#'
#' @return An N x 1 matrix of predicted values, or a list if there was only one cue.
#' 
#' @export
predict.franklinModel <- function(object, ...) {
  args <- eval(substitute(alist(...)))
  if (length(args)==0) {
    return(object$fit_predictions)
  } else if (length(args)==1) {
    test_data <- eval(args[[1]])
    return(predictWithWeights(test_data, object$cols_to_fit, object$linear_coef))
  } else {
    stop("Expected only one unevaluated argument (test_data) but got " +
          length(args) + ":" + args)
  }
}

predictRoot.franklinModel <- function(object, row1, row2) {
  direction_plus_minus_1 <- sign(coef(object) %*% sign(row1 - row2))
  # Convert from the range [-1, 1] to the range [0, 1], which is the 
  # probability that row 1 > row 2.
  0.5 * (direction_plus_minus_1 + 1)
}

#' Predict which of a pair of rows has a higher criterion, using Franklin's Model.
#'
#' @param object A fitted franklinModel.
#' @inheritParams predictPair
#'
#' @seealso
#' \code{\link{franklinModel}} for example code.
#'
#' @export
predictPair.franklinModel <- function(object, test_data, verbose_output=TRUE) {
  predictions <- predictPairMatrix(object, test_data)
  return(structure(list(predictions=predictions), class="pairPredictor"))
}

### Wrappers for linear regression models ###

#' Just to share documentation
#'
#' @param train_matrix A matrix (or data.frame) of data to train (fit) the model with.
#' @param criterion_col The index of the criterion column-- "y" in the formula.
#' @param cols_to_fit A vector of column indexes to fit-- the "x's" in the formula.
# Private
regModelForDocumentation <- function(train_matrix, criterion_col, cols_to_fit) NULL

#' A wrapper to create a lm model just specifying columns, generating a formula for you.
#' 
#' @inheritParams regModelForDocumentation 
#' @param include_intercept A boolean of whether to include an intercept in the formula.
#'
#' @return An object of class lm.
#'
# Private because the exported versions are below.
lmWrapper <- function(train_matrix, criterion_col, cols_to_fit, include_intercept=TRUE) {
  train_df = as.data.frame(train_matrix)
  formula_str = paste(colnames(train_df)[criterion_col], "~",
                            paste(colnames(train_df)[cols_to_fit], collapse = "+"),
                        sep = "")
   if (include_intercept == FALSE) {
    formula_str = paste(formula_str, "-1", sep = "")
  }
  return(lm(as.formula(formula_str), data=train_df))
}

#' Linear regression wrapper for hueristica
#'
#' A wrapper to create a lm model just specifying columns, generating
#' a model formula for you.  This makes it easier to run automated comparisons with
#' other models in heuristica.
#'
#' This version assumes you always want to include the intercept.
#' 
#' @inheritParams regModelForDocumentation 
#'
#' @return An object of class regModel, which is a subclass of lm.
#'
#' @seealso
#' \code{\link{regNoIModel}} for a version that excludes the intercept.
#' @seealso
#' \code{\link{predict.lm}} for prediction.
#' @seealso
#' \code{\link{predictPair.regModel}} for predicting between a pair of rows.
#'
#' @export
regModel <- function(train_matrix, criterion_col, cols_to_fit) {
  stopIfTrainingSetHasLessThanTwoRows(train_matrix)
  model <- lmWrapper(train_matrix, criterion_col, cols_to_fit, include_intercept=TRUE)
  class(model) <- c("regModel", class(model))
  # Functions in this package assume all models track criterion_col and cols_to_fit.
  model$criterion_col <- criterion_col
  model$cols_to_fit <- cols_to_fit
  return(model)
}

predictRoot.regModel <- function(object, row1, row2) {
  col_weights_clean <- coef(object)
  # Set na to zero.
  col_weights_clean[is.na(col_weights_clean)] <- 0
  # Because the intercept is 0 for row1 and ro2, ignore it.
  if ("(Intercept)" %in% names(col_weights_clean)) {
    intercept_index <- which(names(col_weights_clean)=="(Intercept)")
    col_weights_clean <- col_weights_clean[-intercept_index]
  }
  direction_plus_minus_1 <- sign(col_weights_clean %*% sign(row1 - row2))
  # Convert from the range [-1, 1] to the range [0, 1], which is the 
  # probability that row 1 > row 2.
  0.5 * (direction_plus_minus_1 + 1)
}

#' Predict which of a pair of rows has a higher criterion, using regression.
#'
#' @param object A fitted regModel.
#' @inheritParams predictPair
#'
#' @seealso
#' \code{\link{regModel}} for example code.
#'
#' @export
predictPair.regModel <- function(object, test_data, verbose_output=TRUE) {
  predictions <- predictPairMatrix(object, test_data)
  return(structure(list(predictions=predictions), class="pairPredictor"))
}

#' Linear regression (no intercept) wrapper for hueristica
#'
#' A wrapper to create a lm model just specifying columns, generating
#' a model formula for you __without an intercept__.
#' This makes it easier to run automated comparisons with
#' other models in heuristica.
#'
#' This version assumes you do NOT want to include the intercept.
#' Excluding the intercept typically has higher out-of-sample accuracy if the goal is
#' predicting rank order because the intercept does not affect the ranking, but
#' estimating it wastes a degree of freedom.
#' 
#' @inheritParams regModelForDocumentation 
#'
#' @return An object of class regNoIModel, which is a subclass of lm.
#'
#' @seealso
#' \code{\link{regModel}} for a version that includes the intercept.
#' @seealso
#' \code{\link{predict.lm}} for prediction.
#' @seealso
#' \code{\link{predictPair.regNoIModel}} for predicting between a pair of alternatives.
#'
#' @export
regNoIModel <- function(train_matrix, criterion_col, cols_to_fit) {
  model <- lmWrapper(train_matrix, criterion_col, cols_to_fit, include_intercept=FALSE)
  class(model) <- c("regNoIModel", class(model))
  # Functions in this package assume all models track criterion_col and cols_to_fit.
  model$criterion_col <- criterion_col
  model$cols_to_fit <- cols_to_fit
  return(model)
}

predictRoot.regNoIModel <- function(object, row1, row2) {
  direction_plus_minus_1 <- sign(coef(object) %*% sign(row1 - row2))
  # Convert from the range [-1, 1] to the range [0, 1], which is the 
  # probability that row 1 > row 2.
  0.5 * (direction_plus_minus_1 + 1)
}

#' Predict which of a pair of rows has a higher criterion, using regression no intercept.
#'
#' @param object A fitted regNoIModel.
#' @inheritParams predictPair
#'
#' @seealso
#' \code{\link{regNoIModel}} for example code.
#'
#' @export
predictPair.regNoIModel <- function(object, test_data, verbose_output=TRUE) {
  predictions <- predictPairMatrix(object, test_data)
  return(structure(list(predictions=predictions), class="pairPredictor"))
}

#' Logistic Regression model
#'
#' Create a logistic regression model by specifying columns and a dataset.  It fits the model
#' with R's glm function.
#'
#' This version assumes you always want to include the intercept.
#' 
#' @inheritParams heuristicaModel
#' @return An object of class logRegModel.
#' @param row_pairs Optional matrix.  TODO(jean): share documentation.
#' @param suppress_warnings Optional argument specifying whether glm warnings should be suppressed or not. Default is TRUE.
#' @export
logRegModel <- function(train_data, criterion_col, cols_to_fit,row_pairs=NULL,suppress_warnings=NULL){
  stopIfTrainingSetHasLessThanTwoRows(train_data)
  if (is.null(row_pairs)) {
    n <- nrow(train_data)
    all_pairs <- rowPairGenerator(n)
  } else {
    all_pairs <- row_pairs
  }
  
  transform <- train_data[all_pairs[,1],c(criterion_col,cols_to_fit)] - train_data[all_pairs[,2],c(criterion_col,cols_to_fit)]
  criterion <- transform[,1]
  criterion <- ifelse(criterion>0,1,ifelse(criterion==0,0.5,0))
  
  predictors <- transform[,2:ncol(transform)]
  
  training_set <- cbind(criterion,predictors)
  training_set <- as.data.frame(training_set)
  
  formula <- paste(colnames(training_set)[1], "~",paste(colnames(training_set)[-1], collapse = "+"),sep = "")
  # Do not fit intercept by default.
  formula <- paste(formula, "-1")
  
  if(is.null(suppress_warnings)){
    model <- suppressWarnings(glm(formula,family=binomial,data=training_set))
  } else { 
    model <- glm(formula,family=binomial,data=training_set)  
  }
    
  col_weights <- coef(model)
  
  structure(list(criterion_col=criterion_col, cols_to_fit=cols_to_fit,
                 linear_coef=col_weights,model=model), 
            class="logRegModel")
}


#' @export
coef.logRegModel <- function(object, ...) object$linear_coef

predictRoot.logRegModel <- function(object, row1, row2) {
  col_weights_clean <- coef(object)
  # Set na to zero.
  col_weights_clean[is.na(col_weights_clean)] <- 0
  # Because the intercept is 0 for row1 and ro2, ignore it.
  if ("(Intercept)" %in% names(col_weights_clean)) {
    intercept_index <- which(names(col_weights_clean)=="(Intercept)")
    col_weights_clean <- col_weights_clean[-intercept_index]
  }
  direction_plus_minus_1 <- sign(col_weights_clean %*% sign(row1 - row2))
  # Convert from the range [-1, 1] to the range [0, 1], which is the 
  # probability that row 1 > row 2.
  0.5 * (direction_plus_minus_1 + 1)
}

#' Predict which of a pair of rows has a higher criterion, using logistic regression.
#'
#' @param object A fitted logRegModel.
#' @inheritParams predictPair
#'
#' @seealso
#' \code{\link{logRegModel}} for example code.
#'
#' @export
predictPair.logRegModel <- function(object, test_data, verbose_output=TRUE) {
  predictions <- predictPairMatrix(object, test_data)
  return(structure(list(predictions=predictions), class="pairPredictor"))
}


#' Single Cue Model
#'
#' Create a single cue model by specifying columns and a dataset.  
#'
#' 
#' @inheritParams heuristicaModel
#' @inheritParams reversingModel
#' @export
#' @seealso
#' \code{\link{predictPair.singleCueModel}} (via \code{\link{predictPair}}) for prediction.
#' @seealso
#'
#' @export
singleCueModel <- function(train_data, criterion_col, cols_to_fit, reverse_cues=TRUE) {
  stopIfTrainingSetHasLessThanTwoRows(train_data)
  cue_validities <- matrixCueValidity(train_data, criterion_col, cols_to_fit)
  if (reverse_cues) {
    reverse_info = reverseAsNeeded(cue_validities)
    cue_validities_with_reverse <- reverse_info$cue_validities_with_reverse
    cue_directions <- reverse_info$cue_directions
  } else {
    cue_validities_with_reverse <- cue_validities
    cue_directions <- rep(1, length(cue_validities_with_reverse))
  }

  unsigned_linear_coef <- sapply(cue_validities_with_reverse, function(v)
    if (v==max(cue_validities_with_reverse)) 1 else 0)
  linear_coef <- cue_directions * unsigned_linear_coef
  
  structure(list(criterion_col=criterion_col, cols_to_fit=cols_to_fit,
                 cue_validities=cue_validities,
                 cue_validities_with_reverse=cue_validities_with_reverse,
                 linear_coef=linear_coef),
            class="singleCueModel")
}

#' Linear weights that can be used to compare pairs of cue directions.
#'
#' Do NOT apply these directly to raw data.
#'
#' @inheritParams stats::coef
#' @export
coef.singleCueModel <- function(object, ...) object$linear_coef

#' Predict which of a pair of rows has a higher criterion, using Single Cue Model.
#'
#' @param object A fitted singleCueModel.
#' @inheritParams predictPair
#'
#' @seealso
#' \code{\link{singleCueModel}} for example code.
#'
#' @export
predictPair.singleCueModel <- function(object, test_data, verbose_output=TRUE) {
  predictPairWithWeights(object, test_data, verbose_output=verbose_output)
}

#' Minimalist Model
#'
#' Fit the Minimalist heuristic by specifying columns and a dataset.  
#'
#' 
#' @inheritParams heuristicaModel
#' @inheritParams reversingModel
#' @export
#' @seealso
#' \code{\link{predictPair.minModel}} (via \code{\link{predictPair}}) for prediction.
#' @seealso
#'
#' @export
minModel <- function(train_data, criterion_col, cols_to_fit, reverse_cues=TRUE) {
  stopIfTrainingSetHasLessThanTwoRows(train_data)
  cue_validities <- matrixCueValidity(train_data, criterion_col, cols_to_fit)
  if (reverse_cues) {
    reverse_info = reverseAsNeeded(cue_validities)
    cue_validities_with_reverse <- reverse_info$cue_validities_with_reverse
    cue_directions <- reverse_info$cue_directions
  } else {
    cue_validities_with_reverse <- cue_validities
    cue_directions <- rep(1, length(cue_validities_with_reverse))
  }
  
  raw_ranks <- rank(cue_validities_with_reverse, ties.method="random")
  # Reverse ranks so first is last.
  cue_ranks <- length(cue_validities_with_reverse) - raw_ranks + 1
  cue_ranks <- as.numeric(cue_ranks)
  unsigned_linear_coef <- sapply(sample(cue_ranks), function(n) 2^(length(cue_ranks)-n) )
  
  
  linear_coef <- as.numeric(cue_directions) * unsigned_linear_coef
  
  structure(list(criterion_col=criterion_col, cols_to_fit=cols_to_fit,
                 cue_validities=cue_validities,
                 cue_validities_with_reverse=cue_validities_with_reverse,
                 linear_coef=linear_coef),
            class="minModel")
}

#' Linear weights that can be used to compare pairs of cue directions.
#'
#' Do NOT apply these directly to raw data.
#'
#' @inheritParams stats::coef
#' @export
coef.minModel <- function(object, ...) sample(object$linear_coef)

#' Predict which of a pair of rows has a higher criterion, using Minimalist Model.
#'
#' @param object A fitted minModel.
#' @inheritParams predictPair
#'
#' @seealso
#' \code{\link{minModel}} for example code.
#'
#' @export
predictPair.minModel <- function(object, test_data, verbose_output=TRUE) {
  predictPairWithWeights(object, test_data, verbose_output=verbose_output)
}




