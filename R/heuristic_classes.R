#' Heuristica Model
#' @param train_data Training/fitting data as a matrix or data.frame.
#' @param criterion_col The index of the colum in train_data that has the criterion.
#' @param cols_to_fit A vector of column indices in train_data, used to fit the criterion.
# Private.  This is just an easy way to share parameter documentation.
heuristicaModel <- function(train_data, criterion_col, cols_to_fit) NULL 


## New generics ##

#' Generic function to use two rows of cues to predict which row has a higher criterion.
#'
#' @param object The object that implements predictAlternative, e.g. a ttb model.
#' @param test_data The matrix of data to predict on.  As with predict, columns
#'  must match those used for fitting.
#' @param rowPairs The optionalrowPairs is a matrix of pairs of column indices
#'  you want to have predicted.  By default (if no list is given), it will use
#'  all unique pairs.
#'  E.g. for 3 rows, it will use [[1,2], [1,3], [2,3]].  
#' @return A matrix with 3 columns: row1index, row2index, and the predicted greater index.
#'  The first two columns are the rowPairs (provided as input or all).
#'  The 3rd column is the model's predicted probability (0 to 1) that the first
#'  row index has a larger criterion than the 2nd row index.  0.5 is a tie.
#'  For example:
#'                              [[1,2,1], [1,3,0], [2,3,1]].
#'  Between row 1 and 2, there is probabily 1 that row 1 is bigger.
#'  Between row 1 and 3, there is probabily 0 that row 1 is bigger.
#'     (That is, it predicts that row 3 is bigger.)
#' Between row 2 and 3, there is probabily 1 that row 2 is bigger.
#' @export
predictAlternative <- function(object, test_data, rowPairs=NULL) UseMethod("predictAlternative")

## Shared helper functions ##

# Private.
inequalityToValue <- function(a,b) {
  if (a > b) {
    return(1)
  } else if (b > a) {
    return(-1)
  } else {
    return(0)
  }
}

# Private.
pairToValue <- function(pair) {
  if (pair[1] > pair[2]) {
    return(1)
  } else if (pair[2] > pair[1]) {
    return(0)
  } else {
    return(0.5)
  }
}

###  Take the Best binary (ttbBinModel) ###

#' Take The Best for binary cues
#'
#' An implementation of the Take The Best heuristic for binary cues.
#' It sorts cues in order of \code{\link{cueValidity}}, making a decision based on the first cue that
#' discriminates (has differing values on the two objects).
#' Accepting only binary cues allows it to implement the predict function.
#' Warning: it will not error if you give it non-binary (real-valued) cues.
#'
#' Developer TODO: Have TTB reverse a cue with validity < 0.5.
#' 
#' @inheritParams heuristicaModel
#'
#' @return An object of \code{\link[base]{class}} ttbBinModel.  This is a list containing at least the following components:
#'   \itemize{
#'    \item "cue_validities": A list of cue validities for the cues in order of cols_to_fit.
#'    \item "cue_ranks": A list of integer ranks of the cues in order of cols_to_fit.
#'                       The cue ranked 1 will be used first, then 2, etc.
#'   }
#'
#' @examples
#' ## Fit column (5,4) to column (1,0), having validity 1.0, and column (0,1), validity 0.
#' ttb <- ttbBinModel(matrix(c(5,4,1,0,0,1), 2, 3), 1, c(2,3))
#' ## Outputs predicted values for the first and second values, but Take The Best
#' ## is only trying to achieve sort order, so this makes a correct prediction. 
#' predict(ttb, matrix(c(5,4,1,0,0,1), 2, 3)) 
#' ## But this input results in an incorrect prediction:
#' predict(ttb, matrix(c(5,4,0,1,0,1), 2, 3)) 
#'
#' @seealso
#' \code{\link{predictAlternative.ttbBinModel}} (via \code{\link{predictAlternative}}) for prediction.
#' @seealso
#' \code{\link{predict.ttbBinModel}} (via \code{\link[stats]{predict}}) for prediction.
#' @seealso
#' Wikipedia's entry on \url{http://en.wikipedia.org/wiki/Take-the-best_heuristic}.
#'
#' @export
ttbBinModel <- function(train_data, criterion_col, cols_to_fit) {
  cue_validities <- matrixCueValidity(train_data, criterion_col, cols_to_fit)
  # Reverse ranks so first is last.
  cue_ranks <- length(cue_validities) - rank(cue_validities, ties.method="random") + 1
  linear_coef <- sapply(cue_ranks, function(n) 2^(length(cue_ranks)-n) )
  # Need to save fit_predictions in case user calls predict without test_data.
  fit_predictions <- predictWithWeights(train_data, cols_to_fit, linear_coef)
  fit_accuracy <- cueValidity(train_data[,criterion_col], fit_predictions)
  structure(list(criterion_col=criterion_col, cols_to_fit=cols_to_fit,
                 cue_validities=cue_validities, cue_ranks=cue_ranks,
                 linear_coef=linear_coef, fit_predictions=fit_predictions,
                 fit_accuracy=fit_accuracy), 
            class="ttbBinModel")
}

# TODO: Make this private.  The external world need not know the implementation actual uses a linear model
# under the hood.  However, for now I need it until I get predict working correctly for a named m_test input.
#' @export
coef.ttbBinModel <- function(object, ...) object$linear_coef

#' Generates predictions for Take The Best with binary cues
#'
#' Implementation of \code{\link[stats]{predict}} for ttbBinModel,
#' Take The Best with binary cues.
#'
#' @param object A ttbBinModel.
#' @param ... Normally this would be the test data. 
#'  It is used to predict and can be a matrix or data.frame.  
#'  It must have the same cols_to_fit indices as those used in train_data.
#'
#' @return An N x 1 matrix of predicted values, or a list if there was only one cue.
#'  Only the sort order of these is relevant for Take The Best.
#' 
#' @seealso
#' \code{\link{ttbBinModel}} for example code.
#'
#' @export
# Under the hood, TTB is implemented as a linear prediction with exponentiall-decaying weights.
# This is just because it was more convenient to code it that way in R.
# The output is equivalent to the Take The Best model description.
predict.ttbBinModel <- function(object, ...) {
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

#' Predict which alternative has higher criterion for Take The Best with binary cues
#'
#' @param object A ttbBinModel.
#' @inheritParams predictAlternative
#'
#' @seealso
#' \code{\link{ttbBinModel}} for example code.
#'
#' @export
predictAlternative.ttbBinModel <- function(object, test_data, rowPairs=NULL) {
  predictions <- predictWithWeights(test_data, object$cols_to_fit, object$linear_coef)
  if (is.null(rowPairs)) {
    pairsMatrix <- t(combn(nrow(predictions), 2))
  } else {
    if (ncol(rowPairs) != 2) {
      stop(paste("rowPairs should be pairs matrix with two columns but got",
                 rowPairs))
    }
    pairsMatrix <- rowPairs
  }
  predictPairs <- t(apply(pairsMatrix, 1,
    function(rowPair) predictions[rowPair,]))
  predictDirection <- matrix(apply(predictPairs, 1, pairToValue))
  return(cbind(pairsMatrix, predictDirection))
}

#' Do not use.  Still under development.
#' @param fitted_heuristic A heuristic already fitted to data, e.g. ttbBinModel.
#' @param test_data Data to try to predict; must match columns in fit.
#' @return Same matrix as predictAlternative but with columns on correctness
#' @seealso
#' \code{\link{predictAlternative}}
#' @export
predictAlternativeWithCorrect <- function(fitted_heuristic, test_data) {
  rowPairs <- t(combn(nrow(test_data), 2))
  predictMatrix <- predictAlternative(fitted_heuristic, test_data, rowPairs=rowPairs)
  correctValues <- test_data[,fitted_heuristic$criterion_col]
  correctProbFirstRow <-  apply(rowPairs, 1,
                                function(rowPair) pairToValue(correctValues[rowPair]))
  extendedMatrix <- cbind(predictMatrix, correctProbFirstRow)
  # Is there a more efficient way to do the lines below?
  # This gave subscript out of bounds: m[,5] = m[,4]-m[,3]
  extendedMatrix <- cbind(extendedMatrix,
                          extendedMatrix[,4] - extendedMatrix[,3],
                          abs(extendedMatrix[,4] - extendedMatrix[,3]))
  return(extendedMatrix)
}

#' Do not use.  Still under development.
#' @param fitted_heuristic A heuristic already fitted to data, e.g. ttbBinModel.
#' @param test_data Data to try to predict; must match columns in fit.
#' @return A numeric from 0 to 1 indicating the percent correct.
#' @export
pctCorrectOfPredictAlternative <- function(fitted_heuristic, test_data) {
  predictWithCorrectMatrix <- predictAlternativeWithCorrect(fitted_heuristic, test_data)
  sumError <- sum(predictWithCorrectMatrix[,6])
  return((nrow(predictWithCorrectMatrix)-sumError)
         /nrow(predictWithCorrectMatrix))
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
#'
#' @return An object of \code{\link[base]{class}} dawesModel.  This is a list containing at least the following components:
#'   \itemize{
#'    \item "cue_validities": A list of cue validities for the cues in order of cols_to_fit.
#'    \item "linear_coef": A list of linear model coefficents (-1 or +1)
#'           for the cues in order of cols_to_fit.
#'   }
#'
#' @seealso
#' \code{\link{predict.dawesModel}} (via \code{\link[stats]{predict}}) for prediction.
#' @seealso
#' Wikipedia's entry on \url{http://en.wikipedia.org/wiki/Unit-weighted_regression}.
#'
#' @export
dawesModel <- function(train_data, criterion_col, cols_to_fit) {
  cue_validities <- matrixCueValidity(train_data, criterion_col, cols_to_fit)
  linear_coef <- sapply(cue_validities, function(x) sign(x-0.5))
  # Need to save fit_predictions in case user calls predict without test_data.
  fit_predictions <- predictWithWeights(train_data, cols_to_fit, linear_coef)
  fit_accuracy <- cueValidity(train_data[,criterion_col], fit_predictions)
  structure(list(criterion_col=criterion_col, cols_to_fit=cols_to_fit,
                 fit_predictions=fit_predictions, fit_accuracy=fit_accuracy,
                 cue_validities=cue_validities, linear_coef=linear_coef), class="dawesModel")
}

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


### Franklin's Model ###

#' Franklin's Model, a linear model weighted by cue validities
#' 
#' Franklin's Model is a linear model with weights calculated by \code{\link{cueValidity}}.
#' The name is because it was inspired by a method used by Ben Franklin.
#'
#' @inheritParams heuristicaModel
#'
#' @return An object of \code{\link[base]{class}} franklinModel.  This is a list containing at least the following components:
#'   \itemize{
#'    \item "cue_validities": A list of cue validities for the cues in order of cols_to_fit.
#'    \item "linear_coef": Same as cue validities for this model.
#'   }
#'
#' @seealso
#' \code{\link{predict.franklinModel}} (via \code{\link[stats]{predict}}) for prediction.
#' @export 
franklinModel <- function(train_data, criterion_col, cols_to_fit) {
  cue_validities <- matrixCueValidity(train_data, criterion_col, cols_to_fit)
  structure(list(criterion_col=criterion_col, cols_to_fit=cols_to_fit, cue_validities=cue_validities, linear_coef=cue_validities), class="franklinModel")
}

#' @export
coef.franklinModel <- function(object, ...) object$linear_coef

#' Generates predictions for Franklin's Model 
#'
#' Implementation of \code{\link[stats]{predict}} for franklinModel.
#'
#' @param object A franklinModel.
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
#'
#' @export
regModel <- function(train_matrix, criterion_col, cols_to_fit) {
  model <- lmWrapper(train_matrix, criterion_col, cols_to_fit, include_intercept=TRUE)
  class(model) <- c("regModel", class(model))
  return(model)
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
#'
#' @export
regNoIModel <- function(train_matrix, criterion_col, cols_to_fit) {
  model <- lmWrapper(train_matrix, criterion_col, cols_to_fit, include_intercept=FALSE)
  class(model) <- c("regNoIModel", class(model))
  return(model)
}
