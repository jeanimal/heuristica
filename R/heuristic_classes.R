#' Heuristica Model
#' @param train_data Training/fitting data as a matrix or data.frame.
#' @param criterion_col The index of the colum in train_data that has the criterion.
#' @param cols_to_fit A vector of column indices in train_data, used to fit the criterion.
# Private.  This is just an easy way to share parameter documentation.
heuristicaModel <- function(train_data, criterion_col, cols_to_fit) NULL 


###  Take the Best (ttbModel) ###

#' Take The Best
#'
#' An implementation of the Take The Best heuristic.
#' It sorts cues in order of \code{\link{cueValidity}}, making a decision based on the first cue that
#' discriminates (has differing values on the two objects).
#'
#' Developer TODO: Have TTB reverse a cue with validity < 0.5.
#' 
#' @inheritParams heuristicaModel
#'
#' @return An object of \code{\link[base]{class}} ttbModel.  This is a list containing at least the following components:
#'   \itemize{
#'    \item "cue_validities": A list of cue validities for the cues in order of cols_to_fit.
#'    \item "cue_ranks": A list of integer ranks of the cues in order of cols_to_fit.
#'                       The cue ranked 1 will be used first, then 2, etc.
#'   }
#'
#' @examples
#' ## Fit column (5,4) to column (1,0), having validity 1.0, and column (0,1), validity 0.
#' ttb <- ttbModel(matrix(c(5,4,1,0,0,1), 2, 3), 1, c(2,3))
#' ## Outputs predicted values for the first and second values, but Take The Best
#' ## is only trying to achieve sort order, so this makes a correct prediction. 
#' predict(ttb, matrix(c(5,4,1,0,0,1), 2, 3)) 
#' ## But this input results in an incorrect prediction:
#' predict(ttb, matrix(c(5,4,0,1,0,1), 2, 3)) 
#'
#' @seealso
#' \code{\link{predict.ttbModel}} (via \code{\link[stats]{predict}}) for prediction.
#' @seealso
#' Wikipedia's entry on \url{http://en.wikipedia.org/wiki/Take-the-best_heuristic}.
#'
#' @export
ttbModel <- function(train_data, criterion_col, cols_to_fit) {
  cue_validities <- matrixCueValidity(train_data[,c(criterion_col,cols_to_fit)], criterion_col)
  cue_ranks <- rev(rank(cue_validities, ties.method="random"))
  linear_coef <- sapply(cue_ranks, function(n) 2^(length(cue_ranks)-n) )
  # Need to save fit_predictions in case user calls predict without test_data.
  fit_predictions <- predictWithWeights(train_data, cols_to_fit, linear_coef)
  fit_accuracy <- cueValidity(train_data[,criterion_col], fit_predictions)
  structure(list(criterion_col=criterion_col, cols_to_fit=cols_to_fit,
                 cue_validities=cue_validities, cue_ranks=cue_ranks,
                 linear_coef=linear_coef, fit_predictions=fit_predictions,
                 fit_accuracy=fit_accuracy), 
            class="ttbModel")
}

# Private.  The external world need not know the implementation actual uses a linear model
# under the hood.
coef.ttbModel <- function(model) model$linear_coef

#' Generates predictions for Take The Best
#'
#' Implementation of \code{\link[stats]{predict}} for ttbModel.
#'
#' @param object A ttbModel.
#' @param ... Normally this would be the test data. 
#'  It is used to predict and can be a matrix or data.frame.  
#'  It must have the same cols_to_fit indices as those used in train_data.
#'
#' @return An N x 1 matrix of predicted values, or a list if there was only one cue.
#'  Only the sort order of these is relevant for Take The Best.
#' 
#' @seealso
#' \code{\link{ttbModel}} for example code.
#'
#' @export
# Under the hood, TTB is implemented as a linear prediction with exponentiall-decaying weights.
# This is just because it was more convenient to code it that way in R.
# The output is equivalent to the Take The Best model description.
predict.ttbModel <- function(object, ...) {
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
#'   }
#'
#' @seealso
#' \code{\link{predict.dawesModel}} (via \code{\link[stats]{predict}}) for prediction.
#' @seealso
#' Wikipedia's entry on \url{http://en.wikipedia.org/wiki/Unit-weighted_regression}.
#'
#' @export
dawesModel <- function(train_data, criterion_col, cols_to_fit) {
  cue_validities <- matrixCueValidity(train_data[,c(criterion_col,cols_to_fit)], criterion_col)
  linear_coef <- sapply(cue_validities, function(x) sign(x-0.5))
  # Need to save fit_predictions in case user calls predict without test_data.
  fit_predictions <- predictWithWeights(train_data, cols_to_fit, linear_coef)
  fit_accuracy <- cueValidity(train_data[,criterion_col], fit_predictions)
  structure(list(criterion_col=criterion_col, cols_to_fit=cols_to_fit,
                 fit_predictions=fit_predictions, fit_accuracy=fit_accuracy,
                 cue_validities=cue_validities, linear_coef=linear_coef), class="dawesModel")
}

coef.dawesModel <- function(model) model$linear_coef

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
