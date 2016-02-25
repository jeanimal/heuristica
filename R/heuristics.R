##############################################
# Heuristics: The interesting part           #
# Implementations and a few helper functions #
##############################################

## New generics ##

#' Generic function to predict which of a pair of rows has higher criterion.
#' 
#' Implement this for every heuristic in order to use with row_pair and
#' aggregate functions.
#'
#' @param object The object that implements predictPair, e.g. a ttb model.
#' @param row1 The first row of cues (object$cols_to_fit columns), as a
#'   one-row matrix.
#' @param row2 The second row of cues.
#' @return A value from 0 to 1, representing the probability that row1's
#'   criterion is greater than row2's criterion.
#' @export
predictRoot <- function(object, row1, row2) UseMethod("predictRoot")

### Shared documentation stubs ###
## TODO: Find a way so these do not show up in actual documenentation.

#' Documentation stub.
#' @param train_data Training/fitting data as a matrix or data.frame.
#' @param criterion_col The index of the colum in train_data that has the
#'   criterion.
#' @param cols_to_fit A vector of column indices in train_data, used to fit
#'   the criterion.
# Private.  This is just an easy way to share parameter documentation.
heuristicaModel <- function(train_data, criterion_col, cols_to_fit) NULL 

#' Documentation stub.
#' @param reverse_cues Optional parameter to reverse cues as needed.  By
#' default, the model will reverse the cue values for cues with cue validity
#' < 0.5, so a cue with validity 0 becomes a cue with validity 1.
#' Set this to FALSE if you do not want that, i.e. the cue stays validity 0.
# Private.  This is just an easy way to share parameter documentation.
reversingModel <- function(reverse_cues=TRUE) NULL


### Helper functions ###

getCuePairDirections <- function(row1, row2) {
  sign(row1 - row2)
}

getWeightedCuePairDirections <- function(coefficients, row1, row2) {
  sign(getCuePairDirections(row1, row2) %*% coefficients)
}

# Do not take the sign of the difference of row pairs.
getWeightedCuePairDiffs <- function(coefficients, row1, row2) {
  sign((row1 - row2) %*% coefficients)
}

rescale0To1 <- function(direction_plus_minus_1) {
  0.5 * (direction_plus_minus_1 + 1)
}

# Private for now. Will export and test when I settle on a name.
reverseAsNeeded <- function(cue_validities) {
  cue_validities_with_reverse <- abs(cue_validities - 0.5) + 0.5
  cue_directions <- sign(cue_validities - 0.5)
  structure(list(cue_validities_with_reverse=cue_validities_with_reverse,
                 cue_directions=cue_directions))
}

### Take The Best ###

#' Take The Best
#'
#' An implementation of the Take The Best heuristic.
#' It sorts cues in order of \code{\link{cueValidity}}, making a decision
#' based on the first cue that discriminates (has differing values on the
#' two objects).
#'
#' It does NOT implement predict on purpose.
#' 
#' @inheritParams heuristicaModel
#' @inheritParams reversingModel
#'
#' @return An object of \code{\link[base]{class}} ttbModel.  This is a list
#'   containing at least the following components:
#'   \itemize{
#'    \item "cue_validities": A list of cue validities for the cues in order of
#'      cols_to_fit.
#'   }
#'
#' @examples
#' ## Fit column (5,4) to column (1,0), having validity 1.0, and column (0,1),
#' ## validity 0.
#' train_matrix <- cbind(y=c(5,4), x1=c(1,0), x2=c(0,1))
#' ttb <- ttbModel(train_matrix, 1, c(2,3))
#' predictRowPair(oneRow(train_matrix, 1), oneRow(train_matrix, 2), ttb)
#' ## But this test data results in an incorrect prediction because x1 is
#' ## unexpected.
#' test_matrix <- cbind(y=c(5,4), x1=c(0,1), x2=c(0,1))
#' predictRowPair(oneRow(test_matrix, 1), oneRow(test_matrix, 2), ttb)
#'
#' @seealso
#' \code{\link{predictRowPair}} for prediction.
#' @seealso
#' Wikipedia's entry on
#' \url{http://en.wikipedia.org/wiki/Take-the-best_heuristic}.
#'
#' @export
ttbModel <- function(train_data, criterion_col, cols_to_fit,
                     reverse_cues=TRUE) {
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
  unsigned_linear_coef <- sapply(cue_ranks,
                                 function(n) 2^(length(cue_ranks)-n) )
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
  direction_plus_minus_1 <- getWeightedCuePairDirections(object$linear_coef,
                                                         row1, row2)
  # Convert from the range [-1, 1] to the range [0, 1], which is the 
  # probability that row 1 > row 2.
  return(rescale0To1(direction_plus_minus_1))
}


### Dawes Model ###

#' DawesModel, a unit-weight linear model
#'
#' DawesModel is a unit-weight linear model inspired by Robyn Dawes.
#' Dawes Model assigns unit (+1 or -1) weights based on
#' \code{\link{cueValidity}}.
#'   \itemize{
#'     \item A cue validity > 0.5 results in a weight of +1.
#'     \item A cue validity < 0.5 results in a weight of -1.
#'   }
#' This version differs from others in that it uses a weight of 0 if cue
#' validity is 0.5 (rather than randomly assigning +1 or -1) to give faster
#' convergence of average accuracy.
#'
#' @inheritParams heuristicaModel
#' @inheritParams reversingModel
#'
#' @return An object of \code{\link[base]{class}} dawesModel.  This is a list
#' containing at least the following components:
#'   \itemize{
#'    \item "cue_validities": A list of cue validities for the cues in order of
#'      cols_to_fit.
#'    \item "linear_coef": A list of linear model coefficents (-1 or +1)
#'           for the cues in order of cols_to_fit.  (It can only return -1's if
#'           reverse_cues=TRUE.)
#'   }
#'
#' @seealso
#' \code{\link{predictRowPair}} for predicting among a pair of alternatives.
#' @seealso
#' Wikipedia's entry on
#' \url{http://en.wikipedia.org/wiki/Unit-weighted_regression}.
#'
#' @param reverse_cues Optional parameter to reverse cues as needed.
#' @export
dawesModel <- function(train_data, criterion_col, cols_to_fit,
                       reverse_cues=TRUE) {
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

  structure(list(criterion_col=criterion_col, cols_to_fit=cols_to_fit,
                 cue_validities=cue_validities,
                 cue_validities_with_reverse=cue_validities_with_reverse,
                 linear_coef=linear_coef), class="dawesModel")
}

#' @inheritParams stats::coef
#' @export
coef.dawesModel <- function(object, ...) object$linear_coef

predictRoot.dawesModel <- function(object, row1, row2) {
  direction_plus_minus_1 <- getWeightedCuePairDirections(object$linear_coef,
                                                         row1, row2)
  # Convert from the range [-1, 1] to the range [0, 1], which is the 
  # probability that row 1 > row 2.
  return(rescale0To1(direction_plus_minus_1))
}


### Franklin's Model ###

#' Franklin's Model, a linear model weighted by cue validities
#' 
#' Franklin's Model is a linear model with weights calculated by
#' \code{\link{cueValidity}}.
#' The name is because it was inspired by a method used by Ben Franklin.
#'
#' @inheritParams heuristicaModel
#' @inheritParams reversingModel
#'
#' @return An object of \code{\link[base]{class}} franklinModel.  This is a
#' list containing at least the following components:
#'   \itemize{
#'    \item "cue_validities": A list of cue validities for the cues in order of
#'      cols_to_fit.
#'    \item "linear_coef": Same as cue validities for this model.
#'   }
#'
#' @seealso
#' \code{\link{predictRowPair}} for predicting among a pair of rows.
#' @export 
franklinModel <- function(train_data, criterion_col, cols_to_fit,
                          reverse_cues=TRUE) {
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

predictRoot.franklinModel <- function(object, row1, row2) {
  direction_plus_minus_1 <- getWeightedCuePairDirections(object$linear_coef,
                                                         row1, row2)
  # Convert from the range [-1, 1] to the range [0, 1], which is the 
  # probability that row 1 > row 2.
  return(rescale0To1(direction_plus_minus_1))
}

### Wrappers for linear regression models ###

#' Just to share documentation
#'
#' @param train_matrix A matrix (or data.frame) of data to train (fit) the
#'   model with.
#' @param criterion_col The index of the criterion column-- "y" in the formula.
#' @param cols_to_fit A vector of column indexes to fit-- the "x's" in the
#'   formula.
# Private
regInterceptModelForDocumentation <- function(train_matrix, criterion_col,
                                              cols_to_fit) NULL

#' Create an lm model just specifying columns, generating a formula for you.
#' 
#' @inheritParams regInterceptModelForDocumentation 
#' @param include_intercept A boolean of whether to include an intercept in
#' the formula.
#'
#' @return An object of class lm.
#'
# Private because the exported versions are below.
lmWrapper <- function(train_matrix, criterion_col, cols_to_fit,
                      include_intercept=TRUE) {
  train_df = as.data.frame(train_matrix)
  formula_str = paste(colnames(train_df)[criterion_col], "~",
                            paste(colnames(train_df)[cols_to_fit],
                                  collapse = "+"),
                        sep = "")
   if (include_intercept == FALSE) {
    formula_str = paste(formula_str, "-1", sep = "")
  }
  return(lm(as.formula(formula_str), data=train_df))
}

#' Linear regression wrapper for hueristica
#'
#' A wrapper to create a lm model just specifying columns, generating
#' a model formula for you.  This makes it easier to run automated comparisons
#' with other models in heuristica.
#'
#' This version assumes you always want to include the intercept.
#' 
#' @inheritParams regInterceptModelForDocumentation 
#'
#' @return An object of class regInterceptModel, which is a subclass of lm.
#'
#' @seealso
#' \code{\link{regModel}} for a version that excludes the intercept.
#' @seealso
#' \code{\link{predict.lm}} for prediction.
#' @seealso
#' \code{\link{predictRowPair}} for predicting between a pair of rows.
#'
#' @export
regInterceptModel <- function(train_matrix, criterion_col, cols_to_fit) {
  stopIfTrainingSetHasLessThanTwoRows(train_matrix)
  model <- lmWrapper(train_matrix, criterion_col, cols_to_fit,
                     include_intercept=TRUE)
  class(model) <- c("regInterceptModel", class(model))
  # Functions in this package require criterion_col and cols_to_fit.
  model$criterion_col <- criterion_col
  model$cols_to_fit <- cols_to_fit

  # Make clean weights that can be easily used in predictRoot.
  col_weights_clean <- coef(model)
  # Set na to zero.
  col_weights_clean[is.na(col_weights_clean)] <- 0
  # Because the intercept is 0 for row1 and ro2, ignore it.
  if ("(Intercept)" %in% names(col_weights_clean)) {
    intercept_index <- which(names(col_weights_clean)=="(Intercept)")
    col_weights_clean <- col_weights_clean[-intercept_index]
  }
  model$col_weights_clean <- col_weights_clean

  return(model)
}

# If you have a model that implements predict, you can confirm you
# implemented predictRoot correctly using this, which is too slow
# to use in practice.
# TODO: use this in tests to compare with hand-coded predictions.
predictRootUsingPredict <- function(object, row1, row2) {
  p1 <- predict(object, as.data.frame(row1))
  p2 <- predict(object, as.data.frame(row2))
  if (p1 > p2) {
    return(1)
  } else if (p1 < p2) {
    return(0)
  } else {
    return(0.5)
  }
}

# This does the equivalent of predictPairUsingPredict but faster.
predictRoot.regInterceptModel <- function(object, row1, row2) {
  direction_plus_minus_1 <- getWeightedCuePairDiffs(object$col_weights_clean,
                                                    row1, row2)
  # Convert from the range [-1, 1] to the range [0, 1], which is the 
  # probability that row 1 > row 2.
  return(rescale0To1(direction_plus_minus_1))
}

#' Linear regression (no intercept) wrapper for hueristica
#'
#' A wrapper to create a lm model just specifying columns, generating
#' a model formula for you __without an intercept__.
#' This makes it easier to run automated comparisons with
#' other models in heuristica.
#'
#' This version assumes you do NOT want to include the intercept.
#' Excluding the intercept typically has higher out-of-sample accuracy if the
#' goal is predicting rank order because the intercept does not affect the
#' ranking, but estimating it wastes a degree of freedom.
#' 
#' @inheritParams regInterceptModelForDocumentation 
#'
#' @return An object of class regModel, which is a subclass of lm.
#'
#' @seealso
#' \code{\link{regInterceptModel}} for a version that includes the intercept.
#' @seealso
#' \code{\link{predict.lm}} for prediction.
#' @seealso
#' \code{\link{predictRowPair}} for predicting between a pair of alternatives.
#'
#' @export
regModel <- function(train_matrix, criterion_col, cols_to_fit) {
  model <- lmWrapper(train_matrix, criterion_col, cols_to_fit,
                     include_intercept=FALSE)
  class(model) <- c("regModel", class(model))
  # Functions in this package require criterion_col and cols_to_fit.
  model$criterion_col <- criterion_col
  model$cols_to_fit <- cols_to_fit
  # Make clean weights that can be easily used in predictRoot.
  col_weights_clean <- coef(model)
  # Set na to zero.
  col_weights_clean[is.na(col_weights_clean)] <- 0
  model$col_weights_clean <- col_weights_clean
  return(model)
}

# This does the equivalent of predictPairUsingPredict but faster.
predictRoot.regModel <- function(object, row1, row2) {
  # When we subtract rows predictions, the intercept cancels out.  So it's
  # safe to exclude the intercept from the weights below.
  direction_plus_minus_1 <- getWeightedCuePairDiffs(object$col_weights_clean,
                                                    row1, row2)
  # Convert from the range [-1, 1] to the range [0, 1], which is the 
  # probability that row 1 > row 2.
  return(rescale0To1(direction_plus_minus_1))
}

### Logistic regression ###

# This is a shared function for predictions.
sigmoid <- function(z) { 1/(1+exp(-z)) }

# An implementation of cue_order_fn that ranks cues by cue validity.
rankByCueValidity <- function(train_data, criterion_col, cols_to_fit) {
  cue_validities <- matrixCueValidity(train_data, criterion_col, cols_to_fit)
  reverse_info = reverseAsNeeded(cue_validities)
  # rank by default ranks in ascending order.  A trick to get a ranking in
  # descending order is to pass it the negative of the data.
  cue_ranks <- rank(-reverse_info$cue_validities_with_reverse,
                    ties.method="random")
  return(unname(cue_ranks))
}

# An implementation of cue_order_fn that ranks by each cue's correlation with
# the criterion.  WARNING: This should perhaps be applied to data transformed
# to row pairs, if you are using it for logRegModel.
rankByCorrelation <- function(train_data, criterion_col, cols_to_fit) {
  data_columns <- train_data[,c(criterion_col, cols_to_fit)]
  # Get correlations for all pairs, then limit to criterion column.
  all_cor_with_criterion <- cor(data_columns)[,1]
  # Drop correlation of criterion with itself.
  cue_cor_with_criterion <- all_cor_with_criterion[-1]
  # rank by default ranks in ascending order.  A trick to get a ranking in
  # descending order is to pass it the negative of the data.
  cue_ranks <- rank(-cue_cor_with_criterion, ties.method="random")
  return(unname(cue_ranks))
}

# An implementation of cue_order_fn that keeps the order the same as
# cols_to_fit.
keepOrder <- function(train_data, criterion_col, cols_to_fit) {
  return(c(1:length(cols_to_fit)))
}

# An implementation of cue_order_fn that uses a random order of cues so
# that there is no bias to the order that happened to come in the data set.
randomOrder <- function(train_data, criterion_col, cols_to_fit) {
  return(sample(c(1:length(cols_to_fit))))
}

# Logistic regression constructor that can use different row_pair_functions.
# cue
logRegModelGeneral <- function(train_data, criterion_col, cols_to_fit,
                               row_pair_fn, class_name,
                               cue_order_fn,
                               suppress_warnings=TRUE) {
  stopIfTrainingSetHasLessThanTwoRows(train_data)
  
  cue_ordering <- cue_order_fn(train_data, criterion_col, cols_to_fit)
  sorted_cols_to_fit <- cols_to_fit[cue_ordering]
  
  training_set <- toRowPairData(train_data, criterion_col, sorted_cols_to_fit,
                                row_pair_fn)
  training_set <- as.data.frame(training_set)
  
  cue_formula <- paste(colnames(training_set)[-1], collapse = "+")
  formula <- paste(colnames(training_set)[1], "~", cue_formula, sep = "")
  # Do not fit intercept by default.
  formula <- paste(formula, "-1")
  
  if(suppress_warnings) {
    model <- suppressWarnings(glm(formula,family=binomial,data=training_set))
  } else { 
    model <- glm(formula,family=binomial,data=training_set)  
  }
  
  col_weights <- coef(model)
  
  # Make clean weights that can be easily used in predictRoot.
  col_weights_clean <- col_weights
  # Set na to zero.
  col_weights_clean[is.na(col_weights_clean)] <- 0
  # Because the intercept is 0 for row1 and ro2, ignore it.
  if ("(Intercept)" %in% names(col_weights_clean)) {
    intercept_index <- which(names(col_weights_clean)=="(Intercept)")
    col_weights_clean <- col_weights_clean[-intercept_index]
  }
  
  # This will inherit all functionality from the glm model (such as
  # coef and predict) and add to it.
  class(model) <- c(class_name, class(model))
  # All models in this package must track criterion_col and cols_to_fit.
  model$criterion_col <- criterion_col
  model$cols_to_fit <- cols_to_fit
  # Cue_ordering is useful when already restricted to cols_to_fit.
  model$cue_ordering <- cue_ordering
  # If you have raw data, you can go straight to sorted_cols_to_fit.
  model$sorted_cols_to_fit <- sorted_cols_to_fit
  # Col_weights_clean and row_pair_fn are needed for faster prediction
  # than using the "predict" function.
  model$col_weights_clean <- col_weights_clean
  model$row_pair_fn <- row_pair_fn
  
  return(model)
}

#' Logistic Regression model using cue differences as predictors
#'
#' Create a logistic regression model by specifying columns and a dataset.  It
#' fits the model with R's glm function.
#'
#' This version assumes you do not want to include the intercept.
#' 
#' For a discussion of how logistic regression works, see:
#' http://www.r-bloggers.com/what-does-a-generalized-linear-model-do/
#' Note that our criterion is the probability that row 1 is greater than row 2
#' when a pair is encountered.
#' 
#' @inheritParams heuristicaModel
#' @param cue_order_fn Optional argument as a function that orders cues.  This
#'   affects which cues are dropped for underspecified models. The rightmost
#'   cues in the order are dropped first, so the function rankByCueValidity
#'   means cues with the lowest cueValidity in the training set will be
#'   be dropped first.  The function must have the signature
#'   function(train_data, criterion_col, cols_to_fit).
#' @param suppress_warnings Optional argument specifying whether glm warnings
#'   should be suppressed or not. Default is TRUE.
#' @return An object of class logRegModel.
#' @export
logRegModel <- function(train_data, criterion_col, cols_to_fit,
                        cue_order_fn=rankByCueValidity,
                        suppress_warnings=TRUE) {
  return(logRegModelGeneral(train_data, criterion_col, cols_to_fit,
                            rowDiff, "logRegModel", cue_order_fn,
                            suppress_warnings))
}

generalPredictRootLogReg <- function(row1_raw, row2_raw, cue_ordering,
                                     col_weights_clean, row_pair_fn) {
  row1 <- row1_raw[, cue_ordering, drop=FALSE]
  row2 <- row2_raw[, cue_ordering, drop=FALSE]
  raw_predict <- row_pair_fn(row1, row2) %*% col_weights_clean
  return(sigmoid(raw_predict))
}

# This is equivalent to the glm predict like this:
# predict(model, newdata=as.data.frame(row1 - row2), type="response"))
predictRoot.logRegModel <- function(object, row1, row2) {
  generalPredictRootLogReg(row1, row2, object$cue_ordering, 
                           object$col_weights_clean, object$row_pair_fn)
}

logRegKeepModel <- function(train_data, criterion_col, cols_to_fit,
                           suppress_warnings=TRUE) {
  return(logRegModelGeneral(train_data, criterion_col, cols_to_fit,
                            rowDiff, "logRegKeepModel", keepOrder,
                            suppress_warnings))
}

predictRoot.logRegKeepModel <- function(object, row1, row2) {
  generalPredictRootLogReg(row1, row2, object$cue_ordering, 
                           object$col_weights_clean, object$row_pair_fn)
}

#' Logistic Regression model using the sign of the difference of cues
#'
#' Create a logistic regression model by specifying columns and a dataset.
#' It fits the model with R's glm function.
#'
#' This version assumes you do not want to include the intercept.
#' 
#' @inheritParams heuristicaModel
#' @param cue_order_fn Optional argument as a function that orders cues.  This
#'   affects which cues are dropped for underspecified models. The rightmost
#'   cues in the order are dropped first, so the function rankByCueValidity
#'   means cues with the lowest cueValidity in the training set will be
#'   be dropped first.  The function must have the signature
#'   function(train_data, criterion_col, cols_to_fit).
#' @param suppress_warnings Optional argument specifying whether glm warnings
#'   should be suppressed or not. Default is TRUE.
#' @return An object of class logRegModel.
#' @export
logRegSignModel <- function(train_data, criterion_col, cols_to_fit,
                            cue_order_fn=keepOrder,
                            suppress_warnings=TRUE){
  return(logRegModelGeneral(train_data, criterion_col, cols_to_fit,
                            rowDiffSign, "logRegSignModel", cue_order_fn,
                            suppress_warnings))
}

# This is equivalent to the glm predict like this:
# predict(model, newdata=as.data.frame(sign(row1 - row2)),
#  type="response"))
predictRoot.logRegSignModel <- function(object, row1, row2) {
  generalPredictRootLogReg(row1, row2, object$cue_ordering, 
                           object$col_weights_clean, object$row_pair_fn)
}



#' Single Cue Model
#'
#' Create a single cue model by specifying columns and a dataset.  It sorts
#' cues in order of cueValidity and uses the cue with the highest cueValidity.
#' If the cue does not discriminate it guesses randomly.  If several cues have
#' the highest validity, then on each prediction it randomly selects which one
#' to use (so it might not give the same answer every time).
#' 
#' @inheritParams heuristicaModel
#' @inheritParams reversingModel
#' @examples
#' ##Fit column (5,4) to column (1,0), having validity 1.0, and column (0,1),
#' ## validity 0.
#' train_matrix <- cbind(y=c(5,4), x1=c(1,0), x2=c(0,1))
#' singlecue <- singleCueModel(train_matrix, 1, c(2,3))
#' predictRowPair(oneRow(train_matrix, 1), oneRow(train_matrix, 2), singlecue) 
#' @seealso
#' \code{\link{predictRowPair}} for prediction.
#' @seealso
#'
#' @export
singleCueModel <- function(train_data, criterion_col, cols_to_fit,
                           reverse_cues=TRUE) {
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

predictRoot.singleCueModel <- function(object, row1, row2) {
  direction_plus_minus_1 <- getWeightedCuePairDirections(object$linear_coef,
                                                         row1, row2)
  # Convert from the range [-1, 1] to the range [0, 1], which is the 
  # probability that row 1 > row 2.
  return(rescale0To1(direction_plus_minus_1))
}

#' Minimalist Model
#'
#' Fit the Minimalist heuristic by specifying columns and a dataset. It
#' searches cues in a random order, making a decision based on the first cue
#' that discriminates (has differing values on the two objects).
#'
#' 
#' @inheritParams heuristicaModel
#' @inheritParams reversingModel
#' @examples
#' ## Fit column (5,4) to column (1,0), having validity 1.0, and column (0,1),
#' ## validity 0.
#' train_matrix <- cbind(c(5,4), c(1,0), c(0,1))
#' min <- minModel(train_matrix, 1, c(2,3))
#' predictRowPair(oneRow(train_matrix, 1), oneRow(train_matrix, 2), min) 
#'
#' @seealso
#' \code{\link{predictRowPair}} for prediction.
#' @seealso
#'
#' @export
minModel <- function(train_data, criterion_col, cols_to_fit,
                     reverse_cues=TRUE) {
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
  unsigned_linear_coef <- sapply(cue_ranks,
                                 function(n) 2^(length(cue_ranks)-n) )
  
  structure(list(criterion_col=criterion_col, cols_to_fit=cols_to_fit,
                 cue_validities=cue_validities, cue_directions=cue_directions,
                 cue_validities_with_reverse=cue_validities_with_reverse,
                 unsigned_linear_coef=unsigned_linear_coef,
                 cue_sample_fn=sample),
            class="minModel")
}

#' Linear weights that can be used to compare pairs of cue directions.
#'
#' Do NOT apply these directly to raw data.
#'
#' @inheritParams stats::coef
#' @export
coef.minModel <- function(object, ...) return(object$unsigned_linear_coef)

predictRoot.minModel <- function(object, row1, row2) {
  random_order_coefficients <- object$cue_sample_fn(
    object$unsigned_linear_coef)
  coefficients <- object$cue_directions * random_order_coefficients
  direction_plus_minus_1 <- sign(
    getCuePairDirections(row1, row2) %*% coefficients)
  # Convert from the range [-1, 1] to the range [0, 1], which is the 
  # probability that row 1 > row 2.
  return(rescale0To1(direction_plus_minus_1))
}


