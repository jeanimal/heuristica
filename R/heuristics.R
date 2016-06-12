##############################################
# Heuristics: The interesting part           #
# Implementations and a few helper functions #
##############################################

## New generics ##

#' Generic function to predict which of a pair of rows has a higher criterion.
#' 
#' Do not call this directly (which is why it is called "internal").
#' Instead, call predictPair.  Heuristics implement this function in order to
#' be callable with predictPair.
#'
#' @param object The object that implements predictPair, e.g. a ttb model.
#' @param row1 The first row of cues (object$cols_to_fit columns), as a
#'   one-row matrix.
#' @param row2 The second row of cues.
#' @return A number in the set {-1, 0, 1}, where 1 means row1 is predicted to
#'   have a greater criterion, -1 means row2 is greater, and 0 is a tie.
#' @keywords internal
#' @export
predictPairInternal <- function(object, row1, row2) {
  UseMethod("predictPairInternal")
}

#' Generic function to predict the probability row 1 has a higher criterion.
#'
#' Do not call this directly (which is why it is called "internal").
#' Instead, call predictPairProb.  Heuristics implement this function in order
#' to be callable with predictPairProb.
#'
#' Most heuristics have not implemented this.  Also, the output cannot (and
#' should not) be assessed with categorical measures like percent correct.
#'
#' @param object The object that implements predictPair, e.g. a ttb model.
#' @param row1 The first row of cues (object$cols_to_fit columns), as a
#'   one-row matrix.
#' @param row2 The second row of cues.
#' @return A value from 0 to 1, representing the probability that row1's
#'   criterion is greater than row2's criterion.
#' @keywords internal
#' @export
predictProbInternal <- function(object, row1, row2) UseMethod("predictProbInternal")

### Shared documentation stubs ###

#' Documentation stub.  Just a way to share parameter documentation.
#' @param train_data Training/fitting data as a matrix or data.frame.
#' @param criterion_col The index of the colum in train_data that has the
#'   criterion.
#' @param cols_to_fit A vector of column indices in train_data, used to fit
#'   the criterion.
#' @keywords internal
zzDocumentationStubModelParams <- function(train_data, criterion_col, cols_to_fit) NULL


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

### Take The Best ###

#' Take The Best
#'
#' An implementation of the Take The Best heuristic.
#' It sorts cues in order of \code{\link{cueValidity}}, making a decision
#' based on the first cue that discriminates (has differing values on the
#' two objects).
#'
#' Cues that are tied in validity are sorted once at fitting time, and that
#' order is used consistently for all predictions with that model.  But re-
#' fitting may lead to a different cue order.  (An alternative would be to
#' randomly re-order on every prediction.)
#' 
#' @inheritParams zzDocumentationStubModelParams
#' @inheritParams zzDocumentationStubReverseCues
#' @param fit_name Optional The name other functions can use to label output.
#'   It defaults to the class name.  It is useful to change this to a unique name
#'   if you are making multiple fits, e.g. "ttb1", "ttb2", "ttbNoReverse."
#'
#' @return An object of \code{\link[base]{class}} ttbModel, which can be passed
#' to a variety of functions to make predictions, e.g.
#' \code{\link{predictPair}} and \code{\link{percentCorrectList}}.
#'
#' @examples
#' # Fit column 1 (y) to columns 2 and 3 (x1 and x2) of train_matrix.
#' train_matrix <- cbind(y=c(5,4), x1=c(1,0), x2=c(0,0))
#' ttb <- ttbModel(train_matrix, 1, c(2,3))
#' # Have ttb predict whether row 1 or 2 has a greater value for y.  The
#' # output is 1, meaning it predicts row1 is bigger.
#' predictPair(oneRow(train_matrix, 1), oneRow(train_matrix, 2), ttb)
#'
#' # Now ask it the reverse-- predict whther row 2 or row 1 is greater.  The
#' # output is -1, meaning it still predicts row1 is bigger.  (It is a
#' # symmetric heuristic.)
#' predictPair(oneRow(train_matrix, 2), oneRow(train_matrix, 1), ttb)
#'
#' # But this test data results in an incorrect prediction-- that row1 has a
#' # smaller criterion than row2-- because x1 has a reversed direction.
#' test_matrix <- cbind(y=c(5,4), x1=c(0,1), x2=c(0,0))
#' predictPair(oneRow(test_matrix, 1), oneRow(test_matrix, 2), ttb)
#'
#' @seealso
#' \code{\link{cueValidity}} for the metric used to sort cues.
#'
#' @seealso
#' \code{\link{predictPair}} for predicting whether row1 is greater.
#' 
#' @seealso
#' \code{\link{predictPairProb}} for predicting the probability row1 is
#' greater.
#' 
#' @seealso
#' \code{\link{percentCorrectList}} for the accuracy of predicting all
#' row pairs in a matrix or data.frame.
#'
#' @references
#' Gigerenzer, G. & Goldstein, D. G. (1996). "Reasoning the fast and frugal
#'  way: Models of bounded rationality". Psychological Review, 103, 650-669.
#' @references
#' Wikipedia's entry on
#' \url{http://en.wikipedia.org/wiki/Take-the-best_heuristic}.
#'
#' @export
ttbModel <- function(train_data, criterion_col, cols_to_fit,
                     reverse_cues=TRUE, fit_name="ttbModel") {
  stopIfTrainingSetHasLessThanTwoRows(train_data)
  cv <- cueValidityComplete(train_data, criterion_col, cols_to_fit,
                           reverse_cues=reverse_cues)

  unsigned_linear_coef <- sapply(cv$cue_ranks,
                                 function(n) 2^(length(cv$cue_ranks)-n) )
  # Give negative weights to cues pointing the other way.
  linear_coef <- cv$cue_directions * unsigned_linear_coef
  
  structure(list(criterion_col=criterion_col, cols_to_fit=cols_to_fit,
                 cue_validities=cv$cue_validities,
                 cue_validities_unreversed=cv$cue_validities_unreversed,
                 cue_directions=cv$cue_directions,
                 linear_coef=linear_coef,
                 fit_name=fit_name),
            class="ttbModel")
}

coef.ttbModel <- function(object) {
  return(object$linear_coef)
}

predictPairInternal.ttbModel <- function(object, row1, row2) {
  direction_plus_minus_1 <- getWeightedCuePairDirections(object$linear_coef,
                                                         row1, row2)
  return(direction_plus_minus_1)
}

# Returns index of the first (highest-validity) discriminating cue.
# For lexicographic functions like Take The Best.
# TODO(jean): In case of ties, randomly select.
indexOfCueUsed <- function(cue_validities, row1, row2) {
  usable_abs_cue_validities <- abs(cue_validities * sign(row1-row2))
  index <- which.max(usable_abs_cue_validities)
  # The cue actually did not discriminate.  It was a degenerate max.
  if (sign(row1[index]-row2[index]) == 0) {
    return(-1)
  }
  return(index)
}

getProbabilityFromCueUsed <- function(cue_used_validity, cue_used_direction,
                                      cue_row1, cue_row2) {
  data_direction <- sign(cue_row1 - cue_row2)
  # If this cue is a guess or the data does not discriminate, it is a guess,
  # which is probability 0.5 that row1 is greater.
  if (cue_used_direction == 0 || data_direction == 0) {
    return(0.5)
  }
  # If disagree, the second row is more likely greater, so use 1-validity.
  if (cue_used_direction * data_direction < 0) {
    return(1 - cue_used_validity)
  }
  return(cue_used_validity)
}

predictProbInternal.ttbModel <- function(object, row1, row2) {
  index <- indexOfCueUsed(object$cue_validities, row1, row2)
  if (index == -1) {
    return(0.5)
  }
  cue_used_validity <- object$cue_validities[index]
  cue_used_direction <- object$cue_directions[index]
  return(getProbabilityFromCueUsed(cue_used_validity, cue_used_direction,
                                   row1[index], row2[index]))
}

### Greedy Take The Best Model ###

#' Greedy Take The Best
#'
#' A variant of the Take The Best heuristic with a different cue order, namely
#' using conditional cue validity, where the validity of a cue is judged only
#' on row pairs not already decided by prior cues.  Specifically, it uses the
#' cue ranks returned by \code{\link{conditionalCueValidityComplete}}.
#' 
#' @inheritParams zzDocumentationStubModelParams
#' @inheritParams zzDocumentationStubReverseCues
#' @param fit_name Optional The name other functions can use to label output.
#'   It defaults to the class name.  It is useful to change this to a unique name
#'   if you are making multiple fits, e.g. "ttb1", "ttb2", "ttbNoReverse."
#'
#' @return An object of \code{\link[base]{class}} ttbGreedyModel, which can
#'   be passed in to \code{\link{predictPair}}.
#'
#' @examples
#' ## A data set where Take the Best and Greedy Take the Best disagree.
#' matrix <- cbind(y=c(3:1), x1=c(1,0,0), x2=c(1,0,1))
#' ttb <- ttbModel(matrix, 1, c(2,3))
#' ttb$cue_validities
#' # Returns
#' #  x1  x2 
#' # 1.0 0.5
#' ttbG <- ttbGreedyModel(matrix, 1, c(2:3))
#' ttbG$cue_validities
#' # Returns
#' #  x1  x2 
#' #   1   1
#' # because after using x1, only decisions between row 2 and 3 are left,
#' # and x2 gets 100% right  on those (after reversal).  However, these
#' # cue_validities depend on using x1, first, so cue_rank is key.
#' ttbG$cue_ranks
#' # Returns
#' #  x1  x2 
#' #   1   2
#'
#' # Now see how this affects predictions on row 2 vs. 3.
#' # Take the best guesses (output 0).
#' predictPair(oneRow(matrix, 2), oneRow(matrix, 3), ttb)
#' # Greedy Take The Best selects row 2 (output 1).
#' predictPair(oneRow(matrix, 2), oneRow(matrix, 3), ttbG)
#'
#' @seealso
#' \code{\link{conditionalCueValidityComplete}} for the metric used to sort cues.
#'
#' @seealso
#' \code{\link{ttbModel}} for the original version of Take The Best.
#'
#' @seealso
#' \code{\link{predictPair}} for predicting whether row1 is greater.
#'
#' @seealso
#' \code{\link{predictPairProb}} for predicting the probability row1 is
#' greater.
#'
#' @references
#' Martignon, L., & Hoffrage, U.  (2002).  Fast, frugal, and fit: Simple
#' heuristics for paired comparisons.  Theory and Decision, 52: 29-71.
#'
#' @export
ttbGreedyModel <- function(train_data, criterion_col, cols_to_fit,
                           fit_name="ttbGreedyModel") {
  cv <- conditionalCueValidityComplete(train_data, criterion_col, cols_to_fit)
  unsigned_linear_coef <- sapply(cv$cue_ranks,
                                 function(n) 2^(length(cv$ue_ranks)-n) )
  # Now give negative signs for cues pointing the other way.
  linear_coef <- cv$cue_directions * unsigned_linear_coef
  # Replace NA with zero weight.
  linear_coef[which(is.na(linear_coef))] <- 0

  structure(list(criterion_col=criterion_col, cols_to_fit=cols_to_fit,
                 cue_ranks=cv$cue_ranks,
                 cue_validities_unreversed=cv$cue_validities_unreversed,
                 cue_validities=cv$cue_validities, linear_coef=linear_coef,
                 fit_name=fit_name),
            class="ttbGreedyModel")
  
}

predictPairInternal.ttbGreedyModel <- function(object, row1, row2) {
  direction_plus_minus_1 <- getWeightedCuePairDirections(object$linear_coef,
                                                         row1, row2)
  return(direction_plus_minus_1)
}

### Unit Weight Model ###

#' Unit-weight linear model
#'
#' Unit-weight linear model inspired by Robyn Dawes.
#' Unit Weight Model assigns unit (+1 or -1) weights based on
#' \code{\link{cueValidity}}.
#'   \itemize{
#'     \item A cue validity > 0.5 results in a weight of +1.
#'     \item A cue validity < 0.5 results in a weight of -1.
#'   }
#' This version differs from others in that it uses a weight of 0 if cue
#' validity is 0.5 (rather than randomly assigning +1 or -1) to give faster
#' convergence of average accuracy.
#'
#' @inheritParams zzDocumentationStubModelParams
#' @inheritParams zzDocumentationStubReverseCues
#' @param fit_name Optional The name other functions can use to label output.
#'   It defaults to the class name.
#'
#' @return An object of \code{\link[base]{class}} unitWeightModel.  This is a list
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
#' \code{\link{cueValidity}} for the metric used to to determine cue direction.
#'
#' @seealso
#' \code{\link{predictPair}} for predicting whether row1 is greater.
#' 
#' @seealso
#' \code{\link{predictPairProb}} for predicting the probability row1 is
#' greater.
#' 
#' @references
#' Wikipedia's entry on
#' \url{http://en.wikipedia.org/wiki/Unit-weighted_regression}.
#'
#' @param reverse_cues Optional parameter to reverse cues as needed.
#' @export
unitWeightModel <- function(train_data, criterion_col, cols_to_fit,
                            reverse_cues=TRUE, fit_name="unitWeightModel") {
  stopIfTrainingSetHasLessThanTwoRows(train_data)
  cv <- cueValidityComplete(train_data, criterion_col, cols_to_fit,
                           reverse_cues=reverse_cues)

  linear_coef <- cv$cue_directions

  structure(list(criterion_col=criterion_col, cols_to_fit=cols_to_fit,
                 cue_validities_unreversed=cv$cue_validities_unreversed,
                 cue_validities=cv$cue_validities,
                 linear_coef=linear_coef), class="unitWeightModel")
}

coef.unitWeightModel <- function(object) {
  return(object$linear_coef)
}

predictPairInternal.unitWeightModel <- function(object, row1, row2) {
  direction_plus_minus_1 <- getWeightedCuePairDirections(object$linear_coef,
                                                         row1, row2)
  return(direction_plus_minus_1)
}

### Validity Weight Model ###

#' Validity Weight Model, a linear model weighted by cue validities
#' 
#' Validity Weight Model is a linear model with weights calculated by
#' \code{\link{cueValidity}}.
#'
#' @inheritParams zzDocumentationStubModelParams
#' @inheritParams zzDocumentationStubReverseCues
#' @param fit_name Optional The name other functions can use to label output.
#'   It defaults to the class name.
#'
#' @return An object of \code{\link[base]{class}} validityWeightModel.  This is a
#' list containing at least the following components:
#'   \itemize{
#'    \item "cue_validities": A list of cue validities for the cues in order of
#'      cols_to_fit.
#'    \item "linear_coef": Same as cue validities for this model.
#'   }
#'
#' @seealso
#' \code{\link{cueValidity}} for the metric used to to determine cue direction.
#'
#' @seealso
#' \code{\link{predictPair}} for predicting whether row1 is greater.
#'
#' @seealso
#' \code{\link{predictPairProb}} for predicting the probability row1 is
#' greater.
#'
#' @export 
validityWeightModel <- function(train_data, criterion_col, cols_to_fit,
                                reverse_cues=TRUE,
                                fit_name="validityWeightModel") {
  stopIfTrainingSetHasLessThanTwoRows(train_data)
  cv <- cueValidityComplete(train_data, criterion_col, cols_to_fit,
                           reverse_cues=reverse_cues)

  linear_coef <- cv$cue_directions * cv$cue_validities
  structure(list(criterion_col = criterion_col, cols_to_fit = cols_to_fit,
                 cue_validities_unreversed=cv$cue_validities_unreversed,
                 cue_validities=cv$cue_validities, linear_coef=linear_coef,
                 fit_name=fit_name),
            class="validityWeightModel")
}

coef.validityWeightModel <- function(object) {
  return(object$linear_coef)
}

predictPairInternal.validityWeightModel <- function(object, row1, row2) {
  direction_plus_minus_1 <- getWeightedCuePairDirections(object$linear_coef,
                                                         row1, row2)
  return(direction_plus_minus_1)
}

### Wrappers for linear regression models ###

#' Documentation stub. Just to share documentation.
#'
#' @param train_matrix A matrix (or data.frame) of data to train (fit) the
#'   model with.
#' @param criterion_col The index of the criterion column-- "y" in the formula.
#' @param cols_to_fit A vector of column indexes to fit-- the "x's" in the
#'   formula.
#' @keywords internal
zzDocumentationStubFormulaModelParams <- function(train_matrix, criterion_col,
                                              cols_to_fit) NULL

#' Create an lm model just specifying columns, generating a formula for you.
#' 
#' @inheritParams zzDocumentationStubFormulaModelParams 
#' @param include_intercept A boolean of whether to include an intercept in
#' the formula.
#'
#' @return An object of class lm.
#' @keywords internal
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
  return(stats::lm(stats::as.formula(formula_str), data=train_df))
}

#' Linear regression wrapper for hueristica
#'
#' A wrapper to create a lm model just specifying columns, generating
#' a model formula for you.  This makes it easier to run automated comparisons
#' with other models in heuristica.
#'
#' This version assumes you always want to include the intercept.
#' 
#' @inheritParams zzDocumentationStubFormulaModelParams 
#'
#' @return An object of class regInterceptModel, which is a subclass of lm.
#'
#' @seealso
#' \code{\link{regModel}} for a version that excludes the intercept.
#' @seealso
#' \code{\link{predict.lm}} for prediction.
#' @seealso
#' \code{\link{predictPairProb}} for predicting between a pair of rows.
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

  # Make clean weights that can be easily used in predictPair.
  col_weights_clean <- stats::coef(model)
  # Set na to zero.
  col_weights_clean[is.na(col_weights_clean)] <- 0
  # Because the intercept is 0 for row1 and row2, ignore it.
  if ("(Intercept)" %in% names(col_weights_clean)) {
    intercept_index <- which(names(col_weights_clean)=="(Intercept)")
    col_weights_clean <- col_weights_clean[-intercept_index]
  }
  model$col_weights_clean <- col_weights_clean

  return(model)
}

# If you have a model that implements predict, you can confirm you
# implemented predictProbInternal correctly using this, which is too slow
# to use in practice.
# TODO: use this in tests to compare with hand-coded predictions.
predictPairInternalUsingPredict <- function(object, row1, row2) {
  p1 <- stats::predict(object, as.data.frame(row1))
  p2 <- stats::predict(object, as.data.frame(row2))
  if (p1 > p2) {
    return(1)
  } else if (p1 < p2) {
    return(-1)
  } else {
    return(0)
  }
}

predictPairInternal.regInterceptModel <- function(object, row1, row2) {
  direction_plus_minus_1 <- getWeightedCuePairDiffs(object$col_weights_clean,
                                                    row1, row2)
  return(direction_plus_minus_1)
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
#' @inheritParams zzDocumentationStubFormulaModelParams 
#' @param fit_name Optional The name other functions can use to label output.
#'   It defaults to the class name.
#'
#' @return An object of class regModel, which is a subclass of lm.
#'
#' @seealso
#' \code{\link{lm}} for the regression function being wrapped.
#'
#' @seealso
#' \code{\link{predictPair}} for predicting whether row1 is greater.
#' greater.
#'
#' @export
regModel <- function(train_matrix, criterion_col, cols_to_fit,
                     fit_name="regModel") {
  model <- lmWrapper(train_matrix, criterion_col, cols_to_fit,
                     include_intercept=FALSE)
  class(model) <- c("regModel", class(model))
  # Functions in this package require criterion_col and cols_to_fit.
  model$criterion_col <- criterion_col
  model$cols_to_fit <- cols_to_fit
  # Make clean weights that can be easily used in predictPair.
  col_weights_clean <- stats::coef(model)
  # Set na to zero.
  col_weights_clean[is.na(col_weights_clean)] <- 0
  model$col_weights_clean <- col_weights_clean
  model$fit_name <- fit_name
  return(model)
}

predictPairInternal.regModel <- function(object, row1, row2) {
  direction_plus_minus_1 <- getWeightedCuePairDiffs(object$col_weights_clean,
                                                    row1, row2)
  return(direction_plus_minus_1)
}

### Logistic regression ###

# An implementation of cue_order_fn that ranks cues by cue validity.
rankByCueValidity <- function(train_data, criterion_col, cols_to_fit) {
  cv <- cueValidityComplete(train_data, criterion_col, cols_to_fit)
  return(unname(cv$cue_ranks))
}

# An implementation of cue_order_fn that ranks by each cue's correlation with
# the criterion.  WARNING: This should perhaps be applied to data transformed
# to row pairs, if you are using it for logRegModel.
rankByCorrelation <- function(train_data, criterion_col, cols_to_fit) {
  data_columns <- train_data[,c(criterion_col, cols_to_fit)]
  # Get correlations for all pairs, then limit to criterion column.
  all_cor_with_criterion <- stats::cor(data_columns)[,1]
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
    model <- suppressWarnings(stats::glm(formula, family=stats::binomial,
                                         data=training_set))
  } else { 
    model <- stats::glm(formula, family=stats::binomial, data=training_set)  
  }
  
  col_weights <- stats::coef(model)
  
  # Make clean weights that can be easily used in predictions.
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
#' @inheritParams zzDocumentationStubModelParams
#' @param cue_order_fn Optional argument as a function that orders cues.  This
#'   only matters for overspecified models (e.g. too many cues for the number
#'   of rows), in which case it affects which cues are dropped. The rightmost
#'   cues in the order are dropped first, so the function rankByCueValidity
#'   means cues with the lowest cueValidity in the training set will be
#'   be dropped first.  The function must have the signature
#'   function(train_data, criterion_col, cols_to_fit).
#' @param suppress_warnings Optional argument specifying whether glm warnings
#'   should be suppressed or not. Default is TRUE.
#' @param fit_name Optional The name other functions can use to label output.
#'   It defaults to the class name. 
#' @return An object of class logRegModel.
#' @export
logRegModel <- function(train_data, criterion_col, cols_to_fit,
                        cue_order_fn=rankByCueValidity,
                        suppress_warnings=TRUE, fit_name="logRegModel") {
  model <- logRegModelGeneral(train_data, criterion_col, cols_to_fit,
                              rowDiff, "logRegModel", cue_order_fn,
                              suppress_warnings)
  model$fit_name <- fit_name
  return(model)
}

# This is a shared function for predictions.
sigmoid <- function(z) { 1/(1+exp(-z)) }

generalPredictPairInternalLogReg <- function(row1_raw, row2_raw, cue_ordering,
                                     col_weights_clean, row_pair_fn) {
  row1 <- row1_raw[, cue_ordering, drop=FALSE]
  row2 <- row2_raw[, cue_ordering, drop=FALSE]
  raw_predict <- row_pair_fn(row1, row2) %*% col_weights_clean
  prob_row1_greater <- sigmoid(raw_predict)
  return(prob_row1_greater)
}

# This is equivalent to the glm predict like this:
# predict(model, newdata=as.data.frame(row1 - row2), type="response"))
# BUT then we round to 0, 0.5, or 1.  (That will change)
generalPredictRootLogReg <- function(row1_raw, row2_raw, cue_ordering,
                                     col_weights_clean, row_pair_fn) {
  prob_row1_greater <- generalPredictPairInternalLogReg(
    row1_raw, row2_raw, cue_ordering, col_weights_clean, row_pair_fn)
  # TODO(Jean): When aggregate accuracy is moved off of this, stop
  # doing the rounding.  Just return the probability.
  rounded_prob <- round(prob_row1_greater, digits=2)
  prediction <- ifelse(rounded_prob > 0.5, 1,
                       ifelse(rounded_prob==0.5, 0.5 , 0))
  return(prediction)
}

predictPairInternal.logRegModel <- function(object, row1, row2) {
  prob_row1_greater <-  generalPredictPairInternalLogReg(row1, row2, object$cue_ordering, 
                                   object$col_weights_clean, object$row_pair_fn)
  rounded_prob <- round(prob_row1_greater, digits=2)
  prediction <- ifelse(rounded_prob > 0.5, 1,
                       ifelse(rounded_prob==0.5, 0 , -1))
  return(prediction)
}

# This is equivalent to the glm predict like this:
# predict(model, newdata=as.data.frame(row1 - row2), type="response"))
# And then probabilities > 0.5 return 1 while probabilities < 0.5 return 0.
predictProbInternal.logRegModel <- function(object, row1, row2) {
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
#' This single cue model follows the definition used in this reference:
#' Hogarth, R. & Karelaia, N. (2007). Heuristic and Linear Models of Judgment: 
#' Matching Rules and Environments. Psychological Review. 114(3), pp.733-758.
#' Note that other researchers have sometimes used other measures than cue
#' validity to select the single cue to be used.
#' 
#' @inheritParams zzDocumentationStubModelParams
#' @inheritParams zzDocumentationStubReverseCues
#' @param fit_name Optional The name other functions can use to label output.
#'   It defaults to the class name.
#'
#' @examples
#' ##Fit column (5,4) to column (1,0), having validity 1.0, and column (0,1),
#' ## validity 0.
#' train_matrix <- cbind(y=c(5,4), x1=c(1,0), x2=c(0,1))
#' singlecue <- singleCueModel(train_matrix, 1, c(2,3))
#' predictPair(oneRow(train_matrix, 1), oneRow(train_matrix, 2), singlecue)
#'
#' @seealso
#' \code{\link{predictPairProb}} for prediction.
#'
#' @export
singleCueModel <- function(train_data, criterion_col, cols_to_fit,
                           reverse_cues=TRUE, fit_name="singleCueModel") {
  stopIfTrainingSetHasLessThanTwoRows(train_data)
  cv <- cueValidityComplete(train_data, criterion_col, cols_to_fit,
                           reverse_cues=reverse_cues)
  unsigned_linear_coef <- sapply(cv$cue_ranks, function(v)
   if (v==1) 1 else 0)

  linear_coef <- cv$cue_directions * unsigned_linear_coef
  
  structure(list(criterion_col=criterion_col, cols_to_fit=cols_to_fit,
                 cue_validities_unreversed=cv$cue_validities_unreversed,
                 cue_validities=cv$cue_validities,
                 linear_coef=linear_coef, fit_name=fit_name),
            class="singleCueModel")
}

coef.singleCueModel <- function(object) {
  return(object$linear_coef)
}

predictPairInternal.singleCueModel <- function(object, row1, row2) {
  direction_plus_minus_1 <- getWeightedCuePairDirections(object$linear_coef,
                                                         row1, row2)
  return(direction_plus_minus_1)
}


#' Minimalist Model
#'
#' Fit the Minimalist heuristic by specifying columns and a dataset. It
#' searches cues in a random order, making a decision based on the first cue
#' that discriminates (has differing values on the two objects).
#' 
#' @inheritParams zzDocumentationStubModelParams
#' @inheritParams zzDocumentationStubReverseCues
#' @param fit_name Optional The name other functions can use to label output.
#'   It defaults to the class name. 
#' @examples
#' ## Fit column (5,4) to column (1,0), having validity 1.0, and column (0,1),
#' ## validity 0.
#' train_matrix <- cbind(c(5,4), c(1,0), c(0,1))
#' min <- minModel(train_matrix, 1, c(2,3))
#' predictPair(oneRow(train_matrix, 1), oneRow(train_matrix, 2), min)
#'
#' @seealso
#' \code{\link{predictPairProb}} for prediction.
#' @seealso
#'
#' @export
minModel <- function(train_data, criterion_col, cols_to_fit,
                     reverse_cues=TRUE, fit_name="minModel") {
  stopIfTrainingSetHasLessThanTwoRows(train_data)
  cv <- cueValidityComplete(train_data, criterion_col, cols_to_fit,
                           reverse_cues=reverse_cues)
  
  unsigned_linear_coef <- sapply(cv$cue_ranks,
                                 function(n) 2^(length(cv$cue_ranks)-n) )
  
  structure(list(criterion_col=criterion_col, cols_to_fit=cols_to_fit,
                 cue_validities_unreversed=cv$cue_validities_unreversed,
                 cue_validities=cv$cue_validities,
                 cue_directions=cv$cue_directions,
                 unsigned_linear_coef=unsigned_linear_coef,
                 cue_sample_fn=sample, fit_name=fit_name),
            class="minModel")
}

predictPairInternal.minModel <- function(object, row1, row2) {
  random_order_coefficients <- object$cue_sample_fn(
    object$unsigned_linear_coef)
  coefficients <- object$cue_directions * random_order_coefficients
  direction_plus_minus_1 <- sign(
    getCuePairDirections(row1, row2) %*% coefficients)
  return(direction_plus_minus_1)
}
