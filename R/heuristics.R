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
predictProbInternal <- function(object, row1, row2) UseMethod("predictProbInternal")

#' Generic function to predict which of a pair of rows has higher criterion.
#' 
#' Implement this for every heuristic in order to use with row_pair and
#' aggregate functions.
#'
#' @param object The object that implements predictPair, e.g. a ttb model.
#' @param row1 The first row of cues (object$cols_to_fit columns), as a
#'   one-row matrix.
#' @param row2 The second row of cues.
#' @return A number in the set {-1, 0, 1}, where 1 means row1 is predicted to
#'   have a greater criterion, -1 means row2 is greater, and 0 is a tie.
#' @export
predictPairInternal <- function(object, row1, row2) {
  UseMethod("predictPairInternal")
}

### Shared documentation stubs ###

#' Documentation stub.
#' @param train_data Training/fitting data as a matrix or data.frame.
#' @param criterion_col The index of the colum in train_data that has the
#'   criterion.
#' @param cols_to_fit A vector of column indices in train_data, used to fit
#'   the criterion.
# Private.  This is just an easy way to share parameter documentation.
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
#' @inheritParams zzDocumentationStubModelParams
#' @inheritParams zzDocumentationStubReverseCues
#' @param fit_name Optional The name other functions can use to label output.
#'   It defaults to the class name.  It is useful to change this to a unique name
#'   if you are making multiple fits, e.g. "ttb1", "ttb2", "ttbNoReverse."
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
#' train_matrix <- cbind(y=c(5,4), x1=c(1,0), x2=c(0,0))
#' ttb <- ttbModel(train_matrix, 1, c(2,3))
#' predictPair(oneRow(train_matrix, 1), oneRow(train_matrix, 2), ttb)
#' ## But this test data results in an incorrect prediction-- that row1 has a
#' ## smaller criterion than row2-- because x1 has a reversed direction.
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
                 cue_validities_unreversed=cv$cue_validities_unreversed,
                 cue_validities=cv$cue_validities, linear_coef=linear_coef,
                 fit_name=fit_name),
            class="ttbModel")
}

#' Linear weights that can be used to compare pairs of cue directions.
#'
#' Do NOT apply these directly to raw data.
#'
#' @inheritParams stats::coef
#' @export
coef.ttbModel <- function(object, ...) object$linear_coef

predictPairInternal.ttbModel <- function(object, row1, row2) {
  direction_plus_minus_1 <- getWeightedCuePairDirections(object$linear_coef,
                                                         row1, row2)
  return(direction_plus_minus_1)
}

predictProbInternal.ttbModel <- function(object, row1, row2) {
  direction_plus_minus_1 <- predictPairInternal.ttbModel(object, row1, row2)
  # Convert from the range [-1, 1] to the range [0, 1], which is the 
  # probability that row 1 > row 2.
  return(rescale0To1(direction_plus_minus_1))
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

coef.ttbGreedyModel <- function(object, ...) object$linear_coef

predictPairInternal.ttbGreedyModel <- function(object, row1, row2) {
  direction_plus_minus_1 <- getWeightedCuePairDirections(object$linear_coef,
                                                         row1, row2)
  return(direction_plus_minus_1)
}

predictProbInternal.ttbGreedyModel <- function(object, row1, row2) {
  direction_plus_minus_1 <- predictPairInternal.ttbModel(object, row1, row2)
  # Convert from the range [-1, 1] to the range [0, 1], which is the
  # probability that row 1 > row 2.
  return(rescale0To1(direction_plus_minus_1))
}

### Unit Weight Model ###

#' Unit WeightModel, a unit-weight linear model
#'
#' Unit WeightModel is a unit-weight linear model inspired by Robyn Dawes.
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

#' @inheritParams stats::coef
#' @export
coef.unitWeightModel <- function(object, ...) object$linear_coef

predictPairInternal.unitWeightModel <- function(object, row1, row2) {
  direction_plus_minus_1 <- getWeightedCuePairDirections(object$linear_coef,
                                                         row1, row2)
  return(direction_plus_minus_1)
}

predictProbInternal.unitWeightModel <- function(object, row1, row2) {
  direction_plus_minus_1 <- predictPairInternal.unitWeightModel(object, row1, row2)
  # Convert from the range [-1, 1] to the range [0, 1], which is the 
  # probability that row 1 > row 2.
  return(rescale0To1(direction_plus_minus_1))
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

#' @inheritParams stats::coef
#' @export
coef.validityWeightModel <- function(object, ...) object$linear_coef

predictPairInternal.validityWeightModel <- function(object, row1, row2) {
  direction_plus_minus_1 <- getWeightedCuePairDirections(object$linear_coef,
                                                         row1, row2)
  return(direction_plus_minus_1)
}

predictProbInternal.validityWeightModel <- function(object, row1, row2) {
  direction_plus_minus_1 <- predictPairInternal.validityWeightModel(object, row1, row2)
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

# TODO(jean): A general fitting model for both regModel and regInterceptModel.

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

  # Make clean weights that can be easily used in predictProbInternal.
  col_weights_clean <- coef(model)
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
predictProbInternalUsingPredict <- function(object, row1, row2) {
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

predictPairInternal.regInterceptModel <- function(object, row1, row2) {
  direction_plus_minus_1 <- getWeightedCuePairDiffs(object$col_weights_clean,
                                                    row1, row2)
  return(direction_plus_minus_1)
}

predictProbInternal.regInterceptModel <- function(object, row1, row2) {
  direction_plus_minus_1 <- predictPairInternal.regInterceptModel(object, row1, row2)
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
#' @param fit_name Optional The name other functions can use to label output.
#'   It defaults to the class name.
#'
#' @return An object of class regModel, which is a subclass of lm.
#'
#' @seealso
#' \code{\link{predict.lm}} for prediction.
#'
#' @seealso
#' \code{\link{predictPair}} for predicting whether row1 is greater.
#'
#' @seealso
#' \code{\link{predictPairProb}} for predicting the probability row1 is
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
  # Make clean weights that can be easily used in predictProbInternal.
  col_weights_clean <- coef(model)
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

predictProbInternal.regModel <- function(object, row1, row2) {
  direction_plus_minus_1 <- predictPairInternal.regModel(object, row1, row2)
  # Convert from the range [-1, 1] to the range [0, 1], which is the
  # probability that row 1 > row 2.
  return(rescale0To1(direction_plus_minus_1))
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
  
  # Make clean weights that can be easily used in predictProbInternal.
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
#'   affects which cues are dropped for underspecified models. The rightmost
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

logRegCorrModel <- function(train_data, criterion_col, cols_to_fit,
                           suppress_warnings=TRUE) {
  return(logRegModelGeneral(train_data, criterion_col, cols_to_fit,
                            rowDiff, "logRegCorrModel", rankByCorrelation,
                            suppress_warnings))
}

predictProbInternal.logRegCorrModel <- function(object, row1, row2) {
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
#' @inheritParams zzDocumentationStubModelParams
#' @param cue_order_fn Optional argument as a function that orders cues.  This
#'   affects which cues are dropped for underspecified models. The rightmost
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
logRegSignModel <- function(train_data, criterion_col, cols_to_fit,
                            cue_order_fn=keepOrder,
                            suppress_warnings=TRUE,
                            fit_name="logRegSignModel") {
  model <- logRegModelGeneral(train_data, criterion_col, cols_to_fit,
                              rowDiffSign, "logRegSignModel", cue_order_fn,
                              suppress_warnings)
  model$fit_name <- fit_name
  return(model)
}

# This is equivalent to the glm predict like this:
# predict(model, newdata=as.data.frame(sign(row1 - row2)),
#  type="response"))
predictProbInternal.logRegSignModel <- function(object, row1, row2) {
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
#' @examples
#' ##Fit column (5,4) to column (1,0), having validity 1.0, and column (0,1),
#' ## validity 0.
#' train_matrix <- cbind(y=c(5,4), x1=c(1,0), x2=c(0,1))
#' singlecue <- singleCueModel(train_matrix, 1, c(2,3))
#' predictPair(oneRow(train_matrix, 1), oneRow(train_matrix, 2), singlecue)
#' @seealso
#' \code{\link{predictPairProb}} for prediction.
#' @seealso
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

#' Linear weights that can be used to compare pairs of cue directions.
#'
#' Do NOT apply these directly to raw data.
#'
#' @inheritParams stats::coef
#' @export
coef.singleCueModel <- function(object, ...) object$linear_coef

predictPairInternal.singleCueModel <- function(object, row1, row2) {
  direction_plus_minus_1 <- getWeightedCuePairDirections(object$linear_coef,
                                                         row1, row2)
  return(direction_plus_minus_1)
}

predictProbInternal.singleCueModel <- function(object, row1, row2) {
  direction_plus_minus_1 <- predictPairInternal.singleCueModel(object, row1, row2)
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

#' Linear weights that can be used to compare pairs of cue directions.
#'
#' Do NOT apply these directly to raw data.
#'
#' @inheritParams stats::coef
#' @export
coef.minModel <- function(object, ...) return(object$unsigned_linear_coef)

predictPairInternal.minModel <- function(object, row1, row2) {
  random_order_coefficients <- object$cue_sample_fn(
    object$unsigned_linear_coef)
  coefficients <- object$cue_directions * random_order_coefficients
  direction_plus_minus_1 <- sign(
    getCuePairDirections(row1, row2) %*% coefficients)
  return(direction_plus_minus_1)
}

predictProbInternal.minModel <- function(object, row1, row2) {
  direction_plus_minus_1 <- predictPairInternal.minModel(object, row1, row2)
  # Convert from the range [-1, 1] to the range [0, 1], which is the 
  # probability that row 1 > row 2.
  return(rescale0To1(direction_plus_minus_1))
}


