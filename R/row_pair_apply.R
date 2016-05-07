######################
# Row pair apply
#
# Functions and classes to hep you apply a function to
# pairs of rows, including all pairs of rows in a matrix
# or data.frame.
######################



###
# Function creators-- they implement createFunction and $column_names
# for use in row pair apply functions.

#' Generic function to create functions for allRowPairApply.
#' 
#' An example of solving something with another layer of indirection.
#' https://en.wikipedia.org/wiki/Fundamental_theorem_of_software_engineering
#'
#' @param object The object that implements createFunction, e.g. 
#'   heuristicsProb(ttb).
#' @param test_data The test data that row_pairs will be drawn from.
#'   We recommend 
#' @return A function that can easily be used by allRowPairApply.  This
#'   is not normally used by ordinary users.
#' 
#' @seealso
#' \code{\link{predictPairProb}} for a simpler way to generate predictions
#'   from fitted models.
#' @seealso
#' \code{\link{allRowPairApply}} which uses createFunction.
#' @export
createFunction <- function(object, test_data) UseMethod("createFunction")

# Give it a function of the form fn(row1, row2).  It creates a
# function of the form fn(index_pair) that refer to two row indices,
# e.g. f(c(1,2)) or f(c(2,4)).
# The data will be forced to be a matrix rather than a data.frame,
# so you cannot use data.frame functions (like $col) in fn_to_bind.
bindFunctionToRowPairs <- function(raw_data, fn_to_bind) {
  #TODO(jean): Get rid of as.matrix below, because that limits the fn_to_bind.
  # I already tried once to do that, and it had lots of consequences.
  data <- as.matrix(raw_data)
  new_fn <- function(index_pair) {
    row1 <- oneRow(data, index_pair[1])
    row2 <- oneRow(data, index_pair[2])
    return(fn_to_bind(row1, row2))
  }
  return(new_fn)
}

applyFunctionToRowPairs <- function(data, fn) {
  fn_with_data <- bindFunctionToRowPairs(data, fn)
  # TODO(jean): Remove this hack for column names.
  temp <- fn_with_data(c(1,1))
  results_array <- combn(nrow(data), 2, fn_with_data)
  # R drops dimensions so we need different handling here.
  if (length(dim(results_array))==1) {
    results <- t(t(results_array))
  } else {
    results <- t(results_array[1,,])
  }
  colnames(results) <- colnames(temp)
  return(results)
}

#' Wrapper for fitted heuristics to generate predictions with allRowPairApply.
#'
#' One or more fitted heuristics can be passed in.  They must all implement
#' predictProbInternal.  Users will generally not use the output directly.
#' 
#' @param list_of_fitted_heuristics Normally a list of predictProbInternal
#'   implementers, e.g. a fitted ttb model.
#' @param fn The function to be called on the heuristics, which is typically
#'   either predictPairInternal or predictProbInternal but can be any function
#'   with the signature function(object, row1, row2) that is implemented by
#'   the heuristics in list_of_fitted_heuristics.
#' @return An object of class heuristics, which implements createFunction.
#'   Users will generally not use this directly-- allRowPairApply will.
#' 
#' @examples
#' ## This is typical usage:
#' data <- cbind(y=c(30,20,10,5), x1=c(1,1,0,0), x2=c(1,1,0,1))
#' ttb <- ttbModel(data, 1, c(2:ncol(data)))
#' allRowPairApply(data, heuristicsList(list(ttb), predictPairInternal))
#' ## This outputs ttb's predictions for all 6 row pairs of data.
#' ## (It has 6 row pairs because 4*2/2 = 6.)  It gets the predictions
#' ## by calling ttb's predictPairInternal.
#'
#' @seealso
#' \code{\link{heuristics}} and  \code{\link{heuristicsProb}} for 
#'   simpler versions of this function.
#' @export
heuristicsList <- function(list_of_fitted_heuristics, fn) {
  implementers <- list_of_fitted_heuristics
  # Assume the cols_to_fit are the same for all heuristics.
  cols_to_fit <- implementers[[1]]$cols_to_fit
  # If no fit_name is set, use the first-level class as the name.
  # e.g. Regression has class [regModel, lm], so it will use regModel.
  names <- sapply(implementers, function(implementer) {
      if (is.null(implementer$fit_name)) {
        return(c(class(implementer)[[1]]))
      } else {
        return(c(implementer$fit_name))
      }
    })
  structure(list(predictProbInternal_implementers=implementers,
                 cols_to_fit=cols_to_fit,
                 column_names=names, fn=fn),
            class="heuristics")
}

#' Wrap fitted heuristics to pass to allRowPairApply to call predictPair.
#' 
#' One or more fitted heuristics can be passed in.  They must all implement
#' predictPairInternal.  Users will generally not use the output directly
#' but instead pass this to allRowPairApply.
#' 
#' @param ... A list of predictPairInternal implementers, e.g. a fitted ttb model.
#' @return An object of class heuristics, which implements createFunction.
#'   Users will generally not use this directly-- allRowPairApply will.
#'
#' @examples
#' ## This is typical usage:
#' data <- cbind(y=c(30,20,10,5), x1=c(1,1,0,0), x2=c(1,1,0,1))
#' ttb <- ttbModel(data, 1, c(2:ncol(data)))
#' allRowPairApply(data, heuristics(ttb))
#' ## This outputs ttb's predictions for all 6 row pairs of data.
#' ## (It has 6 row pairs because 4*2/2 = 6.)  It gets the predictions
#' ## by calling ttb's predictPairInternal.
#' 
#' @seealso
#' \code{\link{allRowPairApply}} and \code{\link{rowPairApply}} which are
#'    what heuristicsProb is normally passed in to.
#' @seealso
#' \code{\link{predictPairInternal}} which must be implemented by heuristics in
#'    order to use them with the heuristics() wrapper function.
#' @export
heuristics <- function(...) {
  implementers <- list(...)
  return(heuristicsList(implementers, predictPairInternal))
}

#' Wrap fitted heuristics to pass to allRowPairApply to call predictProb.
#'
#' One or more fitted heuristics can be passed in.  They must all implement
#' predictProbInternal.  Users will generally not use the output directly
#' but instead pass this to allRowPairApply.
#' 
#' @param ... A list of predictProbInternal implementers, e.g. a fitted ttb model.
#' @return An object of class heuristics, which implements createFunction.
#'   Users will generally not use this directly-- allRowPairApply will.
#' 
#' @examples
#' ## This is typical usage:
#' data <- cbind(y=c(30,20,10,5), x1=c(1,1,0,0), x2=c(1,1,0,1))
#' ttb <- ttbModel(data, 1, c(2:ncol(data)))
#' allRowPairApply(data, heuristicsProb(ttb))
#' ## This outputs ttb's predictions for all 6 row pairs of data.
#' ## (It has 6 row pairs because 4*2/2 = 6.)  It gets the predictions
#' ## by calling ttb's predictProbInternal.
#' 
#' @seealso
#' \code{\link{allRowPairApply}} and \code{\link{rowPairApply}} which are
#'    what heuristicsProb is normally passed in to.
#' @seealso
#' \code{\link{predictProbInternal}} which must be implemented by heuristics in
#'    order to use them with the heuristicsProb() wrapper function.
#' @export
heuristicsProb <- function(...) {
  implementers <- list(...)
  return(heuristicsList(implementers, fn=predictProbInternal))
}

#' Create function for heuristics prediction with allRowPairApply.
#'
#' Creates a function that takes an index pair and returns a prediction
#' for each of the predictProbInternal implementers.
#'
#' @param object A heuristics object.
#' @inheritParams createFunction
#' @return A function that can easily be used by allRowPairApply to
#'   generate predictions for all heuristics the object was created with.
#'   If it was created with M heuristics, it will generate M predictions.
#'
#' @seealso
#' \code{\link{ttbModel}} for example code.
#'
#' @export
createFunction.heuristics <- function(object, test_data) {
  test_data_trim <- as.matrix(test_data[, object$cols_to_fit, drop=FALSE])
  all_predictProbInternal_fn <- function(index_pair) {
    row1 <- oneRow(test_data_trim, index_pair[1])
    row2 <- oneRow(test_data_trim, index_pair[2])
    out_all <- NULL
    for (implementer in object$predictProbInternal_implementers) {
      # print(class(implementer))
      out <- object$fn(implementer, row1, row2)
      # TODO(Jean): Test the checks below.
      #if (out < 0) {
      #  stop(paste("ERROR heuristic of class",class(implementer),"predicted",
      #             out,", which is < 0"))
      #}
      if (out > 1) {
        stop(paste("ERROR heuristic of class",class(implementer),"predicted",
                   out,", which is > 1"))
      }
      if (is.null(out_all)) {
        out_all <- cbind(out)
      } else {
        out_all <- cbind(out_all, out)
      }
    }
    return(out_all)
  }
  return(all_predictProbInternal_fn)
}

# probGreater (criterion function)

#' Creates function for one column with correct probability row1 is greater.
#'
#' Using allRowPairApply, this can generate a column with
#' the correct probability that row 1 > row 2 for each row pair in 
#' the test_data.  It can do this using the criterion column passed in.
#' By default, the output column is called "ProbGreater," but you
#' can override the name with output_column_name.
#'
#' @param criterion_col The integer index of the criterion in test_data.
#' @param output_column_name An optional string
#' @return An object that implements createFunction.
#'   Users will generally not use this directly-- allRowPairApply will.
#'
#' @seealso
#' \code{\link{createFunction}} which is what the returned object implements.
#' @seealso
#' \code{\link{allRowPairApply}} which uses createFunction.
#' @export
probGreater <- function(criterion_col, output_column_name="ProbGreater") {
  structure(list(criterion_col=criterion_col,
                 column_names=c(output_column_name)),
            class="probGreater")
}

createFunction.probGreater <- function(object, test_data) {
  criterion_matrix <- as.matrix(test_data[, object$criterion_col, drop=FALSE])
  correct_fn <- function(index_pair)
    rescale0To1(sign(criterion_matrix[index_pair[1], , drop=FALSE]
                     - criterion_matrix[index_pair[2], , drop=FALSE]))
  return(correct_fn)
}

# correctGreater (criterion function)

#' Creates function for one column indicating whether row1 is greater.
#'
#' Using allRowPairApply, this can generate a column indicating the
#' the correct direction of the criterion in comparing row 1 vs. row2 for
#' all row pairs in test_data.
#'   1 indicates row 1's criterion > row 2's criterion
#'   0 indicates they are equal
#'  -1 indicaties row 2's criterion is greater
#' By default, the output column is called "CorrectGreater," but you
#' can override the name with output_column_name.
#'
#' @param criterion_col The integer index of the criterion in test_data.
#' @param output_column_name An optional string
#' @return An object that implements createFunction.
#'   Users will generally not use this directly-- allRowPairApply will.
#'
#' @seealso
#' \code{\link{createFunction}} which is what the returned object implements.
#' @seealso
#' \code{\link{allRowPairApply}} which uses createFunction.
#' @export
correctGreater <- function(criterion_col, output_column_name="CorrectGreater") {
  structure(list(criterion_col=criterion_col,
                 column_names=c(output_column_name)),
            class="correctGreater")
}

createFunction.correctGreater <- function(object, test_data) {
  criterion_matrix <- as.matrix(test_data[, object$criterion_col, drop=FALSE])
  correct_fn <- function(index_pair)
    sign(criterion_matrix[index_pair[1], , drop=FALSE]
         - criterion_matrix[index_pair[2], , drop=FALSE])
  return(correct_fn)
}

# rowIndexes

#' Wrapper to output two columns, row 1 and row 2.
#'
#' Using allRowPairApply, this can generate two columns, which by default
#' are called "Row1" and "Row2"
#' 
#' @param rowIndexColNames An optional vector of 2 strings for column names.
#' @return An object of class rowIndexes, which implements createFunction.
#'   Users will generally not use this directly-- allRowPairApply will.
#' 
#' @seealso
#' \code{\link{createFunction}} which is what the returned object implements.
#' @seealso
#' \code{\link{allRowPairApply}} which uses createFunction.
#' @export
rowIndexes <- function(rowIndexColNames=c("Row1", "Row2")) {
  if (length(rowIndexColNames) != 2) {
    stop(paste("Expected only 2 column names but got: ",
               length(rowIndexColNames)))
  }
  structure(list(column_names=rowIndexColNames),
            class="rowIndexes")
}

createFunction.rowIndexes<- function(object, test_data) {
  row_index_fn <- function(index_pair) cbind(index_pair[1], index_pair[2])
  return(row_index_fn)
}

# colPairValues (still under development)

# To get the column index by name: which(colnames(df)=="B")
colPairValues <- function(input_column_index, output_column_name) {
  structure(list(input_column_index=input_column_index,
                 column_names=c(paste0(output_column_name,"_1"),
                                paste0(output_column_name,"_2"))),
            class="colPairValues")
}

createFunction.colPairValues<- function(object, test_data) {
  # The column value might not be numeric, so do not convert to a matrix.
  column_df <- test_data[, object$input_column_index, drop=FALSE]
  column_fn <- function(index_pair)
    cbind(column_df[index_pair[1], , drop=FALSE],
          column_df[index_pair[2], , drop=FALSE])
  return(column_fn)
}

#' Apply a function to all unique pairs of row indices up to num_row.
#' @param num_row The number of rows to generate index pairs for.
#' @param pair_evaluator_fn The function you want applied.  It should
#'   accept a list of two numbers, the index of row 1 and the index of row2.
#' @return A matrix of the output of the function for all unique row pairs:
#'    c(pair_evaluator_fn(c(1,2), pair_evaluator_fn(c(1,3)), etc.) 
pairMatrix <- function(num_row, pair_evaluator_fn) {
  out <- combn(num_row, 2, pair_evaluator_fn, simplify=FALSE)
  # The output of combn is a complicated nested mess.  Below we make it a
  # simple matrix by assuming the dimensions of every list element are the
  # same as the first list element.
  if (length(out) < 1) {
    stop("pairMatrix got no output to process")
  }
  rows <- length(out) * nrow(out[[1]])
  cols <- ncol(out[[1]])
  out_matrix <- matrix(unlist(out), rows, cols, byrow=TRUE)
  colnames(out_matrix) <- colnames(out[[1]])
  return(out_matrix)
}

# Combines all functions in function_list into one function that can be
# passed into an apply on data.  Output is one-row matrix with the results of
# functions in columns.  A function can generate more than one column but in
# that case MUST output a row rather than a vector.  Column headers are
# included in output.
combineIntoOneFn <- function(function_list) {
  all_fn <- function(...) {
    out_all <- c()
    for (fun in function_list) {
      out <- fun(...)
      if (is.null(out_all)) {
        out_all <- cbind(out)
      } else {
        out_all <- cbind(out_all, out)
      }
    }
    return(out_all)
  }
  return(all_fn)
}

###
# The most general row pair apply function.  All others call this one.

#' Apply functions to all row pairs.
#' 
#' Apply functions like heuristic predictions to all row pairs in a matrix
#' or data.frame.
#' 
#' @param test_data The data to apply the functions to as a matrix or
#'   data.frame.  Heuristics must have already been fitted to trying data and
#'   must include the same criterion_col and cols_to_fit.
#' @param function_creator_list List of the objects that generate the functions
#'   to apply, using createFunction.  For example,
#'    list(heuristicsProb(ttb, reg), probGreater(col), rowIndexes()).
#' @return A matrix of outputs from the functions.  The number of rows is based
#'   on the number of row pairs in test_data.  If the input has N rows, the
#'   output will have N x (N-1) rows.  The number of columns will be at least
#'   the number of functions but may be more as some functions may output more
#'   than one column.
#
#' @examples
#' ## Fit two models to the city_population data set.
#' ttb <- ttbModel(city_population, 3, c(4:ncol(city_population)))
#' reg <- regInterceptModel(city_population, 3, c(4:ncol(city_population)))
#' 
#' ## Generate predictions for all row pairs for these two models:
#' out1 <- allRowPairApplyList(city_population, list(heuristicsProb(ttb, reg)))
#' head(out1)
#' nrow(out1)
#' ## returns a matrix of 2 columns, named ttbModel and regModel
#' 
#' ## Generate a matrix with the correct values and the heuristics'
#' ## predictions:
#' out2 <- allRowPairApplyList(city_population,
#'                             list(probGreater(3), heuristicsProb(reg, ttb)))
#' head(out2)
#' nrow(out2)
#' ## returns a matrix of 3 columns, ProbGreater, ttbModel and regModel
#'
#' @seealso
#' \code{\link{createFunction}} which must be implemented by the objects
#'    passed in the "..." argument, along with the attribute $column_names.
#' @seealso
#' \code{\link{predictProbInternal}} which must be implemented by heuristics in
#'    order to use them with the heuristicsProb() wrapper function.
#'
#' @export
allRowPairApplyList <- function(test_data, function_creator_list) {
  # TODO(jean): Make a version that handles non-numeric as a data.frame.
  #  It will be slower, but it's a nice option to have for debugging.
  column_names <- vector()
  function_list <- vector()
  for (function_creator in function_creator_list) {
    fn <- createFunction(function_creator, test_data)
    function_list <- c(function_list, fn)
    column_names <- c(column_names, function_creator$column_names)
  }
  all_fn <- combineIntoOneFn(function_list)
  raw_matrix <- pairMatrix(nrow(test_data), all_fn)
  colnames(raw_matrix) <- column_names
  return(raw_matrix)
}

#' Apply functions to all row pairs.
#' 
#' Apply functions like heuristic predictions to all row pairs in a matrix
#' or data.frame.
#' 
#' @param test_data The data to apply the functions to as a matrix or
#'   data.frame.  Heuristics must have already been fitted to trying data and
#'   must include the same criterion_col and cols_to_fit.
#' @param ... The objects that generate the functions to apply, using
#'   createFunction.  For example, heuristicsProb(ttb), probGreater(col), or
#'   colPairValues.
#' @return A matrix of outputs from the functions.  The number of rows is based
#'   on the number of row pairs in test_data.  If the input has N rows, the
#'   output will have N x (N-1) rows.  The number of columns will be at least
#'   the number of functions but may be more as some functions may output more
#'   than one column.
#
#' @examples
#' ## Fit two models to the city_population data set.
#' ttb <- ttbModel(city_population, 3, c(4:ncol(city_population)))
#' reg <- regInterceptModel(city_population, 3, c(4:ncol(city_population)))
#' 
#' ## Generate predictions for all row pairs for these two models:
#' out1 <- allRowPairApply(city_population, heuristicsProb(ttb, reg))
#' head(out1)
#' nrow(out1)
#' ## returns a matrix of 2 columns, named ttbModel and regInterceptModel.
#' 
#' ## Generate a matrix with correct values and the heuristics' predictions:
#' out2 <- allRowPairApply(city_population, probGreater(3),
#'                         heuristicsProb(reg, ttb))
#' head(out2)
#' nrow(out2)
#' ## returns a matrix of 3 columns, ProbGreater, ttbModel and regModel.
#'
#' @seealso
#' \code{\link{createFunction}} which must be implemented by the objects
#'    passed in the "..." argument, along with the attribute $column_names.
#' @seealso
#' \code{\link{predictProbInternal}} which must be implemented by heuristics in
#'    order to use them with the heuristicsProb() wrapper function.
#'
#' @export
allRowPairApply <- function(test_data, ...) {
  function_creator_list <- list(...)
  return(allRowPairApplyList(test_data, function_creator_list))
}

###
# Specific row pair apply functions.


# Private helper.
assert_single_row <- function(row) {
  num_rows <- nrow(row)
  if (is.null(num_rows)) {
    stop(paste("Error: Object does not have row dimension.  To get one row ",
                "of a matrix, be sure to use drop=FALSE, e.g. ",
                "my_matrix[row_num, , drop=FALSE]"))
  } else if (num_rows != 1) {
    stop(paste("Error: Expected a single row but got", num_rows, "rows."))
  }
}

# Private helper.
assert_single_column <- function(obj) {
  num_cols <- ncol(obj)
  if (is.null(num_cols)) {
    stop(paste("Error: Object does not have column dimension."))
  } else if (num_cols != 1) {
    stop(paste("Error: Expected a single column but got", num_cols,
               "columns."))
  }
}

#' Apply all functions to the row pair.
#'
#' @param row1 The first row of cues (will apply cols_to_fit for you, based
#'   on object).
#' @param row2 The second row (will apply cols_to_fit for you, based on
#'   object).
#' @param ... The objects that generate the functions to apply, using
#'   createFunction.  For example, heuristicsProb(ttb), probGreater(col), or
#'   colPairValues.
#' @return A matrix of function outputs.
#' @export
rowPairApply <- function(row1, row2, ...) {
  assert_single_row(row1)
  assert_single_row(row2)
  test_data <- rbind(row1, row2)
  out <- allRowPairApply(test_data, ...)
  # The asserts below ensures the functions produced only one row of output.
  assert_single_row(out)
  return(out)
}

# Given a fitted heuristic object and a function that can be called on that
# object, returns a function with just the signature function(row1, row2)
# where the rows have just the cols_to_fit.  The output should be a matrix
# row with one column.
makeRowPairFunctionForObject <- function(object, internalFn) {
  fn1 <- function(row1, row2) {
    row1_cues <- row1[,object$cols_to_fit, drop=FALSE]
    row2_cues <- row2[,object$cols_to_fit, drop=FALSE]
    # e.g. predictPairInternal(object, row1_cues, row2_cues)
    internalFn(object, row1_cues, row2_cues)
  }
  return(fn1)
}

makeTrimRowPairFunctionForObject <- function(object, internalFn) {
  fn1 <- function(row1_cues, row2_cues) {
    internalFn(object, row1_cues, row2_cues)
  }
  return(fn1)
}

#' Predict which of a pair of rows has a higher criterion.
#'
#' Assumes the object implements predictProbInternal and has $cols_to_fit.
#'
#' @param row1 The first row of data.  The cues object$cols_to_fit will be
#'   passed to the heuristic.
#' @param row2 The second row of data.  The cues object$cols_to_fit will be
#'   passed to the heuristic.
#' @param object The object that implements predictPairInternal, e.g. a fitted
#'   ttbModel or logRegModel.
#' @return A number in the set {-1, 0, 1}, where 1 means row1 is predicted to
#'   have a greater criterion, -1 means row2 is greater, and 0 is a tie.
#' @export
predictPair <- function(row1, row2, object) {
  out <- rowPairApply(row1, row2, heuristics(object))
  # The asserts below ensure predictPairInternal returned just one value.
  assert_single_row(out)
  assert_single_column(out)
  #TODO(jean): stop unnaming!
  return(unname(out[1,1]))
}

predictPairNotQuiteWorking <- function(row1, row2, object) {
  data <- rbind(row1, row2)
  # fn1 is the extra step needed by heuristics.
  # TODO: how get column names for other functions?
  fn1 <- makeRowPairFunctionForObject(object, predictPairInternal)
  fn2 <- bindFunctionToRowPairs(data, fn1)
  raw_matrix <- t(pairMatrix(2, fn2))
  # The asserts below ensure predictPairInternal returned just one value.
  assert_single_row(raw_matrix)
  assert_single_column(raw_matrix)
  return(raw_matrix[1,1])
}

#' Predict the probablity that row1 has a higher criterion than row2.
#'
#' It uses the passed-in object to do the prediction, assuming the object
#' implements predictProbInternal and has $cols_to_fit.
#'
#' @param row1 The first row of cues (will apply cols_to_fit for you, based on
#'   object).
#' @param row2 The second row (will apply cols_to_fit for you, based on
#'   object).
#' @param object The object that implements predictPair, e.g. a fitted ttbModel
#'   or regModel.
#' @return A double from 0 to 1, representing the probability that row1's
#'   criterion is greater than row2's criterion.
#' @export
predictPairProb <- function(row1, row2, object) {
  out <- rowPairApply(row1, row2, heuristicsProb(object))
  # The asserts below ensure predictProbInternal had a reasonable implementation.
  assert_single_row(out)
  assert_single_column(out)
  #TODO(jean): stop unnaming!
  return(unname(out[1,1]))
}

#
# Below are experimental functions.  They avoid an extra level of indirection.
#

createrRowIndexPairFn <- function() {
  # Data is ignored.
  rowIndexPairFn <- function(index_pair, data) {
    rowIndexPair <- cbind(index_pair[1], index_pair[2])
    colnames(rowIndexPair) <- c("Row1", "Row2")
    return(rowIndexPair)
  }
}

# TODO: Change this to work like heuristicWrapperFn2.
createProbGreaterFn <- function(criterionIndex) {
  probGreater_fn <- function(index_pair, data) {
    criterion_matrix <- as.matrix(data[, criterionIndex, drop=FALSE])
    probGreater <- rescale0To1(sign(criterion_matrix[index_pair[1], , drop=FALSE]
                                    - criterion_matrix[index_pair[2], , drop=FALSE]))
    colnames(probGreater) <- c("ProbGreater")
    return(probGreater)
  }
  return(probGreater_fn)
}

heuristicWrapperFn2 <- function(data, object) {
  fn1 <- makeTrimRowPairFunctionForObject(object, predictPairInternal)
  trim_data <- as.matrix(data[, object$cols_to_fit, drop=FALSE])
  fn2 <- bindFunctionToRowPairs(trim_data, fn1)
  fn3 <- function(index_pair) {
    out <- fn2(index_pair)
    colnames(out) <- c(class(object)[[1]])
    return(out)
  }
  return(fn3)
}

createHeuristicWrapperFn2 <- function(object) {
  structure(list(binding_function=heuristicWrapperFn2, extra_args=object), class="bindToData")
}

convertClassBindToData <- function(fn, data) {
  if (class(fn) == "bindToData") {
    return(fn$binding_function(data, fn$extra_args))
  } else {
    return(fn)
  }
}

# The signature of pair_evaluator_fn should be
# function(index_pair, data) where index_pair is a vector of two integer
# row indexes into the data.
simpleRowPairApply <- function(data, pair_evaluator_fn) {
  fn <- convertClassBindToData(pair_evaluator_fn, data)
  out <- combn(nrow(data), 2, fn, simplify=FALSE)
  # The output of combn is a complicated nested mess.  Below we make it a
  # simple matrix by assuming the dimensions of every list element are the
  # same as the first list element.
  if (length(out) < 1) {
    stop("pairMatrix got no output to process")
  }
  rows <- length(out) * nrow(out[[1]])
  cols <- ncol(out[[1]])
  out_matrix <- matrix(unlist(out), rows, cols, byrow=TRUE)
  colnames(out_matrix) <- colnames(out[[1]])
  return(out_matrix)
}

simpleRowPairApplyList <- function(test_data, function_list) {
  converted_function_list <- lapply(function_list, convertClassBindToData, test_data)
  all_fn <- combineIntoOneFn(converted_function_list)
  simpleRowPairApply(test_data, all_fn)
}
