######################
# Row pair apply
#
# Functions and classes to hep you apply a function to
# pairs of rows, including all pairs of rows in a matrix
# or data.frame.
######################



###################################################
# Function creators-- they implement createFunction and $column_names
# for use in row pair apply functions.
################################################

#' Generic function to create functions for rowPairApply.
#' 
#' An example of solving something with another layer of indirection.
#' https://en.wikipedia.org/wiki/Fundamental_theorem_of_software_engineering
#'
#' @param object The object that implements createFunction, e.g. 
#'   heuristicsProb(ttb).
#' @param test_data The test data that row_pairs will be drawn from.
#'   We recommend 
#' @return A function that can easily be used by rowPairApply.  This
#'   is not normally used by ordinary users.
#'
#' @keywords internal
#' @export
createFunction <- function(object, test_data) UseMethod("createFunction")

# Give it a function of the form fn(row1, row2).  It creates a
# function of the form fn(index_pair), referring to two row indices
# of the data.
# e.g. f(c(1,2)) will be f(row1, row2).
# f(c(2,4))  will be f(row2, row4).
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

# fn should be of the form fn(row1, row2)
applyFunctionToRowPairs <- function(data, fn) {
  fn_with_data <- bindFunctionToRowPairs(data, fn)
  # TODO(jean): Remove this hack for column names.
  temp <- fn_with_data(c(1,1))
  results_array <- utils::combn(nrow(data), 2, fn_with_data)
  # R drops dimensions so we need different handling here.
  if (length(dim(results_array))==1) {
    results <- t(t(results_array))
  } else {
    results <- t(results_array[1,,])
  }
  colnames(results) <- colnames(temp)
  return(results)
}

#' Wrapper for fitted heuristics to generate predictions with rowPairApply.
#'
#' A list of fitted heuristics are passed in.  They must all implement
#' the fn function passed in, and they must all have the same cols_to_fit.
#' If they differ on these, then group them in separate heuristicsLists.
#' 
#' Users will generally not use the output directly-- instead just pass this
#' into one of the rowPairApply functions.
#'
#' @param list_of_fitted_heuristics Normally a list of predictProbInternal
#'   implementers, e.g. a fitted ttb model.
#' @param fn The function to be called on the heuristics, which is typically
#'   predictPairInternal (or the experimental function predictProbInternal)
#'   but can be any function with the signature function(object, row1, row2)
#'   that is implemented by the heuristics in list_of_fitted_heuristics.
#' @return An object of class heuristics, which implements createFunction.
#'   Users will generally not use this directly-- rowPairApply will.
#' 
#' @examples
#' # Use one fitted ttbModel with column 1 as criterion and columns 2,3 as
#' # cues.
#' data <- cbind(y=c(30,20,10,5), x1=c(1,1,0,0), x2=c(1,1,0,1))
#' ttb <- ttbModel(data, 1, c(2:3))
#' rowPairApply(data, heuristicsList(list(ttb), predictPairInternal))
#' # This outputs ttb's predictions for all 6 row pairs of data.
#' # (It has 6 row pairs because 4*2/2 = 6.)  It gets the predictions
#' # by calling ttb's predictPairInternal.
#'
#' # Use the same fitted ttbModel plus a unit weight model with the same
#' # criterion and cues.
#' unit <- unitWeightModel(data, 1, c(2,3))
#' rowPairApply(data, heuristicsList(list(ttb, unit), predictPairInternal))
#' # This outputs predictions with column names 'ttbModel' and
#' # 'unitWeightLinearModel'.
#'
#' # Use the same fitted ttbModel plus another ttbModel that has different
#' # cols_to_fit.  This has to be put in a separate heuristicsList function.
#' ttb_just_col_3 <- ttbModel(data, 1, c(3), fit_name="ttb3")
#' rowPairApply(data, heuristicsList(list(ttb), predictPairInternal),
#'   heuristicsList(list(ttb_just_col_3), predictPairInternal))
#' # This outputs predictions with column names 'ttbModel' and
#' # 'ttb3'.
#'
#' @seealso
#' \code{\link{rowPairApply}} which is what the output of heuristicsList is
#' normally passed in to.
#'
#' @seealso
#' \code{\link{heuristics}} for a simpler version of this function with more
#' examples.  It is recommended for most uses.  (It is hard-coded for
#' fn=predictPairInternal, which is what most people use.)
#'
#' @seealso
#'\code{\link{heuristicsProb}} for a version of this function tailored for
#' predictProbInternal rather than predictPairInternal.
#'
#' @export
heuristicsList <- function(list_of_fitted_heuristics, fn) {
  implementers <- list_of_fitted_heuristics
  cols_to_fit <- implementers[[1]]$cols_to_fit

  # If no fit_name is set, use the first-level class as the name.
  # e.g. Regression has class [regModel, lm], so it will use regModel.
  names <- sapply(implementers, function(implementer) {
    if (! isTRUE(all.equal(cols_to_fit, implementer$cols_to_fit)) ) {
      col_str1 <- paste(cols_to_fit, collapse=", ")
      col_str2 <- paste(implementer$cols_to_fit, collapse=", ")
      stop(paste("ERROR: Models with different cols_to_fit:", col_str1,
                 "vs.", col_str2, ".  Instead, put the models in separate",
                 "heuristics functions, as shown in documentation examples."))

      }
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

#' Wrap fitted heuristics to pass to rowPairApply to call predictPair.
#' 
#' One or more fitted heuristics can be passed in.  They must all have the
#' same cols_to_fit.  If they differ on cols_to_fit, then group them in separate
#' heuristics functions.
#' 
#' Users will generally not use the output directly but instead pass this to
#' rowPairApply.
#' 
#' @param ... A list of predictPairInternal implementers, e.g. a fitted ttb model.
#' @return An object of class heuristics, which implements createFunction.
#'   Users will generally not use this directly-- rowPairApply will.
#'
#' @examples
#' # Use one fitted ttbModel with column 1 as criterion and columns 2,3 as
#' # cues.
#' data <- cbind(y=c(30,20,10,5), x1=c(1,1,0,0), x2=c(1,1,0,1))
#' ttb <- ttbModel(data, 1, c(2:3))
#' rowPairApply(data, heuristics(ttb))
#' # This outputs ttb's predictions for all 6 row pairs of data.
#' # (It has 6 row pairs because 4*2/2 = 6.)  It gets the predictions
#' # by calling ttb's predictPairInternal.
#'
#' # Use the same fitted ttbModel plus a unit weight model with the same
#' # criterion and cues.
#' unit <- unitWeightModel(data, 1, c(2,3))
#' rowPairApply(data, heuristics(ttb, unit))
#' # This outputs predictions with column names 'ttbModel' and
#' # 'unitWeightLinearModel'.
#'
#' # Use the same fitted ttbModel plus another ttbModel that has different
#' # cols_to_fit.  This has to be put in a separate heuristicsList function.
#' ttb_just_col_3 <- ttbModel(data, 1, c(3), fit_name="ttb3")
#' rowPairApply(data, heuristics(ttb), heuristics(unit))
#' # This outputs predictions with column names 'ttbModel' and
#' # 'ttb3'.
#'
#' @seealso
#' \code{\link{rowPairApply}} which is what the output of heuristics is
#' normally passed in to.
#'
#' @seealso
#' \code{\link{heuristicsList}} for a version of this function where you can
#' control the function called (not necessarily predictPairInternal).
#'
#' @seealso
#' \code{\link{predictPairInternal}} which must be implemented by heuristics in
#' order to use them with the heuristics() wrapper function.  This only
#' matters for people implementing their own heuristics.
#'
#' @export
heuristics <- function(...) {
  implementers <- list(...)
  return(heuristicsList(implementers, predictPairInternal))
}

#' Wrap fitted heuristics to pass to rowPairApply to call predictProb.
#'
#' One or more fitted heuristics can be passed in.  They must all implement
#' predictProbInternal.  Users will generally not use the output directly
#' but instead pass this to rowPairApply.
#' 
#' @param ... A list of predictProbInternal implementers, e.g. a fitted ttb model.
#' @return An object of class heuristics, which implements createFunction.
#'   Users will generally not use this directly-- rowPairApply will.
#' 
#' @examples
#' ## This is typical usage:
#' data <- cbind(y=c(30,20,10,5), x1=c(1,1,0,0), x2=c(1,1,0,1))
#' ttb <- ttbModel(data, 1, c(2:ncol(data)))
#' rowPairApply(data, heuristicsProb(ttb))
#' ## This outputs ttb's predictions for all 6 row pairs of data.
#' ## (It has 6 row pairs because 4*2/2 = 6.)  It gets the predictions
#' ## by calling ttb's predictProbInternal.
#' 
#' @seealso
#' \code{\link{rowPairApply}} which is what heuristicsProb is passed in to.
#' @seealso
#' \code{\link{predictProbInternal}} which must be implemented by heuristics in
#'    order to use them with the heuristicsProb() wrapper function.
#' @export
heuristicsProb <- function(...) {
  implementers <- list(...)
  return(heuristicsList(implementers, fn=predictProbInternal))
}

#' Create function for heuristics prediction with rowPairApply.
#'
#' Creates a function that takes an index pair and returns a prediction
#' for each of the predictProbInternal implementers.
#'
#' @param object A heuristics object.
#' @inheritParams createFunction
#' @return A function that can easily be used by rowPairApply to
#'   generate predictions for all heuristics the object was created with.
#'   If it was created with M heuristics, it will generate M predictions.
#'
#' @keywords internal
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

# correctGreater (criterion function)

#' Creates function indicating whether row1[col] > row2[col].
#'
#' Using rowPairApply, this can generate a column indicating the
#' the correct direction of the criterion in comparing row 1 vs. row2 for
#' all row pairs in test_data.
#'   1 indicates row 1's criterion > row 2's criterion
#'   0 indicates they are equal
#'  -1 indicaties row 2's criterion is greater
#' By default, the output column is called "CorrectGreater," but you
#' can override the name with output_column_name.
#' 
#' This is meant to be used to measure the performance of heuristics
#' wrapped with \code{\link{heuristics}}.
#'
#' @param criterion_col The integer index of the criterion in test_data.
#' @param output_column_name An optional string
#' @return An object that implements createFunction.
#'   Users will generally not use this directly-- rowPairApply will.
#'
#' @seealso
#' \code{\link{heuristics}} is the wrapper to get the predicted greater
#'   row in the row pair for each heuristic passed in to it.
#'
#' @seealso
#' \code{\link{rowPairApply}} which has an example of using this.
#'
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


# probGreater (criterion function)

#' Creates function for one column with correct probability row1 is greater.
#'
#' Using rowPairApply, this can generate a column with
#' the correct probability that row 1 > row 2 for each row pair in 
#' the test_data.  It can do this using the criterion column passed in.
#' By default, the output column is called "ProbGreater," but you
#' can override the name with output_column_name.
#' 
#' Note this uses a very simplistic "probability" which only looks at
#' the current row pair.  It does not look at all sets of row pairs
#' with the same profile.
#'
#' @param criterion_col The integer index of the criterion in test_data.
#' @param output_column_name An optional string
#' @return An object that implements createFunction.
#'   Users will generally not use this directly-- rowPairApply will.
#'
#' @seealso
#' \code{\link{heuristicsProb}} is the wrapper to get the predicted probability
#'   that the first row in the row pair is greater, with output for each fitted
#'   heuristic passed to it.
#'
#' @seealso
#' \code{\link{rowPairApply}} which has examples of using this.
#'
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

# rowIndexes

#' Wrapper to output two columns, row 1 and row 2.
#'
#' Using rowPairApply, this can generate two columns, which by default
#' are called "Row1" and "Row2"
#' 
#' @param rowIndexColNames An optional vector of 2 strings for column names.
#' @return An object of class rowIndexes, which implements createFunction.
#'   Users will generally not use this directly-- rowPairApply will.
#' 
#' @seealso
#' \code{\link{createFunction}} which is what the returned object implements.
#' @seealso
#' \code{\link{rowPairApply}} which uses createFunction.
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
#' @param also_reverse_row_pairs Optional parameter.  When it has its default
#'   value of FALSE, it will apply every function only once to any given row
#'   pair, e.g. myFunction(1, 2).  When it is true, it will also apply
#'   the function to every reverse row pair, e.g. myFunction(1, 2) and
#'   myFunction(2, 1).
#' @return A matrix of the output of the function for all unique row pairs:
#'    c(pair_evaluator_fn(c(1,2), pair_evaluator_fn(c(1,3)), etc.) 
pairMatrix <- function(num_row, pair_evaluator_fn, also_reverse_row_pairs=FALSE) {
  if (also_reverse_row_pairs) {
    out <- cbind(utils::combn(num_row, 2, pair_evaluator_fn, simplify=FALSE),
                 utils::combn(num_row:1, 2, pair_evaluator_fn, simplify=FALSE))
  } else {
    out <- utils::combn(num_row, 2, pair_evaluator_fn, simplify=FALSE) 
  }
  # The output of combn is a complicated nested mess.  Below we make it a
  # simple matrix by assuming the dimensions of every list element are the
  # same as the first list element.
  if (length(out) < 1) {
    stop("pairMatrix got no output to process")
  }
  if (is.null(nrow(out[[1]]))) {
    stop("pair evaluator function did not return rows")
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

#' Apply list of functions to all row pairs.
#' 
#' Apply a list of functions like heuristic predictions to all row pairs in a
#' matrix or data.frame.  This does not accept arbitrary functions-- they must
#' be functions designed to be run by rowPairApply.
#' 
#' @param test_data The data to apply the functions to as a matrix or
#'   data.frame.  Heuristics must have already been fitted to trying data and
#'   must include the same criterion_col and cols_to_fit.
#' @param function_creator_list List of the functions that generate the
#'   functions to apply, such as heuristics(ttb) and correctGreater(col).
#' @param also_reverse_row_pairs Optional parameter.  When it has its default
#'   value of FALSE, it will apply every function only once to any given row
#'   pair, e.g. myFunction(row1, row2).  When it is true, it will also apply
#'   the function to every reverse row pair, e.g. myFunction(row2, row1).
#' @return A matrix of outputs from the functions.  The number of rows is based
#'   on the number of row pairs in test_data.  If the input has N rows, the
#'   output will have N x (N-1) rows.  The number of columns will be at least
#'   the number of functions but may be more as some functions may output more
#'   than one column.
#'
#' @examples
#' # This function is called like 
#' # rowPairApplyList(data, list(heuristics(ttb, reg)))
#' # instead of
#' # rowPairApply(data, heuristics(ttb, reg))
#' # See rowPairApply for details.
#' 
#' @seealso
#' \code{\link{rowPairApply}} for no need to use a list.  Examples and details
#'   are there.
#'
#' @export
rowPairApplyList <- function(test_data, function_creator_list,
                             also_reverse_row_pairs=FALSE) {
  # TODO(jean): Make a version that handles non-numeric as a data.frame.
  #  It will be slower, but it's a nice option to have for debugging.
  if (class(function_creator_list) != "list") {
    stop(paste("Second argument to rowPairApplyList should be list but got",
               class(function_creator_list)))
  }
  column_names <- vector()
  function_list <- vector()
  for (function_creator in function_creator_list) {
    fn <- createFunction(function_creator, test_data)
    function_list <- c(function_list, fn)
    column_names <- c(column_names, function_creator$column_names)
  }
  all_fn <- combineIntoOneFn(function_list)
  raw_matrix <- pairMatrix(nrow(test_data), all_fn,
                           also_reverse_row_pairs=also_reverse_row_pairs)
  colnames(raw_matrix) <- column_names
  return(raw_matrix)
}

#' Apply functions to all row pairs.
#' 
#' Apply functions like heuristic predictions to all row pairs in a matrix
#' or data.frame.  This does not accept arbitrary functions-- they must be
#' functions designed to be run by rowPairApply.
#' 
#' @param test_data The data to apply the functions to as a matrix or
#'   data.frame.  Heuristics must have already been fitted to trying data and
#'   must include the same criterion_col and cols_to_fit.
#' @param ... The functions that generate the functions to apply, such as
#'   heuristics(ttb) and correctGreater(col)-- see example below.
#' @return A matrix of outputs from the functions.  The number of rows is based
#'   on the number of row pairs in test_data.  If the input has N rows, the
#'   output will have N x (N-1) rows.  The number of columns will be at least
#'   the number of functions but may be more as some functions may output more
#'   than one column.
#
#' @examples
#' ## Fit two models to data.
#' data <- cbind(y=c(30,20,10,5), x1=c(1,1,0,0), x2=c(1,1,0,1))
#' ttb <- ttbModel(data, 1, c(2:ncol(data)))
#' lreg <- logRegModel(data, 1, c(2:ncol(data)))
#' 
#' ## Generate predictions for all row pairs for these two models:
#' rowPairApply(data, heuristics(ttb, lreg))
#' ## Returns a matrix of 2 columns, named ttbModel and regModel, and 6 rows.
#' ## The original data had 4 rows, meaning there are 4*3/2 = 6 row pairs.
#'
#' ## To see which row pair is which row, use rowIndexes:
#' rowPairApply(data, rowIndexes(), heuristics(ttb, lreg))
#' ## Returns a matrix with columns Row1, Row2, ttbModel, logRegModel.
#' ## (RowIndexes returns *two* columns.)
#' 
#' ## To see whether the first row was actually greater than the second in the
#' ## row pair, use correctGreater and give it the criterion column index, in
#' ## this case 1.
#' rowPairApply(data, heuristics(lreg, ttb), correctGreater(1))
#' ## Returns a matrix with columns logRegModel, ttbModel,
#' ## CorrectGreater.  Values are -1, 0, or 1.
#' 
#' ## To do the same analysis for the *probabilty* that the first row is
#' ## greater. use heuristicsProb and probGreater.  Warning: Not all heuristica
#' ## models have implemented the prob greater function.
#' rowPairApply(data, heuristicsProb(lreg, ttb), probGreater(1))
#' ## Returns a matrix with columns logRegModel, ttbModel, ProbGreater.
#' ## Values range from 0.0 to 1.0.
#'
#' @seealso
#' \code{\link{heuristics}} and \code{\link{heuristics}} to wrap heuristics
#' to be applied.
#' 
#' @seealso
#' \code{\link{rowIndexes}} to get apply to output row indexes for the pair.
#' 
#' @seealso
#' \code{\link{correctGreater}} to get the correct output based on the criterion column.
#' (CorrectGreater should be used with heuristics while probGreater should be used with
#' heuristicsProb.)
#'
#' @export
rowPairApply <- function(test_data, ...) {
  function_creator_list <- list(...)
  return(rowPairApplyList(test_data, function_creator_list))
}


################################################
# Wrapper functions to pass to row pair apply. #
################################################

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

#' Apply all functions to the two rows passed in.
#'
#' This has some asserts that exactly one row is passed in and exaclty one row
#' is returned, but otherwise it just calls rowPairApply.
#'
#' @param row1 The first row of cues (will apply cols_to_fit for you, based
#'   on object).
#' @param row2 The second row (will apply cols_to_fit for you, based on
#'   object).
#' @param ... The functions that generate the functions to apply, such as
#'   heuristics(ttb) and correctGreater(col).
#' @return A matrix of function outputs.
#'
#' @seealso
#' \code{\link{rowPairApply}} to apply to all row pairs in a matrix or
#'   data.frame.
#'
#' @keywords internal
rowPairApply2Rows <- function(row1, row2, ...) {
  assert_single_row(row1)
  assert_single_row(row2)
  test_data <- rbind(row1, row2)
  out <- rowPairApply(test_data, ...)
  # The asserts below ensures the functions produced only one row of output.
  assert_single_row(out)
  return(out)
}

#' Predict which of a pair of rows has a higher criterion.
#'
#' Given two rows and a fitted heuristic, returns the heuristic's prediction
#' of whether the criterion of the first row will be greater than that of
#' the 2nd row.
#'
#' @param row1 The first row of data.  The cues object$cols_to_fit will be
#'   passed to the heuristic.
#' @param row2 The second row of data.  The cues object$cols_to_fit will be
#'   passed to the heuristic.
#' @param object The fitted heuristic, e.g. a fitted ttbModel or logRegModel.
#'   (More technically, it's any object that implements predictPairInternal.)
#' @return A number in the set {-1, 0, 1}, where 1 means row1 is predicted to
#'   have a greater criterion, -1 means row2 is greater, and 0 is a guess or
#'   tie.
#'
#' @examples
#' ##Fit column (5,4) to column (1,0), having validity 1.0, and column (0,1),
#' ## validity 0.
#' train_matrix <- cbind(y=c(5,4), x1=c(1,0), x2=c(0,1))
#' singlecue <- singleCueModel(train_matrix, 1, c(2,3))
#' predictPair(oneRow(train_matrix, 1), oneRow(train_matrix, 2), singlecue)
#'
#' @seealso
#' \code{\link{rowPairApply}} to get predictions for all row pairs of a
#' matrix or data.frame.
#'
#' @export
predictPair <- function(row1, row2, object) {
  out <- rowPairApply2Rows(row1, row2, heuristics(object))
  #TODO(jean): stop unnaming!
  return(unname(out[1,1]))
}

#' Predict the probablity that row1 has a higher criterion than row2.
#'
#' Given two rows and a fitted heuristic, returns the heuristic's predicted
#' probability that row1's criterion will be greter than row2's.
#'
#' @param row1 The first row of cues (will apply cols_to_fit for you, based on
#'   object).
#' @param row2 The second row (will apply cols_to_fit for you, based on
#'   object).
#' @param object The fitted heuristic, e.g. a fitted ttbModel or logRegModel.
#'   (More technically, it's any object that implements predictProbInternal.)
#' @return A double from 0 to 1, representing the probability that row1's
#'   criterion is greater than row2's criterion.  0.5 could be a guess or tie.
#'
#' @examples
#' train_matrix <- cbind(y=c(5,4), x1=c(1,0), x2=c(0,1))
#' lreg <- logRegModel(train_matrix, 1, c(2,3))
#' predictPairProb(oneRow(train_matrix, 1), oneRow(train_matrix, 2), lreg)
#'
#' @seealso
#' \code{\link{rowPairApply}} to get predictions for all row pairs of a
#' matrix or data.frame.
#'
#' @export
predictPairProb <- function(row1, row2, object) {
  out <- rowPairApply2Rows(row1, row2, heuristicsProb(object))
  #TODO(jean): stop unnaming!
  return(unname(out[1,1]))
}
