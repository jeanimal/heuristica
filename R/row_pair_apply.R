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
#'   heuristics(ttb).
#' @param test_data The test data that row_pairs will be drawn from.
#'   We recommend 
#' @return A function that can easily be used by allRowPairApply.  This
#'   is not normally used by ordinary users.
#' 
#' @seealso
#' \code{\link{predictRowPair}} for a simpler way to generate predictions
#'   from fitted models.
#' @seealso
#' \code{\link{allRowPairApply}} which uses createFunction.
#' @export
createFunction <- function(object, test_data) UseMethod("createFunction")

#' Wrapper for fitted heuristics to generate predictions with allRowPairApply.
#'
#' One or more fitted heuristics can be passed in.  They must all implement
#' predictRoot.  Users will generally not use the output directly.
#' 
#' @param ... A list of predictRoot implementers, e.g. a fitted ttb model.
#' @return An object of class heuristics, which implements createFunction.
#'   Users will generally not use this directly-- allRowPairApply will.
#' 
#' @examples
#' ## This is typical usage:
#' ttb <- ttbModel(city_population, 3, c(4:ncol(city_population)))
#' out <- allRowPairApply(city_population, heuristics(ttb))
#' head(out)
#' ## Under the hood, it calls ttb's predictRoot.
#' 
#' @seealso
#' \code{\link{predictRowPair}} for a simpler way to generate predictions
#'   from fitted models.
#' @seealso
#' \code{\link{predictRoot}} which must be implemented by heuristics in
#'    order to use them with the heuristics() wrapper function.
#' @seealso
#' \code{\link{createFunction}} which is what the returned object implements.
#' @seealso
#' \code{\link{allRowPairApply}} which uses createFunction.
#' @export
heuristics <- function(...) {
  implementers <- list(...)
  # Assume the cols_to_fit are the same for all heuristics.
  cols_to_fit <- implementers[[1]]$cols_to_fit
  # Use the first-level class as the name of the implementer.
  # e.g. Regression has class [regModel, lm], so it will use regModel.
  names <- sapply(implementers, function(x) { head(class(x), 1) })
  structure(list(predictRoot_implementers=implementers,
                 cols_to_fit=cols_to_fit,
                 column_names=names),
            class="heuristics")
}



#' Create function for heuristics prediction with allRowPairApply.
#'
#' Creates a function that takes an index pair and returns a prediction
#' for each of the predictRoot implementers.
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
  all_predictRoot_fn <- function(index_pair) {
    row1 <- oneRow(test_data_trim, index_pair[1])
    row2 <- oneRow(test_data_trim, index_pair[2])
    out_all <- vector()
    y <- 0
    for (implementer in object$predictRoot_implementers) {
      #print(class(implementer))
      out <- predictRoot(implementer, row1, row2)
      y <- y+1
      out_all[[y]] <- out
    }
    return(out_all)
  }
  return(all_predictRoot_fn)
}

#' Wrapper to get a column of correct probabilities with allRowPairApply.
#'
#' Using allRowPairApply, this can generate a column with
#' the correct probability that row 1 > row 2 for each row pair in 
#' the test_data.  It can do this using the criterion column passed in.
#' By default, the output column is called ProbGreater, but you
#' can override the name with output_column_name.
#' 
#' @param criterion_col The integer index of the criterion in test_data.
#' @param output_column_name An optional string
#' @return An object of class heuristics, which implements createFunction.
#'   Users will generally not use this directly-- allRowPairApply will.
#' 
#' @seealso
#' \code{\link{createFunction}} which is what the returned object implements.
#' @seealso
#' \code{\link{allRowPairApply}} which uses createFunction.
#' @export
criterion <- function(criterion_col, output_column_name="ProbGreater") {
  structure(list(criterion_col=criterion_col,
                 column_names=c(output_column_name)),
            class="criterion")
}

createFunction.criterion <- function(object, test_data) {
  criterion_matrix <- as.matrix(test_data[, object$criterion_col, drop=FALSE])
  correct_fn <- function(index_pair)
    rescale0To1(sign(criterion_matrix[index_pair[1], , drop=FALSE]
                     - criterion_matrix[index_pair[2], , drop=FALSE]))
  return(correct_fn)
}

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
    c(column_df[index_pair[1], , drop=FALSE], column_df[index_pair[2], , drop=FALSE])
  return(column_fn)
}


###
# The most general row pair apply function.  All others call this one.

#' Apply functions to all row pairs.
#' 
#' Apply functions like heuristic predictions to all row pairs in a matrix
#' or data.frame.
#' 
#' @param test_data The data to apply the functions to as a matrix or data.frame.
#'    Heuristics must have already been fitted to trying data and must include the
#'    same criterion_col and cols_to_fit.
#' @param ... The objects that generate the functions to apply, using createFunction.
#'    For example, heuristics(ttb), criterion(col), or colPairValues.
#' @return A matrix of outputs from the functions.  The number of rows is based on
#'    the number of row pairs in test_data.  If the input has N rows, the output
#'    will have N x (N-1) rows.  The number of columns will be at least the number
#'    of functions but may be more as some functions may output more than one column.
#
#' @examples
#' ## Fit two models to the city_population data set.
#' ttb <- ttbModel(city_population, 3, c(4:ncol(city_population)))
#' reg <- regModel(city_population, 3, c(4:ncol(city_population)))
#' 
#' ## Generate predictions for all row pairs for these two models:
#' out1 <- allRowPairApply(city_population, heuristics(ttb, reg))
#' head(out1)
#' nrow(out1)
#' ## returns a matrix of 2 columns, named ttbModel and regModel.
#' 
#' ## Generate a matrix with the correct values and the heuristics' predictions:
#' out2 <- allRowPairApply(city_population, criterion(3), heuristics(reg, ttb))
#' head(out2)
#' nrow(out2)
#' ## returns a matrix of 3 columns, ProbGreater, ttbModel and regModel.
#'
#' @seealso
#' \code{\link{createFunction}} which must be implemented by the objects
#'    passed in the "..." argument, along with the attribute $column_names.
#' @seealso
#' \code{\link{predictRoot}} which must be implemented by heuristics in
#'    order to use them with the heuristics() wrapper function.
#'
#' @export
allRowPairApply <- function(test_data, ...) {
  # TODO(jean): Make a version that handles non-numeric as a data.frame.  It will
  #             be slower, but it's a nice option to have for debugging.
  function_creator_list <- list(...)
  column_names <- vector()
  function_list <- vector()
  for (function_creator in function_creator_list) {
    fn <- createFunction(function_creator, test_data)
    function_list <- c(function_list, fn)
    column_names <- c(column_names, function_creator$column_names)
  }
  all_fn <- function(x) {
    out_all <- c()
    y <- 0
    for (fun in function_list) {
      out <- fun(x)
      y <- y+1
      out_all <- c(out_all, out)
    }
    return(out_all)
  }
  raw_matrix <- t(pairMatrix(nrow(test_data), all_fn))
  # R drops dimensions if there's only one, so make consistent dimensions here.
  if (length(column_names) == 1) {
    raw_matrix <- t(raw_matrix)
  }
  colnames(raw_matrix) <- column_names
  return(raw_matrix)
}

###
# Specific row pair apply functions.


# Private helper.
assert_single_row <- function(row) {
  num_rows <- nrow(row)
  if (is.null(num_rows)) {
    stop(paste("Error: Object does not have row dimension.  To get one row of a",
               "matrix, be sure to use drop=FALSE, e.g. my_matrix[row_num, , drop=FALSE]"))
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
    stop(paste("Error: Expected a single column but got", num_cols, "columns."))
  }
}

#' Apply all functions to the row pair.
#'
#' @param row1 The first row of cues (will apply cols_to_fit for you, based on object).
#' @param row2 The second row (will apply cols_to_fit for you, based on object).
#' @param ... The objects that generate the functions to apply, using createFunction.
#'    For example, heuristics(ttb), criterion(col), or colPairValues.
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

#' Predict which of a pair of rows has a higher criterion.
#' Assumes the object implements predictRoot and has $cols_to_fit.
#' Experimental.  I will give it a different name later.
#'
#' @param row1 The first row of cues (will apply cols_to_fit for you, based on object).
#' @param row2 The second row (will apply cols_to_fit for you, based on object).
#' @param object The object that implements predictPair, e.g. a fitted ttbModel
#'   or regModel.
#' @return A double from 0 to 1, representing the probability
#'   that row1's criterion is greater than row2's criterion.
#' @export
predictRowPair <- function(row1, row2, object) {
  out <- rowPairApply(row1, row2, heuristics(object))
  # The asserts below ensure predictRoot had a reasonable implementation.
  assert_single_row(out)
  assert_single_column(out)
  return(unname(out[1,1]))
}
