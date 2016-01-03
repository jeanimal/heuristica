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

createFunction <- function(object, test_data) UseMethod("createFunction")

# A pair function creator for fitted heuristics.
# For use in allRowPairApply.
# @param ... A list of predictRoot implementers, e.g. a fitted ttb model.
# Example:
# ttb <- ttbModel(city_population, 3, c(4:ncol(city_population)))
# reg <- regModel(city_population, 3, c(4:ncol(city_population)))
# function_creator <- heuristics(ttb, reg))
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

# Creates a function that takes an index pair and returns a prediction
# for each of the predictRoot implementers.
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

criterion <- function(criterion_col, output_column_name="ProbGreater") {
  structure(list(criterion_col=criterion_col,
                 column_names=c(output_column_name)),
            class="criterion")
}

createFunction.criterion <- function(object, test_data) {
  criterion_matrix <- as.matrix(test_data[, object$criterion_col, drop=FALSE])
  correct_fn <- function(index_pair)
    sign(criterion_matrix[index_pair[1], , drop=FALSE]
         - criterion_matrix[index_pair[2], , drop=FALSE])
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

# Example:
# ttb <- ttbModel(city_population, 3, c(4:ncol(city_population)))
# reg <- regModel(city_population, 3, c(4:ncol(city_population)))
# allRowPairApply(city_population, 3, c(4:ncol(city_population)), heuristics(ttb, reg))
#   returns 2 columns, named ttbModel and regModel
# allRowPairApply(head(city_population, 4), 3, c(4:ncol(city_population)),
#   heuristics(ttb), criterion(3))
#   returns 2 columns, named ttbModel and ProbGreater
# TODO: Make a version that handles non-numeric, which will be a slower data.frame,
#       but it's a nice option to have.
allRowPairApply <- function(test_data, ...) {
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
  out <- unname(predictRoot(object, row1_clean, row2_clean))
  # The asserts below ensure predictRoot had a reasonable implementation.
  assert_single_row(out)
  assert_single_column(out)
  return(out[1,1])
}
