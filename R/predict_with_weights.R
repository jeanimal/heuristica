
#' Generates a matrix of pairs of row indices for two-alternative choice.
#'
#' Generates all pairs, including repeating pairs in reverse order, but not a
#' row with itself.  Output is sorted by Row1, then Row2.
#'
#' @param n is the number of rows.
#' @return Returns a data.frame with (n x n-1) rows and 2 columns.  The column
#' names are Row1 and Row2.  Rows are sorted by Row1, then Row2.
#'
#' @examples
#' rowPairGenerator(2)
#'# You should get:
#'#  Row1 Row2
#'#1    1    2
#'#2    2    1
#'
#'rowPairGenerator(3)
#'# You should get:
#'#  Row1 Row2
#'#1    1    2
#'#2    1    3
#'#3    2    1
#'#4    2    3
#'#5    3    1
#'#6    3    2
#'
#' @export
rowPairGenerator <- function(n) {
  allPairs <- expand.grid(Row1=seq(n), Row2=seq(n))
  allPairs <- allPairs[allPairs$Row1!=allPairs$Row2,,drop = FALSE]
  allPairs <- allPairs[order(allPairs$Row1, allPairs$Row2),,drop = FALSE]
  rownames(allPairs) <- NULL
  return(allPairs)
}

#' Linear prediction for use by heuristics.
#'
#' Applies the weights in col_weights to the columns cols_to_fit in test_data.
#' 
#' @param test_data Data to predict for, as either a matrix or a data.frame.
#' @param cols_to_fit Vector of column indexes to use in test_data.
#' @param col_weights Vector of weights to apply to the columns indicated by cols_to_fit.
#'
#' @return A matrix (rows x1) of predictions, or a list if there was just one
#'    column to fit. 
#'  
#' Special features:
#' Treats a weight of NA as zero (useful for rank-deficient regression fits).
#' If there is a column named "(Intercept)" in col_weights, it is added.
#' In other words, there is no need to add an intercept column to test_matrix,
#' which makes the same test_matrix work for other models like ttb, dawes, etc.
#' @export
predictWithWeights <- function(test_data, cols_to_fit, col_weights) {
  test_matrix <- data.matrix(test_data)
  intercept <- 0
  col_weights_clean <- col_weights
  # Set na to zero.
  col_weights_clean[is.na(col_weights_clean)] <- 0
  if ("(Intercept)" %in% names(col_weights_clean)) {
    intercept <- col_weights_clean["(Intercept)"]
    intercept_index <- which(names(col_weights_clean)=="(Intercept)")
    col_weights_clean <- col_weights_clean[-intercept_index]
  }
  if (length(col_weights_clean) == 1) {
    predictions <- test_matrix[,cols_to_fit] * col_weights_clean + intercept
  } else {
    predictions <- test_matrix[,cols_to_fit] %*% col_weights_clean + intercept
  }
  return(predictions)
}

#' Logistic prediction for use by logistic regression.
#'
#' Applies the weights in col_weights to the columns cols_to_fit in test_data.
#' 
#' @param test_data Data to predict for, as either a matrix or a data.frame.
#' @param cols_to_fit Vector of column indexes to use in test_data.
#' @param col_weights Vector of weights to apply to the columns indicated by cols_to_fit.
#' @param criterion_col Column index specifying the criterion.
#' @param row_pairs Optional matrix.  TODO(jean): share documentation.
#' @return A data.frame (rows * (rows-1)) of predictions for each possible paired comparison.
#'   Description of each column:
#'   Row1: The index of row1 of the comparison.
#'   Row2: The index of row2 of the comparison.
#'   Prob_Row1_Bigger: The prediction of the probability that row1 has the bigger criterion.
#'     Specifically, 1 means the first row is bigger, 0 means the 2nd row, 0.5 is a guess. 
#'  
#' Special features:
#' Treats a weight of NA as zero (useful for rank-deficient regression fits).
#' If there is a column named "(Intercept)" in col_weights, it is added.
#' In other words, there 
#' is no need to add an intercept column to test_matrix.
#' @export
predictWithWeightsLog <- function(test_data, cols_to_fit, criterion_col, col_weights,
                                  row_pairs=NULL) {
  if (is.null(row_pairs)) {
    n <- nrow(test_data)
    all_pairs <- rowPairGenerator(n)
  } else {
    all_pairs <- row_pairs
  }
  
  
  transform <- test_data[all_pairs[,1],c(criterion_col,cols_to_fit)] - test_data[all_pairs[,2],c(criterion_col,cols_to_fit)]
  
  predictors <- transform[,2:ncol(transform)]
  predictors[predictors>0] <- 1
  predictors[predictors<0] <- -1
  #if(is.vector(test_d)!=TRUE) test_set <- as.data.frame(test_set)
  
  intercept <- 0
  col_weights_clean <- col_weights
  col_weights_clean[is.na(col_weights_clean)] <- 0
  if ("(Intercept)" %in% names(col_weights_clean)) {
    intercept <- col_weights_clean["(Intercept)"]
    intercept_index <- which(names(col_weights_clean)=="(Intercept)")
    col_weights_clean <- col_weights_clean[-intercept_index]
  }
  if (length(col_weights_clean) == 1) {
    prediction <- predictors * col_weights_clean + intercept
    #prediction <- exp(prediction)/(1+exp(prediction))
    prediction <- plogis(prediction)
    prediction <- round(prediction,digits=2)
    prediction<-ifelse(prediction>0.5,1,ifelse(prediction == 0.5,0.5,0 ))
  } else {
    prediction <- as.matrix(predictors) %*% col_weights_clean + intercept
    #prediction <- exp(prediction)/(1+exp(prediction))
    prediction <- plogis(prediction)
    prediction <- round(prediction,digits=2)
    prediction<-ifelse(prediction>0.5,1,ifelse(prediction == 0.5,0.5,0 ))
  }
  out_df <- data.frame(cbind(all_pairs, prediction))
  names(out_df) <- c("Row1", "Row2", "Prob_Row1_Bigger")
  return(out_df)
}

convertMatrixToPairSigns <- function(test_data, cols_to_fit) {
  directed_matrix <- as.matrix(test_data[,cols_to_fit, drop=FALSE])
  # print(head(directed_matrix))
  # Evaluates pairs of row indexes with third col = 1 is first row is greater, else 0
  pair_evaluator <- function(index_pair) sign(directed_matrix[index_pair[1],]
                                              -directed_matrix[index_pair[2],])
  # Below I correct for R's differing output format based on number of columns.
  if (length(cols_to_fit) > 1) {
    # With transpose.
    pair_signs <- t(combn(nrow(directed_matrix), 2, pair_evaluator))
  } else {
    # Force into matrix and do not transpose.
    pair_signs <- as.matrix(combn(nrow(directed_matrix), 2, pair_evaluator))
  }
  return(pair_signs)
}

predictPairWithWeightsRaw <- function(test_data, cols_to_fit, col_weights,
                                      verbose_output=TRUE) {
  pair_signs <- convertMatrixToPairSigns(test_data, cols_to_fit)
  # print(pair_signs)
  # print("combn finished with this many rows and columns:")
  
  # Intercepts throw off the comparison of signs of pairs, so remove them if they were fit.
  # It would have been better not to fit them.
  col_weights_clean <- col_weights
  if ("(Intercept)" %in% names(col_weights_clean)) {
    intercept <- col_weights_clean["(Intercept)"]
    intercept_index <- which(names(col_weights_clean)=="(Intercept)")
    col_weights_clean <- col_weights_clean[-intercept_index]
  }
  
  predictions_neg_pos <- as.matrix(predictWithWeights(pair_signs,
                                                      c(1:length(cols_to_fit)),
                                                      col_weights_clean))
  # print(head(predictions_neg_pos))
  # Convert predictions to signs, then convert [-1,1] to scale as [0,1].
  predictions_0_1 <- (sign(predictions_neg_pos)+1)*0.5
  if (verbose_output) {
    verbose_predictions <- pairPredictionDF(predictions_0_1)
  } else {
    verbose_predictions <- "Set verbose_output=True to get this output"
  }
  
  return(structure(list(predictions=predictions_0_1,
                        verbose_predictions=verbose_predictions), class="pairPredictor"))
}

predictPairWithWeights <- function(object, test_data, verbose_output=TRUE) {
  return(predictPairWithWeightsRaw(test_data, object$cols_to_fit, coef(object),
                                   verbose_output=verbose_output))
}

