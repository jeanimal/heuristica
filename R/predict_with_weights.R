


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

rowPairGenerator <- function(n) {
  allPairs <- expand.grid(Row1=seq(n), Row2=seq(n))
  allPairs <- allPairs[allPairs$Row1!=allPairs$Row2,]
  allPairs <- allPairs[order(allPairs$Row1, allPairs$Row2),]
  rownames(allPairs) <- NULL
  return(allPairs)
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
#' In other words, there is no need to add an intercept column to test_matrix.
#' @export
predictWithWeightsLog <- function(test_data, cols_to_fit, criterion_col, col_weights,
                                  row_pairs=NULL) {
  if (is.null(row_pairs)) {
    n <- nrow(test_data)
    all_pairs <- rowPairGenerator(n)
  } else {
    all_pairs <- row_pairs
  }
  predictors <- cbind(test_data[all_pairs[,1],cols_to_fit],test_data[all_pairs[,2],cols_to_fit])
  data2 <- cbind(all_pairs,predictors)
  criterion <- ifelse(data2[,criterion_col] < data2[,criterion_col+1],1,ifelse(data2[,criterion_col] == data2[,criterion_col+1],0.5,0 ))
  test_set <- cbind(criterion,data2[,3:ncol(data2)])
  test_set <- test_set[,2:ncol(test_set)]
  #col_weights<-coef(model)
  if(is.vector(test_set)!=TRUE) test_set <- as.data.frame(test_set)
  
  intercept <- 0
  col_weights_clean <- col_weights
  col_weights_clean[is.na(col_weights_clean)] <- 0
  if ("(Intercept)" %in% names(col_weights_clean)) {
    intercept <- col_weights_clean["(Intercept)"]
    intercept_index <- which(names(col_weights_clean)=="(Intercept)")
    col_weights_clean <- col_weights_clean[-intercept_index]
  }
  col_weights_clean <- col_weights_clean[c(cols_to_fit,(length(col_weights_clean)/2)+cols_to_fit ) -1]
  if (length(col_weights_clean) == 1) {
    prediction <- test_set * col_weights_clean + intercept
    prediction <- exp(prediction)/(1+exp(prediction))
    prediction <- round(prediction,digits=1)
    ids <- sapply(1:nrow(test_set),function(x) all(test_set[x,]==1) ||all(test_set[x,]== 0))
    ids <- which(ids==TRUE)
    prediction[ids,]<-0.5
  } else {
    prediction <- as.matrix(test_set) %*% col_weights_clean + intercept
    prediction <- exp(prediction)/(1+exp(prediction))
    prediction <- round(prediction,digits=1)
    ids <- sapply(1:nrow(test_set),function(x) all(test_set[x,]==1) ||all(test_set[x,]== 0))
    ids <- which(ids==TRUE)
    prediction[ids,]<-0.5
  }
  prediction[prediction!=0.5] <- round(prediction[prediction!=0.5])
  out_df <- data.frame(cbind(all_pairs, prediction))
  names(out_df) <- c("Row1", "Row2", "Prob_Row1_Bigger")
  return(out_df)
}

