

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

#' Logistic prediction for use by logistic regression.
#'
#' Applies the weights in col_weights to the columns cols_to_fit in test_data.
#' 
#' @param test_data Data to predict for, as either a matrix or a data.frame.
#' @param cols_to_fit Vector of column indexes to use in test_data.
#' @param col_weights Vector of weights to apply to the columns indicated by cols_to_fit.
#' @param criterion_col Column index specifying the criterion.
#' @return A matrix (rows x1) of predictions, or a list if there was just one
#'    column to fit. 
#'  
#' Special features:
#' Treats a weight of NA as zero (useful for rank-deficient regression fits).
#' If there is a column named "(Intercept)" in col_weights, it is added.
#' In other words, there is no need to add an intercept column to test_matrix.
#' @export
predictWithWeightsLog <- function(test_data, cols_to_fit,criterion_col, col_weights) {
  
  
  test_data <- test_data[order(test_data[,criterion_col],decreasing=T),]
  all.pairs <- t(combn(1:length(test_data[,1]),2))
  all.pairs <-rbind(all.pairs,all.pairs[,c(2,1)])
  predictors <- cbind(test_data[all.pairs[,1],cols_to_fit],test_data[all.pairs[,2],cols_to_fit])
  data2 <- cbind(all.pairs,predictors)
  criterion <- ifelse(data2[,criterion_col] < data2[,criterion_col+1],1,ifelse(data2[,criterion_col] == data2[,criterion_col+1],0.5,0 ))
  test_set <- cbind(criterion,data2[,3:ncol(data2)])
  test_set<- test_set[1:(nrow(test_set)/2),2:ncol(test_set)]
  test_set <- as.data.frame(test_set)
  
  
  intercept <- 0
  col_weights_clean <- col_weights
  col_weights_clean[is.na(col_weights_clean)] <- 0
  if ("(Intercept)" %in% names(col_weights_clean)) {
    intercept <- col_weights_clean["(Intercept)"]
    intercept_index <- which(names(col_weights_clean)=="(Intercept)")
    col_weights_clean <- col_weights_clean[-intercept_index]
  }
  col_weights_clean[is.na(col_weights_clean)]<-0
  if(length(col_weights_clean) == 1){
    fit.predictions.prob <- as.vector(1/(1+exp(-as.matrix(test_set)*as.numeric(col_weights_clean) + intercept) ))
  } else {
    fit.predictions.prob <- as.vector(1/(1+exp(-as.matrix(test_set)%*%as.numeric(col_weights_clean) + intercept) )) 
  }
  pred <- round(fit.predictions.prob,digits=2)
  prediction <- ifelse(pred>0.5,1,ifelse(pred==0.5,0.5,0))
  return(prediction)
}

