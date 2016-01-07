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
