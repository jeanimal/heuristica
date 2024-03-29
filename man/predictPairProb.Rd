% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/row_pair_apply.R
\name{predictPairProb}
\alias{predictPairProb}
\title{Predict the probability that row1 has a higher criterion than row2.}
\usage{
predictPairProb(row1, row2, object)
}
\arguments{
\item{row1}{The first row of cues (will apply cols_to_fit for you, based on
object).}

\item{row2}{The second row (will apply cols_to_fit for you, based on
object).}

\item{object}{The fitted heuristic, e.g. a fitted ttbModel or logRegModel.
(More technically, it's any object that implements predictProbInternal.)}
}
\value{
A double from 0 to 1, representing the probability that row1's
  criterion is greater than row2's criterion.  0.5 could be a guess or tie.
}
\description{
Given two rows and a fitted heuristic, returns the heuristic's predicted
probability that row1's criterion will be greater than row2's.
}
\examples{
train_matrix <- cbind(y=c(5,4), x1=c(1,0), x2=c(0,1))
lreg <- logRegModel(train_matrix, 1, c(2,3))
predictPairProb(oneRow(train_matrix, 1), oneRow(train_matrix, 2), lreg)

}
\seealso{
\code{\link{rowPairApply}} to get predictions for all row pairs of a
matrix or data.frame.
}
