% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/performance.R
\name{statsFromConfusionMatrix}
\alias{statsFromConfusionMatrix}
\title{Accuracy, sensitivity, specificity, and precision of 2x2 confusion matrix.}
\usage{
statsFromConfusionMatrix(confusion_matrix)
}
\arguments{
\item{confusion_matrix}{A 2x2 confusion matrix.}
}
\value{
A list with accuracy, sensitivity, specificity, and precision
}
\description{
In heuristica, "positive" means the row1 > row2.  Other heuristica create
confusion matrices with the expected layout, but below is documentation of
that layout.  A package like 'caret' offers a more general-purpose
confusion matrix.
}
\details{
This assumes the input matrix is 2x2 and will STOP if not.  It also 
assumes negatives are left and higher, and predictions are the rows,
that is:
true negative  [-1,-1]    false negative [-1,1]
false negative [1, -1]    true positive  [1, 1]

The outputs are defined as:
accuracy = (true positive + true negative) / all
sensitivity = true positive rate = true positive / all positive
  (sensitivity is also called recall)
specificity = true negative rate = true negative / all negative
precision = positive predictive value = true positive
}
