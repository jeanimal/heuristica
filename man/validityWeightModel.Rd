% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/heuristics.R
\name{validityWeightModel}
\alias{validityWeightModel}
\title{Validity Weight Model, a linear model weighted by cue validities}
\usage{
validityWeightModel(
  train_data,
  criterion_col,
  cols_to_fit,
  reverse_cues = TRUE,
  fit_name = "validityWeightModel"
)
}
\arguments{
\item{train_data}{Training/fitting data as a matrix or data.frame.}

\item{criterion_col}{The index of the column in train_data that has the
criterion.}

\item{cols_to_fit}{A vector of column indices in train_data, used to fit
the criterion.}

\item{reverse_cues}{Optional parameter to reverse cues as needed.  By
default, the model will reverse the cue values for cues with cue validity
< 0.5, so a cue with validity 0 becomes a cue with validity 1.
Set this to FALSE if you do not want that, i.e. the cue stays validity 0.}

\item{fit_name}{Optional The name other functions can use to label output.
It defaults to the class name.}
}
\value{
An object of \code{\link[base]{class}} validityWeightModel.  This is a
list containing at least the following components:
  \itemize{
   \item "cue_validities": A list of cue validities for the cues in order of
     cols_to_fit.
   \item "linear_coef": Same as cue validities for this model.
  }
}
\description{
Validity Weight Model is a linear model with weights calculated by
\code{\link{cueValidity}}.
}
\seealso{
\code{\link{cueValidity}} for the metric used to to determine cue direction.

\code{\link{predictPair}} for predicting whether row1 is greater.

\code{\link{predictPairProb}} for predicting the probability row1 is
greater.
}
