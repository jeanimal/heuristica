---
title: "Heuristica"
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file and then run: -->
<!-- library(knitr); knit("README.Rmd") -->



This R package implements [Heuristic](http://en.wikipedia.org/wiki/Heuristic) decision models, such as a unit-weighted linear model and Gigerenzer and Goldstein's Take The Best (TTB), which uses just one cue to make its inference.  The models are designed for two-alternative choice tasks, such as which of two cities has a greater population given that one has a university and the other does not.  It also wraps more well-known models like regression and logistic regression into the two-alternative choice framework for easy comparison.  It provides functions to measure accuracy and robustness (out-of-sample accuracy).

The goal is to make it easy to explore the wide range of conditions in which simple heuristics can be more robust than more complex models.  Optimizing is not always better!

# The Task

Take the Best was designed for two-alternative choice tasks, e.g. given two cities, Rostock and Munich, which has a larger population?  The heuristics had __cues__ like whether each city had a university or a soccer team in order to infer the __criterion__, population size.

# A Simple Example

Here is a subset of data on Chicago public high school drop-out rates from the 1995.

```r
schools <- data.frame(Name=c("Bowen", "Collins", "Fenger", "Juarez", "Young"), Dropout_Rate=c(25.5, 11.8, 28.7, 21.6, 4.5), Low_Income_Students=c(82.5, 88.8, 63.2, 84.5, 30.3), Limited_English_Students=c(11.4, 0.1, 0, 28.3, 0.1))
schools
#>      Name Dropout_Rate Low_Income_Students Limited_English_Students
#> 1   Bowen         25.5                82.5                     11.4
#> 2 Collins         11.8                88.8                      0.1
#> 3  Fenger         28.7                63.2                      0.0
#> 4  Juarez         21.6                84.5                     28.3
#> 5   Young          4.5                30.3                      0.1
```

Next we fit Take The Best-- ttbModel-- and a wrapped version of R's "lm" function for a linear regression-- regModel.  The 2nd column, `Dropout_Rate`, is the criterion to be predicted.  The following columns, indexes 3 and 4, which are percent of `Low_Income_Students` and percent of `Limited_English_Students`, are the predictors.


```r
ttb <- ttbModel(schools, 2, c(3:4))
reg <- regModel(schools, 2, c(3:4))
```

To see TTB's predictions, we give two rows and the fitted model to the __predictPair__ function.  It outputs 1 when it selects the first row passed to it and -1 when it selects the second row passed to it.  (We use the package's __oneRow__ helper function to select a row.)  Below we see that between the first and 2nd row, ttb predicts the first row, outputting 1.  Between the first and third row, it outputs -1, predicting the 2nd row passed to it (row 3) is greater.

```r
predictPair(oneRow(schools, 1), oneRow(schools, 2), ttb)
#> [1] 1
redictPair(oneRow(schools, 1), oneRow(schools, 3), ttb)
#> Error in eval(expr, envir, enclos): could not find function "redictPair"
```

To assess how well ttb fit the data overall, we can measure the percent of correct inferences for all pairs of schools in the data with __pctCorrectOfPredictPair__.  Let us also compare it to regression.

```r
pctCorrectOfPredictPair(list(ttb, reg), schools)
#>   ttbModel regModel
#> 1      0.6      0.5
```

Take The Best got 60% correct and regression got 50% correct, which is the same as chance.

# Installation

Uncomment and execute the line below to get the CRAN version:


```r
# install.packages("heuristica") 
```

Uncomment and execute the line below to get the development version.


```r
# Uncomment and execute the line below if you do not have devtools.
# install.packages("devtools") 
# devtools::install_github("jeanimal/heuristica")
# library("heuristica")
```

# Models

The package comes with the following models that implement predictPair.
* __ttbModel__: An implementation of [Take The Best](http://en.wikipedia.org/wiki/Take-the-best_heuristic). It sorts cues in order of cue validity, making a decision based on the first cue that discriminates (has differing values on the two objects).
* __ttbGreedyModel__: Take the Best using conditional cue validity (rather than cue validity).
* __unitWeightModel__: A [unit-weighted linear model](http://en.wikipedia.org/wiki/Unit-weighted_regression) that uses weights of +1 or -1 only.  An Exception is that a cue with no variance-- every value is the same-- gets a weight of 0.  Inspired by psychologist Robyn Dawes-- see citation below.
* __validityWeightModel__: A cue-validity-weighted linear model.  (In some publications, this was called franklinModel after Ben Franklin.)
* __regModel__: A regression model, a wrapper around R's lm to make it easier to compare with heuristics.  It generates a regression formula for you based on the matrix and column indices you give it.  It generates a prediction for each item in the pair-- e.g. estimates the population of Rostock and the population of Munich-- and then picks the item (city) with the higher estimate.
* __logRegModel__: A logistic regression model, a wrapper around R's glm.  This estimates the probability that Rostock is greater than Munich and chooses Rostock if the probability is greater than 50/50.

You can add your own models by also implementing a function related to __predictPair__, as described in a vignette.

# Data

The package comes with two data sets used in the book, Simple Heuristics That Make Us Smart.
* city_population (all binary cues)
* highschool_dropout

# Citations

Take The Best was first described in:
Gigerenzer, G. & Goldstein, D. G. (1996). "Reasoning the fast and frugal way: Models of bounded rationality". Psychological Review, 103, 650-669.

All of these heuristics were run on many data sets and analyzed in:
Gigerenzer, G., Todd, P. M., & the ABC Group (1999). [Simple heuristics that make us smart.](http://www.amazon.com/Simple-Heuristics-That-Make-Smart/dp/0195143817) New York: Oxford University Press. 

The research was also inspried by:
Dawes, Robyn M. (1979). "The robust beauty of improper linear models in decision making". American Psychologist, volume 34, pages 571-582. doi:10.1037/0003-066X.34.7.571 [archived pdf](http://www.cmu.edu/dietrich/sds/docs/dawes/the-robust-beauty-of-improper-linear-models-in-decision-making.pdf)


