---
title: "Heuristica"
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file and then run: -->
<!-- library(knitr); knit("README.Rmd") -->



The `heuristica` R package implements [heuristic](http://en.wikipedia.org/wiki/Heuristic) decision models, such as [Take The Best](http://en.wikipedia.org/wiki/Take-the-best_heuristic) (TTB) and a [unit-weighted linear model](http://en.wikipedia.org/wiki/Unit-weighted_regression).  The models are designed for two-alternative choice tasks, such as which of two schools has a higher drop-out rate.  The package also wraps more well-known models like regression and logistic regression into the two-alternative choice framework so all these models can be assessed side-by-side.  It provides functions to measure accuracy, such as an overall `percentCorrect` and, for advanced users, some [confusion matrix](https://en.wikipedia.org/wiki/Confusion_matrix) functions.  These measures can be applied in-sample or out-of-sample.

The goal is to make it easy to explore the range of conditions in which simple heuristics are better than more complex models.  Optimizing is not always better!

# The Task

This package is focused on two-alternative choice tasks, e.g. given two schools, which has a higher drop-out rate.  The output is categorical, not quantitative.

# A Simple Example

Here is a subset of data on Chicago public high school drop-out rates.  The criterion to predict is the Dropout_Rate, which is in column 2.


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

## Fitting

To fit a model, we give it the data set and the columns to use.  In this case, the 2nd column, `Dropout_Rate`, is the __criterion__ to be predicted.  The __cues__ are the following columns, percent of `Low_Income_Students` and percent of `Limited_English_Students`.  They are at indexes 3 and 4.

Let's fit two models:
* ttbModel, Take The Best, which uses the highest-validity cue that discriminates (more details below).
* regModel, a version of R's "lm" function for linear regression wrapped to fit into heurstica's interface.


```r
criterion_col <- 2
ttb <- ttbModel(schools, criterion_col, c(3:4))
reg <- regModel(schools, criterion_col, c(3:4))
```

What do the fits look like?  We can examine Take The Best's cue validities and the regression coefficients.

```r
ttb$cue_validities
#>      Low_Income_Students Limited_English_Students 
#>                0.6000000                0.5555556
coef(reg)
#>      Low_Income_Students Limited_English_Students 
#>               0.24985315               0.07322294
```
Both Take The Best and regression give a higher weight to `Low_Income_Students` than `Limited_English_Students`, although of course how they use the weights differs.  Take The Best will use a lexicographic order, making its prediction based solely on `Low_Income_Students` as long as the schools have differing values-- which they do for all 5 schools in this data set.  That means it will ignore `Limited_English_Students` when predicting on this data set.  In contrast, regression will use a weighted sum of both cues, but with the most important cues weighted more.  

## Predicting the fitted data

To see a model's predictions, we use the `predictPair` function.  It takes two rows of data-- which together comprise a "row pair"-- and the fitted model.  `predictPair` outputs three possible values:

* 1 predicts the first row passed to it
* -1 predicts the second row passed to it.
* 0 is a guess.

In Bowen vs. Collins, it outputs 1, meaning it predicts Bowen has a higher dropout rate.  In Bowen vs. Fenger, it outputs -1, meaning it predicts Fenger has a higher dropout rate.

```r
predictPair(subset(schools, Name=="Bowen"), subset(schools, Name=="Collins"), ttb)
#> [1] 1
predictPair(subset(schools, Name=="Bowen"), subset(schools, Name=="Fenger"), ttb)
#> [1] -1
```

Note that the output depends on the order of the rows.  In the reversed pair of Collins vs. Bowen, the output is -1.  This is consistent because it still picks Bowen, regardless of order.

```r
predictPair(subset(schools, Name=="Collins"), subset(schools, Name=="Bowen"), ttb)
#> [1] -1
```


## All rows

It is tedious to predict one row pair at a time, so let's use heurstica's `predictPairSummary` function instead.  We simply pass it the data and the heuristics whose predictions we are interested in.  It produces a matrix with all row pairs, which in this case is 10 (5 * 4 / 2).


```r
out <- predictPairSummary(schools, ttb, reg)
# See the first row: It has row indexes.
out[1,]
#>           Row1           Row2 CorrectGreater       ttbModel       regModel 
#>              1              2              1              1             -1
# Convert indexes to school names for easier interpretation
out_df <- data.frame(out)
out_df$Row1 <- schools$Name[out_df$Row1]
out_df$Row2 <- schools$Name[out_df$Row2]
out_df
#>       Row1    Row2 CorrectGreater ttbModel regModel
#> 1    Bowen Collins              1        1       -1
#> 2    Bowen  Fenger             -1       -1        1
#> 3    Bowen  Juarez              1        1       -1
#> 4    Bowen   Young              1       -1        1
#> 5  Collins  Fenger             -1       -1        1
#> 6  Collins  Juarez             -1       -1       -1
#> 7  Collins   Young              1       -1        1
#> 8   Fenger  Juarez              1        1       -1
#> 9   Fenger   Young              1       -1        1
#> 10  Juarez   Young              1       -1        1
```

The first row shows the Bowen vs. Collins example we considered above.  Because CorrectGreater is 1, that means TTB predicted it correctly-- Bowen really does have a higher drop-out rate.  But regression predicted -1 for this row pair, which is incorrect.

predictPairSummary is for beginners.  heuristica offers full flexibility in output with the `rowPairApply` function.  After passing it the data, you can pass it any number of generators to make the columns you want.  Some examples are below, where we print only the first row.


```r
# Same as predictPairSummary.
out_same <- rowPairApply(schools, rowIndexes(), correctGreater(criterion_col), heuristics(ttb, reg))
out_same[1,]
#>           Row1           Row2 CorrectGreater       ttbModel       regModel 
#>              1              2              1              1             -1

# Show first the heuristic predictions, then CorrectGreater.  No row indexes.
out_simple <- rowPairApply(schools, heuristics(ttb, reg), correctGreater(criterion_col))
out_simple[1,]
#>       ttbModel       regModel CorrectGreater 
#>              1             -1              1
```

## Assessing Overall Performance

For an overall measure of performance, we can measure the percent of correct inferences for all pairs of schools in the data with `percentCorrect`, namely the number of correct predictions divided by the total number of predictions.  We give the function the data to be predicted (in this case the same as what was fit) and the fitted models to assess.

```r
percentCorrect(schools, ttb, reg)
#>   ttbModel regModel
#> 1       60       50
```

Take The Best got 60% correct and regression got 50% correct, which is the same as chance.

Regression is the best linear unbiased model for the data.  But this data had a very small sample size of just 5 schools, and good estimates require more data.

This is an unusual case where TTB actually beat regression in a fitting task.  Usually ttb only wins in out-of-sample performance, e.g. fitting 5 schools and then predicting on other schools not used in the fit.

For a more realistic example, see the vignette with cross-validated out-of-sample performance on a complete data set.

# Package Installation

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

The package comes with the following models that you can call with predictPair.

* `logRegModel`: A logistic regression model, a wrapper around R's glm.  This estimates the probability that one school's drop-out rate is greater than the other and chooses the school with probability greater than 50%.
* `minModel`: It searches cues in a random order, making a decision based on the first cue that discriminates (has differing values on the two items / schools).
* `regModel`: A regression model, a wrapper around R's lm to make it easier to compare with heuristics.  It fits a regression based on the column indices.  For predictPair, it predicts the criterion for each item in the pair-- e.g. estimates the drop-out rate of each school-- and then predicts the item with the higher estimate-- higher drop-out rate.  (A variant that fits with an intercept, `regInterceptModel`, is provided in order to confirm prior research results, but it is not recommended for future research.)
* `singleCueModel`: In the fitting stage, this selects the cue with the higest cue validity.  It only uses that cue, and if the cue does not discriminate, it guesses.
* `ttbModel`: An implementation of [Take The Best](http://en.wikipedia.org/wiki/Take-the-best_heuristic). In the fitting stage, it sorts cues in order of cue validity.  When predicting between two items, it finds the highest-validity that discriminates (has differing values on the two items) and bases its prediction on that cue, ignoring other cues.  The cue used can vary based on the cue values of the two items.
* `ttbGreedyModel`: Take the Best using conditional cue validity (rather than cue validity).
* `unitWeightModel`: A [unit-weighted linear model](http://en.wikipedia.org/wiki/Unit-weighted_regression) that uses weights of +1 or -1 only.  An exception is that a cue with no variance-- every value is the same-- gets a weight of 0.  Inspired by psychologist Robyn Dawes-- see citation below.
* `validityWeightModel`: A cue-validity-weighted linear model.  (In some publications, this was called franklinModel after Ben Franklin.)

You can add your own models by also implementing a function related to `predictPair`, as described in a vignette.

# Data

The package comes with two data sets used by many heuristic researchers.

* `city_population`:  The 83 German cities with more than 100,000 inhabitants in 1993.  All cues are binary.  (There is another version called `city_population_original` that has some transciption errors from the almanac source but exactly matches the data set in Simple Heuristics That Make Us Smart.)
* `highschool_dropout`: Drop-out rates for all 63 Chicago public high schools plus associated variables like average students per teacher and percent low income students.  The data is
from 1995.  All cues are real-valued but some have N/A values.  (This data set does not exactly match that used in Simple Heuristics That Make Us Smart.)

# Citations

Take The Best was first described in:
Gigerenzer, G. & Goldstein, D. G. (1996). "Reasoning the fast and frugal way: Models of bounded rationality". Psychological Review, 103, 650-669.

All of these heuristics were run on many data sets and analyzed in:
Gigerenzer, G., Todd, P. M., & the ABC Group (1999). [Simple heuristics that make us smart.](http://www.amazon.com/Simple-Heuristics-That-Make-Smart/dp/0195143817) New York: Oxford University Press. 

The research was also inspired by:
Dawes, Robyn M. (1979). "The robust beauty of improper linear models in decision making". American Psychologist, volume 34, pages 571-582. [archived pdf](http://www.cmu.edu/dietrich/sds/docs/dawes/the-robust-beauty-of-improper-linear-models-in-decision-making.pdf)

# Acknowledgements

Thanks for coding advice and beta testing go to Marcus Buckmann, Daniel G. Goldstein, and Özgür Simsek.
