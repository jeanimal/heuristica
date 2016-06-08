---
title: "Heuristica"
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file and then run: -->
<!-- library(knitr); knit("README.Rmd") -->



This R package implements [heuristic](http://en.wikipedia.org/wiki/Heuristic) decision models, such as a [unit-weighted linear model](http://en.wikipedia.org/wiki/Unit-weighted_regression) and Gigerenzer and Goldstein's [Take The Best](http://en.wikipedia.org/wiki/Take-the-best_heuristic) (TTB), which uses just one cue to make its inference.  The models are designed for two-alternative choice tasks, such as which of two schools has a higher drop-out rate.  The package also wraps more well-known models like regression and logistic regression into the two-alternative choice framework so all these models can be assessed side-by-side.  It provides functions to measure accuracy, such as overall proportion correct.  These measure can be used in-sample or out-of-sample.

The goal is to make it easy to explore the range of conditions in which simple heuristics are better than more complex models.  Optimizing is not always better!

# The Task

This package is focused on two-alternative choice tasks, e.g. given two schools, which has a higher drop-out rate.  The output is categorical, not quantitative.

# A Simple Example

Here is a subset of data on Chicago public high school drop-out rates.


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

To fit a model, we give it the data set and the columns to use.  The 2nd column, `Dropout_Rate`, is the __criterion__ to be predicted.  The __cues__ are the following columns, indexes 3 and 4, percent of `Low_Income_Students` and percent of `Limited_English_Students`.

Let's fit two models:
* ttbModel, Take The Best, which uses the highest-validity cue that discriminates.
* regModel, a wrapped version of R's "lm" function for a linear regression.


```r
ttb <- ttbModel(schools, 2, c(3:4))
reg <- regModel(schools, 2, c(3:4))
```

What does the fit look like?  We can examine Take The Best's cue validities and the regression coefficients.

```r
ttb$cue_validities
#>      Low_Income_Students Limited_English_Students 
#>                0.6000000                0.5555556
coef(reg)
#>      Low_Income_Students Limited_English_Students 
#>               0.24985315               0.07322294
```
Both Take The Best and regression give a higher weight to `Low_Income_Students` than `Limited_English_Students`, although of course how they use the weights differs.  Take The Best will use a lexicographic order, making its prediction based solely on `Low_Income_Students` as long as the schools have differing values-- which they do for all 5 schools in this data set.  That means it will ignore `Limited_English_Students` when predicting on this data set.  In contrast, regression will use a weighted sum of both cues, but with the most important cues weighted more.  

## Predicting

To see a model's predictions, we give it two rows and the fitted model to the __predictPair__ function.  It outputs 1 when it selects the first row passed to it and -1 when it selects the second row passed to it.  In Bowen vs. Collins, it outputs 1, meaning it predicts Bowen has a higher dropout rate.  In Bowen vs. Fenger, it outputs -1, meaning it predicts Fenger has a higher dropout rate.

```r
predictPair(subset(schools, Name=="Bowen"), subset(schools, Name=="Collins"), ttb)
#> [1] 1
predictPair(subset(schools, Name=="Bowen"), subset(schools, Name=="Fenger"), ttb)
#> [1] -1
```

Note that the output depends on the order of the rows.  In the reversed pair of Collins vs. Bowen, -1 indicate Bowen had the higher drop-out rate.

```r
predictPair(subset(schools, Name=="Collins"), subset(schools, Name=="Bowen"), ttb)
#> [1] -1
```


Now ask the fitted regression model to predict the same school pairs.  Interestingly, it makes the opposite predictions, choosing Collins over Bown and Bowen over Fenger.

```r
predictPair(subset(schools, Name=="Bowen"), subset(schools, Name=="Collins"), reg)
#> [1] -1
predictPair(subset(schools, Name=="Bowen"), subset(schools, Name=="Fenger"), reg)
#> [1] 1
```

Looking at the data set, we see that Take The Best was correct about Bowen vs. Collins because Bowen has the higher `Dropout_Rate`.  Take The Best was also correct about Bowen vs. Fenger.  Regression was wrong for both pairs.

```r
subset(schools, Name %in% c("Bowen", "Collins", "Fenger"))[,c(1:2)]
#>      Name Dropout_Rate
#> 1   Bowen         25.5
#> 2 Collins         11.8
#> 3  Fenger         28.7
```

Using heuristica's __rowPairApply__ function, we can even get a nice summary table of these predictions.

```r
out <- rowPairApply(schools, rowIndexes(), correctGreater(2), heuristics(ttb, reg))
out[c(1,2),]
#>      Row1 Row2 CorrectGreater ttbModel regModel
#> [1,]    1    2              1        1       -1
#> [2,]    1    3             -1       -1        1
# Convert indexes to school names.
out_df <- data.frame(out)
out_df$Row1 <- schools$Name[out_df$Row1]
out_df$Row2 <- schools$Name[out_df$Row2]
out_df[c(1,2),]
#>    Row1    Row2 CorrectGreater ttbModel regModel
#> 1 Bowen Collins              1        1       -1
#> 2 Bowen  Fenger             -1       -1        1
```


## Assessing Overall Performance

For an overall measure of performance, we can measure the percent of correct inferences for all pairs of schools in the data with __percentCorrect__.  The function also lets us readily compare ttb and regression in one function call.

```r
percentCorrect(list(ttb, reg), schools)
#>   ttbModel regModel
#> 1      0.6      0.5
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
* `highschool_dropout`: Drop-out rates for all 63 Chicago public high school plus associated variables like average students per teacher and percent low income students.  The data is
from 1995.  All cues are real-valued but some have N/A values.  (This data set does not exactly match that used in Simple Heuristics That Make Us Smart.)

# Citations

Take The Best was first described in:
Gigerenzer, G. & Goldstein, D. G. (1996). "Reasoning the fast and frugal way: Models of bounded rationality". Psychological Review, 103, 650-669.

All of these heuristics were run on many data sets and analyzed in:
Gigerenzer, G., Todd, P. M., & the ABC Group (1999). [Simple heuristics that make us smart.](http://www.amazon.com/Simple-Heuristics-That-Make-Smart/dp/0195143817) New York: Oxford University Press. 

The research was also inspried by:
Dawes, Robyn M. (1979). "The robust beauty of improper linear models in decision making". American Psychologist, volume 34, pages 571-582. doi:10.1037/0003-066X.34.7.571 [archived pdf](http://www.cmu.edu/dietrich/sds/docs/dawes/the-robust-beauty-of-improper-linear-models-in-decision-making.pdf)


