# heuristica
This R package implements heuristic decision models, such as a unit-weighted linear model and Gigerenzer and Goldstein's Take The Best (TTB), which uses just one cue to make its inference.  The models are designed for two-alternative choice tasks, such as which of two cities has a greater population given that one has a university and the other does not.  It also wraps more well-known models like regression and logistic regression into the two-alternative choice framework for easy comparison.  It provides functions to measure accuracy and robustness (out-of-sample accuracy).

## Goal

[Heuristics](http://en.wikipedia.org/wiki/Heuristic) are simple rules for inference.  Statisticians tend to focus on optimal ways to do things, minimizing error or maximizing likelihood.  But in some situations, simply summing all the positive evidence will work just as well.  The goal of this package is to make it easy to try heuristic models and compare their accuracy and robustness (out-of-sample accuracy) to more common statistical models like linear regression.

## The Task

Take the Best was originally proposed for two-alternative choice tasks, e.g. given two cities, Rostock and Munich, which has a larger population?  The heuristics had __cues__ like whether each city had a university or a soccer team in order to infer the __criterion__, population size.

The key function heuristics must implement is __predictRoot__ rather than the __predict__ function R users are more famliar with.  This function compares a pair of rows in the data set, indicating which row will be higher on the criterion, e.g. which of two cities has a greater population.  See the vignette [how to make a heuristic](vignettes/how-to-make-heuristic.Rmd) for more details.  

## Key helper functions
* [Cue validity](http://en.wikipedia.org/wiki/Cue_validity): A number from 0 to 1 indicating how often the cue would correctly predict the criterion in a two-alternative choice task.
* pctCorrectOfPredictPair: Given a list of fitted models (which implement predictPair) and test data, returns a row with percent correct for each heuristic. 

## Models
The package comes with the following models that implement predictPair. 
* __ttbModel__: An implementation of [Take The Best](http://en.wikipedia.org/wiki/Take-the-best_heuristic). It sorts cues in order of cue validity, making a decision based on the first cue that discriminates (has differing values on the two objects).
* __dawesModel__: a.k.a. Dawes' Rule, which uses weights of +1 or -1 only.  That is, it is a version of a [unit-weighted linear model](http://en.wikipedia.org/wiki/Unit-weighted_regression).  (Excpetion: A cue with no variance-- every value is the same-- gets a weight of 0.)  This was named after psychologist Robyn Dawes-- see citation below.
* __franklinModel__: A cue-validity-weighted linear model.  This was named after Ben Franklin, who described a weighted decision method similar to this.
* __regModel__: A regression model, a wrapper around R's lm to make it easier to compare with heuristics.  It generates a regression formula for you based on the matrix and column indices you give it.
* __regNoIModel__: Same as RegModel but with no intercept.  Out-of-sample accuracy will usually be higher for a regression _without_ an intercept for tasks where the goal is rank order, as in two-alternative choice tasks.  That's because the intercept has no effect on the ranking, but estimating its value uses up a degree of freedom.
* __logRegModel__: A logistic regression model, a wrapper around R's glm.

You can add your own models by also implementing __predictPair__.

## Data

The package comes with two data sets used in Simple heuristics that make us smart:
* city_population (all binary cues)
* highschool_dropout 

## Citations

Take The Best was first described in:  
Gigerenzer, G. & Goldstein, D. G. (1996). "Reasoning the fast and frugal way: Models of bounded rationality". Psychological Review, 103, 650-669.  

All of these heuristics were run on many data sets and analyzed in:  
Gigerenzer, G., Todd, P. M., & the ABC Group (1999). [Simple heuristics that make us smart.](http://www.amazon.com/Simple-Heuristics-That-Make-Smart/dp/0195143817) New York: Oxford University Press.  

The research was also inspried by:  
Dawes, Robyn M. (1979). "The robust beauty of improper linear models in decision making". American Psychologist, volume 34, pages 571-582. doi:10.1037/0003-066X.34.7.571 [archived pdf](http://www.cmu.edu/dietrich/sds/docs/dawes/the-robust-beauty-of-improper-linear-models-in-decision-making.pdf)

