# heuristica
Heuristic functions implemented in R, such as a unit-weighted linear model and Gigerenzer and Goldstein's Take The Best (TTB).

## Goal

[Heuristics](http://en.wikipedia.org/wiki/Heuristic) are simple ways to solve problems.  Statisticians tend to focus on optimal ways to do things, fitting by minimizing error or maximizing likelihood.  But sometimes simple rules of thumb, a.k.a. heuristics, work just as well.  The goal of this package is to make it easy to try out heuristic methods and compare their accuracy and robustness to more common statistical models, like R's `lm` linear regression model.

## The Task

Take the Best was originally proposed for two-alternative choice tasks, e.g. given two cities, Rostock and Munich, which has a larger population?  The heuristics had __cues__ like whether each city had a soccer team in order to infer the __criterion__, population size.  

## Helper functions
* [Cue validity](http://en.wikipedia.org/wiki/Cue_validity): A number from 0 to 1 indicating how often the cue would correctly predict the criterion in a two-alternative choice task.
* Predict with weights: Multiplies cues by cue weights to produce predictions, plus takes care of corner cases.

## Heuristics
* [Take The Best](http://en.wikipedia.org/wiki/Take-the-best_heuristic): Sorts cues in order of cue validity, making a decision based on the first cue that discriminates (has differing values on the two objects).
* DawesModel: a.k.a. Dawes' Rule, which uses weights of +1 or -1 only.  That is, it is a version of a [unit-weighted linear model](http://en.wikipedia.org/wiki/Unit-weighted_regression).  (Excpetion: A cue with no variance-- every value is the same-- gets a weight of 0.)  This was named after psychologist Robyn Dawes-- see citation below.
* FranklinModel: A cue-validity-weighted linear model.  This was named after Ben Franklin, who described a method like this.
* RegModel: A wrapper around R's lm to make it easier to use for multiple simulations.  It generates a regression formula for you based on the matrix and column indices you give it.

## Citations

Take The Best was first described in:  
Gigerenzer, G. & Goldstein, D. G. (1996). "Reasoning the fast and frugal way: Models of bounded rationality". Psychological Review, 103, 650-669.  

All of these heuristics were run on many data sets and analyzed in:  
Gigerenzer, G., Todd, P. M., & the ABC Group (1999). [Simple heuristics that make us smart.](http://www.amazon.com/Simple-Heuristics-That-Make-Smart/dp/0195143817) New York: Oxford University Press.  

The research was also inspried by:  
Dawes, Robyn M. (1979). "The robust beauty of improper linear models in decision making". American Psychologist, volume 34, pages 571-582. doi:10.1037/0003-066X.34.7.571 [archived pdf](http://www.cmu.edu/dietrich/sds/docs/dawes/the-robust-beauty-of-improper-linear-models-in-decision-making.pdf)

