Heuristica
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
This R package implements heuristic decision models, such as a unit-weighted linear model and Gigerenzer and Goldstein's Take The Best (TTB), which uses just one cue to make its inference. The models are designed for two-alternative choice tasks, such as which of two cities has a greater population given that one has a university and the other does not. It also wraps more well-known models like regression and logistic regression into the two-alternative choice framework for easy comparison. It provides functions to measure accuracy and robustness (out-of-sample accuracy).

Goal
----

[Heuristics](http://en.wikipedia.org/wiki/Heuristic) are simple rules for inference. Statisticians tend to focus on optimal ways to do things, minimizing error or maximizing likelihood. But in some situations, simply summing all the positive evidence will work just as well. The goal of this package is to make it easy to try heuristic models and compare their accuracy and robustness (out-of-sample accuracy) to more common statistical models like linear regression.

The Task
--------

Take the Best was originally proposed for two-alternative choice tasks, e.g. given two cities, Rostock and Munich, which has a larger population? The heuristics had **cues** like whether each city had a university or a soccer team in order to infer the **criterion**, population size.
