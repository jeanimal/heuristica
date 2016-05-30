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

Here is a simplified subset of the city data.



