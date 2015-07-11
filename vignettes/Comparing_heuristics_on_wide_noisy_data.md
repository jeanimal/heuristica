Comparing heuristics on wide, noisy data
========================================================

Different heuristics are better-suited to different data environments.  Here we compare heuristics like Take the Best, unit linear models, and multiple linear regression on wide, noisy data sets, such as those found in genetics.

**Background**

Consider a situation common in genetic data: only a few rows of data but many, many columns of noisy data.  For example, in trying to find which genes cause Alzheimer's Disease, we might have data from just 5 patients-- but we have measured thousands or even millions of genes for each.  

Here I simulate this situation by generating data with few row but many columns.  Furthermore, I make only one cue (column) predictive, giving it a cue validity of 0.75 with the criterion, a score of severity of Alzheimer's symptoms.  Then I add additional cues, but all of them are randomly generated.  As more random cues are added, it gets to be harder and harder for heuristics to pick out the one cue that has a "signal" in it and ignore the noise!

**Replication**


```r
# Use this seed to exactly replicate my tables and graphs below.
set.seed(3)
# Remove it to see a new sampling-- and whether my overall conclusions still
# hold.
```





**Helper functions**

First let's load the heuristica package to get the heuristics we will compare.  (The package is only on github for now.)


```r
# Uncomment and execute if you do not already have devtools.
# install.packages('devtools')
devtools::install_github("jeanimal/heuristica")
```

```
## Downloading github repo jeanimal/heuristica@master
## Installing heuristica
## '/Library/Frameworks/R.framework/Resources/bin/R' --no-site-file  \
##   --no-environ --no-save --no-restore CMD INSTALL  \
##   '/private/var/folders/f3/32kk249x74l9nzzdfm9cd3qh0000gn/T/RtmpF3tNae/devtools3a814ee718ae/jeanimal-heuristica-f576d1e'  \
##   --library='/Users/jeanwhitmore/Library/R/3.1/library' --install-tests
```

```r
library("heuristica")
```


Now here is a function to measure prediction accuracy.

```r
predictAccuracy <- function(test_matrix, criterion_col, cols_to_fit, col_weights) {
    predictions <- predictWithWeights(test_matrix, cols_to_fit, col_weights)
    return(cueValidity(test_matrix[, criterion_col], predictions))
}
```


Now here is a function to generate data.  The first cue has a 75% validity.  Then we tell it to generate `num_random_cues` additional cues.  Then it measures predictive accuracy on this data for the list of models we gave it in `vec_of_models`.

```r
multiPredict75 <- function(vec_of_models, num_rand_columns) {
    num_rows <- 5  # adding 0.75 validity vector below hard-codes 5 rows
    m_train <- matrix(sample(0:1, num_rows * num_rand_columns, replace = TRUE), 
        num_rows, num_rand_columns)
    m_train <- cbind(c(num_rows:1), c(1, 1, 1, 0, 1), m_train)
    m_test <- matrix(sample(0:1, num_rows * num_rand_columns, replace = TRUE), 
        num_rows, num_rand_columns)
    m_test <- cbind(c(num_rows:1), c(1, 1, 1, 0, 1), m_test)
    criterion_col <- 1
    cols_to_fit <- c(2:(num_rand_columns + 2))
    df <- data.frame()
    for (model_constructor in vec_of_models) {
        model <- model_constructor(m_train, criterion_col, cols_to_fit)
        predict_accuracy <- predictAccuracy(m_test, criterion_col, cols_to_fit, 
            coef(model))
        model_name <- class(model)[1]  # Take first class, ignoring super-classes.
        df <- rbind(df, cbind.data.frame(model = model_name, num_rand_col = num_rand_columns, 
            accuracy = predict_accuracy))
    }
    return(df)
}
```


Now we need an easy way to summarize a lot of results.  I got this function from an R cookbook.  It's kind of hacky because it loads the plyr library in the middle, so maybe I will clean it up later.

```r
## Summarizes data.  Gives count, mean, standard deviation, standard error of
## the mean, and confidence interval (default 95%).  data: a data frame.
## measurevar: the name of a column that contains the variable to be
## summariezed groupvars: a vector containing names of columns that contain
## grouping variables na.rm: a boolean that indicates whether to ignore NA's
## conf.interval: the percent range of the confidence interval (default is
## 95%) Source: http://www.cookbook-r.com/Manipulating_data/Summarizing_data/
summarySE <- function(data = NULL, measurevar, groupvars = NULL, na.rm = FALSE, 
    conf.interval = 0.95, .drop = TRUE) {
    library(plyr)
    # New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function(x, na.rm = FALSE) {
        if (na.rm) 
            sum(!is.na(x)) else length(x)
    }
    # This does the summary. For each group's data frame, return a vector with
    # N, mean, and sd
    datac <- ddply(data, groupvars, .drop = .drop, .fun = function(xx, col) {
        c(N = length2(xx[[col]], na.rm = na.rm), mean = mean(xx[[col]], na.rm = na.rm), 
            sd = sd(xx[[col]], na.rm = na.rm))
    }, measurevar)
    # Rename the 'mean' column
    datac <- rename(datac, c(mean = measurevar))
    datac$se <- datac$sd/sqrt(datac$N)  # Calculate standard error of the mean
    # Confidence interval multiplier for standard error Calculate t-statistic
    # for confidence interval: e.g., if conf.interval is .95, use .975
    # (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + 0.5, datac$N - 1)
    datac$ci <- datac$se * ciMult
    return(datac)
}
```


Also load the ggplot2 library so we will be able to plot results.

```r
# If you do not have ggplot2 installed yet, uncomment the line below.
# install.packages('ggplot2')
require("ggplot2")
```

```
## Loading required package: ggplot2
```

```
## Warning: package 'ggplot2' was built under R version 3.1.3
```


**Generating data**

For 0 random cues, the models should be 75% accurate.  Let's confirm that.

```r
multiPredict75(c(dawesModel, franklinModel, ttbModel, regNoIModel, regModel), 
    0)
```

```
##           model num_rand_col accuracy
## 1    dawesModel            0     0.75
## 2 franklinModel            0     0.75
## 3      ttbModel            0     0.75
## 4   regNoIModel            0     0.75
## 5      regModel            0     0.75
```


Now consider adding one random cue.  To be sure we are fair, we should run this several times, say 200 times, and take the average.

```r
model_list <- c(dawesModel, franklinModel, ttbModel, regNoIModel, regModel)
out <- data.frame()
for (i in 1:200) {
    out <- rbind(out, multiPredict75(model_list, 1))
}
out_summary <- summarySE(out, "accuracy", groupvars = c("model", "num_rand_col"))
```

```
## Warning: package 'plyr' was built under R version 3.1.3
```

```r
out_summary[c("model", "accuracy")]
```

```
##           model accuracy
## 1    dawesModel   0.6427
## 2 franklinModel   0.6552
## 3      ttbModel   0.6526
## 4   regNoIModel   0.6573
## 5      regModel   0.6069
```

In general, the models will have falled to about 65% accuracy (from 75%).  The random cue definitely adds a challenge.  dawesModel is typically best, and regModel is typically much worse than the rest.

Below is a hacky but effective way to generate a lot of data for a variety of number of random cues.  You need 2 repetitions for 0 random cues because otherwise the summarize function complains.  Using 200 repetitions takes about a minute on my MacBook Air, while 2000 repetitions takes enough time to grab a coffee.

```r
num_rep <- 200
out <- data.frame()
for (i in 1:2) {
    out <- rbind(out, multiPredict75(model_list, 0))
}
for (i in 1:num_rep) {
    out <- rbind(out, multiPredict75(model_list, 1))
}
for (i in 1:num_rep) {
    out <- rbind(out, multiPredict75(model_list, 2))
}
for (i in 1:num_rep) {
    out <- rbind(out, multiPredict75(model_list, 3))
}
for (i in 1:num_rep) {
    out <- rbind(out, multiPredict75(model_list, 4))
}
for (i in 1:num_rep) {
    out <- rbind(out, multiPredict75(model_list, 5))
}
for (i in 1:num_rep) {
    out <- rbind(out, multiPredict75(model_list, 6))
}
for (i in 1:num_rep) {
    out <- rbind(out, multiPredict75(model_list, 7))
}
for (i in 1:num_rep) {
    out <- rbind(out, multiPredict75(model_list, 8))
}
```


Now for the fun part, the analysis!

```r
out_summary <- summarySE(out, "accuracy", groupvars = c("model", "num_rand_col"))
```


And a plot, which shows the trends of the mean accuracy with a 95% confidence interval around it (although the ci assumes a normal distribution, so take it with a grain of salt).

```r
ggplot(out_summary, aes(x = num_rand_col, y = accuracy, colour = model)) + geom_errorbar(aes(ymin = accuracy - 
    ci, ymax = accuracy + ci), width = 0.1) + geom_line() + geom_point() + xlab("number of random cues")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 


It shows model accuracy decreasing as more random cues are added, which is not a surprise.  In general, dawesModel is still best and regModel is worst.  But note that regNoIModel, which is regression without an intercept, performs pretty well.

Take Best is in the middle of the pack.

More good news is that most heuristics continue to maintain above-chance performance (above 50%) even with 8 additional random cues.  They are champions of picking out signals amidst the noise!

**Discussion**

Why does Dawes' Rule do best?

Why is regression worst?