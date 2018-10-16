[![Build Status](https://travis-ci.org/guoyin1688/bis557.svg?branch=master)](https://travis-ci.org/guoyin1688/bis557)

BIS557
===

This is a repository for storing all code, documentation, and digital artifacts for BIS557.

In the first homework, I've implemented and documented a function ‘linear_model’. You can use it like this:

```{R}
library(bis557)
fit <- linear_model(Sepal.Length ~., iris)
summary(fit)
```

In the second homework, I've implemented and documented a function ‘ridge_reg’. You can use it like this:

```{R}
library(bis557)
fit <- linear_model(Sepal.Length ~., lambda = 1, iris)
summary(fit)
```
