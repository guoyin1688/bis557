[![Build Status](https://travis-ci.org/guoyin1688/bis557.svg?branch=master)](https://travis-ci.org/guoyin1688/bis557)

BIS557
===

This is a repository for storing all code, documentation, and digital artifacts for BIS557.

I've implemented and documented a function that realizes linear_model. You can use it like this:

```{R}
library(bis557)
fit <- linear_model(Sepal.Length ~., iris)
summary(fit)
```
