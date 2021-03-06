
<!-- README.md is generated from README.Rmd. Please edit that file -->

# simpleRank

<!-- badges: start -->
<!-- badges: end -->

simpleRank implements the Mann-Kendall rank correlation coefficient and
respective test, as well as a way to calculate the Sen slope (and test
for its significance). Other existing packages, as for example, the
[trend package by Thorsten
Pohlert](https://CRAN.R-project.org/package=trend) don’t allow for
irregular and/or duplicated ranks while [McLeod’s
implementation](https://CRAN.R-project.org/package=Kendall) further
prohibits one-sided tests for significance.

This is a toy package which was mainly created to get used to writing
packages for R. While it (at some point) will be feature-complete in the
sense that it’s usable, I’m not sure if you’d really want to use it.
Documentation is substandard.

## Installation

Download and install this package from [GitHub](https://github.com/)
with:

``` r
# install.packages("devtools")
devtools::install_github("Florian-Katerndahl/simpleRank")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(simpleRank)
## basic example code
```
