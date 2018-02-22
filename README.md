# FSH

The goal of FSH is to provide tools to summarize fatal accident data in the National Highway Traffic Safety Administration's Fatality Analysis Reporting System.

## Installation

You can install FSH from github with:


``` r
# install.packages("devtools")
devtools::install_github("gwis999/FSH")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
setwd("extdata")
fars_summarize_years(2013:2014)
```

[![Travis-CI Build Status](https://travis-ci.org/gwis999/FSH.svg?branch=master)](https://travis-ci.org/gwis999/FSH)
