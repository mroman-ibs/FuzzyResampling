
<!-- README.md is generated from README.Rmd. Please edit that file -->

# FuzzyResampling

<!-- badges: start -->
<!-- badges: end -->

The goal of FuzzyResampling is to provide additional resampling
procedures, apart from the classical bootstrap (i.e. Efron’s approach),
for fuzzy data. These resampling algorithms randomly create values which
are “similar” (but not the same) to values from the initial sample. The
initial sample should consist of triangular or trapezoidal fuzzy
numbers.

The following procedures are available:

-   *classicalBootstrap*
-   *VAmethod*
-   *EWmethod*
-   *VAAmethod*
-   *VAFmethod*
-   *dmethod*
-   *wmethod*

## Installation

You can install the released version of FuzzyResampling with:

``` r
library(devtools)
install_github("mroman_ibs/FuzzyResampling")
```

## Example

``` r
library(FuzzyResampling)

# prepare some fuzzy numbers 

fuzzyValues <- matrix(c(0.25,0.5,1,1.25,0.75,1,1.5,2.2,-1,0,0,2),ncol = 4,byrow = TRUE)

# generate the secondary sample using the VA method

VAmethod(fuzzyValues)
```
