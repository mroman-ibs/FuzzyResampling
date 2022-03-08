
<!-- README.md is generated from README.Rmd. Please edit that file -->

# FuzzyResampling

<!-- badges: start -->

[![R-CMD-check](https://github.com/mroman-ibs/FuzzyResampling/workflows/R-CMD-check/badge.svg)](https://github.com/mroman-ibs/FuzzyResampling/actions)
<!-- badges: end -->

The goal of FuzzyResampling, a library written in R, is to provide
additional resampling procedures, apart from the classical bootstrap
(i.e. Efron’s approach, see (Efron and Tibshirani 1994)), for fuzzy
data. In the classical approach, secondary samples are drawing with
replacement from the initial sample. Therefore most of these bootstrap
samples contain repeated values. Moreover, if the size of the primary
sample is small then all secondary samples consist of only a few
distinct values, which is a serious disadvantage.

To overcome this problem, special resampling algorithms for fuzzy data
were introduced (see (Grzegorzewski, Hryniewicz, and Romaniuk 2019,
2020a, 2020b; Grzegorzewski and Romaniuk 2022; Romaniuk and Hryniewicz
2019)). These methods randomly create values that are “similar” to
values from the initial sample, but not exactly the same. During the
creation process, some of the characteristics of the initial fuzzy
values are kept (e.g., the value, the width, etc.). It was shown that
these algorithms provide serious advantages in some statistical areas
(like standard error estimation or hypothesis testing) if they are
compared with the classical approach. For detailed information
concerning the theoretical foundations and practical applications of
these resampling methods please see the above-mentioned references.

The initial sample (in the form of a vector or a matrix) should consist
of triangular or trapezoidal fuzzy numbers.

Some additional procedures related to these resampling methods are also
provided, like calculation of the Bertoluzza et al.’s distance (aka the
mid/spread distance, see (Bertoluzza, Corral, and Salas 1995)),
estimation of the p-value of the one-sample bootstrapped test for the
mean (see (Lubiano et al. 2016)), and estimation of the standard error
or the mean-squared error for the mean (see (Grzegorzewski and Romaniuk
2021)). Additionally, there are procedures which randomly generate
trapezoidal fuzzy numbers using some well-known statistical
distributions (see (Grzegorzewski, Hryniewicz, and Romaniuk 2020a)).

The following procedures are available in the library:

-   Resampling procedures:

    -   *classicalBootstrap* - classical approach based on Efron’s
        method,
    -   *VAmethod* - resampling method which preserves the value and
        ambiguity (see (Grzegorzewski, Hryniewicz, and Romaniuk 2020a)),
    -   *EWmethod* - resampling method which preserves the expected
        value and width (see (Grzegorzewski, Hryniewicz, and Romaniuk
        2020b)),
    -   *VAAmethod* - resampling method which preserves the value,
        left-hand and right-hand ambiguities (see (Grzegorzewski and
        Romaniuk 2022)),
    -   *VAFmethod* - resampling method which preserves the value,
        ambiguity and fuzziness (see (Grzegorzewski, Hryniewicz, and
        Romaniuk 2020a)),
    -   *dmethod* - resampling method which preserves the left end of
        the cores and increments (see (Romaniuk and Hryniewicz 2019)),
    -   *wmethod* - resampling method which uses the special *w density*
        to “smooth” the output fuzzy value (see (Romaniuk and
        Hryniewicz 2019)).

-   Random generation of the initial samples:

    -   *GeneratorNU* - generation of the initial sample using the
        normal and uniform distributions (see (Grzegorzewski,
        Hryniewicz, and Romaniuk 2020a)),
    -   *GeneratorNExpUU* - generation of the initial sample using the
        normal, exponential and uniform distributions (see
        (Grzegorzewski, Hryniewicz, and Romaniuk 2020a)).

-   Applications of the bootstrapped samples:

    -   *OneSampleCTest* - estimation of the p-value of the one-sample
        test for the mean (see (Lubiano et al. 2016)),
    -   *TwoSampleCTest* - estimation of the p-value of the two-sample
        test for the mean (see (Lubiano et al. 2016)),
    -   *SEResamplingMean* - estimation of the standard error or the
        mean-squared error for the mean (see (Grzegorzewski and
        Romaniuk 2021)).

-   Additional procedures:

    -   *BertoluzzaDistance* - calculation of the Bertoluzza et al.’s
        distance (aka the mid/spread distance, see (Bertoluzza, Corral,
        and Salas 1995)).

## Installation

You can install the latest development version of FuzzyResampling with:

``` r
library(devtools)
install_github("mroman-ibs/FuzzyResampling")
```

You can install the latest stable version from CRAN with:

``` r
install.packages("FuzzyResampling")
```

## Examples

``` r
# set seed

set.seed(12345)

# load library

library(FuzzyResampling)

# prepare some fuzzy numbers

fuzzyValues <- matrix(c(0.25,0.5,1,1.25,0.75,1,1.5,2.2,-1,0,0,2),ncol = 4,byrow = TRUE)

fuzzyValues
#>       [,1] [,2] [,3] [,4]
#> [1,]  0.25  0.5  1.0 1.25
#> [2,]  0.75  1.0  1.5 2.20
#> [3,] -1.00  0.0  0.0 2.00

# seed PRNG

set.seed(12345)

# generate the secondary sample using the classical approach

classicalBootstrap(fuzzyValues)
#>       [,1] [,2] [,3] [,4]
#> [1,]  0.75    1  1.5  2.2
#> [2,] -1.00    0  0.0  2.0
#> [3,]  0.75    1  1.5  2.2


# generate the secondary sample using the VA method

VAmethod(fuzzyValues)
#>            [,1]       [,2]       [,3]     [,4]
#> [1,]  0.9141124  0.9179438  1.7262290 1.747542
#> [2,] -0.5303703  0.8901852  0.9132088 1.423582
#> [3,] -0.3356065 -0.3321967 -0.3321967 2.664393


# generate the secondary sample (6 fuzzy numbers) using the d-method

dmethod(fuzzyValues, b = 6)
#>       [,1] [,2] [,3] [,4]
#> [1,]  0.75  1.0  1.5 3.50
#> [2,]  0.00  1.0  1.5 1.75
#> [3,]  0.25  0.5  1.0 1.25
#> [4,]  0.75  1.0  1.0 3.00
#> [5,] -0.25  0.0  0.5 1.20
#> [6,] -0.50  0.5  1.0 1.25


# calculate the mid/spread distance between the first value
# (from the first row) and the second one (from the second row)

BertoluzzaDistance(fuzzyValues[1,],fuzzyValues[2,])
#> [1] 0.6204837


# seed PRNG

set.seed(1234)

# calculate the p-value using the classical (i.e. Efron's) bootstrap
# for the one-sample test for the mean

OneSampleCTest(fuzzyValues, mu_0 = c(0,0.5,1,1.5))
#> [1] 0.82

# calculate the p-value using the VA resampling method

OneSampleCTest(fuzzyValues, mu_0 = c(0,0.5,1,1.5),resamplingMethod = VAmethod)
#> [1] 0.91

# seed PRNG

set.seed(1234)

# calculate the p-value using the classical (i.e. Efron's) bootstrap
# for the two-sample test for the mean

TwoSampleCTest(fuzzyValues, fuzzyValues+0.1)
#> [1] 0.8

# calculate the p-value using the VA resampling method

TwoSampleCTest(fuzzyValues, fuzzyValues+0.1,resamplingMethod = VAmethod)
#> [1] 1



# seed PRNG

set.seed(1234)

# calculate the SE of the mean using the classical (i.e. Efron's) bootstrap

SEResamplingMean(fuzzyValues)
#> $mean
#> [1] 0.0075000 0.5100000 0.8416667 1.8413333
#> 
#> $SE
#> [1] 0.05487521

# calculate the SE of the mean using the VA resampling method

SEResamplingMean(fuzzyValues, resamplingMethod = VAmethod)
#> $mean
#> [1] -0.2846996  0.5985998  0.8490542  1.7328917
#> 
#> $SE
#> [1] 0.05504196

# calculate the MSE of the given mean using the classical (i.e. Efron's) bootstrap

SEResamplingMean(fuzzyValues, trueMean = c(0,0.5,1,2))
#> $mean
#> [1] 0.0 0.5 1.0 2.0
#> 
#> $SE
#> [1] 0.02721175

# calculate the MSE of the given mean using the VA resampling method

SEResamplingMean(fuzzyValues, resamplingMethod = VAmethod, trueMean = c(0,0.5,1,2))
#> $mean
#> [1] 0.0 0.5 1.0 2.0
#> 
#> $SE
#> [1] 0.03119963

# seed PRNG

set.seed(1234)

# generate 10 trapezoidal fuzzy numbers using the normal and uniform distributions

GeneratorNU(10, 0,1,1,2)
#>             [,1]        [,2]        [,3]       [,4]
#>  [1,] -2.6303454 -1.52367820 -0.75097427 -0.6034145
#>  [2,] -1.3180763 -0.02526413  0.54261591  1.1619891
#>  [3,]  0.3017466  0.92539517  1.38911338  2.8236569
#>  [4,] -3.6293320 -2.38569362 -1.83839083 -0.8292990
#>  [5,] -0.4492152  0.21032515  0.61022090  0.9162188
#>  [6,] -1.3085376 -0.30454266  1.26572653  2.2735935
#>  [7,] -2.4546266 -1.10043751 -0.37349192  0.6144299
#>  [8,] -2.4312725 -1.46129002 -0.28782204  1.2145784
#>  [9,] -1.8836547 -1.39579705  0.42769842  0.7769981
#> [10,] -2.4667277 -0.93580809 -0.08268549  1.6140993
```

``` r
# help concerning the VA method

?VAmethod
```

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-Bertoluzza1995" class="csl-entry">

Bertoluzza, Carlo, Norberto Corral, and Antonia Salas. 1995. “On a New
Class of Distances Between Fuzzy Numbers.” *Mathware and Soft Computing*
2 (2): 71–84. <https://eudml.org/doc/39054>.

</div>

<div id="ref-Efron1994" class="csl-entry">

Efron, B., and R. J. Tibshirani. 1994. *An Introduction to the
Bootstrap*. Chapman; Hall/CRC.

</div>

<div id="ref-grzegorzewski2019" class="csl-entry">

Grzegorzewski, P., O. Hryniewicz, and M. Romaniuk. 2019. “Flexible
Bootstrap Based on the Canonical Representation of Fuzzy Numbers.” In
*Proceedings of EUSFLAT 2019*. Atlantis Press.
<https://doi.org/10.2991/eusflat-19.2019.68>.

</div>

<div id="ref-grzegorzewskietal2020" class="csl-entry">

———. 2020a. “Flexible Bootstrap for Fuzzy Data Based on the Canonical
Representation.” *International Journal of Computational Intelligence
Systems* 13: 1650–62. <https://doi.org/10.2991/ijcis.d.201012.003>.

</div>

<div id="ref-grzegorzewskiamcs2020" class="csl-entry">

———. 2020b. “Flexible Resampling for Fuzzy Data.” *International Journal
of Applied Mathematics and Computer Science* 30: 281–97.
<https://doi.org/10.34768/amcs-2020-0022>.

</div>

<div id="ref-grzegorzewski2021" class="csl-entry">

Grzegorzewski, P., and M. Romaniuk. 2021. “Epistemic Bootstrap for Fuzzy
Data.” In *Joint Proceedings of IFSA-EUSFLAT-AGOP 2021 Conferences*,
538–45. Atlantis Press.

</div>

<div id="ref-GrzegorzewskiRom2021" class="csl-entry">

———. 2022. “Bootstrap Methods for Fuzzy Data.” In *Uncertainty and
Imprecision in Decision Making and Decision Support: New Advances,
Challenges, and Perspectives*, edited by Krassimir T. Atanassov, Vassia
Atanassova, Janusz Kacprzyk, Andrzej Kałuszko, Maciej Krawczak, Jan W.
Owsiński, Sotir S. Sotirov, Evdokia Sotirova, Eulalia Szmidt, and
Sławomir Zadrożny, 28–47. Cham: Springer International Publishing.

</div>

<div id="ref-lubiano2016" class="csl-entry">

Lubiano, M. A., M. Montenegro, B. Sinova, S. De la Rosa de Sáa, and M.
A. Gil. 2016. “Hypothesis Testing for Means in Connection with Fuzzy
Rating Scale-Based Data: Algorithms and Applications.” *European Journal
of Operational Research* 251: 918–29.
<https://doi.org/10.1016/j.ejor.2015.11.016>.

</div>

<div id="ref-romaniuk_hryniewicz" class="csl-entry">

Romaniuk, M., and O. Hryniewicz. 2019. “Interval-Based, Nonparametric
Approach for Resampling of Fuzzy Numbers.” *Soft Computing* 23:
5883–903. <https://doi.org/10.1007/s00500-018-3251-5>.

</div>

</div>
