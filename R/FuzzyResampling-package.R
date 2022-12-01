#' @section
#' The following procedures are available in the library:
#'

#'  + *ClassicalBootstrap* - classical approach based on Efron's method,
#'  + *VAMethod* - resampling method which preserves the value and ambiguity,
#'  + *EWMethod* - resampling method which preserves the expected value and width,
#'  + *VAAMethod* - resampling method which preserves the value, left-hand and right-hand ambiguities,
#'  + *VAFMethod* - resampling method which preserves the value, ambiguity and fuzziness,
#'  + *DMethod* - resampling method which preserves the left end of the cores and increments,
#'  + *WMethod* - resampling method which uses the special *w density* to "smooth" the output fuzzy value,
#'

#'  + *GeneratorNU* - generation of the initial sample using the normal and uniform distributions,
#'  + *GeneratorNExpUU* - generation of the initial sample using the normal, exponential and uniform distributions,
#'  + *GeneratorFuzzyNumbers* - generation of the initial sample using various random distributions,
#'

#'
#'  + *OneSampleCTest* - estimation of the p-value of the one-sample test for the mean,
#'  + *TwoSampleCTest* - estimation of the p-value of the two-sample test for the mean,
#'  + *SEResamplingMean* - estimation of the standard error or the mean-squared error for the mean,
#'

#'  + *BertoluzzaDistance* - calculation of the Bertoluzza et al.'s distance (aka the mid/spread distance),
#'  + *ComparisonOneSampleCTest* - comparison of resampling methods based on percentage of rejections for the one-sample C-test,
#'  + *ComparisonSEMean* - comparison of resampling methods based on the SE/MSE for the mean,
#'  + *ComparePowerOneSampleCTest* - comparison of resampling methods based on percentage of rejections for the one-sample C-test,


#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end




NULL
