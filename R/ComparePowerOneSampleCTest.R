#' Comparison of the resampling approaches based on the power for the one-sample test for the mean.
#'
#' @description
#' `ComparePowerOneSampleCTest` returns the percentage of rejections for the one-sample C-test when different resampling methods
#' are used.
#'
#' @details
#' The function generates a sequence of initial samples (their number is given in \code{initialSamples},
#'  the size is determined by \code{sampleSize}) for fuzzy numbers of the type specified by \code{generator}.
#'  Then a sequence of deterministic shifts described by vector \code{shiftVector} is added to
#'   each fuzzy observation in these samples.
#'  Next, function \code{OneSampleCTest} is executed to calculate the p-value for each combination of the initial sample and
#'  resampling method. Then, by comparing the p-value with the assumed significance level
#'  \code{significance} we make a decision whether to reject the null hypothesis for the one-sample C-test for the mean
#'  (see Lubiano et al. (2016))  or not.
#'  The output of this procedure is the percentage of rejections as a function of values from \code{shiftVector}.
#'
#' @param generator Name of the generator for sampling initial samples.
#' For the possible names check the values of \code{samplingGenerators} vector.
#'
#' @param mu_0 Triangular or trapezoidal fuzzy number which is used for the null hypothesis of the C-test.
#'
#' @param shiftVector Deterministic vector of shifts that are sequentially added to all initial samples.
#'
#' @param sampleSize Size of the single initial sample.
#'
#' @param numberOfSamples Number of the bootstrapped samples used to estimate the p-value.
#'
#'
#' @param initialSamples Number of the generated initial samples.
#' More than one value can be given in the form of matrix.
#'
#' @param theta The weighting parameter for the mid/spread distance applied in the C-test.
#'
#' @param significance The significance value used to accept/reject the hypothesis for the one-sample C-test.
#'
#' @param ... Parameters which are passed to \code{OneSampleCTest} or the respective \code{generator}
#'
#'
#' @return This function returns a matrix of percentage of rejections for the one-sample C-test for the mean.
#' Rows in this matrix
#' are related to the values from \code{shiftVector}, and the columns - to all resampling methods.
#'
#' @family comparison of resampling methods
#'
#' @seealso \code{\link{ComparisonSEMean}} for the comparison of resampling methods based on SE/MSE for the mean,
#' \code{\link{ComparisonOneSampleCTest}} for the comparison of resampling methods based on power for the one-sample C-test
#' for the mean.
#'
#' @importFrom utils txtProgressBar
#' @importFrom utils setTxtProgressBar
#'
#' @examples
#'
#' \dontrun{
#'
#' # seed PRNG
#'
#' set.seed(1234)
#'
#' # compare the resampling methods for the synthetic data generated using GeneratorNU function
#' # and two values of the shifts
#'
#' ComparePowerOneSampleCTest("GeneratorNU",mu_0 = c(-0.4,-0.1,0.1,0.4), shiftVector = c(0,0.5),
#' mu = 0, sigma = 1, a = 0.2, b = 0.6)}
#'
#'
#'

#'
#' @references
#'
#' Lubiano, M.A., Montenegro M., Sinova, B., de Saa, S.R., Gil, M.A. (2016)
#' Hypothesis testing for means in connection with fuzzy rating scale-based data: algorithms and applications
#' European Journal of Operational Research, 251, pp. 918-929
#'
#' @export
#'



# function to compare powers of the one sample C test

ComparePowerOneSampleCTest <- function(generator, mu_0, shiftVector, sampleSize = 10,
                                       numberOfSamples = 10, initialSamples = 100, theta = 1/3,
                                       significance = 0.05, ...)
{
  # checking shiftVector parameter

  if(!is.vector(shiftVector) | !is.numeric(shiftVector) | any(is.na(shiftVector)) |
     any(is.infinite(shiftVector)))
  {
    stop("Parameter shiftVector should be a numeric, finite vector")
  }

  # prealocate matrix

  outputMatrix <- matrix(NA, nrow = length(shiftVector), ncol = length(resamplingMethods),
                         dimnames = list(shiftVector,resamplingMethods))

  # calculate rejection percents for each possible shift

  for (i in 1:length(shiftVector)) {

    outputMatrix[i,] <- ComparisonOneSampleCTest(generator, mu_0 = mu_0, shift=shiftVector[i], sampleSize, numberOfSamples,
                                                 initialSamples, theta, significance, ...)


  }

  outputMatrix

}
