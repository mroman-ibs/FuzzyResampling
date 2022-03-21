#' Calculate p-value of the two-sample test for the mean
#'
#' @description
#' `TwoSampleCTest` returns the p-value of the two-sample test for the mean using the resampling method.
#'
#' @details
#' The input fuzzy values should be triangular or trapezoidal fuzzy numbers, given as a single vector or a whole matrix.
#' In each row, there should be a single fuzzy number in one of the forms:
#' \enumerate{
#'  \item left end of the support, left end of the core, right end of the core, right end of the support, or
#'  \item left increment of the support, left end of the core, right end of the core, right increment of the support.
#' }
#' In this second case, the parameter \code{increases=TRUE} has to be used.
#'
#' The procedure uses the resampling method given in the \code{resamplingMethod} parameter to estimate the p-value of the two-sample
#' test for the mean (denoted further as the two-sample C-test, see Lubiano et al. (2016)).
#' This test checks the null hypothesis that the Aumann-type means of two fuzzy samples are equal.
#'
#'
#' @param initialSample1 The first initial sample which consists of triangular or trapezoidal fuzzy numbers.
#' More than one value can be given in the form of matrix.
#'
#' @param initialSample2 The second initial sample which consists of triangular or trapezoidal fuzzy numbers.
#' More than one value can be given in the form of matrix.
#'
#'
#' @param numberOfSamples Number of the bootstrapped samples used to estimate the p-value.
#'
#' @param resamplingMethod Name of the resampling method, which is used to generate the bootstrapped samples.
#' For the possible names check the values of \code{resamplingMethods} vector.
#'
#'
#' @param theta The weighting parameter for the mid/spread distance applied in the C-test.
#'
#'
#' @param increases If \code{TRUE} is used, then the fuzzy numbers should be given in the form:
#'  left increment of the support, left end of the core, right end of the core,
#' right increment of the support. Otherwise, the default value \code{FALSE} is used and the fuzzy numbers should be given in the form:
#' left end of the support, left end of the core, right end of the core,
#' right end of the support.
#'
#' @return This function returns double value which is equal to the p-value of the two-sample C-test.
#'
#'
#' @examples
#'
#' # prepare some fuzzy numbers (first type of the initial sample)
#'
#' fuzzyValues <- matrix(c(0.25,0.5,1,1.25,0.75,1,1.5,2.2,-1,0,0,2),ncol = 4,byrow = TRUE)
#'
#' # prepare the slightly shifted second sample
#'
#' fuzzyValuesShift <- fuzzyValues + 0.5
#'
#' # seed PRNG
#'
#' set.seed(1234)
#'
#' # calculate the p-value using the classical (i.e. Efron's) bootstrap
#'
#' TwoSampleCTest(fuzzyValues, fuzzyValuesShift)
#'
#' # calculate the p-value using the VA resampling method
#'
#' TwoSampleCTest(fuzzyValues, fuzzyValuesShift, resamplingMethod = VAmethod)
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
#' @family bootstrapped version of test
#'
#' @seealso \code{\link{OneSampleCTest}} for the one-sample C-test


# C bootstrapped test for two means

TwoSampleCTest <- function(initialSample1, initialSample2,
                           numberOfSamples = 100, theta = 1/3, resamplingMethod = classicalBootstrap, increases = FALSE)
{

  # changing possible vector to matrix

  if(is.vector(initialSample1))
  {
    initialSample <- matrix(initialSample1,nrow=1)
  }

  # changing possible vector to matrix

  if(is.vector(initialSample2))
  {
    initialSample <- matrix(initialSample2,nrow=1)
  }

  # check the initial sample

  parameterCheckForInitialSample(initialSample1)

  # check the initial sample

  parameterCheckForInitialSample(initialSample2)

  # checking numberOfSamples parameter

  if(!ifInteger(numberOfSamples) | numberOfSamples <= 1)
  {
    stop("Parameter numberOfSamples should be integer value and > 1")
  }

  # checking theta parameter

  if(!is.double(theta) | theta < 0)
  {
    stop("Parameter theta should be double value and > 0")
  }

  # checking resamplingMethod parameter

  if(!(deparse(substitute(resamplingMethod)) %in% resamplingMethods))
  {
    stop("Parameter resamplingMethod should be a proper name of the resampling method")
  }


  # calculation of C test without bootstrap (step 1)

  n1 <- nrow(initialSample1)

  n2 <- nrow(initialSample2)

  standardStatistics <- valueA(initialSample1, initialSample2, theta) /
    (valueB(initialSample1, theta) / n1 + valueB(initialSample2, theta) / n2)

  # change the initial samples according to H_0

  initialSample1Changed <- sweep(initialSample1,2, meanFuzzyNumber(initialSample2),"+")

  initialSample2Changed <- sweep(initialSample2,2, meanFuzzyNumber(initialSample1),"+")

  # prepare vector

  bootstrappedStatistics <- rep(0,numberOfSamples)

  # pb <- txtProgressBar (1, numberOfSamples, style = 3)

  for (i in 1:numberOfSamples) {

    # generate bootstrap sample (step 3)

    bootstrapSample1 <- resamplingMethod(initialSample1Changed, n1,  increases)

    bootstrapSample2 <- resamplingMethod(initialSample2Changed, n2,  increases)

    # calculate bootstrapped statistics (step 4)

    bootstrappedStatistics[i] <- valueA(bootstrapSample1, bootstrapSample2, theta) /
      (valueB(bootstrapSample1, theta) / n1 + valueB(bootstrapSample2, theta) / n2)

    # setTxtProgressBar(pb, i)

  }

  # cat("\n")

  # calculate p-value

  pvalue <- mean(standardStatistics < bootstrappedStatistics)

  return(pvalue)
}



