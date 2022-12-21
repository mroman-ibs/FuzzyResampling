#' Calculate p-value of the one-sample test for the mean
#'
#' @description
#' `OneSampleCTest` returns the p-value of the one-sample test for the mean using the resampling method.
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
#' The procedure uses the resampling method given in the \code{resamplingMethod} parameter to estimate the p-value of the one-sample
#' test for the mean (denoted further as the one-sample C-test, see Lubiano et al. (2016)).
#' This test checks the null hypothesis that the Aumann-type mean of the fuzzy numbers is equal to a given fuzzy number \code{mu_0}.
#'
#'
#' @param initialSample The initial sample which consists of triangular or trapezoidal fuzzy numbers.
#' More than one value can be given in the form of matrix.
#'
#' @param mu_0 Triangular or trapezoidal fuzzy number which is used for the null hypothesis of the C-test.
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
#' @return This function returns double value which is equal to the p-value of the one-sample C-test.
#'
#' @family bootstrapped version of test
#'
#' @seealso \code{\link{TwoSampleCTest}} for the two-sample C-test

#' @examples
#'
#' # prepare some fuzzy numbers (first type of the initial sample)
#'
#' fuzzyValues <- matrix(c(0.25,0.5,1,1.25,0.75,1,1.5,2.2,-1,0,0,2),ncol = 4,byrow = TRUE)
#'
#' # seed PRNG
#'
#' set.seed(1234)
#'
#' # calculate the p-value using the classical (i.e. Efron's) bootstrap
#'
#' OneSampleCTest(fuzzyValues, mu_0 = c(0,0.5,1,1.5))
#'
#' # calculate the p-value using the VA resampling method
#'
#' OneSampleCTest(fuzzyValues, mu_0 = c(0,0.5,1,1.5),resamplingMethod = "VAMethod")
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




# C bootstrapped test for one mean

OneSampleCTest <- function(initialSample, mu_0,
                           numberOfSamples = 100, theta = 1/3, resamplingMethod = "ClassicalBootstrap", increases = FALSE)
{

  # print(as.list(match.call()))

  # changing possible vector to matrix

  if(is.vector(initialSample))
  {
    initialSample <- matrix(initialSample,nrow=1)
  }

  # checking the validity of increases

  if(!is.logical(increases))
  {
    stop("Parameter increases should have logical value")
  }

  # check the initial sample

  ParameterCheckForInitialSample(initialSample)

  # checking parameter mu_0 for validity

  ParameterMu0Check(mu0 = mu_0, increases)

  # checking numberOfSamples parameter

  if(!IfInteger(numberOfSamples) | numberOfSamples <= 1)
  {
    stop("Parameter numberOfSamples should be integer value and > 1")
  }

  # checking theta parameter

  if(!is.double(theta) | theta < 0)
  {
    stop("Parameter theta should be double value and > 0")
  }

  # checking resamplingMethod parameter

  if (!(resamplingMethod %in% resamplingMethods))
  {
    stop("Parameter resamplingMethod should be a proper name of the resampling method")
  }



  # if we have increases, then all initial fuzzy numbers have to be changed

  if(increases)
  {
    mu_0 <- TransformFromIncreases(mu_0)

    initialSample <- TransformFromIncreases(initialSample)

  }

  # checking consistency of fuzzy numbers

  if(!all(apply(initialSample, 1, IsFuzzy)))
  {
    stop("Some values in initial sample are not correct fuzzy numbers")
  }

  # calculation of C test without bootstrap (step 1)

  n <- nrow(initialSample)

  standardStatistics <- ValueA(initialSample, mu_0, theta) / ValueB(initialSample, theta)

  # prepare vector

  bootstrappedStatistics <- rep(0,numberOfSamples)

  # pb <- txtProgressBar (1, numberOfSamples, style = 3)

  for (i in 1:numberOfSamples) {

    # generate bootstrap sample (step 3)

    bootstrapSample <- get(resamplingMethod)(initialSample, n,  increases=FALSE)

    # calculate bootstrapped statistics (step 4)

    bootstrappedStatistics[i] <- ValueA(bootstrapSample, initialSample, theta) /
      ValueB(bootstrapSample, theta)

    # setTxtProgressBar(pb, i)

  }

  # cat("\n")

  # cat("standardStatistics: ", standardStatistics, ", bootstrappedStatistics: ", bootstrappedStatistics, "\n")

  # calculate p-value

  pvalue <- mean(standardStatistics < bootstrappedStatistics)

  return(pvalue)
}

