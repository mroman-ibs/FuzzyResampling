#' Comparison of the resampling approaches based on the power for the one-sample test for the mean.
#'
#' @description
#' `ComparisonOneSampleCTest` returns the percentage of rejections for the one-sample C-test when different resampling methods
#' are used.
#'
#' @details
#' The function generates a sequence of initial samples (their number is given in \code{initialSamples},
#'  the size is determined by \code{sampleSize}) for fuzzy numbers of the type specified by \code{generator}.
#'  Then some deterministic shift of the size \code{shift} is added to each fuzzy observation in these samples.
#'  Next, function \code{OneSampleCTest} is executed to calculate the p-value for each combination of the initial sample and
#'  resampling method. Then, by comparing the p-value with the assumed significance level
#'  \code{significance} we make a decision whether to reject the null hypothesis for the one-sample C-test for the mean
#'  (see Lubiano et al. (2016))  or not.
#'  The output of this procedure is the percentage of rejections in the sequence of experiments.
#'
#' @param generator Name of the generator for sampling initial samples.
#' For the possible names check the values of \code{samplingGenerators} vector.
#'
#' @param mu_0 Triangular or trapezoidal fuzzy number which is used for the null hypothesis of the C-test.
#'
#' @param shift Deterministic shift added to all initial samples.
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
#' @return This function returns a vector of percentage of rejections for the one-sample C-test for the mean.
#'
#' @family comparison of resampling methods
#'
#' @seealso \code{\link{ComparisonSEMean}} for the comparison of resampling methods based on SE/MSE for the mean,
#' \code{\link{ComparePowerOneSampleCTest}} for the comparison of resampling methods based on power for the one-sample C-test
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
#'
#' ComparisonOneSampleCTest(generator="GeneratorNU",mu_0 = c(-0.4,-0.1,0.1,0.4),
#'  sampleSize = 10,numberOfSamples = 100, initialSamples = 100,mu = 0, sigma = 1,a = 0.2,b = 0.6)}
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






# comparison of outputs for one sample C test for all resampling methods

ComparisonOneSampleCTest<- function(generator, mu_0, shift=0, sampleSize = 10,
                                    numberOfSamples = 10, initialSamples = 100, theta = 1/3,
                                    significance = 0.05, ...)
{
  # print(as.list(match.call()))

  # checking initialSamples parameter

  if(!IfInteger(initialSamples) | initialSamples < 1)
  {
    stop("Parameter initialSamples should be integer value and >= 1")
  }


  # checking generator parameter

  if(!(generator %in% samplingGenerators))
  {
    stop("Parameter generator should be a proper name of the sampling generator")
  }


  # checking shift parameter

  if(!is.double(shift) | is.infinite(shift))
  {
    stop("Parameter shift should be double, finite value")
  }



  # checking sampleSize parameter

  if(!is.double(sampleSize) | sampleSize < 0)
  {
    stop("Parameter sampleSize should be double value and >= 0")
  }

  # checking significance parameter

  if(!is.double(significance) | significance < 0)
  {
    stop("Parameter significance should be double value and >= 0")
  }

  # vector prealocation

  outputVector <- rep(0, length(resamplingMethods))

  # progress bar

  pb <- txtProgressBar (1, initialSamples, style = 3)

  # print(as.list(match.call()))


  # main loop (initial samples)

  for (i in 1:initialSamples) {


    # generate initial sample with possible shift

    sample <- get(generator)(n=sampleSize,...) + shift

    # find p-value for each method

    for (j in 1:length(resamplingMethods)) {

      pvalue <- OneSampleCTest(initialSample = sample, mu_0,
                               numberOfSamples, theta,
                               resamplingMethod = resamplingMethods[j], increases = FALSE)

      # check the output of the bootstrapped test

      if(pvalue < significance)
      {
        # reject H_0

        outputVector[j] <- outputVector[j] + 1
      }


      # cat("Cumulated rejection nr: ", j, " id equal: ", outputVector[j], "\n")

    }



    setTxtProgressBar(pb, i)

  }

  # average the output

  outputVector <- outputVector / initialSamples

  names(outputVector) <- resamplingMethods

  cat("\n")

  return(outputVector)
}
