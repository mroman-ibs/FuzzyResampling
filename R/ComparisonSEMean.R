#' Comparison of the resampling approaches based on the SE/MSE for the mean.
#'
#' @description
#' `ComparisonSEMean` estimates the standard error (SE) or the mean-squared error (MSE) for the mean for
#' all resampling approaches.
#'
#' @details
#' The function generates a sequence of initial samples (their number is given in \code{initialSamples},
#'  the size is determined by \code{sampleSize}) for fuzzy numbers of the type specified by \code{generator}.
#'  Then the SE/MSE is calculated for each combination of the initial sample and
#'  resampling method using \code{SEResamplingMean}. The output values are the SE/MSE averaged by \code{initialSamples}.
#'
#' @param generator Name of the generator for sampling initial samples.
#' For the possible names check the values of \code{samplingGenerators} vector.
#'
#'
#' @param sampleSize Size of the single initial sample.
#'
#' @param numberOfSamples Number of the initial samples.
#'
#' @param repetitions Number of the secondary samples which are created using the selected resampling method.
#'
#' @param trueMean If the value is given, then the mean-squared error (MSE) is calculated for this value and the means of the bootstrapped
#' samples. Otherwise, the standard error (SE) is calculated based on the overall mean of the secondary samples.
#'
#' @param theta The weighting parameter for the mid/spread distance applied in the C-test.
#'
#'
#' @param ... Parameters which are passed to \code{SEResamplingMean} or the respective \code{generator}
#'
#' @return This function returns a vector of the averaged SE/MSE for the mean.
#'
#' @family comparison of resampling methods
#'
#' @seealso \code{\link{ComparisonOneSampleCTest}} for the comparison of resampling methods based on power for the one-sample C-test
#' for the mean,
#' \code{\link{ComparePowerOneSampleCTest}} for the comparison of resampling methods based on power for the one-sample C-test
#' for the mean.
#'
#' @importFrom utils txtProgressBar
#' @importFrom utils setTxtProgressBar
#'
#'
#' @examples
#'
#' \dontrun{
#'
#' # seed PRNG
#'
#' set.seed(1234)
#'
#' # calculate the SE of the mean for the synthetic data generated using GeneratorNU function
#'
#' ComparisonSEMean(generator = "GeneratorNU",sampleSize = 10,
#'  numberOfSamples = 100, repetitions = 10,mu = 0, sigma = 1,a = 0.5, b = 1)}
#'
#'
#' @references
#'
#' Bertoluzza, C., Corral, N., Salas, A. (1995)
#' On a new class of distances between fuzzy numbers
#' Mathware and Soft Computing, 2 (2), pp. 71-84
#'
#' Grzegorzewski, P., Romaniuk, M. (2022)
#' Bootstrap methods for fuzzy data
#' Uncertainty and Imprecision in Decision Making and Decision Support: New Advances, Challenges, and Perspectives, pp. 28-47
#' Springer
#'
#' @export
#'








# comparison of resampling methods using SE/MSE and the same initial samples

ComparisonSEMean <- function(generator, sampleSize = 10, numberOfSamples = 100,
                             repetitions = 100,trueMean = NA, theta = 1/3, ...)
{
  # checking generator parameter

  if(!(generator %in% samplingGenerators))
  {
    stop("Parameter generator should be a proper name of the sampling generator")
  }

  # checking sampleSize parameter

  if(!IfInteger(sampleSize) | sampleSize <= 1)
  {
    stop("Parameter sampleSize should be integer value and > 1")
  }

  # checking numberOfSamples parameter

  if(!IfInteger(numberOfSamples) | numberOfSamples <= 1)
  {
    stop("Parameter sampleSize should be integer value and > 1")
  }


  # prepare matrix

  matrixSE <- matrix(NA, nrow=length(resamplingMethods), ncol = numberOfSamples)

  # progress bar

  pb <- txtProgressBar (1, numberOfSamples, style = 3)


  # main loop for initial samples

  for (i in 1:numberOfSamples) {


    # generate initial sample

    initialSample <- get(generator)(n=sampleSize,increases=FALSE,...)

    # calculate SE/MSE for each resampling method

    for (j in 1:length(resamplingMethods)) {

      partialOutputs <- SEResamplingMean(initialSample, resamplingMethod=resamplingMethods[j],
                                         repetitions, trueMean, theta)

      matrixSE[j,i] <- partialOutputs$SE

      # cat("wartosc SE nr: ", j, i, " rowna: ", matrixSE[j,i], "\n")

    }

    setTxtProgressBar(pb, i)

  }

  cat("\n")

  # the output is averaged using rows

  outputs <- apply(matrixSE, 1, mean)

  names(outputs) <- resamplingMethods

  return(outputs)

}
