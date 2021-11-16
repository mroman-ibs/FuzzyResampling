#' Calculate SE/MSE for the mean of the bootstrapped samples.
#'
#' @description
#' `SEResamplingMean` estimates the standard error (SE) or the mean-squared error (MSE) for the mean while the sample is bootstrapped using
#' one of the applied resampling methods.
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
#' The procedure uses the resampling method given in \code{resamplingMethod} parameter to create the secondary (bootstrapped)
#' samples.
#' Then the mean (in the form of trapezoidal fuzzy number) is calculated for each new sample.
#' If the parameter \code{trueMean} is set, then the mean-squared error (MSE) between this true value and each of the sample means
#' is estimated using the Bertoulzza et al.'s (aka mid/spread) distance (with the given weight \code{theta},
#' see Bertoluzza et al. (1995)).
#' Otherwise, the overall mean is calculated, and the standard error (SE) is estimated based on the overall mean
#' and each of the sample means.
#' In this case the Bertoulzza et al.'s distance (see Grzegorzewski, Romaniuk (2021)) is also applied.
#'
#'
#'
#' @param initialSample The initial sample which consists of triangular or trapezoidal fuzzy numbers.
#' More than one value can be given in the form of matrix.
#'
#'
#' @param resamplingMethod Name of the resampling method, which is used to generate the bootstrapped samples.
#' For the possible names check the values of \code{resamplingMethods} vector.
#'
#' @param repetitions Number of the secondary samples which are created using the selected resampling method.
#'
#' @param trueMean If the value is given, then the mean-squared error (MSE) is calculated for this value and the means of the bootstrapped
#' samples. Otherwise, the standard error (SE) is calculated based on the overall mean of the secondary samples.
#'
#' @param theta The weighting parameter for the mid/spread distance applied to calculate the SE/MSE.
#'
#'
#' @param increases If \code{TRUE} is used, then the fuzzy numbers should be given in the form:
#'  left increment of the support, left end of the core, right end of the core,
#' right increment of the support. Otherwise, the default value \code{FALSE} is used and the fuzzy numbers should be given in the form:
#' left end of the support, left end of the core, right end of the core,
#' right end of the support.
#'
#' @return This function returns list of two double values:
#' \enumerate{
#'  \item \code{mean} which is equal to the overall mean (if the SE is calculated) or
#' \code{trueMean} parameter (if the MSE is calculated),
#' \item \code{SE} which is equal to the estimated SE/MSE of the mean.
#' }
#' The output \code{mean} consists of four values which create the trapezoidal fuzzy number.
#'
#'
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
#' # calculate the SE of the mean using the classical (i.e. Efron's) bootstrap
#'
#' SEResamplingMean(fuzzyValues)
#'
#' # calculate the SE of the mean using the VA resampling method
#'
#' SEResamplingMean(fuzzyValues, resamplingMethod = VAmethod)
#'
#' # calculate the MSE of the given mean using the classical (i.e. Efron's) bootstrap
#'
#' SEResamplingMean(fuzzyValues, trueMean = c(0,0.5,1,2))
#'
#' # calculate the MSE of the given mean using the VA resampling method
#'
#' SEResamplingMean(fuzzyValues, resamplingMethod = VAmethod, trueMean = c(0,0.5,1,2))
#'

#'
#' @references
#'
#' Bertoluzza, C., Corral, N., Salas, A. (1995)
#' On a new class of distances between fuzzy numbers
#' Mathware and Soft Computing, 2 (2), pp. 71-84
#'
#' Grzegorzewski, P., Romaniuk, M. (2021)
#' Bootstrap methods for fuzzy data
#' (to be published)
#'
#' @export
#'





# calculate SE/MSE for the mean

SEResamplingMean <- function(initialSample, resamplingMethod=classicalBootstrap, repetitions = 100, trueMean = NA, theta = 1/3,
                             increases = FALSE)
{

  # changing possible vector to matrix

  if(is.vector(initialSample))
  {
    initialSample <- matrix(initialSample,nrow=1)
  }

  # check the initial sample

  parameterCheckForInitialSample(initialSample)

  # checking repetitions parameter

  if(!isInteger(repetitions) | repetitions <= 1)
  {
    stop("Parameter repetitions should be integer value and > 1")
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


  # matrix for means

  meanMatrix <- matrix(NA, nrow = repetitions, ncol = 4)

  # repeat resampling procedure

  for (i in 1:repetitions)
  {
    # generate secondary sample

    secondarySample <- resamplingMethod(initialSample, b = nrow(initialSample), increases)

    # cat("\n secondarySample: \n")
    # print(as.matrix(secondarySample))

    # find mean

    meanMatrix[i,] <- apply(secondarySample, 2, mean)

    # cat("mean nr: ", i, " is equal to: ", meanMatrix[i,], "\n")


  }



  if(anyNA(trueMean) == TRUE)
  {

    # averaging means

    overallMean <- apply(meanMatrix, 2, mean)

    # cat("\n overall mean: ", overallMean, "\n")

    # calculate SE

    SEmean <- sum((BertoluzzaDistance(meanMatrix, overallMean, theta))^2)

  } else {

    # calculate MSE

    SEmean <- sum((BertoluzzaDistance(meanMatrix, trueMean, theta))^2)

    overallMean <- trueMean

  }




  SEmean <- sqrt(SEmean / (repetitions - 1))

  output <- list("mean" = overallMean, "SE" = SEmean)

  return(output)

}




resamplingMethods <- c("classicalBootstrap", "VAmethod", "EWmethod",
                       "VAFmethod", "dmethod", "wmethod")

