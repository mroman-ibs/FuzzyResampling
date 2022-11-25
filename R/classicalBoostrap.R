#' Classical bootstrap procedure for triangular and trapezoidal fuzzy numbers
#'
#' @description
#' `ClassicalBootstrap` returns the bootstrapped (secondary) sample based on the initial sample and uses the Efron's (i.e. classical) resampling
#' scheme (see (Efron, 1994)).
#'
#' @details
#' The initial sample should consist of triangular or trapezoidal fuzzy numbers, given as a single vector or a whole matrix.
#' In each row, there should be a single fuzzy number in one of the forms:
#' \enumerate{
#'  \item left end of the support, left end of the core, right end of the core, right end of the support, or
#'  \item left increment of the support, left end of the core, right end of the core, right increment of the support.
#' }
#' In this second case, the parameter \code{increases=TRUE} has to be used.
#'
#' The resampling procedure produces \code{b} fuzzy values, which are randomly chosen (with repetition) from the initial sample
#' (without any alternations).
#' If the parameter \code{b} is not specified, it is equal to the length of the initial sample.
#' The output is given in the same form as the initial sample.
#'
#'
#' @param initialSample Initial sample of triangular or trapezoidal fuzzy numbers.
#'
#' @param b The number of fuzzy values in the resampled (secondary) sample.
#' If this parameter is not specified, the number of values in the initial sample is used.
#' The parameter should be integer value more than 0.
#'
#'
#' @param increases If \code{TRUE} is used, then the initial sample should consist of the fuzzy numbers in the form:
#'  left increment of the support, left end of the core, right end of the core,
#' right increment of the support. Otherwise, the default value \code{FALSE} is used and the fuzzy numbers should be given in the form:
#' left end of the support, left end of the core, right end of the core,
#' right end of the support.
#'
#' @return This function returns matrix with \code{b} rows of double values.
#' In each row, there is a single resampled fuzzy number.
#' These fuzzy numbers have the same form as the values from the initial sample depending on the provided parameter \code{increases}.
#'
#' @family resampling functions
#'
#' @seealso \code{\link{VAMethod}} for the VA method,
#' \code{\link{EWMethod}} for the EW method, \code{\link{VAFMethod}} for the VAF method,
#' \code{\link{VAAMethod}} for the VAA method, \code{\link{DMethod}} for the d method, \code{\link{WMethod}} for the w method
#'
#' @importFrom stats runif
#'
#' @examples
#'
#' # prepare some fuzzy numbers (first type of the initial sample)
#'
#' fuzzyValues <- matrix(c(0.25,0.5,1,1.25,0.75,1,1.5,2.2,-1,0,0,2),
#' ncol = 4,byrow = TRUE)
#'
#' # generate bootstrap sample
#'
#' set.seed(12345)
#'
#' ClassicalBootstrap(fuzzyValues)
#'
#' ClassicalBootstrap(fuzzyValues,b=4)
#'
#' # prepare some fuzzy numbers (second type of the initial sample)
#'
#' fuzzyValuesInc <- matrix(c(0.25,0.5,1,0.25,0.25,1,1.5,0.7,1,0,0,2),
#' ncol = 4,byrow = TRUE)
#'
#' # generate bootstrap sample
#'
#' ClassicalBootstrap(fuzzyValuesInc,increases = TRUE)
#'
#' ClassicalBootstrap(fuzzyValuesInc,b=4,increases = TRUE)
#'
#' @references
#'
#' Efron, B. (1994). An Introduction to the Bootstrap.
#' CRC Press
#'
#' @export
#'


# classical bootstrap


ClassicalBootstrap <- function(initialSample, b = n, increases = FALSE)
{
  # changing possible vector to matrix

  if(is.vector(initialSample))
  {
    initialSample <- matrix(initialSample,nrow=1)
  }

  ParameterCheckForInitialSample(initialSample)

  # setting n

  n <- nrow(initialSample)

  # checking b parameter

  if(!IfInteger(b) | b <= 0)
  {
    stop("Parameter b should be integer value and > 0")
  }

  # checking the validity of increases

  if(!is.logical(increases))
  {
    stop("Parameter increases should have logical value")
  }

  # check form of the initial sample

  if(increases)
  {
    initialSample <- TransformFromIncreases(initialSample)
  }

  # checking consistency of fuzzy numbers

  if(!all(apply(initialSample, 1, IsFuzzy)))
  {
    stop("Some values in  initial sample are not correct fuzzy numbers")
  }

  # generation of numbers of TPFNs based on intial sample

  numbers <- sample(n,b, replace = TRUE)

  # cat("Generated numbers:", numbers, "\n")

  # resampling based on generated numbers

  outputSample <- initialSample[numbers,]

  if(increases)
  {
    outputSample <- TransformToIncreases(outputSample)
  }

  return(outputSample)

}

