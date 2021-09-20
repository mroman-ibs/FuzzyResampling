#' d method for resampling triangular and trapezoidal fuzzy numbers
#'
#' @description
#' `dmethod` returns the secondary (bootstrapped) sample and uses the resampling
#' scheme which does not change the left end of the cores and increments (i.e. length of the core,
#'  left and right increment of the support) of the fuzzy variables from
#'  the initial sample (the d method, see (Romaniuk and Hryniewicz, 2019)).
#'
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
#' The resampling procedure produces \code{b} fuzzy values.
#' During the first step, the four values are randomly chosen based on the whole initial sample:
#' left end of the core, length of the core, left and right increment of the support (with equal probabilities
#' for each fuzzy variable).
#' Then the new fuzzy variable, which preserves these characteristics, is created.
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
#'
#' @return This function returns matrix with \code{b} rows of double values.
#' In each row, there is a single resampled fuzzy number.
#' These fuzzy numbers have the same form as the values from the initial sample depending on the provided parameter \code{increases}.
#'
#'
#' @family resampling functions
#'
#' @seealso @seealso \code{\link{classicalBootstrap}},
#' \code{\link{EWmethod}} for the EW method, \code{\link{VAFmethod}} for the VAF method,
#' \code{\link{VAAmethod}} for the VAA method, \code{\link{wmethod}} for the w method
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
#' # generate the secondary sample using the d method
#'
#' dmethod(fuzzyValues)
#'
#' dmethod(fuzzyValues,b=4)
#'
#' # prepare some fuzzy numbers (second type of the initial sample)
#'
#' fuzzyValuesInc <- matrix(c(0.25,0.5,1,0.25,0.25,1,1.5,0.7,1,0,0,2),
#' ncol = 4,byrow = TRUE)
#'
#' # generate the secondary sample using the d method
#'
#' dmethod(fuzzyValuesInc,increases = TRUE)
#'
#' dmethod(fuzzyValuesInc,b=4,increases = TRUE)
#'
#' @references
#'
#' Romaniuk, M., Hryniewicz, O. (2019)
#' Interval-based, nonparametric approach for resampling of fuzzy numbers
#' Soft Computing, 23 (14), pp. 5883-5903
#'
#' @export
#'





# d resampling method

dmethod <- function(initialSample, b = n, increases = FALSE)
{
  # changing possible vector to matrix

  if(is.vector(initialSample))
  {
    initialSample <- matrix(initialSample,nrow=1)
  }

  # setting n

  n <- nrow(initialSample)

  # checking parameters

  parameterCheckForResampling(initialSample,b)


  # check form of the initial sample

  if(increases)
  {
    initialSample <- transformFromIncreases(initialSample)
  }

  # checking consistency of fuzzy numbers

  if(!all(apply(initialSample, 1, is.Fuzzy)))
  {
    stop("Some values in  initial sample are not correct fuzzy numbers")
  }

  # calculate ends and increaments

  spreads <- transformToAllSpreads(initialSample)

  # cat("spreads:\n")
  # print(spreads)

  # generate 4 numbers to find left end of the core and increaments

  numbers <- matrix(sample(1:n, 4*b, replace = TRUE),ncol = 4)

  # cat("numbers:\n")
  # print(numbers)

  # generate the output

  outputSample <- matrix(c(spreads[numbers[,2],2] - spreads[numbers[,1],1],
                           spreads[numbers[,2],2],
                           spreads[numbers[,2],2] + spreads[numbers[,3],3],
                           spreads[numbers[,2],2] + spreads[numbers[,3],3] + spreads[numbers[,4],4]),
                         nrow = b, ncol = 4)


  # change form of the output sample

  if(increases)
  {
    outputSample <- transformToIncreases(outputSample)
  }

  return(outputSample)

}

