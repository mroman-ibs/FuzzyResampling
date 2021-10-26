#' V(alue)A(mbiguity) resampling method for triangular and trapezoidal fuzzy numbers
#'
#' @description
#' `VAmethod` returns the secondary (bootstrapped) sample and uses the resampling
#' scheme which does not change the values and ambiguities of the fuzzy variables from
#'  the initial sample (the VA method, see (Grzegorzewski et al, 2020)).
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
#' During the first step, the fuzzy value from the initial sample is randomly chosen (with repetition).
#' Then the new fuzzy variable, which preserves the value and ambiguity of the old one, is randomly created.
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
#'
#' @family resampling functions
#'
#' @seealso @seealso \code{\link{classicalBootstrap}},
#' \code{\link{EWmethod}} for the EW method, \code{\link{VAFmethod}} for the VAF method,
#' \code{\link{VAAmethod}} for the VAA method, \code{\link{dmethod}} for the d method, \code{\link{wmethod}} for the w method
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
#' # generate the secondary sample using the VA method
#'
#' set.seed(12345)
#'
#' VAmethod(fuzzyValues)
#'
#' VAmethod(fuzzyValues,b=4)
#'
#' # prepare some fuzzy numbers (second type of the initial sample)
#'
#' fuzzyValuesInc <- matrix(c(0.25,0.5,1,0.25,0.25,1,1.5,0.7,1,0,0,2),
#' ncol = 4,byrow = TRUE)
#'
#' # generate the secondary sample using the VA method
#'
#' VAmethod(fuzzyValuesInc,increases = TRUE)
#'
#' VAmethod(fuzzyValuesInc,b=4,increases = TRUE)
#'
#' @references
#'
#' Grzegorzewski, P., Hryniewicz, O., Romaniuk, M. (2020)
#' Flexible resampling for fuzzy data based on the canonical representation
#' International Journal of Computational Intelligence Systems, 13 (1), pp. 1650-1662
#'
#' @export
#'



# VA resampling method

VAmethod <- function(initialSample, b = n, increases = FALSE)
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

  # calculate value and ambiguity for initial sample

  initialValues <- calculateValue(initialSample)

  initialAmbiguites <- calculateAmbiguity(initialSample)

  # cat("Calculated values: ", initialValues, "\n")

  # cat("Calculated ambiguites: ", initialAmbiguites, "\n")


  # generation of numbers of TPFNs based on intial sample

  numbers <- sample(n,b, replace = TRUE)

  # cat("Generated numbers:", numbers, "\n")


  # resample

  # choose value and ambiguity

  selectedValue <- initialValues[numbers]

  selectedAmbiguity <- initialAmbiguites[numbers]

  # cat("Selected value: ", selectedValue, "\n")

  # cat("Selected ambiguity: ", selectedAmbiguity, "\n")

  s <- rep(0,b)

  for (i in 1:b)
  {

    # check if selected fuzzy number is triangular

    if (!is.Triangular(initialSample[numbers[i],]))
    {
      # we have TPFN, generate s

      s[i] <- runif(1,0,selectedAmbiguity[i])

      # cat("i: ", i, "TPFN\n")

    }


  }

  # cat("Values of s: ", s, "\n")

  # build the output

  c <- runif(b,selectedValue-selectedAmbiguity+s,selectedValue+selectedAmbiguity-s)

  l <- 3*(selectedAmbiguity-selectedValue+c-s)

  r <- 3*(selectedAmbiguity+selectedValue-c-s)

  # cat("s: ", s, "c: ", c, "l: ", l, "r: ", r, "\n")

  outputSample <- matrix(c(c-l-s,c-s,c+s,c+r+s),ncol = 4)

  # change form of the output sample

  if(increases)
  {
    outputSample <- transformToIncreases(outputSample)
  }

  return(outputSample)

}
