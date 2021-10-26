#' V(alue)A(mbiguity, left-hand)A(mbiguity, right-hand) resampling method for triangular and trapezoidal fuzzy numbers
#'
#' @description
#' `VAAmethod` returns the secondary (bootstrapped) sample and uses the resampling
#' scheme which does not change the values, left-hand and right-hand ambiguities of the fuzzy variables from
#'  the initial sample (the VAA method, see (Grzegorzewski and Romaniuk, 2021)).
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
#' Then the new fuzzy variable, which preserves the value, left- and right-hand ambiguities of the old one, is randomly created.
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
#'  \code{\link{dmethod}} for the d method, \code{\link{wmethod}} for the w method
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
#' # generate the secondary sample using the VAA method
#'
#' set.seed(12345)
#'
#' VAAmethod(fuzzyValues)
#'
#' VAAmethod(fuzzyValues,b=4)
#'
#' # prepare some fuzzy numbers (second type of the initial sample)
#'
#' fuzzyValuesInc <- matrix(c(0.25,0.5,1,0.25,0.25,1,1.5,0.7,1,0,0,2),
#' ncol = 4,byrow = TRUE)
#'
#' # generate the secondary sample using the VAA method
#'
#' VAAmethod(fuzzyValuesInc,increases = TRUE)
#'
#' VAAmethod(fuzzyValuesInc,b=4,increases = TRUE)
#'
#' @references
#'
#' Grzegorzewski, P., Romaniuk, M. (2021)
#' Bootstrap methods for fuzzy data
#' (to be published)
#'
#' @export
#'













# VAA resampling method

VAAmethod <- function(initialSample, b = n, increases = FALSE)
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

  # calculate value, ambiguity (l and u) for initial sample

  initialValues <- calculateValue(initialSample)

  initialAmbiguitesL <- calculateAmbiguityL(initialSample)

  initialAmbiguitesU <- calculateAmbiguityU(initialSample)

  # cat("Calculated values: ", initialValues, "\n")

  # cat("Calculated ambiguitesL: ", initialAmbiguitesL, "\n")

  # cat("Calculated ambiguitesU: ", initialAmbiguitesU, "\n")



  # generation of numbers of TPFNs based on intial sample

  numbers <- sample(n,b, replace = TRUE)

  # cat("Generated numbers:", numbers, "\n")

  # initialize output

  outputSample <- matrix(0, nrow = b, ncol = 4)


  # resample

  for (i in 1:b)
  {
    # check if selected fuzzy number is triangular

    if (is.Triangular(initialSample[numbers[i],]))
    {
      outputSample[i,] <- initialSample[numbers[i],]

      # cat("i: ", i, "TRFN\n")

    }
    else
    {

      # choose value, ambiguities (L and U)

      selectedValue <- initialValues[numbers[i]]

      selectedAmbiguityL <- initialAmbiguitesL[numbers[i]]

      selectedAmbiguityU <- initialAmbiguitesU[numbers[i]]

      # cat("Selected value: ", selectedValue, "\n")

      # cat("Selected ambiguityL: ", selectedAmbiguityL, "\n")

      # cat("Selected ambiguityU: ", selectedAmbiguityU, "\n")


      # we have TPFN, generate the output

      c <- selectedValue + selectedAmbiguityU - selectedAmbiguityL

      s <- runif(1,0,2*min(selectedAmbiguityL, selectedAmbiguityU))

      l <- 6*selectedAmbiguityL-3*s

      r <- 6*selectedAmbiguityU-3*s

      # cat("s: ", s, "c: ", c, "l: ", l, "r: ", r, "\n")

      # cat("i: ", i, "TPFN\n")

      outputSample[i,] <- c(c-l-s,c-s,c+s,c+r+s)

    }


  }

  # change form of the output sample

  if(increases)
  {
    outputSample <- transformToIncreases(outputSample)
  }

  return(outputSample)


}











