#' Generate initial sample using various random distributions.
#'
#' @description
#' `GeneratorFuzzyNumbers` generates the random sample of trapezoidal fuzzy numbers using the
#' various random distributions, using the functions from the \code{stats} package.
#'
#' @details
#' The procedure simulates the initial sample which consists of \code{n} trapezoidal fuzzy numbers.
#' The "true origin" of each fuzzy number is independently drawn from the random distribution using
#' \code{originalRandomDist} function from the \code{stats} package with the parameters defined by
#' \code{parametersOriginalRD}.
#' The same applies to the increases of the core (the function \code{increasesRandomDist} with the parameters
#' \code{parametersIncreasesRD} is then used), the left increase of the support (the function \code{supportLeftRandomDist}
#'  with the parameters \code{parametersSupportLeftRD}, respectively), and the right increase of the support
#'  (the function \code{supportRightRandomDist}
#'  with the parameters \code{parametersSupportRightRD}, respectively).
#'
#'  Names of these generators for random probabilities and their respective parameters should be in the form
#'  required by \code{stats} package.
#'
#' The output is given as a matrix.
#' In each row, there is a single fuzzy number in one of the forms:
#' \enumerate{
#'  \item left end of the support, left end of the core, right end of the core, right end of the support, or
#'  \item left increment of the support, left end of the core, right end of the core, right increment of the support.
#' }
#' To obtain this second form, the parameter \code{increases=TRUE} has to be used.
#'
#'
#'
#'
#' @param n Number of fuzzy trapezoidal numbers in the created sample.
#'
#'
#' @param originalRandomDist Name of the random generator used to create the "true origin" of fuzzy trapezoidal number (as
#' defined in \code{stats} package).
#'
#' @param parametersOriginalRD List of parameters required by the random generator used
#' to create the "true origin" of fuzzy trapezoidal number.
#'
#' @param increasesRandomDist Name of the random generator used to create the increases of the core of fuzzy
#'  trapezoidal number (as defined in \code{stats} package).
#'
#' @param parametersIncreasesRD List of parameters required by the random generator used
#' to create the increases of the core of fuzzy trapezoidal number.
#'
#' @param supportLeftRandomDist Name of the random generator used to create the increases of the left support of fuzzy
#'  trapezoidal number (as defined in \code{stats} package).
#'
#' @param parametersSupportLeftRD List of parameters required by the random generator used
#' to create the increases of the left support of fuzzy trapezoidal number.
#'
#' @param supportRightRandomDist Name of the random generator used to create the increases of the right support of fuzzy
#'  trapezoidal number (as defined in \code{stats} package).
#'
#' @param parametersSupportRightRD List of parameters required by the random generator used
#' to create the increases of the right support of fuzzy trapezoidal number.
#'
#'
#' @param ... Some additional parameters to pass to other functions.
#'
#' @param increases If \code{TRUE} is used, then the output fuzzy numbers are given in the form:
#'  left increment of the support, left end of the core, right end of the core,
#' right increment of the support. Otherwise, the default value \code{FALSE} is used and the fuzzy numbers are given in the form:
#' left end of the support, left end of the core, right end of the core,
#' right end of the support.
#'
#' @return This function returns matrix which consists of \code{n} trapezoidal fuzzy numbers.
#' Each fuzzy number (four values) ia given as one row in this matrix.
#'
#'
#' @examples
#'
#' # seed PRNG
#'
#' set.seed(1234)
#'
#' # generate 10 trapezoidal fuzzy numbers (the normal distribution for the "true origin",
#' # the exponential distribution for the increases of the core, and two different uniform
#' # distribution for the increases of the support)
#'
#' GeneratorFuzzyNumbers(10,"rnorm",list(mean=0,sd=1),"rexp",list(rate=2),"runif",
#'                      list(min=0,max=0.6),"runif",list(min=0,max=1))
#'
#'

#'
#' @references
#'
#' Grzegorzewski, P., Hryniewicz, O., Romaniuk, M. (2020)
#' Flexible resampling for fuzzy data based on the canonical representation
#' International Journal of Computational Intelligence Systems, 13 (1), pp. 1650-1662
#'
#' Grzegorzewski, P., Romaniuk, M. (2022)
#' Bootstrapped Kolmogorov-Smirnov Test for Epistemic Fuzzy Data
#' Information Processing and Management of Uncertainty in Knowledge-Based Systems
#' Springer
#'
#' @export
#'
#' @family sampling functions
#'
#' @seealso \code{\link{GeneratorNExpUU}}

# initial sample generator with universal calls for the true origin and increases

GeneratorFuzzyNumbers <- function(n, originalRandomDist, parametersOriginalRD,
                                  increasesRandomDist, parametersIncreasesRD,
                                  supportLeftRandomDist, parametersSupportLeftRD,
                                  supportRightRandomDist, parametersSupportRightRD,
                                  increases = FALSE, ...)
{

  # print(as.list(match.call()))

  # checking n parameter

  if(!IfInteger(n) | n <= 1)
  {
    stop("Parameter n should be integer value and > 1")
  }


  # checking the validity of increases

  if(!is.logical(increases) | is.na(increases))
  {
    stop("Parameter increases should have logical value")
  }

  # output matrix of fuzzy numbers

  initialSample <- matrix(1, n, 4)

  # list of number of simulations

  numSim <- list(n)

  # simulate "true" origins and incresases of the core

  origins <- do.call(originalRandomDist,append(numSim,parametersOriginalRD))

  increaseLCore <- do.call(increasesRandomDist,append(numSim,parametersIncreasesRD))

  increaseRCore <- do.call(increasesRandomDist,append(numSim,parametersIncreasesRD))

  # calculate core in the output matrix

  initialSample[,2] <- origins - increaseLCore

  initialSample[,3] <- origins + increaseRCore

  # generate increases of the support

  increaseLSupport <- do.call(supportLeftRandomDist,append(numSim,parametersSupportLeftRD))

  increaseRSupport <- do.call(supportRightRandomDist,append(numSim,parametersSupportRightRD))

  # calculate the support in the output matrix

  if(increases == TRUE)
  {
    initialSample[,1] <- increaseLSupport

    initialSample[,4] <- increaseRSupport

  } else {

    initialSample[,1] <- initialSample[,2] - increaseLSupport

    initialSample[,4] <- initialSample[,3] + increaseRSupport

  }



  return(initialSample)
}



