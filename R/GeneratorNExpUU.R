#' Generate initial sample using the normal and uniform distributions.
#'
#' @description
#' `GeneratorNExpUU` generates the random sample of trapezoidal fuzzy numbers using the normal, exponential and uniform distributions (for
#' the "true" origin of each fuzzy number, and its increases, respectively).
#'
#' @details
#' The procedure simulates the initial sample which consists of \code{n} trapezoidal fuzzy numbers.
#' The "true origin" of each fuzzy number is independently drawn from the normal distribution \code{N (mu, sigma)}.
#' Then increases of its core are independently generated from the exponential distributions \code{Exp (lambda)}.
#' The increases of its support are independently drawn from the uniform distributions
#' \code{U [0,b]} (the left-hand increase) and \code{U [0,c]} (its right-hand counterpart) (see Grzegorzewski et al. (2020)).
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
#' @param mu The expected value of the normal distribution which is used to create the "true origin" of fuzzy trapezoidal number.
#'
#' @param sigma The standard deviation of the normal distribution which is used to create the "true origin" of fuzzy trapezoidal number.
#'
#' @param lambda The parameter of the exponential distributions which are used to create the left and right
#' increases of the core.
#'
#' @param b The right end of the interval of the independent uniform distribution which is used to create the left
#' increase of the support.
#'
#' @param c The right end of the interval of the independent uniform distribution which is used to create the right
#' increase of the support.
#'
#' @param ... Some additional parameters to pass to other functions.
#'
#'
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
#' # generate 10 trapezoidal fuzzy numbers
#'
#' GeneratorNExpUU(10, 0,1,1,1,2)
#'
#'

#'
#' @references
#'
#' Grzegorzewski, P., Hryniewicz, O., Romaniuk, M. (2020)
#' Flexible resampling for fuzzy data based on the canonical representation
#' International Journal of Computational Intelligence Systems, 13 (1), pp. 1650-1662
#'
#'
#' @export
#'
#' @family sampling functions
#'
#' @seealso \code{\link{GeneratorNU}}
#'
#' @importFrom stats runif rnorm rexp


# initial sample generator with true origin from the normal distribution, exp increases for the core,
# and uniform increases for the support

GeneratorNExpUU <- function(n, mu, sigma, lambda, b, c, increases = FALSE, ...)
{
  # checking n parameter

  if(!ifInteger(n) | n <= 1)
  {
    stop("Parameter n should be integer value and > 1")
  }

  # checking sigma parameter

  if(!is.double(sigma) | sigma < 0)
  {
    stop("Parameter sigma should be double value and > 0")
  }

  # checking lambda parameter

  if(!is.double(lambda) | lambda < 0)
  {
    stop("Parameter lambda should be double value and > 0")
  }

  # checking b parameter

  if(!is.double(b) | b < 0)
  {
    stop("Parameter b should be double value and > 0")
  }

  # checking c parameter

  if(!is.double(c) | c < 0)
  {
    stop("Parameter c should be double value and > 0")
  }


  # output matrix of fuzzy numbers

  initialSample <- matrix(1, n, 4)

  # simulate "true" origins and incresases of the core

  origins <- rnorm(n, mu, sigma)

  increaseLCore <- rexp(n, lambda)

  increaseRCore <- rexp(n, lambda)

  # calculate core in the output matrix

  initialSample[,2] <- origins - increaseLCore

  initialSample[,3] <- origins + increaseRCore

  # generate increases of the support

  increaseLSupport <- runif(n, 0, b)

  increaseRSupport <- runif(n, 0, c)

  # calculate support in the output matrix

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
