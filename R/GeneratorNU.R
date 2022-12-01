#' Generate initial sample using the normal and uniform distributions.
#'
#' @description
#' `GeneratorNU` generates the random sample of trapezoidal fuzzy numbers using the normal and uniform distributions (for
#' the "true" origin of each fuzzy number, and its increases, respectively).
#'
#' @details
#' The procedure simulates the initial sample which consists of \code{n} trapezoidal fuzzy numbers.
#' The "true origin" of each fuzzy number is independently drawn from the normal distribution \code{N (mu, sigma)}.
#' Then increases of its core and support are independently generated from the uniform distributions
#' \code{U [0,a]} and \code{U [0,b]} (see Grzegorzewski et al. (2020)).
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
#' @param a The right end of the interval of the independent uniform distributions which are used to create the left and right
#' increases of the core.
#'
#' @param b The right end of the interval of the independent uniform distributions which are used to create the left and right
#' increases of the support.
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
#' # generate 10 trapezoidal fuzzy numbers
#'
#' GeneratorNU(10, 0,1,1,2)
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
#' @seealso \code{\link{GeneratorNExpUU}}, \code{\link{GeneratorFuzzyNumbers}}
#'
#' @importFrom stats runif rnorm

# initial sample generator with true origin from the normal distribution and the uniform increases

GeneratorNU <- function(n, mu, sigma, a, b, increases = FALSE, ...)
{

  # checking n parameter

  if(!IfInteger(n) | n <= 1)
  {
    stop("Parameter n should be integer value and > 1")
  }

  # checking mu parameter

  if(!is.double(mu))
  {
    stop("Parameter mu should be double value")
  }

  # checking sigma parameter

  if(!is.double(sigma) | sigma < 0)
  {
    stop("Parameter sigma should be double value and > 0")
  }

  # checking a parameter

  if(!is.double(a) | a < 0)
  {
    stop("Parameter a should be double value and > 0")
  }

  # checking b parameter

  if(!is.double(b) | b < 0)
  {
    stop("Parameter b should be double value and > 0")
  }

  # checking the validity of increases

  if(!is.logical(increases))
  {
    stop("Parameter increases should have logical value")
  }

  # invoking the universal procedure

  GeneratorFuzzyNumbers(n,"rnorm",list(mean=mu,sd=sigma),"runif",list(min=0,max=a),"runif",list(min=0,max=b),
                        "runif",list(min=0,max=b),increases,...)

}



