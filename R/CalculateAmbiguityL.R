#' Calculation of the left-hand ambiguity for triangular and trapezoidal fuzzy numbers
#'
#' @description
#' `CalculateAmbiguityL` returns the left-hand ambiguity of the triangular or trapezoidal fuzzy number (see, e.g., (Ban et al., 2015),
#'  (Grzegorzewski and Romaniuk, 2022)).
#'
#'
#' @details
#' The input data should consist of triangular or trapezoidal fuzzy numbers, given as a single vector or a whole matrix.
#' In each row, there should be a single fuzzy number in one of the forms:
#' \enumerate{
#'  \item left end of the support, left end of the core, right end of the core, right end of the support, or
#'  \item left increment of the support, left end of the core, right end of the core, right increment of the support.
#' }
#' In this second case, the parameter \code{increases=TRUE} has to be used.
#'
#' Then for each fuzzy number, its characteristics, known as the left-hand ambiguity of fuzzy number, is calculated.
#' For the respective formulas, see, e.g., (Ban et al., 2015), (Grzegorzewski and Romaniuk, 2022).
#'
#'
#' @param fuzzyNumber Input data consist of triangular or trapezoidal fuzzy numbers.
#'
#' @param increases If \code{TRUE} is used, then the initial data should consist of the fuzzy numbers in the form:
#'  left increment of the support, left end of the core, right end of the core,
#' right increment of the support. Otherwise, the default value \code{FALSE} is used and the fuzzy numbers should be given in the form:
#' left end of the support, left end of the core, right end of the core,
#' right end of the support.
#'
#' @return This function returns vector of double values.
#' Each output value is equal to the left-hand ambiguity of the respective fuzzy number.
#'
#'
#' @family characteristics of fuzzy numbers functions
#'
#' @seealso \code{\link{CalculateFuzziness}} for calculation of the fuzziness,
#' \code{\link{CalculateValue}} for calculation of the value,
#' \code{\link{CalculateAmbiguityR}} for calculation of the right-hand ambiguity,
#' \code{\link{CalculateAmbiguity}} for calculation of the ambiguity,
#' \code{\link{CalculateExpValue}} for calculation of the expected value,
#' \code{\link{CalculateWidth}} for calculation of the width
#'
#'
#' @examples
#'
#' # prepare some fuzzy numbers (first type of the initial sample)
#'
#' fuzzyValues <- matrix(c(0.25,0.5,1,1.25,0.75,1,1.5,2.2,-1,0,0,2),
#' ncol = 4,byrow = TRUE)
#'
#' # calculate the left-hand ambiguity of the first fuzzy number
#'
#' CalculateAmbiguityL(fuzzyValues[1,])
#'
#' # calculate the left-hand ambiguity for the whole matrix
#'
#' CalculateAmbiguityL(fuzzyValues)
#'
#' # prepare some fuzzy numbers (second type of the initial sample)
#'
#' fuzzyValuesInc <- matrix(c(0.25,0.5,1,0.25,0.25,1,1.5,0.7,1,0,0,2),
#' ncol = 4,byrow = TRUE)
#'
#' # calculate the left-hand ambiguity of the first fuzzy number
#'
#' CalculateAmbiguityL(fuzzyValuesInc[1,], increases = TRUE)
#'
#'
#' @references
#'
#' Ban, A.I., Coroianu, L., Grzegorzewski, P. (2015)
#' Fuzzy Numbers: Approximations, Ranking and Applications
#' Institute of Computer Sciences, Polish Academy of Sciences
#'
#'
#' Grzegorzewski, P., Romaniuk, M. (2022)
#' Bootstrap methods for fuzzy data
#' Uncertainty and Imprecision in Decision Making and Decision Support: New Advances, Challenges, and Perspectives, pp. 28-47
#' Springer
#'
#' @export
#'










# calculate the left-hand ambiguity of fuzzy number

CalculateAmbiguityL <- function(fuzzyNumber, increases = FALSE)
{

  # changing possible vector to matrix

  if(is.vector(fuzzyNumber))
  {
    fuzzyNumber <- matrix(fuzzyNumber,nrow=1)
  }

  ParameterCheckForInitialSample(fuzzyNumber)

  # checking the validity of increases

  if(!is.logical(increases))
  {
    stop("Parameter increases should have logical value")
  }

  # check form of the initial sample

  if(increases)
  {
    fuzzyNumber <- TransformFromIncreases(fuzzyNumber)
  }

  # checking consistency of fuzzy numbers

  if(!all(apply(fuzzyNumber, 1, IsFuzzy)))
  {
    stop("Some initial values are not correct fuzzy numbers")
  }

  spreads <- TransformToSpreads(fuzzyNumber)

  output <- spreads[,2] / 2 + spreads[,3] / 6

  return(output)
}
