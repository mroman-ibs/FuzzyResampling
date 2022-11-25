#' Calculate Bertoluzza's (mid/spread) distance for triangular and trapezoidal fuzzy numbers
#'
#' @description
#' `BertoluzzaDistance` returns the Bertoulzza et al.'s (aka mid/spread) distance with the given weight \code{theta}
#'  between two triangular or trapezoidal fuzzy numbers.
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
#' The procedure calculates the mid/spread distance between two fuzzy values based on the weight \code{theta},
#' usually we have \code{theta = 1/3} or \code{theta = 1}.
#' The output is given as vector of distances.
#'
#'
#' @param fuzzyNumber1 The first triangular or trapezoidal fuzzy number.
#' More than one value can be given in the form of matrix.
#'
#' @param fuzzyNumber2 The second triangular or trapezoidal fuzzy number.
#' More than one value can be given in the form of matrix.
#'
#' @param theta The weighting parameter for the mid/spread distance.
#'
#'
#' @param increases If \code{TRUE} is used, then the fuzzy numbers should be given in the form:
#'  left increment of the support, left end of the core, right end of the core,
#' right increment of the support. Otherwise, the default value \code{FALSE} is used and the fuzzy numbers should be given in the form:
#' left end of the support, left end of the core, right end of the core,
#' right end of the support.
#'
#' @return This function returns vector of doubles related to the calculated distances.
#'
#'
#' @examples
#'
#' # prepare some fuzzy numbers (first type of the initial sample)
#'
#' fuzzyValues <- matrix(c(0.25,0.5,1,1.25,0.75,1,1.5,2.2,-1,0,0,2),ncol = 4,byrow = TRUE)
#'
#' # calculate the mid/spread distance between the first value
#' # (from the first row) and the second one (from the second row)
#'
#' BertoluzzaDistance(fuzzyValues[1,],fuzzyValues[2,])
#'
#' # calculate the mid/spread distance between the first value
#' # (from the first row) and all of the values (from the first to the third row)
#'
#' BertoluzzaDistance(fuzzyValues[1,],fuzzyValues)
#'

#'
#' @references
#'
#' Bertoluzza, C., Corral, N., Salas, A. (1995)
#' On a new class of distances between fuzzy numbers
#' Mathware and Soft Computing, 2 (2), pp. 71-84
#'
#' @export
#'



# calculate Bertoluzza's distance

BertoluzzaDistance <- function(fuzzyNumber1, fuzzyNumber2, theta = 1/3, increases = FALSE)
{

  # changing possible vector to matrix

  if(is.vector(fuzzyNumber1))
  {
    fuzzyNumber1 <- matrix(fuzzyNumber1,nrow=1)
  }

  if(is.vector(fuzzyNumber2))
  {
    fuzzyNumber2 <- matrix(fuzzyNumber2,nrow=1)
  }

  # checking if there are NA's

  if(any(is.na(fuzzyNumber1)))
  {
    stop("There are some NA in fuzzyNumber1 parameter")
  }

  if(any(is.na(fuzzyNumber2)))
  {
    stop("There are some NA in fuzzyNumber2 parameter")
  }

  # checking if values are numeric

  if(!is.numeric(fuzzyNumber1))
  {
    stop("Some values in fuzzyNumber1 parameter are not numeric ones")
  }


  if(!is.numeric(fuzzyNumber2))
  {
    stop("Some values in fuzzyNumber2 parameter are not numeric ones")
  }

  # checking the respective form of matrix

  if(!is.matrix(fuzzyNumber1))
  {
    stop("Values in fuzzyNumber1 parameter are not given as a matrix/vector")
  }

  if(!is.matrix(fuzzyNumber2))
  {
    stop("Values in fuzzyNumber2 parameter are not given as a matrix/vector")
  }

  # checking the number of columns

  if(!(ncol(fuzzyNumber1)%%4 == 0))
  {
    stop("There should be 4 columns in fuzzyNumber1 parameter")
  }

  if(!(ncol(fuzzyNumber2)%%4 == 0))
  {
    stop("There should be 4 columns in fuzzyNumber2 parameter")
  }

  # checking the validity of increases

  if(!is.logical(increases))
  {
    stop("Parameter increases should have logical value")
  }

  # checking the validity of theta

  # checking the validity of increases

  if(!is.double(theta) | theta < 0)
  {
    stop("Parameter theta should be double value and > 0")
  }


  # check form of fuzzy numbers

  if(increases)
  {
    fuzzyNumber1 <- TransformFromIncreases(fuzzyNumber1)

    fuzzyNumber2 <- TransformFromIncreases(fuzzyNumber2)
  }



  # checking consistency of fuzzy numbers

  if(!all(apply(fuzzyNumber1, 1, IsFuzzy)))
  {
    stop("Some values in fuzzyNumber1 parameter are not correct fuzzy numbers")
  }

  # checking consistency of fuzzy numbers

  if(!all(apply(fuzzyNumber2, 1, IsFuzzy)))
  {
    stop("Some values in fuzzyNumber2 parameter are not correct fuzzy numbers")
  }

  # calculate mid and spreads

  midSpreadFN1 <- TransformToMidSpreads(fuzzyNumber1)
  midSpreadFN2 <- TransformToMidSpreads(fuzzyNumber2)


  # find output

  output <- (midSpreadFN1[,3]-midSpreadFN2[,3])^2 +
    (midSpreadFN1[,1]-midSpreadFN2[,1])^2 +
    (midSpreadFN1[,3]-midSpreadFN2[,3])*(midSpreadFN1[,1]-midSpreadFN2[,1]) +
    theta * (midSpreadFN1[,2]-midSpreadFN2[,2])^2 +
    theta * (midSpreadFN1[,4]-midSpreadFN2[,4])^2 +
    theta * (midSpreadFN1[,2]-midSpreadFN2[,2]) * (midSpreadFN1[,4]-midSpreadFN2[,4])

  output <- sqrt(output/3)

  return(output)

}
