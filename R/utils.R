# transform matrix with fuzzy numbers to matrix with:
# left increment of the support, left end of the core, right end of the core, right increment of the support


TransformToIncreases <- function(inputFuzzyNumbers)
{
  # check if this is not single value

  if(is.vector(inputFuzzyNumbers))
  {
    inputFuzzyNumbers <- matrix(inputFuzzyNumbers,nrow = 1, ncol = 4)
  }

  output <- matrix(0, nrow = nrow(inputFuzzyNumbers), ncol = 4)

  # calculate increases

  output[,1] <- inputFuzzyNumbers[,2] - inputFuzzyNumbers[,1]

  output[,2] <- inputFuzzyNumbers[,2]

  output[,3] <- inputFuzzyNumbers[,3]

  output[,4] <- inputFuzzyNumbers[,4] - inputFuzzyNumbers[,3]

  return(output)
}


# transform matrix with:
# left increment of the support, left end of the core, right end of the core, right increment of the support
# to matrix of fuzzy numbers


TransformFromIncreases <- function(inputFuzzyNumbers)
{
  # check if this is not single value

  if(is.vector(inputFuzzyNumbers))
  {
    inputFuzzyNumbers <- matrix(inputFuzzyNumbers,nrow = 1, ncol = 4)
  }

  output <- matrix(0, nrow = nrow(inputFuzzyNumbers), ncol = 4)

  # calculate fuzzy numbers

  output[,1] <- inputFuzzyNumbers[,2] - inputFuzzyNumbers[,1]

  output[,2] <- inputFuzzyNumbers[,2]

  output[,3] <- inputFuzzyNumbers[,3]

  output[,4] <- inputFuzzyNumbers[,4] + inputFuzzyNumbers[,3]

  return(output)
}

# transform matrix with fuzzy numbers to matrix with:
# mid the core, half increment of the core, left increment of the support, right increment of the support

TransformToSpreads <- function(inputFuzzyNumbers)
{
  # check if this is not single value

  if(is.vector(inputFuzzyNumbers))
  {
    inputFuzzyNumbers <- matrix(inputFuzzyNumbers,nrow = 1, ncol = 4)
  }

  output <- matrix(0, nrow = nrow(inputFuzzyNumbers), ncol = 4)

  # calculate mid, spread and increments

  output[,1] <- (inputFuzzyNumbers[,2] + inputFuzzyNumbers[,3]) / 2

  output[,2] <- inputFuzzyNumbers[,3] -  output[,1]

  output[,3] <- inputFuzzyNumbers[,2] - inputFuzzyNumbers[,1]

  output[,4] <- inputFuzzyNumbers[,4] - inputFuzzyNumbers[,3]


  return(output)

}

# transform matrix with:
# left end of the core, right end of the core, left increment of the support, right increment of the support
# to matrix with:
# mid of the core, half increment of the core, left increment of the support, right increment of the support



TransformIncreasesToSpreads <- function(inputFuzzyNumbers)
{
  # check if this is not single value

  if(is.vector(inputFuzzyNumbers))
  {
    inputFuzzyNumbers <- matrix(inputFuzzyNumbers,nrow = 1, ncol = 4)
  }

  output <- matrix(0, nrow = nrow(inputFuzzyNumbers), ncol = 4)

  # calculate mid of the core, half increment of the core, left increment of the support, right increment of the support

  output[,1] <- (inputFuzzyNumbers[,2] + inputFuzzyNumbers[,3]) / 2

  output[,2] <- inputFuzzyNumbers[,3] -  output[,1]

  output[,3] <- inputFuzzyNumbers[,1]

  output[,4] <- inputFuzzyNumbers[,4]


  return(output)

}

# transform matrix with fuzzy numbers to matrix with:
# mid of the core, spread of the core, mid of the support, spread of the support


TransformToMidSpreads <- function(inputFuzzyNumbers)
{
  # check if this is not single value

  if(is.vector(inputFuzzyNumbers))
  {
    inputFuzzyNumbers <- matrix(inputFuzzyNumbers,nrow = 1, ncol = 4)
  }

  output <- matrix(0, nrow = nrow(inputFuzzyNumbers), ncol = 4)

  # calculate mids/spreads

  output[,1] <- (inputFuzzyNumbers[,2] + inputFuzzyNumbers[,3])/2

  output[,2] <- (inputFuzzyNumbers[,3] - inputFuzzyNumbers[,2])/2

  output[,3] <- (inputFuzzyNumbers[,1] + inputFuzzyNumbers[,4])/2

  output[,4] <- (inputFuzzyNumbers[,4] - inputFuzzyNumbers[,1])/2


  return(output)

}

# transform matrix with:
# mid of the core, half increment of the core, left increment of the support, right increment of the support
# to matrix with fuzzy numbers


TransformFromSpreads <- function(inputSpreads)
{

  # check if this is not single value

  if(is.vector(inputSpreads))
  {
    inputSpreads <- matrix(inputSpreads,nrow = 1, ncol = 4)
  }

  output <- matrix(0, nrow = nrow(inputSpreads), ncol = 4)

  # calculate fuzzy numbers

  output[,1] <- inputSpreads[,1] - inputSpreads[,2] - inputSpreads[,3]

  output[,2] <- inputSpreads[,1] - inputSpreads[,2]

  output[,3] <- inputSpreads[,1] + inputSpreads[,2]

  output[,4] <- inputSpreads[,1] + inputSpreads[,2] + inputSpreads[,4]


  return(output)
}

# transform matrix with fuzzy numbers to matrix with:
# left increment of the support, left end of the core, increment of the core, increment of the right support


TransformToAllSpreads <- function(inputSpreads)
{

  # check if this is not single value

  if(is.vector(inputSpreads))
  {
    inputSpreads <- matrix(inputSpreads,nrow = 1, ncol = 4)
  }

  output <- matrix(0, nrow = nrow(inputSpreads), ncol = 4)

  # calculate left increment of the support, left end of the core, increment of the core, increment of the right support

  output[,1] <- inputSpreads[,2] - inputSpreads[,1]

  output[,2] <- inputSpreads[,2]

  output[,3] <- inputSpreads[,3] - inputSpreads[,2]

  output[,4] <- inputSpreads[,4] - inputSpreads[,3]


  return(output)
}








# check if the initial value is triangular fuzzy number

IsTriangular <- function(fuzzyNumber)
{
  if(fuzzyNumber[2]==fuzzyNumber[3])
  {
    return(TRUE)

  } else {

    return(FALSE)
  }
}


# check if the initial value is correct fuzzy number

IsFuzzy <- function(fuzzyNumber)
{

  if((fuzzyNumber[1] <= fuzzyNumber[2]) & (fuzzyNumber[2] <= fuzzyNumber[3]) & (fuzzyNumber[3] <= fuzzyNumber[4]))
  {

    return(TRUE)

  } else {

    return(FALSE)

  }


}

# checking for correctness of the initial sample

ParameterCheckForInitialSample <- function(initialSample)
{
  # checking the respective form of matrix

  if(!is.matrix(initialSample))
  {
    stop("Values in initialSample are not given as a matrix/vector")
  }

  # checking the number of columns

  if(!(ncol(initialSample)%%4 == 0))
  {
    stop("There should be 4 columns in initial sample")
  }

  # checking if there are NA's

  if(any(is.na(initialSample)))
  {
    stop("There are some NA in initial sample")
  }

  # checking if values are numeric

  if(!is.numeric(initialSample))
  {
    stop("Some values in initial sample are not numeric ones")
  }



}


# function to check if the parameter is given by the integer

IfInteger <- function(x)
{
  if(is.numeric(x))
  {
    test <- all.equal(x, as.integer(x), check.attributes = FALSE)

    if(test == TRUE)
    { return(TRUE) }
    else { return(FALSE) }
  }

  else { return(FALSE) }
}



# general checking of correctness of the initial parameters for resampling

ParameterCheckForResampling <- function(initialSample, b)
{
  # checking the initial sample

  ParameterCheckForInitialSample(initialSample)

  # checking b parameter

  if(!IfInteger(b) | b <= 0)
  {
    stop("Parameter b should be integer value and > 0")
  }


}






# calculate w density for w-method

WFunction <- function(initialValues)
{
  m <- length(initialValues)

  probabilities <- rep(1/m, m+1)

  probabilities[1] <- 1/(2*m)

  probabilities[m+1] <- 1/(2*m)

  # cat("probabilities", probabilities, "\n")

  number <- sample(m+1, 1, prob=probabilities)

  # cat("number", number)

  if((number==1) | (number==m+1))
  {
    if(number==1)
    {
      output <- initialValues[1]
    }
    else {

      output <- initialValues[number-1]
    }


  } else
  {
    output <- runif(1, initialValues[number-1], initialValues[number])
  }

  # cat(" output", output, "\n")

  return(output)
}


# mean for fuzzy number

MeanFuzzyNumber <- function(inputSample)
{
  # check if we have vector

  if(is.vector(inputSample))
  {

    return(inputSample)

  } else {

    return(colMeans(inputSample))

  }


}

# additional calculations for C test

ValueA <- function(x,y, theta)
{
  return((BertoluzzaDistance(MeanFuzzyNumber(x), MeanFuzzyNumber(y), theta))^2)
}

ValueB <- function(x, theta)
{
  n <- nrow(x)

  return(n / (n-1) * mean(BertoluzzaDistance(x, MeanFuzzyNumber(x), theta)^2))

}

# parameter mu0 check for validity

ParameterMu0Check <- function(mu0, increases = FALSE)
{
  # checking if this is a vector

  if(!is.vector(mu0) | length(mu0) != 4)
  {
    stop("Parameter mean should be a vector of length 4")
  }

  # checking if there are NA's

  if(any(is.na(mu0)))
  {
    stop("There are some NA in parameter mean")
  }

  # checking if values are numeric

  if(!is.numeric(mu0))
  {
    stop("Parameter mean should be a numeric value")
  }

  if(increases)
  {
    mu0 <- TransformFromIncreases(mu0)
  }

  # checking consistency of fuzzy numbers

  if(!IsFuzzy(mu0))
  {
    stop("Parameter mean is not a correct fuzzy number")
  }

  return(TRUE)

}
