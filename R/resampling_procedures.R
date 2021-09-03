











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











