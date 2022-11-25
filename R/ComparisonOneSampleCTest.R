# comparison of outputs for one sample C test for all resampling methods

ComparisonOneSampleCTest<- function(generator, mu_0, shift=0, sampleSize = 10,
                                    numberOfSamples = 10, initialSamples = 100, theta = 1/3,
                                    significance = 0.05, ...)
{
  # print(as.list(match.call()))

  # checking initialSamples parameter

  if(!IfInteger(initialSamples) | n < 1)
  {
    stop("Parameter initialSamples should be integer value and => 1")
  }


  # checking shift parameter

  if(!is.double(shift))
  {
    stop("Parameter shift should be double value")
  }

  # vector prealocation

  outputVector <- rep(0, length(resamplingMethods))

  # progress bar

  pb <- txtProgressBar (1, initialSamples, style = 3)

  # print(as.list(match.call()))


  # main loop (initial samples)

  for (i in 1:initialSamples) {


    # generate initial sample with possible shift

    sample <- generator(n=sampleSize,...) + shift

    # find p-value for each method

    for (j in 1:length(resamplingMethods)) {

      pvalue <- OneSampleCTest(initialSample = sample, mu_0,
                               numberOfSamples, theta,
                               resamplingMethod = get(resamplingMethods[j]), incresases = FALSE)

      # check the output of the bootstrapped test

      if(pvalue < significance)
      {
        # reject H_0

        outputVector[j] <- outputVector[j] + 1
      }


      # cat("skumulowane odrzucanie nr: ", j, " rowne: ", outputVector[j], "\n")

    }



    setTxtProgressBar(pb, i)

  }

  # average the output

  outputVector <- outputVector / initialSamples

  names(outputVector) <- resamplingMethods

  cat("\n")

  return(outputVector)
}
