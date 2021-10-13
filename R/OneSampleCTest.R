# C bootstrapped test for one mean

OneSampleCTest <- function(initialSample, mu_0,
                           numberOfSamples = 100, theta = 1/3, resamplingMethod = classicalBootstrap, incresases = FALSE)
{
  # calculation of C test without bootstrap (step 1)

  n <- nrow(initialSample)

  standardStatistics <- valueA(initialSample, mu_0, theta) / valueB(initialSample, theta)

  # prepare vector

  bootstrappedStatistics <- rep(0,numberOfSamples)

  # pb <- txtProgressBar (1, numberOfSamples, style = 3)

  for (i in 1:numberOfSamples) {

    # generate bootstrap sample (step 3)

    bootstrapSample <- resamplingMethod(initialSample, n,  incresases)

    # calculate bootstrapped statistics (step 4)

    bootstrappedStatistics[i] <- valueA(bootstrapSample, initialSample, theta) /
      valueB(bootstrapSample, theta)

    # setTxtProgressBar(pb, i)

  }

  # cat("\n")

  # cat("standardStatistics: ", standardStatistics, ", bootstrappedStatistics: ", bootstrappedStatistics, "\n")

  # calculate p-value

  pvalue <- mean(standardStatistics < bootstrappedStatistics)

  return(pvalue)
}

