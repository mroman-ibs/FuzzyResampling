
# calculate SE/MSE for the mean

SEResamplingMean <- function(initialSample, resamplingMethod=classicalBootstrap, repetitions = 100, trueMean = NA, theta = 1/3,
                             increases = FALSE)
{

  # matrix for means

  meanMatrix <- matrix(NA, nrow = repetitions, ncol = 4)

  # repeat resampling procedure

  for (i in 1:repetitions)
  {
    # generate secondary sample

    secondarySample <- resamplingMethod(initialSample, b = nrow(initialSample), increases)

    # cat("\n secondarySample: \n")
    # print(as.matrix(secondarySample))

    # find mean

    meanMatrix[i,] <- apply(secondarySample, 2, mean)

    # cat("mean nr: ", i, " is equal to: ", meanMatrix[i,], "\n")


  }



  if(anyNA(trueMean) == TRUE)
  {

    # averaging means

    overallMean <- apply(meanMatrix, 2, mean)

    # cat("\n overall mean: ", overallMean, "\n")

    # calculate SE

    SEmean <- sum((BertoluzzaDistance(meanMatrix, overallMean, theta))^2)

  } else {

    # calculate MSE

    SEmean <- sum((BertoluzzaDistance(meanMatrix, trueMean, theta))^2)

    overallMean <- trueMean

  }




  SEmean <- sqrt(SEmean / (repetitions - 1))

  output <- list("mean" = overallMean, "SE" = SEmean)

  return(output)

}




resamplingMethods <- c("classicalBootstrap", "VAmethod", "EWmethod",
                       "VAFmethod", "dmethod", "wmethod")

