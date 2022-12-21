test_that("Function returns correct values", {

  # starting values

  fuzzyValuesA <- matrix(c(0.25,0.5,1,1.25,0.75,1,1.5,2.2,-1,0,0,2,0,1,2,2.2),ncol = 4,byrow = TRUE)

  fuzzyValuesIncA <- matrix(c(0.25,0.5,1,0.25,0.25,1,1.5,0.7,1,0,0,2,1,1,2,0.2), ncol = 4,byrow = TRUE)


  # tests

  expect_snapshot(

    {set.seed(1234567)

      SEResamplingMean(fuzzyValuesA)}
  )

  expect_snapshot(

    {set.seed(1234567)

      SEResamplingMean(fuzzyValuesA, trueMean = c(0,0.5,1,2))}
  )

  expect_snapshot(

    {set.seed(1234567)

      SEResamplingMean(fuzzyValuesA, resamplingMethod = "EWMethod")}
  )

  expect_snapshot(

    {set.seed(1234567)

      SEResamplingMean(fuzzyValuesA, resamplingMethod = "EWMethod", trueMean = c(0,0.5,1,2))}
  )


  expect_snapshot(

    {set.seed(1234567)

      SEResamplingMean(fuzzyValuesIncA, resamplingMethod = "EWMethod", trueMean = fuzzyValuesIncA[1,], increases = TRUE)}
  )

  expect_snapshot(

    {set.seed(1234567)

      SEResamplingMean(fuzzyValuesA, repetitions = 200)}
  )

  expect_snapshot(

    {set.seed(1234567)

      SEResamplingMean(fuzzyValuesA, theta = 0.5)}
  )



})


test_that("Function reports errors", {

  # starting values

  fuzzyValuesA <- matrix(c(0.25,0.5,1,1.25,0.75,1,1.5,2.2,-1,0,0,2,0,1,2,2.2),ncol = 4,byrow = TRUE)

  fuzzyValuesNA <- matrix(c(NA,0.5,1,1.25,0.75,1,1.5,2.2,-1,0,0,2),ncol = 4,byrow = TRUE)

  fuzzyValuesNotFuzzy <- matrix(c(7,0.5,1,1.25,0.75,1,1.5,2.2,-1,0,0,2),ncol = 4,byrow = TRUE)

  fuzzyValues5Elem <- matrix(c(0.25,0.5,1,0.25,0.25,1,1.5,0.7,1,0), ncol = 5,byrow = TRUE)

  strangeValues <- c("c", TRUE, 4, -3)

  arrNotMatrix <- array(c(5,9,3,10,11,12,13,14,15),dim = c(3,3,2))

  InfInVector <- c(Inf,0,2,3)

  # tests

  expect_error(SEResamplingMean(fuzzyValuesNA),
               "There are some NA in initial sample")

  expect_error(SEResamplingMean(fuzzyValuesA, trueMean = strangeValues),
               "Parameter mean should be a numeric value")

  expect_error(SEResamplingMean(fuzzyValuesA, trueMean = InfInVector),
               "Parameter mean is not a correct fuzzy number")

  expect_error(SEResamplingMean(fuzzyValuesA, trueMean = c(2,1,0,4)),
               "Parameter mean is not a correct fuzzy number")

  expect_error(SEResamplingMean(fuzzyValuesA, repetitions = -2),
               "Parameter repetitions should be integer value and > 1")

  expect_error(SEResamplingMean(fuzzyValuesA, repetitions = 3.5),
               "Parameter repetitions should be integer value and > 1")

  expect_error(SEResamplingMean(fuzzyValuesA, theta = -1),
               "Parameter theta should be double value and > 0")

  expect_error(SEResamplingMean(fuzzyValuesA, increases = "v"),
               "Parameter increases should have logical value")

  expect_error(SEResamplingMean(fuzzyValuesA, increases = NA),
               "missing value where TRUE/FALSE needed")

})


