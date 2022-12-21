test_that("Function returns correct values", {

  # starting values

  fuzzyValuesA <- matrix(c(0.25,0.5,1,1.25,0.75,1,1.5,2.2,-1,0,0,2,0,1,2,2.2),ncol = 4,byrow = TRUE)

  fuzzyValuesIncA <- matrix(c(0.25,0.5,1,0.25,0.25,1,1.5,0.7,1,0,0,2,1,1,2,0.2), ncol = 4,byrow = TRUE)

  fuzzyValuesMean <- matrix(c(0.5,1,2,4,0.6,1.1,2.1,3.9,0.45,0.9,2,4.1,0.5,1.1,1.95,3.9),ncol = 4,byrow = TRUE)

  # tests

  expect_snapshot(

    {set.seed(1234567)

      OneSampleCTest(fuzzyValuesA, mu_0 = c(-0.5,-0.1,0,0.5))}
  )

  expect_snapshot(

    {set.seed(1234567)

      OneSampleCTest(fuzzyValuesA, mu_0 = c(-0.5,-0.1,0,0.5),numberOfSamples = 1000)}
  )

  expect_snapshot(

    {set.seed(1234567)

      OneSampleCTest(fuzzyValuesA, mu_0 = c(-0.5,-0.1,0,0.5),theta=1)}
  )

  expect_snapshot(

    {set.seed(1234567)

      OneSampleCTest(fuzzyValuesA, mu_0 = c(-0.5,-0.1,0,0.5),resamplingMethod = "EWMethod")}
  )


  expect_snapshot(

    {set.seed(1234567)

      OneSampleCTest(fuzzyValuesIncA, mu_0 = c(0.5,-0.1,0,0.5),resamplingMethod = "EWMethod",
                     increases = TRUE)}
  )

  expect_snapshot(

    {set.seed(1234567)

      OneSampleCTest(fuzzyValuesMean, mu_0 = c(0.5,1,2,4))}
  )

  expect_snapshot(

    {set.seed(1234567)

      OneSampleCTest(fuzzyValuesMean, mu_0 = c(0.5,1,2,4),numberOfSamples = 500, resamplingMethod = "VAMethod")}
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

  expect_error(OneSampleCTest(initialSample = fuzzyValuesNA,mu_0 = c(0.5,1,2,4)), "There are some NA in initial sample")

  expect_error(OneSampleCTest(initialSample = fuzzyValuesA,mu_0 = NA), "Parameter mean should be a vector of length 4")

  expect_error(OneSampleCTest(initialSample = fuzzyValuesNotFuzzy,mu_0 = c(0.5,1,2,4)),
               "Some values in initial sample are not correct fuzzy numbers")

  expect_error(OneSampleCTest(initialSample = fuzzyValuesA,mu_0 = c(2,1,3,4)),
               "Parameter mean is not a correct fuzzy number")

  expect_error(OneSampleCTest(fuzzyValuesA, mu_0 = c(-0.5,-0.1,0,0.5), theta = -1),
               "Parameter theta should be double value and > 0")

  expect_error(OneSampleCTest(fuzzyValuesA, mu_0 = c(-0.5,-0.1,0,0.5), theta = NA),
               "Parameter theta should be double value and > 0")

  expect_error(OneSampleCTest(fuzzyValuesA, mu_0 = c(-0.5,-0.1,0,0.5), resamplingMethod = "uknown"),
               "Parameter resamplingMethod should be a proper name of the resampling method")

  expect_error(OneSampleCTest(fuzzyValuesA, mu_0 = c(-0.5,-0.1,0,0.5), resamplingMethod = NA),
               "Parameter resamplingMethod should be a proper name of the resampling method")

  expect_error(OneSampleCTest(fuzzyValuesA, mu_0 = c(-0.5,-0.1,0,0.5), increases = "c"),
               "Parameter increases should have logical value")

})
