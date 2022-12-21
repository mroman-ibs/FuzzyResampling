test_that("Function returns correct values", {


  # tests

  expect_snapshot(

    {set.seed(1234567)

      ComparePowerOneSampleCTest("GeneratorNU",mu_0 = c(-0.4,-0.1,0.1,0.4), shiftVector = c(0,0.5),
                                 initialSamples = 20, numberOfSamples = 20,
                                 mu = 0, sigma = 1, a = 0.2, b = 0.6)}
  )

  expect_snapshot(

    {set.seed(1234567)

      ComparePowerOneSampleCTest("GeneratorNExpUU",mu_0 = c(-0.4,-0.1,0.1,0.4), shiftVector = c(-0.5,0.5),
                                 initialSamples = 20, numberOfSamples = 20,
                                 mu = 0, sigma = 1, lambda = 1, b = 0.1, c = 0.4)}
  )





})



test_that("Function reports errors", {



  # tests

  expect_error(ComparePowerOneSampleCTest("GeneratorNU",mu_0 = c(-0.4,-0.1,0.1,0.4), shiftVector = c(NA,0.5),
                                          initialSamples = 20, numberOfSamples = 20,
                                          mu = 0, sigma = 1, a = 0.2, b = 0.6),
               "Parameter shiftVector should be a numeric, finite vector")

  expect_error(ComparePowerOneSampleCTest("GeneratorNU",mu_0 = c(-0.4,-0.1,0.1,0.4), shiftVector = c(Inf,0.5),
                                          initialSamples = 20, numberOfSamples = 20,
                                          mu = 0, sigma = 1, a = 0.2, b = 0.6),
               "Parameter shiftVector should be a numeric, finite vector")

  expect_error(ComparePowerOneSampleCTest("GeneratorNU",mu_0 = c(-0.4,-0.1,0.1,0.4), shiftVector = matrix(c(1,2,3,4),ncol = 2),
                                          initialSamples = 20, numberOfSamples = 20,
                                          mu = 0, sigma = 1, a = 0.2, b = 0.6),
               "Parameter shiftVector should be a numeric, finite vector")

  expect_error(ComparePowerOneSampleCTest("unknown",mu_0 = c(-0.4,-0.1,0.1,0.4), shiftVector = c(0,0.5),
                                          initialSamples = 20, numberOfSamples = 20,
                                          mu = 0, sigma = 1, a = 0.2, b = 0.6),
               "Parameter generator should be a proper name of the sampling generator")

  expect_error(ComparePowerOneSampleCTest("GeneratorNU",mu_0 = c(-0.4,-0.1,0.1,0.4), shiftVector = c("c",0.5),
                                          initialSamples = 20, numberOfSamples = 20,
                                          mu = 0, sigma = 1, a = 0.2, b = 0.6),
               "Parameter shiftVector should be a numeric, finite vector")




})


