test_that("Function returns correct values", {


  # tests

  expect_snapshot(

    {set.seed(1234567)

      ComparisonOneSampleCTest(generator="GeneratorNU",mu_0 = c(-0.4,-0.1,0.1,0.4), sampleSize = 10,numberOfSamples = 10,
                               initialSamples = 10,mu = 0, sigma = 1,a = 0.2,b = 0.6)}
  )

  expect_snapshot(

    {set.seed(1234567)

      ComparisonOneSampleCTest(generator="GeneratorNU",mu_0 = c(-0.4,-0.1,0.1,0.4), sampleSize = 10,numberOfSamples = 10,
                               initialSamples = 10,mu = 0, sigma = 1,a = 0.2,b = 0.6, significance = 0.5)}
  )

  expect_snapshot(

    {set.seed(1234567)

      ComparisonOneSampleCTest(generator="GeneratorNExpUU",mu_0 = c(-0.4,-0.1,0.1,0.4), sampleSize = 10,numberOfSamples = 20,
                               initialSamples = 20,mu = 0, sigma = 1,lambda = 1, b = 0.5,c = 0.5, significance = 0.25)}
  )


  expect_snapshot(

    {set.seed(1234567)

      ComparisonOneSampleCTest(generator="GeneratorNU",mu_0 = c(-0.4,-0.1,0.1,0.4), shift = 0.5,
                               sampleSize = 10,numberOfSamples = 10,
                               initialSamples = 10,mu = 0, sigma = 1,a = 0.2,b = 0.6)}
  )



})



test_that("Function reports errors", {



  # tests

  expect_error(ComparisonOneSampleCTest(generator="unknown",mu_0 = c(-0.4,-0.1,0.1,0.4), sampleSize = 10,numberOfSamples = 10,
                                        initialSamples = 10,mu = 0, sigma = 1,a = 0.2,b = 0.6),
               "Parameter generator should be a proper name of the sampling generator")

  expect_error(ComparisonOneSampleCTest(generator=NA,mu_0 = c(-0.4,-0.1,0.1,0.4), sampleSize = 10,numberOfSamples = 10,
                                        initialSamples = 10,mu = 0, sigma = 1,a = 0.2,b = 0.6),
               "Parameter generator should be a proper name of the sampling generator")

  expect_error(ComparisonOneSampleCTest(generator="GeneratorNU",mu_0 = c(2,1,3,4), sampleSize = 10,numberOfSamples = 10,
                                        initialSamples = 10,mu = 0, sigma = 1,a = 0.2,b = 0.6),
               "Parameter mean is not a correct fuzzy number")

  expect_error(ComparisonOneSampleCTest(generator="GeneratorNU",mu_0 = c(-0.4,-0.1,0.1,0.4), shift = "c",
                                        sampleSize = 10,numberOfSamples = 10,
                                        initialSamples = 10,mu = 0, sigma = 1,a = 0.2,b = 0.6),
               "Parameter shift should be double, finite value")

  expect_error(ComparisonOneSampleCTest(generator="GeneratorNU",mu_0 = c(-0.4,-0.1,0.1,0.4), shift = Inf,
                                        sampleSize = 10,numberOfSamples = 10,
                                        initialSamples = 10,mu = 0, sigma = 1,a = 0.2,b = 0.6),
               "Parameter shift should be double, finite value")

  expect_error(ComparisonOneSampleCTest(generator="GeneratorNU",mu_0 = c(-0.4,-0.1,0.1,0.4),
                                        sampleSize = 10,numberOfSamples = 10,
                                        initialSamples = 10,mu = 0, sigma = 1,a = 0.2,b = 0.6, significance = -2),
               "Parameter significance should be double value and >= 0")

  expect_error(ComparisonOneSampleCTest(generator="GeneratorNU",mu_0 = c(-0.4,-0.1,0.1,0.4),
                                        sampleSize = 10,numberOfSamples = 10,
                                        initialSamples = 10,mu = 0, sigma = 1,a = 0.2,b = 0.6, significance = "c"),
               "Parameter significance should be double value and >= 0")

  expect_error(ComparisonOneSampleCTest(generator="GeneratorNU",mu_0 = c(-0.4,-0.1,0.1,0.4), sampleSize = NA,
                                        numberOfSamples = 10,
                                        initialSamples = 10,mu = 0, sigma = 1,a = 0.2,b = 0.6),
               "Parameter sampleSize should be double value and >= 0")



})

