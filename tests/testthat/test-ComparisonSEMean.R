test_that("Function returns correct values", {


  # tests

  expect_snapshot(

    {set.seed(1234567)

      ComparisonSEMean(generator = "GeneratorNU",sampleSize = 10, numberOfSamples = 100,
                       repetitions = 10,mu = 0, sigma = 1,a = 0.5, b = 1)}
  )

  expect_snapshot(

    {set.seed(1234567)

      ComparisonSEMean(generator = "GeneratorNExpUU",sampleSize = 20, numberOfSamples = 50,
                       repetitions = 10,mu = 0,sigma = 1, lambda = 1, b = 0.1, c = 0.4)}
  )





})



test_that("Function reports errors", {



  # tests

  expect_error(ComparisonSEMean(generator = NA,sampleSize = 20, numberOfSamples = 50,
                                repetitions = 10,mu = 0,sigma = 1, lambda = 1, b = 0.1, c = 0.4),
               "Parameter generator should be a proper name of the sampling generator")

  expect_error(ComparisonSEMean(generator = "GeneratorNU",sampleSize = -2, numberOfSamples = 100,
                                repetitions = 10,mu = 0, sigma = 1,a = 0.5, b = 1),
               "Parameter sampleSize should be integer value and > 1")

  expect_error(ComparisonSEMean(generator = "GeneratorNU",sampleSize = NA, numberOfSamples = 100,
                                repetitions = 10,mu = 0, sigma = 1,a = 0.5, b = 1),
               "Parameter sampleSize should be integer value and > 1")

  expect_error(ComparisonSEMean(generator = "GeneratorNU",sampleSize = 10, numberOfSamples = 23.5,
                                repetitions = 10,mu = 0, sigma = 1,a = 0.5, b = 1),
               "Parameter sampleSize should be integer value and > 1")

  expect_error(ComparisonSEMean(generator = "GeneratorNU",sampleSize = 10, numberOfSamples = "c",
                                repetitions = 10,mu = 0, sigma = 1,a = 0.5, b = 1),
               "Parameter sampleSize should be integer value and > 1")




})

