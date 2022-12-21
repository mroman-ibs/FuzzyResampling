test_that("Function returns correct values", {

  # starting values

  fuzzyValuesA <- matrix(c(0.25,0.5,1,1.25,0.75,1,1.5,2.2,-1,0,0,2,0,1,2,2.2),ncol = 4,byrow = TRUE)

  fuzzyValuesAPlus <- fuzzyValuesA + 0.5

  fuzzyValuesIncA <- matrix(c(0.25,0.5,1,0.25,0.25,1,1.5,0.7,1,0,0,2,1,1,2,0.2), ncol = 4,byrow = TRUE)


  # tests

  expect_snapshot(

    {set.seed(1234567)

      TwoSampleCTest(fuzzyValuesA,fuzzyValuesA)}
  )

  expect_snapshot(

    {set.seed(1234567)

      TwoSampleCTest(fuzzyValuesA,fuzzyValuesAPlus)}
  )

  expect_snapshot(

    {set.seed(1234567)

      TwoSampleCTest(fuzzyValuesA,fuzzyValuesAPlus,numberOfSamples = 500)}
  )

  expect_snapshot(

    {set.seed(1234567)

      TwoSampleCTest(fuzzyValuesA,fuzzyValuesAPlus, theta = 0.5)}
  )


  expect_snapshot(

    {set.seed(1234567)

      TwoSampleCTest(fuzzyValuesA,fuzzyValuesAPlus, theta = 0.5, resamplingMethod = "VAAMethod")}
  )

  expect_snapshot(

    {set.seed(1234567)

      TwoSampleCTest(fuzzyValuesIncA,fuzzyValuesIncA,increases = TRUE)}
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

  expect_error(TwoSampleCTest(initialSample1=fuzzyValuesNA, initialSample2 = fuzzyValuesA),
               "There are some NA in initial sample")

  expect_error(TwoSampleCTest(initialSample1=fuzzyValuesA, initialSample2 = fuzzyValuesNA),
               "There are some NA in initial sample")

  expect_error(TwoSampleCTest(initialSample1=fuzzyValuesNotFuzzy, initialSample2 = fuzzyValuesA),
               "Some values in fuzzyNumber1 parameter are not correct fuzzy numbers")

  expect_error(TwoSampleCTest(initialSample1=fuzzyValuesA, initialSample2 = fuzzyValuesNotFuzzy),
               "Some values in fuzzyNumber2 parameter are not correct fuzzy numbers")

  expect_error(TwoSampleCTest(initialSample1=fuzzyValuesA, initialSample2 = fuzzyValuesA,numberOfSamples = -3),
               "Parameter numberOfSamples should be integer value and > 1")

  expect_error(TwoSampleCTest(initialSample1=fuzzyValuesA, initialSample2 = fuzzyValuesA,numberOfSamples = "c"),
               "Parameter numberOfSamples should be integer value and > 1")

  expect_error(TwoSampleCTest(initialSample1=fuzzyValuesA, initialSample2 = fuzzyValuesA, theta = NA),
               "Parameter theta should be double value and > 0")

  expect_error(TwoSampleCTest(initialSample1=fuzzyValuesA, initialSample2 = fuzzyValuesA, resamplingMethod = 4),
               "Parameter resamplingMethod should be a proper name of the resampling method")

  expect_error(TwoSampleCTest(initialSample1=fuzzyValuesA, initialSample2 = fuzzyValuesA, increases = "c"),
               "Parameter increases should have logical value")

})

