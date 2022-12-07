test_that("Function returns correct values", {

  # starting values

  fuzzyValuesA <- matrix(c(0.25,0.5,1,1.25,0.75,1,1.5,2.2,-1,0,0,2,0,1,2,2.2),ncol = 4,byrow = TRUE)

  fuzzyValuesIncA <- matrix(c(0.25,0.5,1,0.25,0.25,1,1.5,0.7,1,0,0,2,1,1,2,0.2), ncol = 4,byrow = TRUE)

  # tests

  expect_equal(CalculateAmbiguityR(fuzzyValuesA[1,]), 0.1666667, tolerance = 1e-6)

  expect_equal(CalculateAmbiguityR(fuzzyValuesIncA[1,], increases = TRUE), 0.1666667, tolerance = 1e-6)

  expect_equal(CalculateAmbiguityR(fuzzyValuesA), c(0.1666667, 0.2416667, 0.3333333, 0.2833333), tolerance = 1e-6)

  expect_equal(CalculateAmbiguityR(fuzzyValuesIncA, increases = TRUE), c(0.1666667, 0.2416667, 0.3333333, 0.2833333),
               tolerance = 1e-6)

})



test_that("Function reports errors", {

  # starting values

  fuzzyValuesNA <- matrix(c(NA,0.5,1,1.25,0.75,1,1.5,2.2,-1,0,0,2),ncol = 4,byrow = TRUE)

  fuzzyValuesNotFuzzy <- matrix(c(7,0.5,1,1.25,0.75,1,1.5,2.2,-1,0,0,2),ncol = 4,byrow = TRUE)

  fuzzyValues5Elem <- matrix(c(0.25,0.5,1,0.25,0.25,1,1.5,0.7,1,0), ncol = 5,byrow = TRUE)

  strangeValues <- c("c", TRUE, 4, -3)

  arrNotMatrix <- array(c(5,9,3,10,11,12,13,14,15),dim = c(3,3,2))

  InfInVector <- c(Inf,0,2,3)

  fuzzyValues <- matrix(c(0.25,0.5,1,1.25,0.75,1,1.5,2.2,-1,0,0,2),ncol = 4,byrow = TRUE)

  # tests

  expect_error(CalculateAmbiguityR(fuzzyValuesNA), "There are some NA in initial sample")

  expect_error(CalculateAmbiguityR(fuzzyValuesNotFuzzy), "Some initial values are not correct fuzzy numbers")

  expect_error(CalculateAmbiguityR(fuzzyValues5Elem), "There should be 4 columns in initial sample")

  expect_error(CalculateAmbiguityR(strangeValues), "Some values in initial sample are not numeric ones")

  expect_error(CalculateAmbiguityR(arrNotMatrix), "Values in initialSample are not given as a matrix/vector")

  expect_error(CalculateAmbiguityR(InfInVector), "Some initial values are not correct fuzzy numbers")

  expect_error(CalculateAmbiguityR(fuzzyValues,increases = "15"), "Parameter increases should have logical value")

})


