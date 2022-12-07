test_that("Function returns correct values", {

  # starting values

  fuzzyValues <- matrix(c(0.25,0.5,1,1.25,0.75,1,1.5,2.2,-1,0,0,2),ncol = 4,byrow = TRUE)

  fuzzyValuesInc <- matrix(c(0.25,0.5,1,0.25,0.25,1,1.5,0.7,1,0,0,2), ncol = 4,byrow = TRUE)

  # tests

  expect_equal(CalculateAmbiguity(fuzzyValues[1,]), 0.3333333, tolerance = 1e-6)

  expect_equal(CalculateAmbiguity(fuzzyValuesInc[1,], increases = TRUE), 0.3333333, tolerance = 1e-6)

  expect_equal(CalculateAmbiguity(fuzzyValues), c(0.3333333, 0.4083333, 0.5000000), tolerance = 1e-6)

  expect_equal(CalculateAmbiguity(fuzzyValuesInc, increases = TRUE), c(0.3333333, 0.4083333, 0.5000000), tolerance = 1e-6)

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

  expect_error(CalculateAmbiguity(fuzzyValuesNA), "There are some NA in initial sample")

  expect_error(CalculateAmbiguity(fuzzyValuesNotFuzzy), "Some initial values are not correct fuzzy numbers")

  expect_error(CalculateAmbiguity(fuzzyValues5Elem), "There should be 4 columns in initial sample")

  expect_error(CalculateAmbiguity(strangeValues), "Some values in initial sample are not numeric ones")

  expect_error(CalculateAmbiguity(arrNotMatrix), "Values in initialSample are not given as a matrix/vector")

  expect_error(CalculateAmbiguity(InfInVector), "Some initial values are not correct fuzzy numbers")

  expect_error(CalculateAmbiguity(fuzzyValues,increases = "15"), "Parameter increases should have logical value")

})
