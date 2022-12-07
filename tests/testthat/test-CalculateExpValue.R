test_that("Function returns correct values", {

  # starting values

  fuzzyValuesA <- matrix(c(0.25,0.5,1,1.25,0.75,1,1.5,2.2,-1,0,0,2,0,1,2,2.2),ncol = 4,byrow = TRUE)

  fuzzyValuesIncA <- matrix(c(0.25,0.5,1,0.25,0.25,1,1.5,0.7,1,0,0,2,1,1,2,0.2), ncol = 4,byrow = TRUE)

  # tests

  expect_equal(CalculateExpValue(fuzzyValuesA[1,]), 0.75, tolerance = 1e-6)

  expect_equal(CalculateExpValue(fuzzyValuesIncA[1,], increases = TRUE), 0.75, tolerance = 1e-6)

  expect_equal(CalculateExpValue(fuzzyValuesA), c(0.7500, 1.3625, 0.2500, 1.3000), tolerance = 1e-6)

  expect_equal(CalculateExpValue(fuzzyValuesIncA, increases = TRUE), c(0.7500, 1.3625, 0.2500, 1.3000),
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

  expect_error(CalculateExpValue(fuzzyValuesNA), "There are some NA in initial sample")

  expect_error(CalculateExpValue(fuzzyValuesNotFuzzy), "Some initial values are not correct fuzzy numbers")

  expect_error(CalculateExpValue(fuzzyValues5Elem), "There should be 4 columns in initial sample")

  expect_error(CalculateExpValue(strangeValues), "Some values in initial sample are not numeric ones")

  expect_error(CalculateExpValue(arrNotMatrix), "Values in initialSample are not given as a matrix/vector")

  expect_error(CalculateExpValue(InfInVector), "Some initial values are not correct fuzzy numbers")

  expect_error(CalculateExpValue(fuzzyValues,increases = "15"), "Parameter increases should have logical value")

})


