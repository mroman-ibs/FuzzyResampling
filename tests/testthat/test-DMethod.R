test_that("Function returns correct values", {

  # starting values

  fuzzyValuesA <- matrix(c(0.25,0.5,1,1.25,0.75,1,1.5,2.2,-1,0,0,2,0,1,2,2.2),ncol = 4,byrow = TRUE)

  fuzzyValuesIncA <- matrix(c(0.25,0.5,1,0.25,0.25,1,1.5,0.7,1,0,0,2,1,1,2,0.2), ncol = 4,byrow = TRUE)

  # snapshots tests

  expect_snapshot(

    {set.seed(1234567)

      DMethod(fuzzyValuesA)}
  )


  expect_snapshot(

    {set.seed(1234567)

      DMethod(fuzzyValuesA, b=5)}
  )


  expect_snapshot(

    {set.seed(1234567)

      DMethod(fuzzyValuesIncA, increases = TRUE)}
  )


  expect_snapshot(

    {set.seed(1234567)

      DMethod(fuzzyValuesIncA, b=5, increases = TRUE)}
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

  expect_error(DMethod(fuzzyValuesNA), "There are some NA in initial sample")

  expect_error(DMethod(fuzzyValuesNotFuzzy), "Some values in  initial sample are not correct fuzzy numbers")

  expect_error(DMethod(fuzzyValues5Elem), "There should be 4 columns in initial sample")

  expect_error(DMethod(strangeValues), "Some values in initial sample are not numeric ones")

  expect_error(DMethod(arrNotMatrix), "Values in initialSample are not given as a matrix/vector")

  expect_error(DMethod(InfInVector), "Some values in  initial sample are not correct fuzzy numbers")

  expect_error(DMethod(fuzzyValuesA,increases = "15"), "Parameter increases should have logical value")

  expect_error(DMethod(fuzzyValuesA, b = -4), "Parameter b should be integer value and > 0")

  expect_error(DMethod(fuzzyValuesA, b = 4.3), "Parameter b should be integer value and > 0")

})

