test_that("Function returns correct values", {

  # starting values

  fuzzyValuesA <- matrix(c(0.25,0.5,1,1.25,0.75,1,1.5,2.2,-1,0,0,2,0,1,2,2.2),ncol = 4,byrow = TRUE)

  fuzzyValuesIncA <- matrix(c(0.25,0.5,1,0.25,0.25,1,1.5,0.7,1,0,0,2,1,1,2,0.2), ncol = 4,byrow = TRUE)

  # tests

  expect_equal(BertoluzzaDistance(fuzzyValuesA[1,],fuzzyValuesA[2,]), 0.6204837, tolerance = 1e-6)

  expect_equal(BertoluzzaDistance(fuzzyValuesIncA[1,],fuzzyValuesIncA[2,], increases = TRUE), 0.6204837, tolerance = 1e-6)

  expect_equal(BertoluzzaDistance(fuzzyValuesA[1,],fuzzyValuesA), c(0.0000000, 0.6204837, 0.6009252, 0.6159906),
               tolerance = 1e-6)

  expect_equal(BertoluzzaDistance(fuzzyValuesIncA[1,],fuzzyValuesIncA, increases = TRUE),
               c(0.0000000, 0.6204837, 0.6009252, 0.6159906),
               tolerance = 1e-6)

  expect_equal(BertoluzzaDistance(fuzzyValuesA,fuzzyValuesA[2,]),
               c(0.6204837, 0.0000000, 1.1384688, 0.2635231),
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

  fuzzyValuesA <- matrix(c(0.25,0.5,1,1.25,0.75,1,1.5,2.2,-1,0,0,2,0,1,2,2.2),ncol = 4,byrow = TRUE)

  fuzzyValuesIncA <- matrix(c(0.25,0.5,1,0.25,0.25,1,1.5,0.7,1,0,0,2,1,1,2,0.2), ncol = 4,byrow = TRUE)

  # tests

  expect_error(BertoluzzaDistance(fuzzyValuesIncA[1,],fuzzyValuesIncA, increases = 5),
               "Parameter increases should have logical value")

  expect_error(BertoluzzaDistance(fuzzyValues[1, ], fuzzyValues, theta = "b"),
               "Parameter theta should be double value and > 0")

  expect_error(BertoluzzaDistance(fuzzyValues[1,],fuzzyValues, theta = -3),
               "Parameter theta should be double value and > 0")

  expect_error(BertoluzzaDistance(fuzzyValuesNA[1,],fuzzyValues),
               "There are some NA in fuzzyNumber1 parameter")

  expect_error(BertoluzzaDistance(fuzzyValues[1,],fuzzyValuesNA),
               "There are some NA in fuzzyNumber2 parameter")

  expect_error(BertoluzzaDistance(fuzzyValuesNotFuzzy[1,],fuzzyValues),
               "Some values in fuzzyNumber1 parameter are not correct fuzzy numbers")

  expect_error(BertoluzzaDistance(fuzzyValues5Elem[1,],fuzzyValues[2,]),
               "There should be 4 columns in fuzzyNumber1 parameter")

  expect_error(BertoluzzaDistance(strangeValues, fuzzyValues[2, ]),
               "Some values in fuzzyNumber1 parameter are not numeric ones")

  expect_error(BertoluzzaDistance(arrNotMatrix,fuzzyValues[2,]),
               "Values in fuzzyNumber1 parameter are not given as a matrix/vector")

  expect_error(BertoluzzaDistance(fuzzyValuesA, fuzzyValuesNotFuzzy[1, ]),
               "Some values in fuzzyNumber2 parameter are not correct fuzzy numbers")


})
