test_that("Function returns correct values", {


  # tests

  expect_snapshot(

    {set.seed(1234567)

      GeneratorFuzzyNumbers(10,"rnorm",list(mean=0,sd=1),"rexp",list(rate=2),"runif",
                            list(min=0,max=0.6),"runif",list(min=0,max=1))}
  )


  expect_snapshot(

    {set.seed(1234567)

      GeneratorFuzzyNumbers(n=10,"rnorm",list(mean=0,sd=1),"rexp",list(rate=2),"runif",
                            list(min=0,max=0.6),"runif",list(min=0,max=1),increases = TRUE)}
  )


})




test_that("Function reports errors", {



  # tests

  expect_error(GeneratorFuzzyNumbers(n=-10,"rnorm",list(mean=0,sd=1),"rexp",list(rate=2),"runif",
                                     list(min=0,max=0.6),"runif",list(min=0,max=1)),
               "Parameter n should be integer value and > 1")

  expect_error(GeneratorFuzzyNumbers(n=3.4,"rnorm",list(mean=0,sd=1),"rexp",list(rate=2),"runif",
                                     list(min=0,max=0.6),"runif",list(min=0,max=1)),
               "Parameter n should be integer value and > 1")

  expect_error(GeneratorFuzzyNumbers(n=10,"rnorm",list(mean=0,sd=1),"rexp",list(rate=2),"runif",
                                     list(min=0,max=0.6),"runif",list(min=0,max=1),increases = NA),
               "Parameter increases should have logical value")





})

