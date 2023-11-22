test_that("Function returns correct values", {


  # snapshots tests

  expect_snapshot(

    {set.seed(123456)

      GeneratorNU(10, 0,1,1,2)}
  )


  expect_snapshot(

    {set.seed(123456)

      GeneratorNU(5, 0,1,1,2,increases = TRUE)}
  )


  expect_snapshot(

    {set.seed(123456)

      GeneratorNU(8, -1,2,1.3,2.1)}
  )

  expect_snapshot(

    {set.seed(123456)

      GeneratorNU(5, 0,1,1,2,increases = FALSE)}
  )


  expect_snapshot(

    {set.seed(123456)

      GeneratorNU(n=5, mu=1.5,sigma=1.5 , a= 1.5, b=2,increases = FALSE)}
  )

  expect_snapshot(

    {set.seed(123456)

      GeneratorNU(n=5, mu=1.5,sigma=1.5 , a= 0, b=0,increases = FALSE)}
  )

})



test_that("Function reports errors", {



  # tests

  expect_error(GeneratorNU(n=-15, mu=1.5,sigma=1.5 , a= 1.5, b=2,increases = FALSE),
               "Parameter n should be integer value and > 0")

  expect_error(GeneratorNU(n=.2, mu=1.5,sigma=1.5 , a= 1.5, b=2,increases = FALSE),
               "Parameter n should be integer value and > 0")

  expect_error(GeneratorNU(n=NA, mu=1.5,sigma=1.5 , a= 1.5, b=2,increases = FALSE),
               "Parameter n should be integer value and > 0")

  expect_error(GeneratorNU(n=10, mu="c",sigma=1.5 , a= 1.5, b=2,increases = FALSE),
               "Parameter mu should be double value")

  expect_error(GeneratorNU(n=10, mu=0,sigma=-1.5 , a= 1.5, b=2,increases = FALSE),
               "Parameter sigma should be double value and > 0")

  expect_error(GeneratorNU(n=10, mu=0,sigma=1 , a= -1, b=2,increases = FALSE),
               "Parameter a should be double value and >= 0")

  expect_error(GeneratorNU(n=10, mu=0,sigma=1 , a= "c", b=2,increases = FALSE),
               "Parameter a should be double value and >= 0")

  expect_error(GeneratorNU(n=10, mu=0,sigma=1 , a= 1, b=-2,increases = FALSE),
               "Parameter b should be double value and >= 0")

  expect_error(GeneratorNU(n=10, mu=0,sigma=1 , a= 1, b=-3,increases = FALSE),
               "Parameter b should be double value and >= 0")

  expect_error(GeneratorNU(n=10, mu=0,sigma=1 , a= 1, b=3,increases = 5),
               "Parameter increases should have logical value")


})

