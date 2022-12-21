test_that("Function returns correct values", {


  # snapshots tests

  expect_snapshot(

    {set.seed(123456)

      GeneratorNExpUU(10, 0,1,1,1,2)}
  )


  expect_snapshot(

    {set.seed(123456)

      GeneratorNExpUU(5, 0,1,1,1,2,increases = TRUE)}
  )


  expect_snapshot(

    {set.seed(123456)

      GeneratorNExpUU(10, -2,1.4,2.5,1.5,2.5)}
  )

  expect_snapshot(

    {set.seed(123456)

      GeneratorNExpUU(10, -2,1.4,3,0,0,increases = TRUE)}
  )


  expect_snapshot(

    {set.seed(123456)

      GeneratorNExpUU(n=10,mu=0.5,sigma = 1,lambda = 0.5,b=1,c=2)}
  )

  expect_snapshot(

    {set.seed(123456)

      GeneratorNExpUU(n=10,mu=0.5,sigma = 1,lambda = 0.5,b=1,c=2,increases = TRUE)}
  )

})



test_that("Function reports errors", {



  # tests

  expect_error(GeneratorNExpUU(n=-2,mu=0.5,sigma = 1,lambda = 0.5,b=1,c=2),
               "Parameter n should be integer value and > 1")

  expect_error(GeneratorNExpUU(n=10,mu=c(1,"a"),sigma = 1,lambda = 0.5,b=1,c=2),
               "Parameter mu should be double value")

  expect_error(GeneratorNExpUU(n=NA,mu=10,sigma = 1,lambda = 0.5,b=1,c=2),
               "Parameter n should be integer value and > 1")

  expect_error(GeneratorNExpUU(n=10,mu=10,sigma = -1,lambda = 0.5,b=1,c=2),
               "Parameter sigma should be double value and > 0")

  expect_error(GeneratorNExpUU(n=10,mu=10,sigma = 1,lambda = -2,b=1,c=2),
               "Parameter lambda should be double value and > 0")

  expect_error(GeneratorNExpUU(n=10,mu=10,sigma = 1,lambda = 0,b=1,c=2),
               "Parameter lambda should be double value and > 0")

  expect_error(GeneratorNExpUU(n=10,mu=10,sigma = 1,lambda = 1,b=-1,c=2),
               "Parameter b should be double value and > 0")

  expect_error(GeneratorNExpUU(n=10,mu=10,sigma = 1,lambda = 1,b=1,c=-2),
               "Parameter c should be double value and > 0")

  expect_error(GeneratorNExpUU(n=10,mu=10,sigma = 1,lambda = 1,b=1,c=NA),
               "Parameter c should be double value and > 0")

  expect_error(GeneratorNExpUU(n=10,mu=10,sigma = 1,lambda = 1,b=1,c=2,increases = "b"),
               "Parameter increases should have logical value")


})


