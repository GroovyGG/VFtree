############################### test_treePlot.R
# Date: 5 October 2019
# Author: Bihan Zhu <bihan.zhu@mail.utoronto.ca>

###############################


test_that("treeInputProcess works", {

  expect_equal()

})

test_that("circleFun works", {
  x <- c(1.000000e+01,6.123234e-16,-1.000000e+01,-1.836970e-15,1.000000e+01)
  y <- c(0.000000e+00,1.000000e+01,1.224647e-15,-1.000000e+01,-2.449294e-15)
  try5 <- data.frame(x,y)

  x1 <- c(10,0)
  y1 <- c(10,-2.449294e-15)

  expect_equal(circleFun(npoints = 5), try5 )
  expect_equal(circleFun(npoints = 2), try2 )
})

test_that("getNPoints works", {

  expect_equal()
})

test_that("getLayers works", {

  expect_equal()
})

test_that("getCoordinates works", {

  expect_equal()
})

test_that("nodeGroup works", {

  expect_equal()
})

test_that("treePlot works", {

  expect_equal()
})

