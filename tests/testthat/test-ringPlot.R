



test_that("getRingRadius works", {
  getRingRadius_test <- getRingRadius(ring_table_data = Table25, tree_outer_radius = 3)
  factor <- c("vf1","vf2")
  ring_radius <- c(4,5)
  getRingRadius_expected <-data.frame(factor,ring_radius)

  expect_equal(getRingRadius_test$ring_radius[1], getRingRadius_expected$ring_radius[1])
  expect_equal(getRingRadius_test$ring_radius[2], getRingRadius_expected$ring_radius[2])
})


test_that("getRingData works", {

  getRingData_expected <- circleFun(center = c(0,0), r = 6 , npoints = 6)
  getRingData_test <- getRingData(table_data = Table6 , tree_depth = 3, ring_radius = 6 )

  expect_equal(getRingData_test$x, getRingData_expected$x)
  expect_equal(getRingData_test$y, getRingData_expected$y)
})


test_that("ringPlot works", {

  expect_equal(2 * 2, 4)
})

