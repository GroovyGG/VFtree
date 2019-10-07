############################### test_treePlot.R
# Date: 5 October 2019
# Author: Bihan Zhu <bihan.zhu@mail.utoronto.ca>

###############################


test_that("treeInputProcess works", {

  treeInputProcess_test <- treeInputProcess(tree = Tree100)

  expect_equal(treeInputProcess_test$depth[101],0)
  expect_equal(treeInputProcess_test$depth_length [101],0)
  expect_equal(max(treeInputProcess_test$depth),14)
  expect_equal(treeInputProcess_test$angle[1] ,0)
  expect_equal(treeInputProcess_test$parent[1],109)
  expect_equal(treeInputProcess_test$c1[101] ,100)

})

test_that("circleFun works", {
  x <- c(1.000000e+01,6.123234e-16,-1.000000e+01,-1.836970e-15,1.000000e+01)
  y <- c(0.000000e+00,1.000000e+01,1.224647e-15,-1.000000e+01,-2.449294e-15)
  circleFun_expected <- data.frame(x,y)

  expect_equal(circleFun(npoints = 5), circleFun_expected)

})

test_that("getNPoints works", {

  getNPoints_test1 <- getNPoints( ntips = 100)
  getNPoints_test2 <- getNPoints( ntips = 200)
  getNPoints_test3 <- getNPoints( ntips = 20,refine_factor = 30)

  expect_equal(getNPoints_test1, 10000)
  expect_equal(getNPoints_test2, 20000)
  expect_equal(getNPoints_test3, 600)
})

test_that("getLayers works", {
  t1 <-treeInputProcess(tree = Tree100)
  p1 <- getNPoints( ntips = 100)
  getLayers_test <- getLayers(data = t1, npoint = p1 , ntips = 100)

  expect_equal(max(getLayers_test$layer_id), 14)
  expect_equal(nrow(getLayers_test),13 * 10000 + 100 + 14)
  expect_equal(sum(getLayers_test$x), 105)
})

test_that("getCoordinates works", {
  t1 <-treeInputProcess(tree = Tree100)
  p1 <- getNPoints( ntips = 100)
  l1 <- getLayers(data = t1, npoint = p1 , ntips = 100)

  getCoordinates_test <- getCoordinates(tree =  t1, layers = l1, npoint = p1)
  expect_equal(nrow(getCoordinates_test),199)
  expect_equal(ncol(getCoordinates_test),11)

  tips_test<-filter(l1,l1$layer_id == max(l1$layer_id))
  tips_expected <- filter(getCoordinates_test,getCoordinates_test$c1 == -1)

  expect_equal(tips_test$x[1:100], tips_expected$x[1:100])
  expect_equal(nrow(getCoordinates_test),199)
})


